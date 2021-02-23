# This file contains all the functions and libraries needed for the COVID-19 calculator
library(DT)
library(readxl)
library(reshape2)
library(ggrepel)
library(mapview)
library(grid)
library(gpclib)
library(maptools)
library(scales)
library(rjson)
library(tidyverse)

initialize_webshot <- function(){
  if(!webshot::is_phantomjs_installed())
    webshot::install_phantomjs()
}

jira_issuecollector_code <- "
shinyjs.init = function(){
jQuery.ajax({
    url: \"https://jira.oberlab.de/s/85c54421d2c746860d226906598dbdc4-T/gwty9g/812003/6411e0087192541a09d88223fb51a6a0/4.0.3/_/download/batch/com.atlassian.jira.collector.plugin.jira-issue-collector-plugin:issuecollector-embededjs/com.atlassian.jira.collector.plugin.jira-issue-collector-plugin:issuecollector-embededjs.js?locale=de-DE&collectorId=8eb41bc9\",
    type: \"get\",
    cache: true,
    dataType: \"script\"
});
}
"

initialize_polygons <- function(){
  gpclibPermit()
  polygons_exist <- file.exists("gadm36_DEU_3_sp.rds")
  if(polygons_exist){
    landkreise_rds <- readRDS("gadm36_DEU_3_sp.rds")
    landkreis_mb_shape <- landkreise_rds[landkreise_rds$NAME_2=='Miesbach',]
    return (landkreis_mb_shape)
  }
 
}

rolling_sum <- function(x, n = 7L) tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1)

oberlab_colors <- c("#00a4c4", "#c7326c", "#fabb3a", "#3db8c1")

### Data extraction ###########################################################

## Extraction of the current data from the raw data file
extract_current_data <- function(ags="09182"){
  data <- fromJSON(file = paste0("https://api.corona-zahlen.org/districts/", ags))
  
  current_data<- as.data.frame(data$data[[ags]])
  current_data_meta <- as.data.frame(data$meta)
  
  current_data_out <- data.frame(
    "date" = as.Date(current_data_meta$lastCheckedForUpdate, format = "%Y-%m-%d"),
    "total_cases" = as.integer(current_data$cases),
    "active_cases" = as.integer(current_data$cases - current_data$recovered - current_data$deaths),
    "closed_cases" = as.integer(current_data$recovered),
    "deaths" = as.integer(current_data$deaths),
    "new_cases" = as.integer(current_data$delta.cases),
    "new_deaths" = as.integer(current_data$delta.deaths),
    "new_closed" = as.integer(current_data$delta.recovered),
    "incidence" = as.integer(round(current_data$weekIncidence))
  )
  
  return(current_data_out)
}

extract_short_plot_data <- function(ags="09182"){
  raw_incidence14 <- fromJSON(file = paste0("https://api.corona-zahlen.org/districts/", ags, "/history/incidence/14"), method='C')
  raw_cases14 <- fromJSON(file = paste0("https://api.corona-zahlen.org/districts/", ags, "/history/cases/14"), method='C')
  
  #Aus dem RKI-Datensatz die richtigen Daten herauslesen
  incidence14 <- raw_incidence14$data[[ags]]$history
  incidence14 <- ldply(incidence14, data.frame)
  incidence14$weekIncidence <- round(incidence14$weekIncidence,0)
  incidence14$date <- as.Date(incidence14$date,"%Y-%m-%d")
  
  
  cases14<- raw_cases14$data[[ags]]$history
  cases14 <- ldply(cases14, data.frame)
  cases14$date <- as.Date(cases14$date,"%Y-%m-%d")
  
  # Daten so herrichten, dass sie mit unseren üblichen Plots funktionieren
  colnames(incidence14) <- c('7-Tage-Inzidenz','Datum')
  colnames(cases14) <- c('Neuinfektionen','Datum')
  
  short_plot_dataframe <- merge(incidence14, cases14, by='Datum')
  short_plot_dataframe <- melt(short_plot_dataframe, id.vars=c("Datum"), na.rm = TRUE)
  return(short_plot_dataframe)
}
  

extract_final_data <- function(old_data, new_data){
  
  data_before_sep <- read.csv(old_data)
  data_before_sep$date <- as.Date(data_before_sep$date, "%Y-%m-%d")
  
  #Berechnung der geschlossenen Fälle
  
  closed_per_date <- as.data.frame(table(new_data$abgeschlossen))
  closed_per_date$Var1 <- as.Date(closed_per_date$Var1, "%Y-%m-%d")
  closed_per_date <- closed_per_date[closed_per_date$Var1 > "2020-08-31",]
  
  closed_cases_end_august <- 641 +3 #drei Personen von außerhalb des Landkreises wurden fälschlicherweise mitgeführt. Sie werden als "genesen" geführt.
  closed_per_date$cumulative <- cumsum(closed_per_date$Freq) + closed_cases_end_august
  # Hier kontrolle einbauen!
  
  #Berechnung der Todesfälle
  
  dead_per_date <- as.data.frame(table(new_data$verstorben))
  dead_per_date$Var1 <- as.Date(dead_per_date$Var1, "%Y-%m-%d")
  dead_per_date <- dead_per_date[dead_per_date$Var1 > "2020-08-31",]
  dead_end_august <- 10
  dead_per_date$cumulative <- cumsum(dead_per_date$Freq) + dead_end_august

  #Hier Kontrolle einbauen!
  
  #Berechnung der neuen Fälle
  
  new_cases <- as.data.frame(table(new_data$Datum)) #Summiert wieviele Fälle an einem Tag aufkamen
  colnames(new_cases) <- c("date", "new_cases")
  new_cases$date <- as.Date(new_cases$date, "%Y-%m-%d")
  
  #Berechnung der 7-Tage Inzidenz
  #new_cases$incidence <- c(rep.int(0,6) ,rolling_sum(new_cases$new_cases))
  
  data_after_sep <- data.frame(
    "date" = seq(as.Date("2020-09-01"), max(new_data$Datum, na.rm = TRUE), "days"),
    "total_cases" = NA,
    "active_cases" = NA,
    "closed_cases" = NA,
    "deaths" = NA,
    "new_cases" = NA,
    "incidence" = NA
    
  )
  
  for (date in data_after_sep$date){
    
    case_nr <- max(new_data$Fallnummer[new_data$Datum <= date], na.rm = TRUE)
    closed <- closed_per_date$cumulative[closed_per_date$Var1 == max(closed_per_date$Var1[closed_per_date$Var1 <= date])]
    new <- new_cases$new_cases[new_cases$date == date]
    if(length(new) == 0){
      new <- 0
    }
    
    if(length(dead_per_date$cumulative) == 0){
      dead <- 10
      
    } else if(length(dead_per_date$cumulative[dead_per_date$Var1 <= date]) == 0) {
      dead <- 10
      
    } else {
      dead <- tail(dead_per_date$cumulative[dead_per_date$Var1 <= date], n=1)
      print(dead)
    }
    
    data_after_sep$total_cases[data_after_sep$date == date] <- case_nr
    data_after_sep$closed_cases[data_after_sep$date == date] <- closed
    data_after_sep$deaths[data_after_sep$date == date] <- dead
    data_after_sep$new_cases[data_after_sep$date == date] <- new
    data_after_sep$active_cases <- data_after_sep$total_cases - data_after_sep$closed_cases - data_after_sep$deaths  #remove the 3 cases which were falsly registered in LK MB
    
  }
  
  population <- 100010
  data_after_sep$incidence <- round((c(rep.int(0,6) ,rolling_sum(data_after_sep$new_cases))/population)*100000, 0)
  data_output <- rbind(data_before_sep, data_after_sep)
  
  #Übergang bei der Inzidenz schaffen
  
  uebergang <- rolling_sum(data_output$new_cases[data_output$date > "2020-08-20"  & data_output$date < "2020-09-10"])
  
  data_output$incidence[data_output$date > (as.Date("2020-08-20")+6) & data_output$date < "2020-09-10"] <-  uebergang
  
  return(data_output)
}

extract_active_locations <- function(raw_data){
  active_locations <- raw_data$Postleitzahl[raw_data$Status == 1] #Plz aller aktiven Fälle
  location_summary <- data.frame(table(active_locations))
  location_summary$active_locations <- as.integer(as.character(location_summary$active_locations))

  final_location_data <- read.csv("plz_ref.csv") #Referenztabelle der Postleitzahlen
  final_location_data$cases <- NA

  
  for (plz in final_location_data$PLZ){
    if(length(location_summary$Freq[location_summary$active_location == plz]) == 0){
      final_location_data$cases[final_location_data$PLZ == plz] <- 0L
    } else {
      final_location_data$cases[final_location_data$PLZ == plz] <- location_summary$Freq[location_summary$active_location == plz] 
    }
  }

  if(sum(location_summary$Freq) != sum(final_location_data$cases)){
    # check which zip codes outside the region were provided
    not_considered <- location_summary$active_locations[!location_summary$active_locations %in% final_location_data$PLZ]
    difference <- sum(location_summary$Freq) - sum(final_location_data$cases)
    warning(paste0("Achtung! Es konnten "), difference, " Fällen keine Gemeinde im Landkreis zugeordnet werden. Die entsprechenen Postleitzahlen lauten: ", paste(shQuote(not_considered), collapse=", "))
  }
  
  final_location_data$Datum <- replicate(nrow(final_location_data), format(max(raw_data$Datum, na.rm = TRUE)), "%d.%m.%Y")
  return(final_location_data)
}

reduce_rows <-function(dataframe, n){
  dataframe[(seq(n,to=nrow(dataframe),by=n)),]
}

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

### Daten Auswertung ###########################################################


### Plot Theme

set_font <- "Roboto Mono"
set_background <- "white"
theme_oberlab_white = function(base_size = 12, base_family = "") {
  
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "black", lineheight = 0.9, family = set_font),  
      axis.text.y = element_text(size = base_size*0.9, color = "black", lineheight = 0.9, family = set_font),  
      #axis.ticks = element_line(color = "black", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "black", margin = margin(10, 20, 0, 0), family = set_font),  
      axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0), family = set_font),  
      #axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = set_background),  
      legend.key = element_rect(color = set_background,  fill = set_background),  
      legend.key.size = unit(1, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size, color = "black", family = set_font),  
      #legend.title = element_text(size = base_size*0.9, hjust = 0, color = "black", family = set_font),
      legend.title = element_blank(),
      legend.position = "bottom",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "horizontal",  
      legend.box = NULL, 
      legend.justification = "left",
      # Specify panel options
      panel.background = element_rect(fill = set_background, color  =  "white"),  
      panel.border = element_rect(fill = NA, color  =  "grey95"), 
      panel.grid.major = element_line(color = "grey95"),  
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "black", family = set_font),  
      strip.text.y = element_text(size = base_size*0.8, color = "black",angle = -90, family = set_font),  
      # Specify plot options
      plot.background = element_rect(color = set_background, fill = set_background),  
      plot.title = element_text(size = base_size*1.2, color = "#c7326c", family = set_font, vjust = 5),  
      plot.margin = unit(c(2,10,0.5,0.1) , "lines"),
      plot.caption = element_text(size = base_size, vjust = 5, hjust = 1)
      
    )
  
}