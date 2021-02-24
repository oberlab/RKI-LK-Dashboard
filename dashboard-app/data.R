
## Extraction of the current data from the raw data file
extract_current_data <- function(ags="09182"){
  data <- fromJSON(file = paste0("https://api.corona-zahlen.org/districts/", ags))
  
  current_data<- as.data.frame(data$data[[ags]])
  current_data_meta <- as.data.frame(data$meta)
  
  current_data<- as.data.frame(data$data[[ags]])
  current_data_meta <- as.data.frame(data$meta)
  
  call_date <- as.POSIXct(current_data_meta$lastCheckedForUpdate, format("%Y-%m-%dT%H:%M:%S"), tz ="UTC")
  call_date <- call_date + 1*60*60 #um die Stunde Zeitverschiebung einzubeziehen
  
  data_date <- as.POSIXct(current_data_meta$lastUpdate, format("%Y-%m-%dT%H:%M:%S"), tz ="UTC")
  
  current_data_out <- data.frame(
    "data_date" = data_date,
    "call_date" = call_date,
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
  
  #Abrufzeiten ziehen
  raw_incidence14_date <- raw_incidence14$meta$lastUpdate
  raw_cases14_date <- raw_cases14$meta$lastUpdate
  
  if(raw_cases14_date == raw_incidence14_date){
    data_date <- as.POSIXct(raw_incidence14_date, format("%Y-%m-%dT%H:%M:%S"), tz ="UTC")
  } else {
    data_date <- NA
  }
  
  call_date <- as.POSIXct(raw_cases14$meta$lastCheckedForUpdate, format("%Y-%m-%dT%H:%M:%S"), tz ="UTC")
  call_date <- call_date + 1*60*60 #um die Stunde Zeitverschiebung einzubeziehen
  
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
  
  output <- list(
    "data" = short_plot_dataframe,
    "data_date" = data_date,
    "call_date" = call_date
    
  )
  return(output)
}
