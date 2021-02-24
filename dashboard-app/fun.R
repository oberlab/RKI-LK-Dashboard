# This file contains all the functions and libraries needed for the COVID-19 calculator


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

rolling_sum <- function(x, n = 7L) tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1)

oberlab_colors <- c("#00a4c4", "#c7326c", "#fabb3a", "#3db8c1")

### Data extraction ###########################################################


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