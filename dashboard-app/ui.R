library(shiny)
library(colourpicker)
source("fun.R") # includes all functions/libraries

shinyUI(fluidPage(
  titlePanel("Landkreis Dashboard für die Corona Daten des RKI"),
  includeCSS("jira.css"),
  navlistPanel(
      "RKI Daten",
      tabPanel("Aktuelle Daten", 
               tableOutput("new_numbers_table")),

#      tabPanel("Werte über die Zeit", 
#               plotOutput("summary_plot", 
#                          width = "1200px", 
#                          height = "600px"),
#               downloadButton(outputId = "summary_plot_download",
#                              label = "Grafik Herunterladen"), 
#               checkboxInput("fill_it", 
#                             label = "Flächen unter den Linien füllen", 
#                             value = TRUE), 
#               checkboxInput("label_it", 
#                             label = "Label mit Zahlen hinzufügen", 
#                             value = TRUE), 
#               sliderInput("label_summary", 
#                           label = "Anzahl der angezeigten Datenpunkte", 
#                           min = 1, max = 50, value = 15), 
#               sliderInput("labelx_time", "X-Position des Logos",
#                           min = 0, max = 100,value = 0, step = 10), 
#               sliderInput("labely_time", "Y-Position des Logos",
#                           min = 0, max = 1000,value = 0, step = 100)),
      tabPanel("Kurzfristige Entwicklung", 
               plotOutput("short_plot", width = "1200px", height = "600px"), 
               downloadButton(outputId = "short_plot_download",
                              label = "Grafik Herunterladen"), 
               sliderInput("labelx_short", "X-Position des Logos",
                           min = 0, max = 10,value = 2, step = 0.5), 
               sliderInput("labely_short", "Y-Position des Logos",
                           min = 0, max = 200,value = 50, step = 10)),
      a("Feedback geben", href="https://jira.oberlab.de/rest/collectors/1.0/template/form/8eb41bc9", target="_self", class="atlwdg-trigger atlwdg-RIGHT")
      
  
)
 
))
  
