library(shiny)
library(colourpicker)
source("fun.R") # includes all functions/libraries

shinyUI(fluidPage(
  titlePanel("Landkreis Dashboard für die Corona Daten des RKI"),
  includeCSS("jira.css"),
  navlistPanel(
      "RKI Daten",
      tabPanel("Aktuelle Daten", 
               tableOutput("new_numbers_table"),
               plotOutput("daily_summary_plot", width = "800px"),
               downloadButton(outputId = "daily_summary_download",
                              label = "Grafik Herunterladen")),

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
               column(4, downloadButton(outputId = "short_plot_download",
                              label = "Grafik Herunterladen")), 
               column(4, sliderInput("labelx_short", "X-Position der Logos",
                           min = 0, max = 10, value = 0, step = 0.5), 
               sliderInput("labely_short", "Y-Position der Logos",
                           min = 0, max = 200,value = 0, step = 5)),
               column(4 ,sliderInput("logo_size_short", "Größe der Logos",
                    min = 0, max = 5,value = 1, step = 0.5),
                    sliderInput("short_label_pos", "Position der Label",
                               min = -20, max = 20,value = 0, step = 1))), 
      a("Feedback geben", href="https://jira.oberlab.de/rest/collectors/1.0/template/form/8eb41bc9", target="_self", class="atlwdg-trigger atlwdg-RIGHT")
      
  
)
 
))
  
