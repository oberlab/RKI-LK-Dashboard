library(shiny)
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
      a("Feedback geben", href="https://jira.oberlab.de/rest/collectors/1.0/template/form/4820f6d3", target="_self", class="atlwdg-trigger atlwdg-RIGHT")
      
  
)
 
))
  
