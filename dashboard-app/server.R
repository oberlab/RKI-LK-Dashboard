library(shiny)
source("fun.R")
source("data.R")
source("plots.R")

shinyServer(function(input, output) {
  
  
###########################################
# Initialisierungen  
###########################################  
  
  
  
    oberlab_logo <- get_png("oberlab_logo.png")
    landkreis_logo <- get_png("landkreis_logo.png")
    logos <- get_png("logos.png")
    background <- get_png("data_background.png")
    
    initialize_webshot()
    
    
###########################################
# Daten  
###########################################      
    
    #############################################
    #
    # Daten für "Neue Zahlen" berechnen
    #
    #############################################
    
    get_current_data <- reactive({
        
        current_data <- extract_current_data()
        
        return(current_data)
    })
    

    
    
###########################################
# Outputs
###########################################
    
    #############################################
    #
    # Output "Neue Zahlen" darstellen
    #
    #############################################
    output$new_numbers_table <- renderTable({
         data <- get_current_data()
         colnames(data) <- c("Datenstand", "Letzter Abruf", "Fälle Gesamt", "Akive Fälle", "Gesundete", "Todesfälle", "Neuinfizierte", "Neue Todesfälle", "Neue Genesene", "Inzidenz")
         data[,1] <- format(data[,1], "%d.%m.%Y")
         data[,2] <- format(data[,2], "%d.%m.%Y")
         return(data)   
    })
    
    #############################################
    #
    # Output "Daily Update" (Plot)darstellen
    #
    #############################################
    output$daily_summary_plot <- renderPlot({
        daily_summary()
    })
    
    daily_summary <- reactive(({
        data <- get_current_data()
        create_daily_summary(data, background)
    }))
   

    
    #############################################
    #
    # Output "Kurzfristige Entwicklung" darstellen
    #
    #############################################
    output$short_plot <- renderPlot({
      prepare_short_plot()
    })
        
    prepare_short_plot <- reactive({
      
      short_plot_data <- extract_short_plot_data()
      
      create_short_plot(short_plot_data$data, logos, input$labelx_short, input$labely_short, input$logo_size_short, input$short_label_pos, short_plot_data$data_date, short_plot_data$call_date)
    })
    
        
###########################################
# Download Handler
###########################################
    
    
    #############################################
    #
    # "Kurzfristige Entwicklung" herunterladen
    #
    #############################################    
    output$short_plot_download <- downloadHandler(
      filename = function(){
        
        paste0(Sys.Date(), "_shortplot.png")
      },
      content = function(file){
        ggsave(prepare_short_plot(), filename = file, height = 15, width = 30, units = "cm")
      })
    
    #############################################
    #
    # "Daily Summary" herunterladen
    #
    #############################################    
    output$daily_summary_download <- downloadHandler(
        filename = function(){
            
            paste0(Sys.Date(), "_summary.png")
        },
        content = function(file){
            ggsave(daily_summary(), filename = file, width = 10, units = "in")
        })
})

