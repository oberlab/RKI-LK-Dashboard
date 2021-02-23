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
    # Daten für "Werte über die Zeit" und "Kurzfristige Entwicklung" berechnen
    #
    #############################################
    
    get_summary_data <- reactive({
      
        new_data <- read_data() # Daten einlesen
        if (is.null(new_data))
            return(NULL)
        
        new_data <- new_data[!is.na(new_data$Datum),] #Extrahiere Datensätze mit einem Datum
        new_data$Datum <- as.Date(new_data$Datum, "%Y-%m-%d") #Umwandeln in Datetime Objekt
        new_data$abgeschlossen <- as.Date(as.numeric(new_data$abgeschlossen), origin = "1899-12-30") #Umwandeln in Datetime Objekt
        new_data$verstorben <- as.Date(as.numeric(new_data$verstorben), origin = "1899-12-30") #Umwandeln in Datetime Objekt
        
        # Den kompletten Datensatz einlesen und mit den Daten aus der Zeit vor September kombinieren
        data <- extract_final_data("data_before_september.csv", new_data)

        # Die Spaltennamen umbenennen
        colnames(data) <- c("Datum", "Fälle", "Aktive Fälle", "Genesen", "Todesfälle", "Neuinfektionen", "7-Tage-Inzidenz")
        # Zusätzlich zu den normalen Daten auch noch einen Datensatz mit "Melted" Daten erstellen
        data_melted <- melt(data, id.vars=c("Datum"), na.rm = TRUE)
        data_melted$Datum <- as.Date(data_melted$Datum, "%d.%m.%Y") #Umwandeln in Datetime Objekt
        
        # Output-Objekt enthält sowohl normale, aus auch melted-Daten
        output <- list(
            "normal" = data,
            "melted" = data_melted)
        
        return(output)
    })
    
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
    # Output "Daten Gemeinden" darstellen
    #
    #############################################
    output$location_table <- renderTable({
        location_data <- get_location_data()
        if(is.null(location_data))
            return('Keine Daten. Bitte Datensatz in "Tägliches Update" hochladen')
        
        if(!is.null(location_data) & input$sort_alphabetically){
            location_data <- location_data[order(location_data$Gemeinde),]
        }
        
        print(sum(location_data$cases))
        #return data without "searchString", "lat" and "long" column" for display
        subset(location_data, select=-c(searchString, lat, long))
    })
    
    #############################################
    #
    # Output "Zusammenfassung" darstellen
    #
    #############################################    
    output$summary_table <- DT::renderDataTable({
        summary <- get_summary_data()$normal
        if(is.null(summary))
            return(NULL)
        if (input$reverse_summary){
            summary <- summary[order(nrow(summary):1),] # inverts the list
        }
        
        return(summary)
        
    })

    #############################################
    #
    # Output "Kartendarstellung" darstellen
    #
    #############################################
    output$map_plot <- renderPlot({
      landkreis_map()
    })
    
    landkreis_map <- reactive({
      create_landkreis_plot(get_location_data(), input$maxFaelleSlider, input$use_oberlab_palette, oberlab_logo, landkreis_logo, input$labelx_map, input$labely_map)
    })

    #############################################
    #
    # Output "Werte über die Zeit" darstellen
    #
    #############################################        
    output$summary_plot <- renderPlot({
        summary_data <- get_summary_data()$melted
        if(is.null(summary_data))
            return(NULL)
        
        subset_summary <- subset(summary_data, variable %in% c("Fälle", "Aktive Fälle", "Genesen", "Todesfälle"))
        
        create_summary_plot(subset_summary, input$label_summary, oberlab_logo, landkreis_logo, input$fill_it, input$label_it, input$labelx_time, input$labely_time)
        
    })

    
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
    # Zusammenfassung herunterladen
    #
    #############################################
    
    output$summary_table_download <- downloadHandler(
      filename = function(){
        file <- input$raw_upload
        name <- file$name
        gsub(".xlsx", "_ausgewertet.csv", name)
      },
      content = function(file){
        summary <- get_summary_data()$normal
        if(input$excel_compatible){
          write_csv2(summary, file)
        } else {
          write_csv(summary, file)
        }
        
      })
    
    #############################################
    #
    # "Werte über die Zeit" herunterladen
    #
    #############################################    
    output$summary_plot_download <- downloadHandler(
      filename = function(){
        file <- input$raw_upload
        name <- file$name
        gsub(".xlsx", "_summaryplot.png", name)
      },
      content = function(file){
        summary_data <- get_summary_data()$melted
        subset_summary <- subset(summary_data, variable %in% c("Fälle", "Aktive Fälle", "Genesen", "Todesfälle"))
        ggsave(create_summary_plot(subset_summary, input$label_summary, oberlab_logo, landkreis_logo, input$fill_it, input$label_it), filename = file, height = 15, width = 30, units = "cm")
      })
    
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

