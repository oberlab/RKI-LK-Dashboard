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
    
    initialize_webshot()
    
    read_data <- reactive({
        file <- input$raw_upload
        if (is.null(file))
            return(NULL)
        
        data <- read_excel(file$datapath)

        return(data)
    })
    
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
        
        warning <- tryCatch(extract_current_data(), warning = function(w) { w })
        
        if(!is.null(warning)){
          showNotification(warning$message, type = "warning", duration = 0)
        }
        #Anpassung der Spaltennamen zur besseren Darstellung
        colnames(current_data) <- c("Datum", "Fälle Gesamt", "Akive Fälle", "Gesundete", "Todesfälle", "Neuinfizierte", "Neue Todesfälle", "Neue Genesene", "Inzidenz")
        
        return(current_data)
    })
    
    #############################################
    #
    # Daten für "Daten Gemeinden" berechnen
    #
    #############################################    
    get_location_data <- reactive({
        new_data <- read_data()
        if (is.null(new_data))
            return(NULL)
        
        active_locations <- extract_active_locations(new_data)
        
        warning <- tryCatch(extract_active_locations(new_data), warning = function(w) { w })
        
        if(!is.null(warning)){
            showNotification(warning$message, type = "warning", duration = 0)
        }
        
        
        return(active_locations)
    })
    
    
    #############################################
    #
    # Daten für "Entwicklung pro Gemeinde" berechnen
    #
    ############################################# 
    get_plz_data <- reactive({
      
      new_data <- read_data() # Daten einlesen
      if (is.null(new_data))
        return(NULL)
      
      new_data <- new_data[!is.na(new_data$Datum),] #Extrahiere Datensätze mit einem Datum
      new_data$Datum <- as.Date(new_data$Datum, "%Y-%m-%d") #Umwandeln in Datetime Objekt
      new_data$abgeschlossen <- as.Date(as.numeric(new_data$abgeschlossen), origin = "1899-12-30") #Umwandeln in Datetime Objekt
      new_data$verstorben <- as.Date(as.numeric(new_data$verstorben), origin = "1899-12-30") #Umwandeln in Datetime Objekt
      
      #Berechnung der neuen Fälle
      
      new_cases <- as.data.frame(table(new_data$Datum, new_data$Postleitzahl)) #Summiert wieviele Fälle an einem Tag aufkamen
      colnames(new_cases) <- c("date",  "plz", "anz_faelle")
      new_cases$date <- as.Date(new_cases$date, "%Y-%m-%d")
      
      #------------------------------------
      #Berechnung der 7-Tage Inzidenz
      #------------------------------------
      
      # Die CSV-Datei einlesen, in der die PLZ und Einwohnerzahlen enthalten sind
      final_location_data <- read.csv("plz_ref.csv") 
      inzidenz_pro_gemeinde <- merge(new_cases,final_location_data[,c("einwohner","PLZ")], by.x="plz",by.y="PLZ")
      inzidenz_pro_gemeinde$incidence <- round((c(rep.int(0,6) ,rolling_sum(inzidenz_pro_gemeinde$anz_faelle))/inzidenz_pro_gemeinde$einwohner)*100000, 0)
      
      # Die Spaltennamen umbenennen
      colnames(inzidenz_pro_gemeinde) <- c(  "PLZ", "Datum", "Neuinfektionen","Einwohner","7-Tage-Inzidenz")
      
      
      # Zusätzlich zu den normalen Daten auch noch einen Datensatz mit "Melted" Daten erstellen
      data_melted <- melt(inzidenz_pro_gemeinde, id.vars=c("Datum","PLZ"), na.rm = TRUE)
      data_melted$Datum <- as.Date(data_melted$Datum, "%d.%m.%Y") #Umwandeln in Datetime Objekt
      
      #Info zu den verschiedenen Orten einfügen (-> wird für die Beschriftung der Plots gebraucht)
      data_plz <- final_location_data[,c("PLZ","Gemeinde","einwohner")]
      
      # Output-Objekt enthält sowohl normale, aus auch melted-Daten
      output <- list(
        "normal" = data,
        "melted" = data_melted,
        "plz_info" = data_plz)
      
      return(output)
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
        get_current_data()
            
    })

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
      
      create_short_plot(short_plot_data, oberlab_logo, landkreis_logo, input$labelx_short, input$labely_short)
    })
    
    #############################################
    #
    # Output "Entwicklung pro Gemeinde" darstellen
    #
    #############################################    

    output$short_plot_plz <- renderPlot({
      prepare_short_plot_plz()
    })    
    
    prepare_short_plot_plz <- reactive({

      plz_data <- get_plz_data()
      
      summary_data <- plz_data$melted
      if(is.null(summary_data))
        return(NULL)
      
      # Nur die letzten 21 Tage im Bericht anzeigen
      max_date <- max(summary_data$Datum)
      last_reporting_date <- max_date-14
      subset_short <- subset(summary_data, Datum > last_reporting_date & variable %in% c("Neuinfektionen", "7-Tage-Inzidenz", "PLZ"))
      
      # Informationen zum aktuell gesuchten Ort ermitteln
      ort_info <- plz_data$plz_info[plz_data$plz_info$PLZ==input$short_plot_plz_auswahl,]
      
      
      create_short_plot_plz(subset_short, ort_info, oberlab_logo, landkreis_logo, input$labelx_plz_short, input$labely_plz_short)
    })

        
###########################################
# Download Handler
###########################################
    

    #############################################
    #
    # Kartendarstellung herunterladen
    #
    #############################################
    output$lkrMapDl <-  downloadHandler(
      filename = function(){
        file <- input$raw_upload
        name <- file$name
        gsub(".xlsx", "_karte.png", name)
      },
      content = function(file){
        ggsave(landkreis_map(), filename = file, height = 30, width = 30, units = "cm")
      }
     # end of content() function
    ) # end of downloadHandler() function

    
    #############################################
    #
    # "Daten Gemeinden" herunterladen
    #
    #############################################
    output$gemeinde_daten <- downloadHandler(
      filename = function(){
        paste0("GemeindeDaten_", Sys.Date(), ".csv")
      },
      
      content = function(file){
        location_data <- get_location_data()
        if(!is.null(location_data) & input$sort_alphabetically){
          location_data <- location_data[order(location_data$Gemeinde),]
        }
        clean_data <- subset(location_data, select=-c(searchString, lat, long))
        
        if(input$excel_compatible){
          write_csv2(clean_data, file)
        } else {
          write_csv(clean_data, file)
        }
      }
    )
    
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
        file <- input$raw_upload
        name <- file$name
        gsub(".xlsx", "_shortplot.png", name)
      },
      content = function(file){
        ggsave(prepare_short_plot(), filename = file, height = 15, width = 30, units = "cm")
      })
})

