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
		data_after_sep$active_cases <- data_after_sep$total_cases - data_after_sep$closed_cases - data_after_sep$deaths
		
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

