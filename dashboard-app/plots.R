### Hier entstehen die Plots ###################################################


create_landkreis_plot <- function(location_data, maxFaelleThreshold, use_oberlab_palette, logo, logo2, logox = 0.00, logoy = 0.00){
	
	set_font <- "Roboto Mono" 
	
	if(!is.null(location_data)){
		landkreis_mb_shape <- initialize_polygons()
		gemeinden_in_mb <- landkreis_mb_shape[landkreis_mb_shape$NAME_2=='Miesbach',]
		
		oberlab_palette <- c("#00a4c4", "#3db8c1", "#fabb3a","#c7326c")
		normal_palette <- c("#339933", "#339933", "#fabb3a","#ff0000")
		if(use_oberlab_palette){
			used_palette <- oberlab_palette  
		}else{
			used_palette <- normal_palette  
		}
		
		gemeinden_fortification <- fortify(gemeinden_in_mb, region = "NAME_3")
		print(gemeinden_fortification)
		faelle_labels <- aggregate(cbind(long,lat) ~ id, data = gemeinden_fortification, FUN = mean)
		faelle_labels <-merge(location_data[,c("Gemeinde","cases")], faelle_labels, by.x="Gemeinde", by.y="id", all=T)
		print(faelle_labels)
		faelle_labels <- mutate(faelle_labels,label=paste(faelle_labels$Gemeinde,"\n", faelle_labels$cases))
		print(faelle_labels)
		
		einfache_lkr_map <- fortify(gemeinden_in_mb, region = "NAME_3")
		einfache_lkr_map <-merge(einfache_lkr_map,location_data[,c("Gemeinde","cases","einwohner")],by.x="id",by.y="Gemeinde")
		print(einfache_lkr_map)
		einfache_lkr_map <- mutate(einfache_lkr_map,quoten=einfache_lkr_map$cases/einfache_lkr_map$einwohner*100)
		print(einfache_lkr_map)
		
		einfache_lkr_ggplot_map <- ggplot(einfache_lkr_map) + 
			geom_polygon(aes(x = long, y = lat, group = group, fill=quoten), colour = "white") +
			labs(title = "Aktive COVID-19 Fälle nach Gemeinden des Landkreis Miesbach",
					 subtitle = paste0("Lizenz: CC-BY-SA, FabLab Oberland e.V., ","Datenstand: ", format(as.Date(max(location_data$Datum)), "%d.%m.%Y"), ", ",format(Sys.time(),"%H:00"))) +
			theme_void()  +
			theme(legend.position = c(0.5,0),
						legend.direction = "horizontal",
						legend.key.width=unit(2,"cm"),
						legend.text = element_text(size = rel(1.0), color = "black", family = set_font),
						legend.title = element_text(size = rel(1.0), color = "black", family = set_font),
						legend.text.align = 0,
						legend.title.align = 0,
						plot.title = element_text(size = 12*1.2, color = "#c7326c", family = set_font, vjust = 5),  
						plot.margin = unit(c(2,10,4.5,0.1) , "lines"),
						plot.caption = element_text(size = 12, vjust = 5, hjust = 1))
		
		einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map +
			geom_text(data = faelle_labels, aes(x=long, y=lat, label=label), fontface = "bold", fontfamily = "Roboto Mono") +
			annotation_custom(grob = textGrob("Erstellt mit \u2661\n in Zusammenarbeit von",
																				gp=gpar(fontfamily = "Roboto Mono", fontsize = 12*0.9)),
												xmin=11.85+logox, xmax=12.00+logox, ymin=47.91+logoy, ymax=Inf) +
			annotation_custom(logo, xmin=11.85+logox, xmax=11.95+logox, ymin=47.89+logoy, ymax=47.92+logoy) +
			annotation_custom(logo2, xmin=11.95+logox, xmax=12.0+logox, ymin=47.89+logoy, ymax=47.92+logoy)
		
		if(maxFaelleThreshold == 0){
			einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map + 
				theme(legend.position = "none")
			
			used_palette <- c("#3db8c1")
		}
		
		
		
		#einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map + scale_fill_distiller(name = "Aktive\nFälle", palette = "RdYlGn")
		einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map + scale_fill_gradientn(name="Relative Betroffenheit nach\nGemeinde (in % der Einwohner)",colours = used_palette, limits=c(0,maxFaelleThreshold),oob=squish)
		return (einfache_lkr_ggplot_map)
	}
	
	
	
}

create_summary_plot <- function(data, num_labels_per_set, logo, logo2, show_fill = TRUE, label_it = TRUE, logox = 0, logoy = 0){
	
	max_date <- max(data$Datum)
	min_date <- min(data$Datum)
	last_reporting_date <- max_date -21
	
	
	# 1 Label pro Woche
	
	nr_of_labels_value <- length(unique(data$Datum)) / num_labels_per_set
	data_reduced = reduce_rows(data, nr_of_labels_value)
	
	plot <- ggplot(data, aes(Datum, value, color = variable)) + 
		geom_line(size = 1) + 
		#geom_label_repel(data = reduce_rows(data, nr_of_labels_value), aes(label=value, color=variable ), segment.color = "black", show.legend = FALSE, box.padding = 0.4, point.padding = 0.1, ) +
		
		ylab("") +
		xlab("") +
		labs(color="") +
		labs(title = "COVID-19 Fälle im Landkreis Miesbach",
				 caption = paste0("Lizenz: CC-BY-SA, FabLab Oberland e.V., ","Datenstand: ", format(max_date, "%d.%m.%Y"), ", ",format(Sys.time(),"%H:00"))) +
		theme_oberlab_white() +
		annotation_custom(logo, xmin=min_date + logox, xmax=min_date +40+ logox, ymin=1000 + logoy, ymax=1300 + logoy) +
		annotation_custom(logo2, xmin=min_date +45 + logox, xmax=min_date +75+ logox, ymin=1000 + logoy, ymax=1300 + logoy) +
		coord_cartesian(clip = "off") +
		theme(plot.margin = unit(c(1, 1, 3, 1), "lines")) +
		scale_color_manual(values = c("#00a4c4", "#c7326c", "#fabb3a", "#000000")) +
		scale_fill_manual(values = c("#00a4c4", "#c7326c", "#fabb3a", "#000000")) +
		scale_x_date(date_labels = "%b", breaks = "month") +
		theme(plot.margin = unit(c(2,1,0.5,0) , "lines"), legend.margin = margin(t=-1), axis.title.x = element_blank())+
		annotation_custom(grob = textGrob("Erstellt mit \u2661\n in Zusammenarbeit von",
																			gp=gpar(fontfamily = "Roboto Mono", fontsize = 12*0.9)),
											xmin=min_date+25+ logox, xmax=min_date+50+ logox, ymin=1350+ logoy, ymax=1550+ logoy)
	
	if(show_fill){
		plot <- plot +
			geom_area(data = data[data$variable == "Fälle",],aes(fill = "#00a4c4"), alpha = 0.5, show.legend = FALSE) +
			geom_area(data = data[data$variable == "Genesen",], aes(fill = "#fabb3a"), alpha = 0.8, show.legend = FALSE) +
			geom_area(data = data[data$variable == "Aktive Fälle",], aes(fill = "#c7326c"), alpha = 0.7, show.legend = FALSE) 
		#geom_area(data = data[data$variable == "Todesfälle",], aes(fill = "#000000"), alpha = 0.7, show.legend = FALSE)
	}
	
	if(label_it){
		plot <- plot +
			geom_label(data = data_reduced[data_reduced$variable == "Fälle",]
								 , aes(label=value, color=variable ), show.legend = FALSE, nudge_y = 90) +
			geom_label(data = data_reduced[data_reduced$variable == "Aktive Fälle",]
								 , aes(label=value, color=variable ), show.legend = FALSE, nudge_y = 20) +
			geom_label(data = data_reduced[data_reduced$variable == "Genesen",]
								 , aes(label=value, color=variable ), show.legend = FALSE, nudge_y = -40) +
			geom_label(data = data_reduced[data_reduced$variable == "Todesfälle",]
								 , aes(label=value, color=variable ), show.legend = FALSE, nudge_y = -80)
	}
	return(plot)
}

create_short_plot_plz <- function(data, ort_info, logo, logo2, x_adjust = 7, y_adjust = 0){
  einzelne_gemeinde <- data[data$PLZ==ort_info$PLZ,]
  create_short_plot(einzelne_gemeinde,logo, logo2, x_adjust, y_adjust, ort_info)
}


create_short_plot <- function(data, logo, logo2, x_adjust = 7, y_adjust = 0){
	max_date <- max(data$Datum)
	min_date <- min(data$Datum)
	
	ort <- "Landkreis Miesbach"
  
	ggplot(data, aes(Datum, value, color = variable)) + 
		geom_line(size = 1.5, alpha = 0.8) +
		ylab("") +
		xlab("") +
		labs(title = paste0("Kurzfristige Entwicklung Neuinfektionen pro Tag und 7-Tage-Inzidenz COVID-19 ",ort),
				 caption = paste0("Lizenz: CC-BY-SA, FabLab Oberland e.V., ","Datenstand: ", format(max_date, "%d.%m.%Y"), ", ",format(Sys.time(),"%H:00"))) +
		theme_minimal() +
		geom_label(aes(label=value), show.legend = FALSE, nudge_y = 10) +
		annotation_custom(logo, xmin=min_date+x_adjust, xmax=min_date+2+x_adjust, ymin=20+y_adjust, ymax=60+y_adjust) +
		annotation_custom(logo2, xmin=min_date+2+x_adjust, xmax=min_date+4+x_adjust, ymin=20+y_adjust, ymax=60+y_adjust) +
		annotation_custom(grob = textGrob("Erstellt mit \u2661\n in Zusammenarbeit von",
																			gp=gpar(fontfamily = "Roboto Mono", fontsize = 12*0.9)),
											xmin=min_date+1+x_adjust,
											xmax=min_date+2.5+x_adjust,
											ymin=60+y_adjust, ymax=85+y_adjust) +
		
		coord_cartesian(clip = "off") +
		scale_x_date(date_labels = "%d.%m", breaks = "day") +
		scale_color_manual(values = oberlab_colors) +
		theme_oberlab_white() +
		#scale_y_continuous(limits=c(0, 200), expand = c(0,0)) +
		expand_limits(y=c(0, 0))+
		theme(legend.margin = margin(t=-1), plot.margin = unit(c(2,1,0.5,0) , "lines"))
}
