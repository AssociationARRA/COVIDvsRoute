setwd("~/Documents/R projects/R_covid_data/R_cepiDC")
dt=read.csv("Nbmortsparjour.csv", sep=";")

# Check covid data
dt$Day=as.Date(dt$Day)
summary(dt)
TOT=sum(dt$Nb.death)

#data between mars 2022 and march 2023
dt=subset(dt, Day >= as.Date('2022-04-01'))
dt=subset(dt, Day <= as.Date('2023-04-01'))
summary(dt)

# Cumulative sum of the dead :
dt[, 2] <- cumsum(dt[, 2])
summary(dt)
plot(dt)


# data route
route1=read.csv("./mortalité routière/Morts_route_2023janv-mars.csv", sep=";")
summary(route1)

route2=read.csv("./mortalité routière/Morts_route_2022avri-dec.csv", sep=";")
summary(route2)

# take only what interests me
route1=route1[,c(3,4)]
route2=route2[,c(3,4)]

route=rbind(route1, route2)

# transform data to get Date only
route$Date=gsub("\\ -.*","",route$Date, perl = TRUE)
route$Date=as.Date(route$Date, format = "%d/%m/%Y")
head(route)

# regroup sum of dead by date
Route=aggregate(Tué.s.~Date, data = route, sum)

#Cumulated deaths
Route[, 2] <- cumsum(Route[, 2])
head(Route); tail(Route)
plot(Route)


# Plot both using a FT format
library(ggplot2)
library(ggrepel)
library(showtext) # Package for using extra fonts
library(patchwork)  # composition of multiple plots

# https://rpubs.com/chidungkt/925353

# Input some labels and colors
# The Financial colors (https://www.r-bloggers.com/2018/06/the-financial-times-and-bbc-use-r-for-publication-graphics/): 

bgr_color <- "#262a33"
# thème clair :
#bgr_color <- "#F2DFCE" 
pink_color <- "#ff0055"
blue1_color <- "#0d64fc"
grey_color <- "#526171"
grey_text <- "#9b9da1"

some_countries <- c("SARS-CoV2", "Accidents de la route")

p_title <- "Pourquoi aucune prévention covid ?"
p_subtitle <- "Si les accidents routiers étaient prévenus de la même façon que le covid : \n on aurait ni code de la route, ni ethylotest, ni ceinture."
p_caption <- "Source : ONISR; SPF | Idée et Design : WSP"
left_text <- "Nombre cumulé de morts"

# Fira Sans Condensed
my_font <- "Fira Sans Condensed" 

# Load font for ploting: 
font_add_google(name = my_font, family = my_font) 

showtext_auto() # Automatically render text. 

ggplot() + 
	geom_point(data = dt, 
  					 aes(Day, Nb.death), 
  					 color = "#90aac6", 
  					 size = 1, alpha = 0.7) +
	geom_line(data = dt, 
  					 aes(Day, Nb.death), 
  					 color = "#90aac6") +
  geom_point(data = Route, 
  					 aes(Date, Tué.s.), 
  					 color = pink_color, 
  					 size = 0.75, alpha = 0.7) + 
	annotate(geom="text", x=as.Date("2023-01-01"), 
					 y=22000, label="SARS-CoV2",
           family = my_font,  fontface = "bold",
					 size = 6, color = "#90aac6") +
	annotate(geom="text", x=as.Date("2023-04-01"), 
					 y=21000, label=tail(dt$Nb.death,1),
           family = my_font, 
					 size = 5, color = "#90aac6") +
	annotate(geom="text", x=as.Date("2023-01-01"), 
					 y=5000, label="Accidents de la route",
           family = my_font,  fontface = "bold",
					 size = 6, color = pink_color) +
		annotate(geom="text", x=as.Date("2023-04-01"), 
					 y=4200, label=tail(Route$Tué.s.,1),
           family = my_font, 
					 size = 5, color = pink_color) +
	theme(plot.background = element_rect(fill = bgr_color, color = NA)) + 
  theme(panel.background = element_rect(fill = bgr_color, color = NA)) + 
  theme(panel.grid.major = element_line(linewidth = 0.8, 
  																			color = "#3e4046")) + 
  theme(panel.grid.minor = element_blank()) +
	labs(subtitle = left_text, x = "", y="") +
  theme(axis.title.y.right = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(plot.subtitle = element_text(hjust = 1, 
  																	 color = "grey95", 
  																	 family = my_font, size = 12, vjust = 0)) + # titre de l'axe y
  theme(axis.title.x = element_text(color = "grey95", 
  																	family = my_font, 
  																	size = 12)) + 
  theme(axis.text = element_text(color = "grey95", 
  															 family = my_font, 
  															 size = 14)) +

  plot_annotation(title = p_title, subtitle = p_subtitle, 
  								caption = p_caption) + 
  plot_annotation(theme = theme(plot.background = element_rect(fill = bgr_color))) + 
  plot_annotation(theme = theme(plot.title = element_text(family = my_font, 
  																												size = 18, face = "bold", vjust = -1.5, color = "white"))) + 
  plot_annotation(theme = theme(plot.subtitle = element_text(family = my_font,
  																													 size = 13, color = "grey95", vjust = -4))) + # sous-titre
  plot_annotation(theme = theme(plot.caption = element_text(family = my_font, 
  																													color = "grey60", size = 11, hjust = 0.5))) + # truc en bas grisé
  theme(plot.margin = unit(c(0.5, 0.5, 0.1, 0.7), "cm")) 


# thème clair
bgr_color <- "#F2DFCE" 


ggplot() + 
	geom_point(data = dt, 
  					 aes(Day, Nb.death), 
  					 color = "#90aac6", 
  					 size = 1, alpha = 0.7) +
	geom_line(data = dt, 
  					 aes(Day, Nb.death), 
  					 color = "#90aac6") +
  geom_point(data = Route, 
  					 aes(Date, Tué.s.), 
  					 color = pink_color, 
  					 size = 0.75, alpha = 0.7) + 
	annotate(geom="text", x=as.Date("2023-01-01"), 
					 y=22000, label="SARS-CoV2",
           family = my_font,  fontface = "bold",
					 size = 6, color = "#90aac6") +
	annotate(geom="text", x=as.Date("2023-04-01"), 
					 y=21000, label=tail(dt$Nb.death,1),
           family = my_font, 
					 size = 5, color = "#90aac6") +
	annotate(geom="text", x=as.Date("2023-01-01"), 
					 y=5000, label="Accidents de la route",
           family = my_font,  fontface = "bold",
					 size = 6, color = pink_color) +
		annotate(geom="text", x=as.Date("2023-04-01"), 
					 y=4200, label=tail(Route$Tué.s.,1),
           family = my_font, 
					 size = 5, color = pink_color) +
	theme(plot.background = element_rect(fill = bgr_color, color = NA)) + 
  theme(panel.background = element_rect(fill = bgr_color, color = NA)) + 
  theme(panel.grid.major = element_line(linewidth = 0.8, 
  																			color = "grey80")) + 
  theme(panel.grid.minor = element_blank()) +
	labs(subtitle = left_text, x = "", y="") +
  theme(axis.title.y.right = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(plot.subtitle = element_text(hjust = 1, 
  																	 color = "grey5", 
  																	 family = my_font, size = 12, vjust = 0)) + # titre de l'axe y
  theme(axis.title.x = element_text(color = "grey5", 
  																	family = my_font, 
  																	size = 12)) + 
  theme(axis.text = element_text(color = "grey5", 
  															 family = my_font, 
  															 size = 14)) +

  plot_annotation(title = p_title, subtitle = p_subtitle, 
  								caption = p_caption) + 
  plot_annotation(theme = theme(plot.background = element_rect(fill = bgr_color))) + 
  plot_annotation(theme = theme(plot.title = element_text(family = my_font, 
  																												size = 18, face = "bold", vjust = -1.5, color = "black"))) + 
  plot_annotation(theme = theme(plot.subtitle = element_text(family = my_font,
  																													 size = 13, color = "grey5", vjust = -4))) + # sous-titre
  plot_annotation(theme = theme(plot.caption = element_text(family = my_font, 
  																													color = "grey40", size = 11, hjust = 0.5))) + # truc en bas grisé
  theme(plot.margin = unit(c(0.5, 0.5, 0.1, 0.7), "cm")) 

