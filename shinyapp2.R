

###packages
library(dplyr)
library(fmsb)
library(shiny)


#read in the data 
d.stats_fbref <- read_excel("C:\\Users\\41798\\switchdrive\\SyncVM\\Sports data analytics\\stats_fbref.xlsx")
d.outlierplayers <- read_excel("C:\\Users\\41798\\switchdrive\\SyncVM\\Sports data analytics\\outlierplayers.xlsx")

#tidy up stats fbref 


#remove the second position only the first position matters
d.stats_fbref$position_split <- substring(d.stats_fbref$position, 1,2)

#remove % 

d.stats_fbref$abgeschlossene_paesse_prozent_m <- gsub("%", "", d.stats_fbref$abgeschlossene_paesse_prozent_m)

#delete the rows with NA

indices <- which(is.na(d.stats_fbref$nicht_elfmetertore_o ))
d.stats_fbref <- d.stats_fbref[-indices,]

#define the columns which should be changed
columns <- colnames(d.stats_fbref)[3:17]

#numeric values for the numbers 
d.stats_fbref <- d.stats_fbref %>% 
  mutate(across(all_of(columns), ~ as.numeric(unlist(.))))

#calculate the mean of the values
d.stats_fbref_fw <- d.stats_fbref %>% 
  group_by(position_split) %>% 
  summarise(across(all_of(columns), ~ mean(.)))


#calculate min of each column
d.min <- d.stats_fbref %>%
  summarise(across(all_of(columns), ~ min(.)))

#calculate max of each column
d.max <- d.stats_fbref %>%
  summarise(across(all_of(columns), ~ max(.)))


#create vectors
vec_min <- unlist(unname(as.vector(d.min[1,])))
vec_max <- unlist(unname(as.vector(d.max[1,])))

#row names
row_names <- as.vector(d.stats_fbref_fw$position_split)
as.vector(d.stats_fbref_fw$position_split)




#get rid of the first column
d.stats_fbref_fw <- d.stats_fbref_fw[,-1]

#name the rows 
rownames(d.stats_fbref_fw) <- row_names



#rbind max min


d.stats_fbref_fw <- rbind(vec_max, vec_min, d.stats_fbref_fw)



############################################################################################################# get player stats ro rbind it to 
#the rest of the data

#add all outlierplayers to the d.stats_fbref_fw

#try to join the ourlierplayers with d.stats_fbref


d.outlierplayers$name_lower <- str_extract(d.outlierplayers$players, "(?<=transfermarkt.ch/)[^/]+")


#lower the the names

d.stats_fbref$player_name <- tolower(d.stats_fbref$player_name)

#change the name savinho to savio in d.outlierplayers because data would be missing
#still missing are the players ramus hojlund, evan ferguson, leny yoro
#minus 3 players

d.outlierplayers$name_lower[5] <- "savio"

#get the names 
joined <- left_join(d.outlierplayers, d.stats_fbref, by=c("name_lower" = "player_name"))

#delte the NAs 

indices <- which(is.na(joined$links))
joined <- joined[-indices,]

#only extract the names of it 


names <- joined$name_lower

#get the indices form the player which are in d.outlierplayer and d.stats_fbre
indices <- which(d.stats_fbref$player_name %in% names)

#create a dataframe with only the players which are also in d.outlierplayers with the additional stats
d.stats_fbref_join <- d.stats_fbref[indices,]




#paste the position to the names for the shiny app

d.stats_fbref_join <- d.stats_fbref_join %>% 
  mutate(player_name = paste0(player_name,"_", position_split))


#only choose the columns which are needed for the shinyapp plot

names_col <- names(d.stats_fbref_join)[4:length(d.stats_fbref_join)-1]


#choose only the columns which are needed
d.stats_fbref_join <- d.stats_fbref_join %>% 
  select(all_of(names_col))


#####get the rows name

#row names
row_names <- as.vector(d.stats_fbref_join$player_name)


#get rid of the first column
d.stats_fbref_join <- d.stats_fbref_join[,-length(d.stats_fbref_join)]

#name the rows 
rownames(d.stats_fbref_join) <- row_names
############################################################################rbind the this dataset with the average dataset fbref



d.stats_fbref_fw <- rbind(d.stats_fbref_fw, d.stats_fbref_join)







######################################################################################################################



#######start shiny app


# Definiere die Benutzeroberfläche (UI)
ui <- fluidPage(
  titlePanel("Premierleague Analyse"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_rows",
                  "Wähle die Spieler für die Analyse aus:",
                  choices = rownames(d.stats_fbref_fw)[3:nrow(d.stats_fbref_fw)], # Nur Zeilen ab der dritten zur Auswahl
                  selected = NULL, # Startet mit einer leeren Auswahl
                  multiple = TRUE)
    ),
    
    mainPanel(
      plotOutput("radarPlot", width = "100%", height = "1000px")  # Plot-Ausgabe für das Radarchart
    )
  )
)

# Definiere die Serverlogik
server <- function(input, output) {
  
  # Plot für das Radarchart erstellen
  output$radarPlot <- renderPlot({
    
    # Überprüfe, ob eine Auswahl getroffen wurde
    if (is.null(input$selected_rows) || length(input$selected_rows) == 0) {
      return()  # Beendet die Plot-Erstellung, wenn keine Auswahl getroffen wurde
    }
    
    # Subset des DataFrames basierend auf der Benutzerauswahl
    selected_data <- d.stats_fbref_fw[c(1, 2, which(rownames(d.stats_fbref_fw) %in% input$selected_rows)), , drop = FALSE]
    
    # Definiere die Farben mit Transparenz entsprechend der Auswahl
    colors_border <- adjustcolor(c("red", "green", "blue", "orange", "purple"), alpha.f = 0.2)[1:(nrow(selected_data) - 2)]
    colors_in <- adjustcolor(c("red", "green", "blue", "orange", "purple"), alpha.f = 0.2)[1:(nrow(selected_data) - 2)]
    
    # Passe die Margen des Plots an
    #default margin 5,4,4,8
    par(mar = c(7, 7, 7, 7))  # Passe die Margen an: unten, links, oben, rechts
    
    # Erstelle den Radarchart-Plot
    radarchart(selected_data, axistype = 1, 
               pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
               cglcol = "grey", cglty = 1, axislabcol = "grey", 
               caxislabels = seq(0, 20, 5), cglwd = 0.8, 
               vlcex = 1)
    
    # Füge eine Legende hinzu
    legend(x = 1, y = 1, legend = input$selected_rows, bty = "n", pch = 20, 
           col = colors_in, text.col = "black", cex = 0.8, pt.cex = 2, text.font = 2)
  }, width = 1000, height = 1000)  # Setze die Breite und Höhe des Plots in renderPlot
}

# Starte die Shiny-App
shinyApp(ui = ui, server = server)



####end of shiny app




















