library(here) #setup
library(sf) #vector data   
library(spData) #datasets
library(tidyverse) #data manipulation
library(shiny)
library(shinythemes)

#' Create the user interface for app:
ui <- fluidPage(
  titlePanel("Choosing Countries"),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "country_select",
                             label = "Choose a Country",
                             choices = unique(world$name_long))),
    mainPanel(p("Country's Life Expectancy:"),
              tableOutput(outputId = "lifeexp_table"),
              p("Country's Location:"),
              plotOutput(outputId = "world_map")
    )))

#' World dataset
world <- world

#' Subsetting dataset to just what user wants (on server side now):
server <- function(input, output) {
  # Life expectancy table
  lifeexp <- reactive({ # Reactive object: input for life expectancy table
    world %>%
      filter(name_long == input$country_select) %>%
      select(lifeExp)})
  output$lifeexp_table <- renderTable({ # Output for life expectancy table
   lifeexp()})
  
  # World map plot
  world_map <- reactive({ # Input for world map
    world %>%
      filter(name_long == input$country_select)
      select(name_long, geom)})
  output$world_map <- renderPlot({ # Output for world map
    ggplot() +
      geom_sf(data = world, color = "grey", fill = "ivory", size = 0.25) + # world layer
      geom_sf(data = world_map, # Layering countries on world
             color = "red", fill = "pink", size = 0.25) + 
      theme(panel.background = element_rect(fill = "lightcyan")) # Changing background color
    
})}


#' Combine them into an app:
shinyApp(ui = ui, server = server)
