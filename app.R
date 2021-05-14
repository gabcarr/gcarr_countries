
# Load relevant libraries
library(here) # Setup
library(sf) # Using vector data   
library(spData) # Includes world dataset
library(shiny) # Shiny
library(shinythemes) # Shiny themes
library(tidyverse) # Tidyverse data manipulation
library(scales) # Number formatting

#' Making world dataset into an object and re-formatting
world <- world %>%
  rename("Subregion" = "subregion") %>%
  rename("Area in km2" = "area_km2") %>%
  rename("Population" = "pop") %>%
  rename("Life Expectancy" = "lifeExp") %>%
  rename("GDP Per Capita" = "gdpPercap") %>%
  arrange(name_long) %>% 
  drop_na("Subregion") # Needs me to drop NAs from a column for table to output; dropping NAs from subregion column bc no NAs there

#' Formatting numbers for output
world$`Area in km2` <- prettyNum(world$`Area in km2`, big.mark = ",", scientific = FALSE)
world$Population <- prettyNum(world$Population, big.mark = ",", scientific = FALSE)
world$`GDP Per Capita` <- dollar(world$`GDP Per Capita`)
world$`Life Expectancy` <- formatC(world$`Life Expectancy`, digits = 3)

#' Create the user interface for app:
ui <- fluidPage(
          
          # Title
          titlePanel("Choose a Country"),
          
          # Side bar panel
              sidebarLayout(
                  sidebarPanel(selectInput(inputId = "country_select",
                                           label = "Choose a Country",
                                           choices = unique(world$name_long))),
                  mainPanel(p("Country's Stats:"),
                            tableOutput(outputId = "lifeexp_table"),
                            p("Country's Location:"),
                            plotOutput(outputId = "world_map")))
      )

#' Subsetting dataset to just what user wants (on server side now):
server <- function(input, output) {

          # Life expectancy table: Input (reactive object)
          lifeexp <- reactive({
                        world %>%
                          filter(name_long == input$country_select) %>%
                          select(Subregion, "Area in km2", Population, "Life Expectancy", "GDP Per Capita")})
          
          # Life expectancy table: Output
          output$lifeexp_table <- renderTable({
                                      lifeexp()})
            
          # World map plot: Input
          country <- reactive({
                        world %>%
                          filter(name_long == input$country_select) %>% 
                          select(geom)})
          
          # World map plot: Output (code from Lecture 7)
          output$world_map <- renderPlot({
                                  ggplot() +
                                    geom_sf(data = world, color = "grey", fill = "white", aes(geometry = geom)) +  # World layer
                                    geom_sf(data = country(), 
                                            fill = "red", aes(geometry = geom)) + # Layering countries on world - works if use afghanistan object but not reactive object
                                    theme(panel.grid.major = element_line(color = "#ecebec"), 
                                          panel.background = element_rect(fill = "#c6e1f1")) # Coloring background and grid lines
                                })
}

#' Combine them into an app
shinyApp(ui = ui, server = server)
