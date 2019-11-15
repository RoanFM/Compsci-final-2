library(shiny)
library(tidyverse)
library(readr)
library(rsconnect)

rsconnect::setAccountInfo(name='roan-comspci',
                          token='23C5C6AE43A28B28DE56BECB5FBA5FF2',
                          secret='Xo4EnYmlM2OZ1p7x6DolJW27M+4B234X0/eUmITs')

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv") %>% 
  mutate(min_age = fct_reorder(min_age, year_published))

ui <- fluidPage(
  sliderInput(inputId = "year", label = "Year Published",
              min = 1950, max = 2019, value = c(1950,2019), sep = ""),
  textInput("max_players", "Maximum Number of Players", value = "", placeholder = ""),
  selectInput("minage", "Minimum Recommended Age", choices = board_games$min_age),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    board_games %>%
      arrange(min_age) %>%
      filter(max_players == input$max_players, min_age == input$minage) %>%
      ggplot(aes(x = year_published, y = average_rating, color=min_playtime, label = name))+
      geom_point()+
      geom_text()+
      scale_x_continuous(limits = input$year) +
      theme_classic()
  })
}

shinyApp(ui, server)
