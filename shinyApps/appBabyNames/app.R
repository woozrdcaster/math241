library(shiny)
library(tidyverse)
library(babynames)
library(DT)

# User interface
ui <- fluidPage(
  titlePanel("Which Math 241 name is most popular?"),
  sidebarLayout(
    sidebarPanel(
      # Create a text input widget
      textInput(inputId = "names", # otherwise, could have used selectInput to have users choose between options
                label = "Enter Math 241 names here",
                value = "Adrien"),
      p("Put single space between the names.")
    ),
    mainPanel(
      plotOutput(outputId = "graph"),  # Plot output
      dataTableOutput(outputId = "table")  # Table output
    )
  )
)

server <- function(input, output){
  
  output$graph <- renderPlot({ # Plot based on user input
    dat_names <- babynames %>%
      group_by(year, name) %>%
      summarize(n = sum(n)) %>%
      group_by(year) %>%
      mutate(prop = n/sum(n)) %>%
      filter(name %in% c(unlist(str_split(input$names, " "))),
             year >= 1980) 
    
    ggplot(data = dat_names, 
           mapping = aes(x = year, y = prop,color = name)) +
      geom_line(size = 2)
    
  })
  
  output$table <-  renderDataTable({  # Table based on user input
    
    dat_names <- babynames %>%
      group_by(year, name) %>%
      summarize(n = sum(n)) %>%
      group_by(year) %>%
      mutate(prop = n/sum(n)) %>%
      filter(name %in% c(unlist(str_split(input$names, " "))),
             year >= 1980) 
    
    dat_names %>%
      group_by(name) %>%
      summarize(count = sum(n)) %>%
      arrange(desc(count))
  })
}

# Creates app
shinyApp(ui = ui, server = server)
