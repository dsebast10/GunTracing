library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(readxl)
library(magrittr)

source("DataPrep.R")

trace_start_year <- min(traces$Year)
trace_end_year <- max(traces$Year)


ui <- fluidPage(
  titlePanel("Hello Shiny!"),
    sidebarLayout(
      sidebarPanel(
       radioButtons("source_recovery",
                     "Choose Data to Display on Map",
                     choices = c("Source States", "Recovery States"),
                     selected = "Source States"),
       selectInput("state",
                   "State",
                   levels(factor(traces$RecoveryState)),
                   selected = "Alabama"),
       sliderInput("years",
                   "Years",
                   min = trace_start_year,
                   max = trace_end_year,
                   value = c(trace_start_year, trace_end_year),
                   step = 1,
                   sep = ""),
       checkboxInput("exclude_state",
                     "Exclude Selected State in Totals?",
                     value = TRUE)
                
      ),
    mainPanel(
      plotOutput("source_time_chart")
    )  
  )
)




server <- function(input, output){
  reactive({print(input$state)})

  
  #Chart of gun sources by time
  output$source_time_chart <- renderPlot({
    
    state_data <- traces %>%
      filter(RecoveryState == input$state,
             between(Year, input$years[1], input$years[2])) %>% 
      {if (input$exclude_state) filter(., SourceState != input$state) else .} %>%
      group_by(SourceState) %>%
      mutate(GunsTotal = sum(Guns)) %>%
      ungroup() %>%
      arrange(desc(GunsTotal)) %>%
      head(5*(input$years[2]-input$years[1] + 1))

    
    g <- ggplot(state_data, aes(x=Year, y=Guns))+
      geom_line(aes(color=SourceState))+
      geom_point(aes(color=SourceState))+
      guides(color = guide_legend(reverse = F)) +
      ggtitle(paste("Where guns recovered in", str_to_title(input$state), "were purchased, by Year Traced"))
    g
  })

    
}

shinyApp(ui, server)
