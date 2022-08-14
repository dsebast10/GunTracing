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
  titlePanel("Gun Tracing Dashboard v.2"),
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

  
  #Chart of gun sources by time
  output$source_time_chart <- renderPlot({
    
    if (input$source_recovery == "Source States") {
    
      state_data <- traces %>%
        filter(RecoveryState == input$state,
               between(Year, input$years[1], input$years[2])) %>% 
        {if (input$exclude_state) filter(., SourceState != input$state) else .} %>%
        group_by(SourceState) %>%
        mutate(GunsTotal = sum(Guns)) %>%
        ungroup() %>%
        arrange(desc(GunsTotal)) %>%
        head(5*(input$years[2]-input$years[1] + 1)) %>%
        arrange(desc(Year), desc(Guns)) %>%
        mutate(SourceState = factor(SourceState, levels = rev(SourceState %>% head(5))))
      
      g <- ggplot(state_data, aes(x=Year, y=Guns, color = SourceState, group = SourceState))+
        geom_line(size = 2)+
        geom_point(size = 4)+
        scale_x_continuous("Trace Year",
          labels = c(trace_start_year:trace_end_year), 
          breaks = c(trace_start_year:trace_end_year))+
        ggtitle(paste("Where guns recovered in", str_to_title(input$state), "were purchased, by Year Traced")) +
        guides(color = guide_legend(reverse = T))
      
      g + theme_classic()
      
    } else {
      state_data <- traces %>%
        filter(SourceState == input$state,
               between(Year, input$years[1], input$years[2])) %>% 
        {if (input$exclude_state) filter(., RecoveryState != input$state) else .} %>%
        group_by(RecoveryState) %>%
        mutate(GunsTotal = sum(Guns)) %>%
        ungroup() %>%
        arrange(desc(GunsTotal)) %>%
        head(5*(input$years[2]-input$years[1] + 1)) %>%
        arrange(desc(Year), desc(Guns)) %>%
        mutate(RecoveryState = factor(RecoveryState, levels = rev(RecoveryState %>% head(5))))
      
      
      g <- ggplot(state_data, aes(x=Year, y=Guns, color = RecoveryState, group = RecoveryState))+
        geom_line(size = 2)+
        geom_point(size = 4)+
        scale_x_continuous("Trace Year",
                           labels = c(trace_start_year:trace_end_year), 
                           breaks = c(trace_start_year:trace_end_year))+
        ggtitle(paste("Where guns traced to", str_to_title(input$state), "were recovered, by Year Traced"))+
        guides(color = guide_legend(reverse = T))
      
      g + theme_classic()
    }
    
    
  })

    
}

shinyApp(ui, server)
