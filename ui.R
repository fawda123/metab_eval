library(shiny)
library(dplyr)

source('R/funcs.R')
data(wq_2015)

dts <- as.Date(c('2015-01-01', '2015-12-31'))

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # main panel for variable selection
  mainPanel(width = 12,
      
  #spacing
  fluidRow(
                   
      # first row of plots
      fluidRow(
          
        column(width = 2,
          
          selectInput(inputId = 'varin1',
              label = h4('Plot variable top'),
              choices = c('temp', 'sal', 'do_mgl'), 
              selected = 'do_mgl'
            )
          
        ),
        
        column(width = 2,
          
          selectInput(inputId = 'varin2',
              label = h4('Plot variable bottom'),
              choices = c('temp', 'sal', 'do_mgl'), 
              selected = 'sal'
            )
          
        ),
        
        column(width = 2,
          
          numericInput(inputId = 'num_int', 
              label = h4('Interpolation points'),
              min = 2, 
              value = 100, 
              max = 1000, 
              step = 1
            )
          
        ),
        
        column(width = 2,
          
          numericInput(inputId = 'ncol', 
              label = h4('Number of colors'),
              min = 1, 
              value = 100, 
              max = 1000, 
              step = 1
            )
          
        ),
        
        column(width = 2,
          
          numericInput(inputId = 'num_levs', 
              label = h4('Contour levels'),
              min = 0, 
              value = 8, 
              max = 100, 
              step = 1
            )
          
        ), 
        
        column(width = 2,
          
          selectInput(inputId = 'aggs',
              label = h4('Aggregate by days'),
              choices = c(T, F), 
              selected = T
            )
          
        )
        
      ),
    
    fluidRow(
      
      column(width = 4, 
        
        sliderInput('deprng', label = h4('Depth range'),
          min = 0, max = 4, value = c(0.35, 3.1), step = 0.05)
        
      ),
      
      column(width = 4, 
        
        sliderInput('dtrng', label = h4('Date range'),
          min = dts[1], max = dts[2], value = dts)
      
      )
      
    ),
    
    fluidRow(
      
      column(width = 12,
        plotOutput("conplot1")
      )
      
    ),
    
    fluidRow(
      
      column(width = 12,
        plotOutput("conplot2")
      )
      
    )

))))