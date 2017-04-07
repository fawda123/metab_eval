library(shiny)
library(dplyr)
library(RColorBrewer)

source('R/funcs.R')

dts <- as.Date(c('2015-01-01', '2015-12-31'))

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # main panel for variable selection
  mainPanel(width = 12,
  
  fluidRow(
    
    column(width = 2,
              
        selectInput(inputId = 'varin1',
            label = h4('Plot top'),
            choices = c('temp', 'sal', 'do', 'sig', 'atemp', 'bp', 'wspd'), 
            selected = 'do'
          )
          
        ),
            
    column(width = 2,
      
      selectInput(inputId = 'varin2',
          label = h4('Plot bottom'),
          choices = c('temp', 'sal', 'do', 'sig', 'atemp', 'bp', 'wspd'), 
          selected = 'sal'
        )
              
      ),  
    
    column(width = 2, 
            
      selectInput(inputId = 'zmix',
                  label = h4('Show mixing depth'),
                  choices = c(T, F), 
                  selected = F
                )
          
      ),
    
    column(width = 6, 
            
      sliderInput('dtrng', label = h4('Date range'),
        min = dts[1], max = dts[2], value = dts)
          
      )
    
  ),
    
  tabsetPanel(
    
    tabPanel('CTD contours', 
      
      # spacing
      fluidRow(
                       
          # first row of plots
          fluidRow(
              
            
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
              
            ), 
            
            
            column(width = 4, 
              
              sliderInput('deprng', label = h4('Depth range'),
                min = 0.5, max = 3, value = c(0.5, 3), step = 0.05)
              
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
        
      ) 
      
    ), 
    
    tabPanel('Time series', 
      
      # first row of plots
      fluidRow(
          
        column(width = 2,
       
          uiOutput('col')
          
        ),
        
        column(width = 2,
       
          selectInput(inputId = 'colpal',
                  label = h4('Color palette'),
                  choices = row.names(brewer.pal.info), 
                  selected = 'Spectral'
                )
          
        )
  
      ),
      
      fluidRow(
        
        column(width = 12,
            plotOutput("tsplot1")
          )
          
      ),
      
      fluidRow(
        
        column(width = 12,
            plotOutput("tsplot2")
          )
          
      )
      
    )
    
  )

)))