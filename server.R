source('R/funcs.R')
library(dplyr)
library(ggplot2)
library(RColorBrewer)

data(wq_2015)
data(met_2015)

wq_2015$date <- as.Date(wq_2015$datetimestamp)
met_2015$date <- as.Date(met_2015$datetimestamp)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  output$col1 <- renderUI({

    varin1 <- input$varin1
    
    if(varin1 %in% c('do_mgl', 'sal', 'temp')){
      
      selectInput(inputId = 'col1',
        label = h4('Color variable top'),
        choices = c('depth', 'solar_period'), 
        selected = 'depth'
      )

    } else {
  
      selectInput(inputId = 'col1',
        label = h4('Color variable top'),
        choices = c('solar_period'), 
        selected = 'solar_period'
      )
      
    }
  
  })
  
  output$col2 <- renderUI({

    varin2 <- input$varin2

    if(varin2 %in% c('do_mgl', 'sal', 'temp')){
      
      selectInput(inputId = 'col2',
        label = h4('Color variable bottom'),
        choices = c('depth', 'solar_period'), 
        selected = 'depth'
      )

    } else {
  
      selectInput(inputId = 'col2',
        label = h4('Color variable bottom'),
        choices = c('solar_period'), 
        selected = 'solar_period'
      )
      
    }
  
  })
  
  # ctd contour plot 1
  output$conplot1 <- renderPlot({
  
    varin <- input$varin1
    num_int <- input$num_int
    num_levs <- input$num_levs
    ncol <- input$ncol
    deprng <- input$deprng
    dtrng <- input$dtrng
    aggs <- input$aggs
    
    # default to do_mgl if varin is a wx variable
    if(varin %in% c('bp', 'atemp', 'wspd')) return(NULL)
    
    ctd_time(wq_2015, var = varin, num_int = num_int, ncol = ncol, num_levs = num_levs, deprng = deprng,
      dtrng = dtrng, aggs = aggs)
    
    })
  
  # ctd contour plot 2
  output$conplot2 <- renderPlot({
  
    varin <- input$varin2
    num_int <- input$num_int
    num_levs <- input$num_levs
    ncol <- input$ncol
    deprng <- input$deprng
    dtrng <- input$dtrng
    aggs <- input$aggs
    
    # default to do_mgl if varin is a wx variable
    if(varin %in% c('bp', 'atemp', 'wspd')) return(NULL)
    
    ctd_time(wq_2015, var = varin, num_int = num_int, ncol = ncol, num_levs = num_levs, deprng = deprng,
      dtrng = dtrng, aggs = aggs)
    
    })
  
  # first time series plot
  output$tsplot1 <- renderPlot({
  
    varin <- input$varin1
    dtrng <- input$dtrng
    col1 <- input$col1
    colpal1 <- input$colpal1 

    # use wx data if applicalbe
    if(varin %in% c('bp', 'atemp', 'wspd')){ 
      
      toplo <- met_2015
      toplo$grp <- 'a'
      
    } else {
      
      toplo <- wq_2015
      toplo$depth <- factor(toplo$depth)
      toplo$grp <- toplo$depth
      
    }
    
    # format data
    toplo <- filter(toplo, date >= dtrng[1] & date <= dtrng[2])
    names(toplo)[names(toplo) %in% varin] <- 'yvar'
    names(toplo)[names(toplo) %in% col1] <- 'col'      
  
    ggplot(toplo, aes(x = datetimestamp, y = yvar, group = grp, colour = col)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(varin) + 
      theme_minimal() + 
      theme(
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.title.x = element_blank(), 
        legend.position = 'top', 
        legend.title = element_blank()
      ) +
      scale_colour_brewer(palette = colpal1)
    
    })
  
  # second time series plot
  output$tsplot2 <- renderPlot({
  
    varin <- input$varin2
    dtrng <- input$dtrng
    col2 <- input$col2
    colpal2 <- input$colpal2

    # use wx data if applicalbe
    if(varin %in% c('bp', 'atemp', 'wspd')){ 
      
      toplo <- met_2015
      toplo$grp <- 'a'
      
    } else {
      
      toplo <- wq_2015
      toplo$depth <- factor(toplo$depth)
      toplo$grp <- toplo$depth
      
    }
    
    # format data
    toplo <- filter(toplo, date >= dtrng[1] & date <= dtrng[2])
    names(toplo)[names(toplo) %in% varin] <- 'yvar'
    names(toplo)[names(toplo) %in% col2] <- 'col'      
  
    ggplot(toplo, aes(x = datetimestamp, y = yvar, group = grp, colour = col)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(varin) + 
      theme_minimal() + 
      theme(
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.title.x = element_blank(), 
        legend.position = 'top', 
        legend.title = element_blank()
      ) +
      scale_colour_brewer(palette = colpal2)
    
    })
  
})