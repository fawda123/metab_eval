source('R/funcs.R')
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(EstuaryMetabolism)

data(datest)
datest$date <- as.Date(datest$datetimestamp)
tzone <- attr(datest$datetimestamp, 'tzone')
datest <- rename(datest, 
  depth = binmd)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  output$col <- renderUI({

    varin1 <- input$varin1
    
    if(varin1 %in% c('do', 'sal', 'temp', 'sig', 'ddo', 'ddo', 'dosat', 'ds', 'dv', 'dz')){
      
      selectInput(inputId = 'col',
        label = h4('Color variable top'),
        choices = c('depth', 'solar_period'), 
        selected = 'depth'
      )

    } else {
  
      selectInput(inputId = 'col',
        label = h4('Color variable top'),
        choices = c('solar_period'), 
        selected = 'solar_period'
      )
      
    }
  
  })
  
  # ctd contour plot 1
  output$conplot1 <- renderPlot({
  
    varin <- input$varin1
    num_levs <- input$num_levs
    ncol <- input$ncol
    deprng <- input$deprng
    dtrng <- input$dtrng
    aggs <- as.logical(input$aggs)
    zmix <- as.logical(input$zmix)
    
    # default to do_mgl if varin is a wx variable
    if(varin %in% c('bp', 'atemp', 'wspd')) return(NULL)
    
    contplo1 <- datest[, c('datetimestamp', 'depth', varin, 'zmix')] %>% 
      na.omit
    ctd_time(contplo1, var = varin, num_int = 100, ncol = ncol, num_levs = num_levs, deprng = deprng,
      dtrng = dtrng, aggs = aggs, mix = zmix)
    
    })
  
  # ctd contour plot 2
  output$conplot2 <- renderPlot({
  
    varin <- input$varin2
    num_levs <- input$num_levs
    ncol <- input$ncol
    deprng <- input$deprng
    dtrng <- input$dtrng
    aggs <- as.logical(input$aggs)
    zmix <- as.logical(input$zmix)
    
    # default to do_mgl if varin is a wx variable
    if(varin %in% c('bp', 'atemp', 'wspd')) return(NULL)
    
    contplo2 <- datest[, c('datetimestamp', 'depth', varin, 'zmix')] %>% 
      na.omit
    ctd_time(contplo2, var = varin, num_int = 100, ncol = ncol, num_levs = num_levs, deprng = deprng,
      dtrng = dtrng, aggs = aggs, lines = !zmix, mix = zmix)
    
    })
  
  # first time series plot
  output$tsplot1 <- renderPlot({
  
    varin <- input$varin1
    dtrng <- input$dtrng
    col <- input$col
    colpal <- input$colpal 
    obs <- input$obs

    # use wx data if applicalbe
    if(varin %in% c('bp', 'atemp', 'wspd')){ 
      
      toplo <- met_2015
      toplo$grp <- 'a'
      
    } else {
      
      toplo <- datest
      toplo$depth <- factor(round(toplo$depth, 3))
      toplo$grp <- toplo$depth
      
    }
    
    # format data
    toplo <- filter(toplo, date >= dtrng[1] & date <= dtrng[2])
    names(toplo)[names(toplo) %in% varin] <- 'yvar'
    names(toplo)[names(toplo) %in% col] <- 'col'      
    toplo <- select(toplo, datetimestamp, yvar, grp, col) %>% 
      na.omit
  
    # hr back, forward for flux comparison
    obs <- as.POSIXct(input$obs, format = '%Y-%m-%d %H:%M', tz = tzone)
    obs <- c(obs - 3600, obs, obs + 3600) %>% 
      as.numeric

    ggplot(toplo, aes(x = datetimestamp, y = yvar, group = grp, colour = col)) + 
      geom_line() +
      geom_vline(xintercept = obs) +
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
      scale_colour_brewer(palette = colpal)
    
    })
  
  # second time series plot
  output$tsplot2 <- renderPlot({
  
    varin <- input$varin2
    dtrng <- input$dtrng
    col <- input$col
    colpal <- input$colpal
    obs <- input$obs

    # use wx data if applicable
    if(varin %in% c('bp', 'atemp', 'wspd')){ 
      
      toplo <- met_2015
      toplo$grp <- 'a'
      
    } else {
      
      toplo <- datest
      toplo$depth <- factor(round(toplo$depth, 3))
      toplo$grp <- toplo$depth
      
    }
    
    # format data
    toplo <- filter(toplo, date >= dtrng[1] & date <= dtrng[2])
    names(toplo)[names(toplo) %in% varin] <- 'yvar'
    names(toplo)[names(toplo) %in% col] <- 'col'  
    toplo <- select(toplo, datetimestamp, yvar, grp, col) %>% 
      na.omit
  
    # hr back, forward for flux comparison
    obs <- as.POSIXct(input$obs, format = '%Y-%m-%d %H:%M', tz = tzone)
    obs <- c(obs - 3600, obs, obs + 3600) %>% 
      as.numeric
    
    ggplot(toplo, aes(x = datetimestamp, y = yvar, group = grp, colour = col)) + 
      geom_line() +
      geom_vline(xintercept = obs) +
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
      scale_colour_brewer(palette = colpal)
    
    })
  
  # hourly observation, flux plots - back one hour
  output$hrplot1 <- renderPlot({
    
    obs <- as.POSIXct(input$obs, format = '%Y-%m-%d %H:%M', tz = tzone)
    obsi <- as.character(obs - 3600)
    hrplot(datest, obsi)
    
    })
  
  # hourly observation, flux plots, current selection
  output$hrplot2 <- renderPlot({
    
    obs <- as.POSIXct(input$obs, format = '%Y-%m-%d %H:%M', tz = tzone)
    obsi <- as.character(obs)
    hrplot(datest, obsi)
    
    })
  
  # hourly observation, flux plots, up one hour
  output$hrplot3 <- renderPlot({
    
    obs <- as.POSIXct(input$obs, format = '%Y-%m-%d %H:%M', tz = tzone)
    obsi <- as.character(obs + 3600)
    hrplot(datest, obsi)
    
    })
  
  # hourly observation, data plus minus one hours
  output$hrdat <- renderDataTable({
    
    obs <- as.POSIXct(input$obs, format = '%Y-%m-%d %H:%M', tz = tzone)
    obsrng <- c(obs - 3600, obs + 3600)
    dat <- filter(datest, datetimestamp <= obsrng[2] & datetimestamp >= obsrng[1])
    dat
    
    })
  
  
})