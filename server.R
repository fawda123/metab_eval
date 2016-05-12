source('R/funcs.R')
data(wq_2015)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  # ctd contour plot 1
  output$conplot1 <- renderPlot({
  
    varin <- input$varin1
    num_int <- input$num_int
    num_levs <- input$num_levs
    ncol <- input$ncol
    deprng <- input$deprng
    dtrng <- input$dtrng
    aggs <- input$aggs
    
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
    
    ctd_time(wq_2015, var = varin, num_int = num_int, ncol = ncol, num_levs = num_levs, deprng = deprng,
      dtrng = dtrng, aggs = aggs)
    
    })
  
})