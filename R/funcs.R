######
# plot CTD data by depth and time
# returns an interpolated contour plot
#
# dat_in input ctd data, all dates and stations
# num_levs number of contour levels
# var variable to plot from dat_in
# ylab labels for y axis
# cols color vector
# ncol number of colors for smoothing plot
# num_int length of sequenced depth, time variables for interpolation
# deprng depth range to plot
# dtrng date range to plot (date object)
# aggs data are averaged by day if TRUE
ctd_time <- function(dat_in, num_levs = 8, var = 'do_mgl', ylab = 'Depth (m)',
  cols = c('tomato', 'lightblue', 'lightgreen','green'),
  ncol = 100, num_int = 100, deprng = c(0.35, 3.1), 
  dtrng = NULL, aggs = TRUE, lines = TRUE, mix = FALSE){
  
  library(dplyr) 
  library(tidyr)
  library(akima)
  
  # timezone for datetimestamp back to posix
  tz <- attr(dat_in$datetimestamp, 'tzone')

  # dtrng is data record if empty
  if(is.null(dtrng)) dtrng <- range(as.Date(dat_in$datetimestamp))

  # safety check for aggby hour
  if(diff(dtrng) > 31 & aggs == FALSE) 
    stop('Cannot aggregate by hour for more than one month')

  if(diff(dtrng) > 31 & mix == TRUE)
    stop('Cannot show mixing depth for more than one month')
  
  # data format
  dat_mat <- dat_in
  names(dat_mat)[names(dat_mat) %in% var] <- 'varcol'
  dat_mat <- select(dat_mat, datetimestamp, depth, varcol) %>% 
    filter(
      depth <= deprng[2] & depth >= deprng[1]
      ) %>% 
    mutate(
      date = as.Date(datetimestamp),
      depth = round(depth, 1)
      ) %>% 
    filter(date >= dtrng[1] & date <= dtrng[2]) 

  # agg by days if true
  if(aggs){
    
    if(mix) stop('Cannot aggregate if mix = TRUE')
    
    dat_mat <- group_by(dat_mat, date, depth) %>% 
      summarize(varcol = mean(varcol)) %>% 
      ungroup
    
  } else {
    
    dat_mat <- group_by(dat_mat, datetimestamp, depth) %>% 
      summarize(varcol = mean(varcol)) %>% 
      ungroup %>% 
      rename(date = datetimestamp)
    
  }

  newvals <- interp(
    x = dat_mat$date, 
    y = dat_mat$depth,
    z = dat_mat$varcol, 
    nx = num_int, ny = num_int
  )
    
  # spread, get separate variables
  x.val <- newvals$x
  y.val <- newvals$y
  z.val <- newvals$z
  in_col <- colorRampPalette(cols)
  
  # plot margins
  plot.new()
  par(new = "TRUE",plt = c(0.1,0.87,0.15,0.9),las = 1,cex.axis = 1)
  
  # contour plot
  filled.contour3(x = x.val,  y = y.val, z = z.val,
    color.palette = in_col, ylab = ylab,
    nlevels = ncol, # for smoothed colors
    axes = F, ylim = rev(range(y.val)))

  # isolines if T
  if(!mix){
    contour(x = x.val, y =  y.val, z = z.val, nlevels= num_levs,
      axes = F, add = T, ylim = rev(range(y.val)))
  }
  
  # add mixing layer depth
  if(mix & !aggs){
     
    zmix <- select(dat_in, datetimestamp, zmix) %>% 
      unique
  
    lines(zmix$datetimestamp, zmix$zmix)
    
  }
    
  # axis labels
  axis(side = 2)
  if(aggs){
    axis.Date(side = 1, x = as.Date(x.val, origin = '1970-01-01'), format = '%m-%Y')
  } else {
    axis.POSIXct(side = 1, x = as.POSIXct(x.val, origin = '1970-01-01', tz = tz))
  }
  box()
  
  # legend
  par(new = "TRUE", plt = c(0.90,0.94,0.15,0.9), las = 1,cex.axis = 1)
  filled.legend(x.val,y.val,z.val,color=in_col,xlab = "",
    nlevels = num_levs,
    ylab = "",
    ylim = c(min(z.val),max(z.val)))
  
}


######
# plotting functions (not mine)
# used in 'ctd_plot' above
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
{
  # modification by Ian Taylor of the filled.contour function
  # to remove the key and facilitate overplotting with contour()
  # further modified by Carey McGilliard and Bridget Ferris
  # to allow multiple plots on one page

  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
 # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
 # on.exit(par(par.orig))
 # w <- (3 + mar.orig[2]) * par("csi") * 2.54
 # par(las = las)
 # mar <- mar.orig
 plot.new()
 # par(mar=mar)
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                            col = col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
  # modification of filled.contour by Carey McGilliard and Bridget Ferris
  # designed to just plot the legend
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")

    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
  }



