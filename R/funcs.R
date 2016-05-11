######
# plot bottom DO from CTD by distance and time  
# returns an interpolated two contour plot using methods similar to ctd_plot
#
# dat_in input ctd data, all dates and stations
# num_levs number of contour levels
# var variable to plot from dat_in
# ylab labels for y axis
# cols color vector
# ncol number of colors for smoothing plot
# num_int length of sequenced depth, time variables for interpolation
# dat_out logical to return data used to create plot
ctd_time <- function(dat_in, num_levs = 8, var = 'do_mgl', ylab = 'Depth (m)',
  cols = c('tomato', 'lightblue', 'lightgreen','green'),
  ncol = 100, num_int = 200, dat_out = FALSE){
  
  library(dplyr) 
  
  browser()
  
  # interp for plot
  
  # first create new grid
  uni_dts <- sort(unique(dat_in$Date))
  dists <- unique(dat_in$dist)
  num_int <- 200
  new_grd <- expand.grid(
      approx(dists, n = num_int)$y, 
      approx(uni_dts, n = num_int)$y
      )
  
  # then interp
  int_val <- fields::interp.surface(
    obj = list(  
      x = dists, 
      y = uni_dts, 
      z = do_mat[,-1]), 
    loc = new_grd
    )
  do_mat <- cbind(new_grd, int_val)
  names(do_mat) <- c('Distance', 'Date', 'DO')
  do_mat <- spread(do_mat, Date, DO)
  if(dat_out) return(do_mat)
  
  x.val <- as.numeric(names(do_mat)[-1])
  y.val <- do_mat$Distance
  z.val <- as.matrix(do_mat[, -1])
  in_col <- colorRampPalette(cols)
  
  # function to transpose
  rotate <- function(x) t(apply(x, 2, rev))
  
  # plot margins
  plot.new()
  par(new = "TRUE",plt = c(0.1,0.83,0.15,0.9),las = 1,cex.axis = 1)
  
  # contour plot with isolines
  filled.contour3(x = x.val, y = -1 * rev(y.val), z = rotate(z.val),
    color.palette = in_col, ylab = ylab,
    nlevels = ncol, # for smoothed colors
    axes = F)
  contour(x = x.val, y =  -1 * rev(y.val), z = rotate(z.val), nlevels= num_levs,
    axes = F, add = T)
  
  # axis labels
  axis(side = 2, at = -1 * seq(0, 35, by = 5), labels = seq(0, 35, by = 5))
  axis.Date(side = 3, x = as.Date(x.val), format = '%m-%Y')
  axis(side = 1, at = uni_dts, labels = uni_dts, tick = F, cex.axis = 0.5, las = 2, line = -0.5)
  axis(side = 4, at = -1 * rev(dists), labels = rev(unique(dat_in$Station)), tick = F, 
    cex.axis = 0.5, las = 1, line = -0.5)
  box()
  
  # legend
  par(new = "TRUE", plt = c(0.90,0.94,0.15,0.9), las = 1,cex.axis = 1)
  filled.legend(x.val,y.val,rotate(z.val),color=in_col,xlab = "",
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
