

process_wave_channel <- function(x, xunit){
  channel <- if(is(x, "WaveMC")) x@.Data[,1] else x@left
  null <- if(x@bit == 8) 127 else 0
  l <- length(channel)

  if(simplify && (l > nr)){
    nr <- ceiling(l / round(l / nr))
    index <- seq(1, l, length = nr)
    if(xunit == "time") index <- index / x@samp.rate
    mat <- matrix(c(channel, if(l %% nr > 0) rep(NA, nr - (l %% nr))),
                  ncol = nr)
    rg <- apply(mat, 2, range, na.rm = TRUE)
    audio_data <- tibble::tibble(x = index, y0=rg[1,], y1 = rg[2,]) %>%
      tidyr::pivot_longer(c(y0, y1), names_to = "GRP", values_to = "y")
  }else{
    index <- seq(along = channel)
    if(xunit == "time") index <- index / x@samp.rate
    audio_data <- tibble::tibble(x = index, y=channel)
    dev_warning("Really short, potentially different audio: plotting code may have to be adjusted")
  }

  return(audio_data)
}



plot_Wave_channel <- function(audio_data){
  ggplot(data = audio_data) + aes(x = x, y = y, group = GRP) +
    geom_line()
}

#' @details
#' plot_wave_channel and plot_wave_audio were inspired by
#' tuneR's plotting method:
#' https://github.com/cran/tuneR/blob/master/R/plot-Wave.R
#' We want more control, the ability to plot the average (versus left and right),
#' and an interactive plot
#'
#' @keywords internal
plot_Wave_channel_base <-
  function(
    x,
    format,
    xunit,
    ylim,
    xlab,
    ylab,
    main,
    nr,
    simplify,
    axes = TRUE,
    yaxt = par("yaxt"),
    las = 1,
    center = TRUE
  ){

    audio_data <- process_wave_channel(x, xunit)

    if(format == "fancy"){
      # New plotting method (WIP)


    }else{
      # original plotting method
      index <- unique(audio_data$x)
      if(simplify && (l > nr)){
        rg <- tibble::tibble(
          y0 = audio_data$y[audio_data$GRP=="y0"],
          y1 = audio_data$y[audio_data$GRP=="y1"]
        )

        plot(rep(index, 2), c(rg[["y0"]], rg[["y1"]]), type = "n", yaxt = "n", ylim = ylim,
             xlab = xlab, ylab = NA, main = main, axes = axes, las = las)
        segments(x0 = index, y0 = rg[["y0"]], y1 = rg[["y1"]])
      }else{
        plot(audio_data,
             type = "l", yaxt = "n", ylim = ylim, xlab = xlab,
             ylab = NA, main = main, axes = axes, las = las)
      }
      mtext(ylab, side = 4, line = 0.5, at = mean(par("usr")[3:4]), cex = par("cex.lab"))

      if(!center || all(ylim <= 0)) {
        at <- axTicks(2)
      } else {
        at <- round((ylim[2] - null) * 2/3, -floor(log(ylim[2], 10)))
        at <- null + c(-at, 0, at)
      }
      if(axes) axis(2, at = at, yaxt = yaxt, las = las)
    }

  }



plot_wave_audio <- function(
    x,
    # new args
    type = c("left", "right", "both", "average"),
    format = c("fancy", "base"),
    # existing (potentially modified) args
    include_info = TRUE,
    xunit = c("Time", "Samples"),
    ylim = NULL,
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    simplify = TRUE,
    nr = 2500,
    axes = TRUE,
    yaxt = par("yaxt"),
    las = 1,
    center = TRUE
){

  if(!inherits(x, "Wave")){
    dev_bug("Different audio type object. Wont be able to plot")
  }

  format <- match.arg(format)
  xunit <- match.arg(xunit)

  if(is.null(xlab)) xlab <- xunit
  stereo <- x@stereo
  l <- length(x@left)
  if(center && is.null(ylim)){
    ylim <- range(x@left, x@right)
    if(x@bit == 8)
      ylim <- c(-1, 1) * max(abs(ylim - 127)) + 127
    else
      ylim <- c(-1, 1) * max(abs(ylim))
  }
  if(stereo){
    if(length(ylab)==1) ylab <- rep(ylab, 2)
    opar <- par(mfrow = c(2,1),
                oma = c((if(include_info) 6.1 else 5.1) + if(!is.null(sub)) 0.5 else 0, 0, 4.1, 0))
    on.exit(par(opar))
    mar <- par("mar")
    par(mar = c(0, mar[2], 0, mar[4]))
    plot_Wave_channel(tuneR::mono(x, "left"), format = format, xunit = xunit,
                      ylab = if(is.null(ylab)) "left channel" else ylab[1],
                      main = NULL, xlab = NA, ylim = ylim,
                      simplify = simplify, nr = nr,
                      axes = axes, yaxt = yaxt, las = las, center = center)
    plot_Wave_channel(tuneR::mono(x, "right"), format = format, xunit = xunit,
                      ylab = if(is.null(ylab)) "right channel" else ylab[2],
                      main = NULL, xlab = NA, ylim = ylim,
                      simplify = simplify, nr = nr,
                      axes = axes, yaxt = yaxt, las = las, center = center)
    title(main = main, outer = TRUE, line = 2)
    title(xlab = xlab, outer = TRUE, line = 3)
    title(sub  = sub , outer = TRUE, line = 4)
    par(mar = mar)
  }
  else{
    if(include_info){
      opar <- par(oma = c(2, 0, 0, 0))
      on.exit(par(opar))
    }
    plot_Wave_channel(x, format = format, xunit = xunit,
                      ylab = if(is.null(ylab)) "" else ylab,
                      main = main, sub = sub, xlab = xlab, ylim = ylim,
                      simplify = simplify, nr = nr,
                      axes = axes, yaxt = yaxt, las = las, center = center)
  }
  if(include_info){
    mtext(paste("Wave Object: ",
                l, " samples (",
                round(l / x@samp.rate, 2),  " sec.), ",
                x@samp.rate, " Hertz, ",
                x@bit, " bit, ",
                if(stereo) "stereo." else "mono.", sep = ""),
          side = 1, outer = TRUE, line = (if(stereo) 5 else 0) + if(!is.null(sub)) 0.5 else 0)
  }
}


