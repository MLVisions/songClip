

#' Plot wave channels of audio file
#'
#' @param audio_obj a `tuneR` audio object.
#' @param type channel to plot. One of `c("left", "right", "stereo")`.
#' @param format format type. `"fancy"` is experimental and will eventually be
#'        the main one used within the app.
#' @param simplify Logical (`TRUE`/`FALSE`). Whether to simplify large audio files. Defaults to `TRUE`.
#' @param nr noise reduction. Only takes affect if `simplify = TRUE`.
#' @param include_info Logical (`TRUE`/`FALSE`). If `TRUE`, append information
#'        about the audio file.
#' @param plot_title a title for the plot (optional).
#' @param xunit unit for x-axis. One of `c("Time", "Samples")`.
#' @param ylim y-limit. Will be determined if not set.
#' @param xlab label for x-axis. Defaults to `xunit` if not set.
#' @param ylab label for y-axis. Will be determined if not set.
#'
#' @details
#' plot_wave_channel and plot_wave_audio were inspired by
#' tuneR's plotting method:
#' https://github.com/cran/tuneR/blob/master/R/plot-Wave.R
#' We want more control, the ability to plot the average (versus left and right),
#' and an interactive plot
#'
#'
#' @examples
#' \dontrun{
#' audio_obj <- tuneR::readMP3(file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3"))
#'
#' plot_wave_audio(audio_obj)
#'
#' plot_wave_audio(audio_obj, type = "right")
#'
#' plot_wave_audio(audio_obj, type = "stereo")
#' }
#'
#'
#' @return a `ggplot` object if `format = "fancy"`. Otherwise `NULL` invisibly
#' @export
plot_wave_audio <- function(audio_obj,
                            type = c("left", "right", "stereo"),
                            format = c("base", "fancy"),
                            simplify = TRUE,
                            nr = 2500,
                            include_info = TRUE,
                            plot_title = NULL,
                            xunit = c("Time", "Samples"),
                            ylim = NULL,
                            xlab = NULL,
                            ylab = NULL
){

  if(!inherits(audio_obj, "Wave")){
    dev_bug("Different audio type object. Wont be able to plot")
  }

  format <- match.arg(format)
  type <- match.arg(type)
  xunit <- match.arg(xunit)

  # x label and unit
  if(is.null(xlab)) xlab <- xunit

  # decide whether to split audio
  has_stereo <- audio_obj@stereo
  use_stereo <- has_stereo && type == "stereo"

  # Define plotting function
  plot_wave_channel <- switch (format,
                               "base" = plot_wave_channel_base,
                               "fancy" = plot_wave_channel_fancy
  )

  # ylim
  if(is.null(ylim)){
    ylim <- range(audio_obj@left, audio_obj@right)
    if(audio_obj@bit == 8)
      ylim <- c(-1, 1) * max(abs(ylim - 127)) + 127
    else
      ylim <- c(-1, 1) * max(abs(ylim))
  }

  ### Multiple Audio Channels ###
  if(isTRUE(use_stereo)){

    if(length(ylab)==1) ylab <- rep(ylab, 2)

    # List of inputs to map across
    inputs <- list(
      audio_obj = list(
        tuneR::mono(audio_obj, "left"),
        tuneR::mono(audio_obj, "right")
      ),
      ylab = list(
        if(is.null(ylab)) "Left Channel" else ylab[1],
        if(is.null(ylab)) "Right Channel" else ylab[2]
      )
    )

    # Create plots
    if(format == "base"){

      # Set margin
      opar <- par(mfrow = c(2,1),
                  oma = c((if(include_info) 6.1 else 5.1), 0, 4.1, 0))
      on.exit(par(opar))
      mar <- par("mar")
      par(mar = c(0, mar[2], 0, mar[4]))

      # Print plots directly to viewer using base R plot() function
      purrr::pwalk(inputs, function(audio_obj, ylab){
        # browser()
        # process each wave channel
        wave_channel <- process_wave_channel(
          audio_obj,
          xunit = xunit,
          simplify = simplify,
          nr = nr
        )
        # plot each channel stacked vertically (according to set margins)
        plot_wave_channel(
          audio_data = wave_channel$audio_data,
          audio_params = wave_channel$params,
          ylab = ylab, ylim = ylim,
          plot_title = NULL, xlab = NA
        )
      })

      # titles and labels for combined plot
      title(main = plot_title, outer = TRUE, line = 2)
      title(xlab = xlab, outer = TRUE, line = 3)
      # adjust margin back
      par(mar = mar)
    }else{

      # Store plots as list of ggplot objects
      plots <- purrr::pmap(inputs, function(audio_obj, ylab){
        wave_channel <- process_wave_channel(
          audio_obj,
          xunit = xunit,
          simplify = simplify,
          nr = nr
        )
        # plot each channel stacked vertically
        plot_wave_channel(
          audio_data = wave_channel$audio_data,
          audio_params = wave_channel$params,
          ylab = ylab, ylim = ylim,
          plot_title = NULL, xlab = NA
        )
      })

      pl <- facet_grid(plots, cols = 1)
    }
  }else{
    ### Single Audio Channel ###

    # set ylab
    if(is.null(ylab)){
      if(isTRUE(has_stereo)){
        ylab <- switch (type,
                        "left" = "Left Channel",
                        "right" = "Right Channel"
        )
      }else{
        ylab <- "Audio Channel"
      }
    }

    # Process wave channel
    audio_obj <- tryCatch(tuneR::mono(audio_obj, type), error = identity)
    wave_channel <- process_wave_channel(
      audio_obj,
      xunit = xunit,
      simplify = simplify,
      nr = nr
    )

    # Create plot
    if(format == "base"){
      if(include_info){
        opar <- par(oma = c(4, 0, 1, 1))
        on.exit(par(opar))
      }

      plot_wave_channel(
        audio_data = wave_channel$audio_data,
        audio_params = wave_channel$params,
        ylab = ylab, ylim = ylim,
        plot_title = NULL, xlab = NA
      )
    }else{
      pl <- plot_wave_channel(
        audio_data = wave_channel$audio_data,
        audio_params = wave_channel$params,
        ylab = ylab, ylim = ylim,
        plot_title = plot_title, xlab = xlab
      )
    }
  }

  # Optionally Append Info
  if(isTRUE(include_info)){
    l <- length(audio_obj@left)
    caption_txt <- paste("Wave Object: ", l, " samples (",
                         round(l / audio_obj@samp.rate, 2),  " sec.), ",
                         audio_obj@samp.rate, " Hertz, ",
                         audio_obj@bit, " bit, ",
                         if(audio_obj@stereo) "stereo." else "mono.", sep = "")

    if(format == "base"){
      mtext(
        caption_txt,
        side = 1, outer = TRUE,
        line = (if(use_stereo) 5 else 2.5)
      )
    }else{
      pl <- pl + ggplot2::labs(caption = caption_txt)
    }
  }

  # Return object if fancy format; nothing to return for base plot
  if(format == "base"){
    return(invisible(NULL))
  }else{
    return(pl)
  }

}




#' New plotting method
#'
#' @inheritParams plot_wave_channel_base
#'
#' @keywords internal
plot_wave_channel_fancy <- function(audio_data,
                                    audio_params,
                                    ylim,
                                    xlab,
                                    ylab,
                                    plot_title = NULL,
                                    axes = TRUE
){
  ggplot(data = audio_data) + aes(x = x, y = y, group = GRP) +
    geom_line()
}



#' Original plotting method
#'
#' @param audio_data Audio data returned from `process_wave_channel`.
#' @param audio_params List of audio parameters returned from `process_wave_channel`.
#' @inheritParams plot_wave_audio
#' @param axes Logical (`TRUE`/`FALSE`). If `TRUE`, add axes to the plot
#' @param center Logical (`TRUE`/`FALSE`). If `TRUE`, center the plot
#'
#' @keywords internal
plot_wave_channel_base <- function(audio_data,
                                   audio_params,
                                   ylim,
                                   xlab,
                                   ylab,
                                   plot_title = NULL,
                                   axes = TRUE,
                                   center = TRUE
){
  simplified <- audio_params$simplified
  null <- audio_params$null
  index <- unique(audio_data$x)

  if(isTRUE(simplified)){

    rg <- tibble::tibble(
      y0 = audio_data$y[audio_data$GRP=="y0"],
      y1 = audio_data$y[audio_data$GRP=="y1"]
    )

    plot(rep(index, 2), c(rg[["y0"]], rg[["y1"]]), type = "n", yaxt = "n", ylim = ylim,
         xlab = xlab, ylab = NA, main = plot_title, axes = axes, las = 1)
    segments(x0 = index, y0 = rg[["y0"]], y1 = rg[["y1"]])
  }else{
    plot(audio_data,
         type = "l", yaxt = "n", ylim = ylim, xlab = xlab,
         ylab = NA, main = plot_title, axes = axes, las = 1)
  }

  mtext(ylab, side = 4, line = 0.5, at = mean(par("usr")[3:4]), cex = par("cex.lab"))

  if(!center || all(ylim <= 0)) {
    at <- axTicks(2)
  } else {
    at <- round((ylim[2] - null) * 2/3, -floor(log(ylim[2], 10)))
    at <- null + c(-at, 0, at)
  }

  if(axes) axis(2, at = at, yaxt = par("yaxt"), las = 1)
}



#' Process Wave channel
#'
#' @inheritParams plot_wave_audio
#'
#' @keywords internal
process_wave_channel <- function(audio_obj,
                                 xunit = "Time",
                                 simplify = TRUE,
                                 nr = 2500
){
  channel <- if(is(audio_obj, "WaveMC")) audio_obj@.Data[,1] else audio_obj@left
  null <- if(audio_obj@bit == 8) 127 else 0
  l <- length(channel)

  simplified <- simplify && (l > nr)
  if(isTRUE(simplified)){
    nr <- ceiling(l / round(l / nr))
    index <- seq(1, l, length = nr)
    if(xunit == "Time") index <- index / audio_obj@samp.rate
    mat <- matrix(c(channel, if(l %% nr > 0) rep(NA, nr - (l %% nr))),
                  ncol = nr)
    rg <- apply(mat, 2, range, na.rm = TRUE)
    audio_data <- tibble::tibble(x = index, y0=rg[1,], y1 = rg[2,]) %>%
      tidyr::pivot_longer(c(y0, y1), names_to = "GRP", values_to = "y")
  }else{
    index <- seq(along = channel)
    if(xunit == "Time") index <- index / audio_obj@samp.rate
    audio_data <- tibble::tibble(x = index, y=channel)
    dev_warning("Unexpected audio format (may just be short): plotting code may have to be adjusted")
  }

  return(
    list(
      audio_data = audio_data,
      params = list(
        simplified = simplified,
        null = null,
        l = l
      )
    )
  )
}
