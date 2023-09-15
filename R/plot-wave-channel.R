

#' Plot wave channels of audio file
#'
#' @param audio_obj a `tuneR` audio object.
#' @param type channel to plot. One of `c("left", "right", "stereo")`.
#' @param format format type. `"fancy"` is experimental and will eventually be
#'        the main one used within the app.
#' @param hollow Logical (`TRUE`/`FALSE`). If `TRUE`, make the plot hollow. Only usable for `"fancy"` plots.
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
#'
#' # Experimental
#' plot_wave_audio(audio_obj, format = "fancy")
#' }
#'
#'
#' @return a `plotly` object if `format = "fancy"`. Otherwise `NULL` invisibly
#' @export
plot_wave_audio <- function(audio_obj,
                            type = c("left", "right", "stereo"),
                            format = c("fancy", "base"),
                            hollow = FALSE,
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
    # This will almost certainly happen at some point, but want to catch when.
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
      audio_obj_i = list(
        tuneR::mono(audio_obj, "left"),
        tuneR::mono(audio_obj, "right")
      ),
      ylab_i = list(
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
      purrr::pwalk(inputs, function(audio_obj_i, ylab_i){
        # browser()
        # process each wave channel
        wave_channel <- process_wave_channel(
          audio_obj_i,
          xunit = xunit,
          simplify = simplify,
          nr = nr
        )
        # plot each channel stacked vertically (according to set margins)
        plot_wave_channel(
          audio_data = wave_channel$audio_data,
          audio_params = wave_channel$params,
          ylab = ylab_i, ylim = ylim,
          plot_title = NULL, xlab = NA
        )
      })

      # titles and labels for combined plot
      title(main = plot_title, outer = TRUE, line = 2)
      title(xlab = xlab, outer = TRUE, line = 3)
      # adjust margin back
      par(mar = mar)
    }else{

      # Store plots as list of plotly objects
      plots <- purrr::pmap(inputs, function(audio_obj_i, ylab_i){
        wave_channel <- process_wave_channel(
          audio_obj_i,
          xunit = xunit,
          simplify = simplify,
          nr = nr
        )
        # plot each channel stacked vertically
        plot_wave_channel(
          audio_data = wave_channel$audio_data,
          audio_params = wave_channel$params,
          hollow = hollow,
          ylab = ylab_i, ylim = ylim,
          plot_title = NULL, xlab = xlab
        )
      })

      # This wont work anymore
      pl <- cowplot::plot_grid(plotlist=plots, ncol = 1)
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
    # TODO: have informative error/warning messages and handling for tryCatch
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
        hollow = hollow,
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
      # For some reason, the margins are intepreted differently when run in
      # a shiny environment
      y_shift <- ifelse(shiny::isRunning(), -0.6, -0.3)
      pl <- pl %>% plotly::layout(
        annotations = list(
          x = 1, y = y_shift, text = caption_txt,
          showarrow = F, xref='paper', yref='paper',
          xanchor='right', yanchor='auto', xshift=0, yshift=0,
          font=list(size=13)
        )
      )
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
#' @param hollow Logical (`TRUE`/`FALSE`). If `TRUE`, make the plot hollow
#'
#' @keywords internal
plot_wave_channel_fancy <- function(audio_data,
                                    audio_params,
                                    ylim = NULL,
                                    xlab = NULL,
                                    ylab = NULL,
                                    plot_title = NULL,
                                    axes = TRUE,
                                    hollow = FALSE,
                                    line_color = "#ADD8E6",
                                    ft_color = "lightgrey",
                                    bg_color = "#252525"
){

  n_points <- length(unique(audio_data$x))
  duration <- audio_params$duration/60 # convert to minutes

  # Group data
  group <- ifelse(isTRUE(hollow), "y_point", "line_group")
  pl_data <- audio_data %>%
    # convert to minutes
    dplyr::mutate(x = x/60) %>%
    dplyr::group_by(!!sym(group))

  # font and styling
  t1 <- list(size = 15, color = ft_color)
  # Add unit if unchanged xlab and time
  if(xlab == audio_params$xunit && xlab == "Time"){
    xlab <- paste(xlab, "(minutes)")
  }

  # Core plot
  pl <- plotly::plot_ly(pl_data, x = ~x, y = ~y) %>%
    plotly::add_lines( color = I(line_color))

  # Format
  pl %>%
    plotly::layout(
      yaxis = list(
        range = ylim*2,
        fixedrange = TRUE, title = ylab,
        tickvals = ylim,
        zerolinecolor = "black",
        tickfont = list(size = 18)
      ),
      xaxis = list(
        title = xlab,
        range = duration
      ),
      # styling
      font = t1,
      paper_bgcolor = bg_color,
      plot_bgcolor = bg_color,
      # margin
      margin = list(pad = 30, b = 130, t = 80),
      # Legend
      showlegend = FALSE
    ) %>% config_plotly()
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
                                   ylim = NULL,
                                   xlab = NULL,
                                   ylab = NULL,
                                   plot_title = NULL,
                                   axes = TRUE,
                                   center = TRUE
){
  simplified <- audio_params$simplified
  null <- audio_params$null
  index <- unique(audio_data$x)

  if(isTRUE(simplified)){

    # Format table of y points
    rg <- tibble::tibble(
      y0 = audio_data$y[audio_data$y_point=="y0"],
      y1 = audio_data$y[audio_data$y_point=="y1"]
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
  # Get channel and other parameters
  channel <- if(is(audio_obj, "WaveMC")) audio_obj@.Data[,1] else audio_obj@left
  null <- if(audio_obj@bit == 8) 127 else 0
  l <- length(channel)
  simplified <- simplify && (l > nr)
  duration <- get_audio_dur(audio_obj)

  if(isTRUE(simplified)){
    # Simplify Data
    nr <- ceiling(l / round(l / nr))
    index <- seq(1, l, length = nr)
    if(xunit == "Time") index <- index / audio_obj@samp.rate
    mat <- matrix(c(channel, if(l %% nr > 0) rep(NA, nr - (l %% nr))),
                  ncol = nr)
    rg <- apply(mat, 2, range, na.rm = TRUE)

    # Standardize output to be more understandable regardless of plotting method
    # `line_group` column is the grouping that connects the segments meant to connect.
    # see ?segments and tuneR::plot() methods for Wave files for more details.
    audio_data <- tibble::tibble(x = index, y0=rg[1,], y1 = rg[2,]) %>%
      tidyr::pivot_longer(c(y0, y1), names_to = "y_point", values_to = "y") %>%
      dplyr::mutate(line_group = rep(c(1, 1, 2, 2), length.out = n()))
  }else{
    # Take whole channel
    index <- seq(along = channel)
    if(xunit == "Time") index <- index / audio_obj@samp.rate
    audio_data <- tibble::tibble(x = index, y=channel)
    # TODO: I got this warning, when I accidentally passed the right channel of an already left-filtered channel:
    # i.e. new_channel <- tuneR::mono(audio_obj, "left")
    # new_channel@right --> this makes sense, as it's empty. Might want to check for empty channels separately
    dev_warning("Unexpected audio format (may just be short): plotting code may have to be adjusted")
  }

  return(
    list(
      audio_data = audio_data,
      params = list(
        duration = duration,
        simplified = simplified,
        xunit = xunit,
        null = null,
        l = l
      )
    )
  )
}
