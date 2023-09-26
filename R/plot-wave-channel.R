

#' Plot wave channels of audio file
#'
#' @param audio_obj a `tuneR` audio object.
#' @param source a character string of length 1. Match the value of this string
#'        with the source argument in `plotly::plot_ly()` to respond to events
#'        emitted from that specific plot. Only used if `format = "fancy"`.
#'        See `?plotly::event_data` for more details.
#' @param type channel to plot. One of `c("left", "right", "stereo")`.
#' @param format format type. `"fancy"` is experimental and will eventually be
#'        the main one used within the app.
#' @param include_info Logical (`TRUE`/`FALSE`). If `TRUE`, append information
#'        about the audio file.
#' @param range_slider Logical (`TRUE`/`FALSE`). If `TRUE`, add a \code{\link{rangeslider}}.
#' @param simplify Logical (`TRUE`/`FALSE`). Whether to simplify large audio files. Defaults to `TRUE`.
#' @param nr Number of lines to draw for simplified plots. Only takes affect if `simplify = TRUE`.
#' @param plot_title a title for the plot (optional).
#' @param xunit unit for x-axis. One of `c("Time", "Samples")`.
#' @param ylim y-limit. Will be determined if not set.
#' @param xlab label for x-axis. Defaults to `xunit` if not set.
#' @param ylab label for y-axis. Will be determined if not set.
#'
#' @details
#' `plot_wave_channel_base` and `process_wave_channel` were inspired by
#' `tuneR`'s plotting method for `wave` files:
#' `https://github.com/cran/tuneR/blob/master/R/plot-Wave.R`
#'
#' We want more control over the visual, the ability to plot the different wave channels,
#' and for the plot to be interactive, so this code had to be pulled out and refactored.
#' `plot_wave_channel_fancy` uses `plotly` to create a much nicer plot with additional
#' features.
#'
#' Not sure if `source` will be supported for `stereo` types
#'
#' @seealso [add_play_tracker_line()]
#' @examples
#' # Read in audio file with `tuneR`
#' audio_obj <- tuneR::readMP3(file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3"))
#'
#' # Plot variations
#' plot_wave_audio(audio_obj)
#' plot_wave_audio(audio_obj, format = "base") # similar to `tuneR::plot()`
#' plot_wave_audio(audio_obj, type = "right")
#' plot_wave_audio(audio_obj, type = "stereo")
#' plot_wave_audio(audio_obj, range_slider = FALSE, include_info = FALSE)
#'
#' # step-wise (can only process one channel at a time - defaults to `audio_obj@left`)
#' wave_channel <- process_wave_channel(audio_obj, simplify = TRUE, nr = 2500)
#' plot_wave_channel_fancy(
#'     audio_data = wave_channel$audio_data,
#'     audio_params = wave_channel$params
#' )
#'
#'
#' @importFrom graphics par title mtext segments axis axTicks
#' @return a `plotly` object if `format = "fancy"`. Otherwise `NULL` invisibly
#' @export
plot_wave_audio <- function(audio_obj,
                            source = "wave_audio",
                            type = c("left", "right", "stereo"),
                            format = c("fancy", "base"),
                            include_info = TRUE,
                            range_slider = TRUE,
                            inner_plot = TRUE,
                            hollow = FALSE,
                            simplify = TRUE,
                            nr = 4000,
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
    ylim <- get_audio_limits(audio_obj)
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
        # Only add range_slider to one of the plots if set
        index <- grep(ylab_i, unlist(inputs$ylab_i))
        range_slider_i <- ifelse(
          index == length(inputs$audio_obj_i), range_slider, FALSE
        )

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
          source = source,
          hollow = hollow,
          ylab = ylab_i, xlab = xlab, ylim = ylim,
          range_slider = range_slider_i
        )
      })

      # Create subplot
      pl <- plotly::subplot(plots, shareY = TRUE, shareX = TRUE, nrows = 2)
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
        source = source,
        hollow = hollow,
        ylab = ylab, xlab = xlab, ylim = ylim,
        range_slider = range_slider
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
      # Set starting location (bottom of plot)
      y_shift <- -0.2
      # Adjust for range slider
      y_shift <- ifelse(isTRUE(range_slider), y_shift - 0.15, y_shift)
      # Adjust for shiny environment
      y_shift <- ifelse(shiny::isRunning(), y_shift - 0.09, y_shift)

      pl <- pl %>% plotly::layout(
        annotations = list(
          x = 1, y = y_shift, text = caption_txt,
          showarrow = F, xref='paper', yref='paper',
          xanchor='right', yanchor='auto', xshift=0, yshift=0,
          font = list(size=11)
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
#' @param audio_data Audio data returned from `process_wave_channel`.
#' @param audio_params List of audio parameters returned from `process_wave_channel`.
#' @param line_color line color
#' @param ft_color font color
#' @param bg_color background color
#' @param inner_plot Logical (`TRUE`/`FALSE`). If `TRUE`, add an inner plot consisting of two lines.
#' @param hollow Logical (`TRUE`/`FALSE`). If `TRUE`, make the plot hollow. Only usable for `"fancy"` plots.
#' @param inner_line_color1 line color of inner line 1
#' @param inner_line_color2 line color of inner line 2 (smallest of the three)
#' @param show_y_axis Logical (`TRUE`/`FALSE`). If `TRUE`, show the y-axis (limits only).
#'
#' @rdname plot_wave_audio
#' @keywords internal
plot_wave_channel_fancy <- function(audio_data,
                                    audio_params,
                                    source = NULL,
                                    ylim = NULL,
                                    xlab = "Time",
                                    ylab = "Audio Channel",
                                    hollow = FALSE,
                                    show_y_axis = FALSE,
                                    line_color = "#1b5fa6",
                                    ft_color = "lightgrey",
                                    bg_color = "#252525",
                                    range_slider = TRUE,
                                    inner_plot = TRUE,
                                    inner_line_color1 = "#4d91c8",
                                    inner_line_color2 = "#75b8ed"
){

  # Durations for fixing the x-axes
  duration_min <- audio_params$duration/60
  duration_min_date <- format_seconds(audio_params$duration, as_date = TRUE)

  # Group data
  group <- ifelse(isTRUE(hollow), "y_point", "line_group")
  pl_data <- audio_data %>%
    # convert to minutes
    dplyr::mutate(
      x = .data$x/60,
      # Convert x-axis to {date:time} (date will be dropped later)
      x_time = format_seconds(.data$x*60, as_date = TRUE)
    ) %>%
    dplyr::relocate("line_group", "y_point") %>%
    dplyr::group_by(!!sym(group))


  # font and styling
  t1 <- list(size = 15, color = ft_color)
  ty <- list(size = 14, color = ft_color)
  tx <- list(size = 18, color = ft_color)
  y_tick_vals <- if(isTRUE(show_y_axis)) ylim else NULL
  # Add unit if unchanged xlab and time
  if(!is.null(xlab) && xlab == audio_params$xunit && xlab == "Time"){
    xlab <- paste(xlab, "(minutes)")
  }

  # set margin
  slider_adjust <- ifelse(isTRUE(range_slider), 10, 40)
  margin <- if(shiny::isRunning()){
    list(b = 50 + slider_adjust, l = 70, r = 10)
  }else{
    list(b = 80 + slider_adjust, l = 70, r = 10)
  }

  # Core plot
  pl <- plotly::plot_ly(pl_data, x = ~x, y = ~y, source = source) %>%
    plotly::add_lines(color = I(line_color))

  # Add Inner plot (looks nicer)
  if(isTRUE(inner_plot)){
    pl_data_inner <- pl_data %>% dplyr::mutate(
      y2 = .data$y/1.35,
      y3 = .data$y/5.5
    )
    pl <- pl %>%
      plotly::add_lines(data = pl_data_inner, y = ~y2, color = I(inner_line_color1)) %>%
      plotly::add_lines(data = pl_data_inner, y = ~y3, color = I(inner_line_color2))
  }

  # Secondary x-axis for using tracker (invisible trace)
  # Note: this was necessary because updating the tracker using a date scale would
  # only allow 1 second interval jumps (less smooth).
  # Only use range to plot less data, while still ensuring 1:1 overlaying scales.
  pl_data2 <- tibble::tibble(
    x = seq(range(pl_data$x)[1], range(pl_data$x)[2], length.out = 100),
    y = seq(range(pl_data$y)[1], range(pl_data$y)[2], length.out = 100)
  ) %>% dplyr::mutate(
    # Convert x-axis to {date:time} (date will be dropped later)
    x_time = format_seconds(.data$x*60, as_date = TRUE)
  )
  pl <- pl %>% plotly::add_lines(data = pl_data2, x = ~x_time, y=~y,
                                 xaxis = "x2", color = I("transparent"),
                                 visible = "legendonly")


  # Range slider
  rangeslider <- if(isTRUE(range_slider)){
    list(visible = TRUE, thickness = 0.125, range = range(pl_data$x_time),
         yaxis = list(range = range(pl_data$y),rangemode = "fixed"))
  }else{
    FALSE
  }

  # Format
  pl %>%
    plotly::layout(
      # y-axis
      yaxis = list(
        title = list(text = ylab, font = ty),
        range = ylim*1.1, # increase data padding by 10%
        fixedrange = TRUE,
        tickvals = y_tick_vals,
        zerolinecolor = "black",
        tickfont = list(size = 15)
      ),
      # main x-axis (Date)
      xaxis = list(
        title = list(text = xlab, font = tx),
        range = duration_min,
        side = "bottom",
        anchor = "y",
        showgrid  = FALSE,
        showticklabels = FALSE,
        rangeslider = rangeslider,
        ticks = ""
      ),
      # secondary x-axis (numeric)
      xaxis2 = list(
        type = "date",
        range = duration_min_date,
        overlaying = "x",
        anchor = "free",
        position = 0.015,
        side = "bottom",
        showticklabels = TRUE,
        tickfont = list(size = 12),
        ticks = "inside",
        # ticklen = 10,
        # Format as {minutes:seconds}
        tickformat="%M:%S" #  add \n(%L ms) to show milliseconds (looks ugly)
      ),
      # styling
      font = t1,
      paper_bgcolor = bg_color,
      plot_bgcolor = bg_color,
      # margin
      margin = margin,
      # Legend
      showlegend = FALSE
    ) %>% config_plotly(edit_shapes = FALSE)
}



#' Original plotting method
#'
#' @param audio_data Audio data returned from `process_wave_channel`.
#' @param audio_params List of audio parameters returned from `process_wave_channel`.
#' @param axes Logical (`TRUE`/`FALSE`). If `TRUE`, add axes to the plot
#' @param center Logical (`TRUE`/`FALSE`). If `TRUE`, center the plot
#'
#' @rdname plot_wave_audio
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
#'
#' @rdname plot_wave_audio
#' @keywords internal
process_wave_channel <- function(audio_obj,
                                 xunit = "Time",
                                 simplify = TRUE,
                                 nr = 2500
){
  # Get channel and other parameters
  channel <- if(inherits(audio_obj, "WaveMC")) audio_obj@.Data[,1] else audio_obj@left
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
      tidyr::pivot_longer(c("y0", "y1"), names_to = "y_point", values_to = "y") %>%
      dplyr::mutate(line_group = rep(c(1, 1, 2, 2), length.out = n()))
  }else{
    # Take whole channel
    index <- seq(along = channel)
    if(xunit == "Time") index <- index / audio_obj@samp.rate
    audio_data <- tibble::tibble(x = index, y=channel, line_group = 1, y_point = "y")
    if(l > 0.5e6){
      warning("This is a large audio file and `simplify = FALSE` was passed. You may experience lags in the plot")
    }
    if(isTRUE(simplify)){
      # TODO: I got this warning, when I accidentally passed the right channel of an already left-filtered channel:
      # i.e. new_channel <- tuneR::mono(audio_obj, "left")
      # new_channel@right --> this makes sense, as it's empty. Might want to check for empty channels separately
      dev_warning("Unexpected audio format (may just be short): plotting code may have to be adjusted")
    }
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



#' Add vertical line to `plotly` object to track current play time
#'
#' Add vertical line to `plotly` returned from \code{\link{plot_wave_audio}}, to track
#' current play time. This function can also be used to update its underlying data inside
#' a shiny app by supplying a \code{\link{plotlyProxy}} object instead of `pl_plotly`.
#'
#' @param pl_plotly a `plotly` object. Only one of `proxy`, `pl_plotly` should be supplied.
#' @param x_val x-axis coordinate for placing the vertical line.
#' @param proxy a \code{\link{plotlyProxy}} object used for updating the location of the tracker.
#'        Only one of `proxy`, `pl_plotly` should be supplied.
#' @param x_axis which x-axis to add the tracker to.
#' @param color color of the play tracker line
#' @param shapeId id of the shape to be tracked. Required for updating the location.
#'
#' @note
#' `add_play_tracker_line` will *not* work with 'stereo' outputs of \code{\link{plot_wave_audio}}
#'
#'
#' @examples
#' # Read in audio file with `tuneR`
#' audio_obj <- tuneR::readMP3(file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3"))
#'
#' # Process wave file
#' wave_channel <- process_wave_channel(audio_obj)
#' pl_plotly <- plot_wave_channel_fancy(
#'                audio_data = wave_channel$audio_data,
#'                audio_params = wave_channel$params
#'              )
#'
#' ## Add tracker ##
#' # 1 min (numeric scale is in minutes)
#' pl_plotly %>% add_play_tracker_line(1)
#'
#' # 1 min (date formatting function takes in seconds)
#' pl_plotly %>% add_play_tracker_line(60, x_axis = "Date")
#'
#' ## Add loop trackers ##
#' pl_plotly %>% toggle_loop_trackers(x_range = c(0, 3.36), y_val = -32768)
#'
#' @keywords internal
add_play_tracker_line <- function(
    pl_plotly = NULL,
    x_val = 0,
    proxy = NULL,
    x_axis = c("numeric", "Date"),
    color = "red",
    shapeId = "redTrackerLine"
){

  x_axis <- match.arg(x_axis)

  # Function for creating vertical tracker line
  make_tracker_shape <- function(x_val, color, shapeId, xref){
    list(
      # vertical line
      list(
        type = "line",
        xref = xref,
        yref = "paper",
        line = list(color = color),
        x0 = x_val, x1 = x_val,
        y0 = 0.05, y1 = 0.95,
        name = shapeId
      )
    )
  }

  # x-axis handling
  if(x_axis == "Date"){
    op <- options(digits.secs = 6)
    on.exit(options(op), add = TRUE)
    x_val <- format_seconds(x_val, as_date = TRUE)
  }

  # Make shapes
  xref <- switch(x_axis, "Date" = "x2", "numeric" = "x1")
  shapes <- make_tracker_shape(x_val, color, shapeId, xref = xref)

  if(!is.null(pl_plotly) && inherits(pl_plotly, "plotly")){
    # add tracker line to `plotly` object
    pl_plotly %>% plotly::layout(shapes = shapes)
  }else if(!is.null(proxy)){
    # update tracker line in `renderPlotly` object
    plotly::plotlyProxyInvoke(proxy, "relayout", list(shapes = shapes))
  }else{
    stop("Only one of `proxy`, `pl_plotly` should be supplied. Make sure you passed the right object")
  }
}


#' @rdname add_play_tracker_line
#'
#' @param toggle Logical (`TRUE`/`FALSE`). If `TRUE`, add loop trackers. If `FALSE`,
#'        remove them if they exist.
#' @param x_range starting x-axis values for the loop tracker
#' @param y_val Y-axis value for the loop trackers. Should be the minimum of the
#'        y-axis range
#'
#' @keywords internal
toggle_loop_trackers <- function(
    pl_plotly = NULL,
    toggle = TRUE,
    proxy = NULL,
    channel_type = c("left", "right", "stereo"),
    x_range = c(0, 3.36),
    y_val = -32768,
    shapeId = "loopTrackers"
){

  is_valid_plotly <- !is.null(pl_plotly) && inherits(pl_plotly, "plotly")
  channel_type <- match.arg(channel_type)

  if(isFALSE(toggle)){
    if(isTRUE(is_valid_plotly)){
      # remove tracker attribute
      attrs <- purrr::keep(pl_plotly$x$attrs, function(attr){
        is.null(attr$name) || attr$name != shapeId
      })
      pl_plotly$x$attrs <- attrs
      return(pl_plotly)
    }else{
      # Remove trace from proxy
      # TODO: find a way to reference shapeId
      trace <- ifelse(channel_type != "stereo", 4, 7)
      plotly::plotlyProxyInvoke(proxy, "deleteTraces", list(as.integer(trace)))
    }
  }else{
    checkmate::assert_numeric(x_range, len = 2)
    checkmate::assert_numeric(y_val, len = 1)

    # Adjust y_val for shiny and stereo channels
    y_val_pl <- ifelse(shiny::isRunning(), y_val*0.8, y_val*1.1)
    y_val_pl <- ifelse(channel_type != "stereo", y_val_pl, y_val_pl*0.8)

    loop_data <- tibble::tibble(x = x_range, y = rep(y_val_pl, 2))

    # Make shapes
    markers <- list(symbol = "arrow-up", size = 30, color = c("green", "red"))
    y_axis <- ifelse(channel_type != "stereo", "y1", "y2")

    if(isTRUE(is_valid_plotly)){
      # add loop trackers to `plotly` object
      pl_plotly <- pl_plotly %>% plotly::add_markers(
        data = loop_data, marker = markers, name = shapeId,
        xaxis = "x1", yaxis = y_axis, hoverinfo = "none"
      )
      return(pl_plotly)
    }else{
      # add or update loop trackers in `renderPlotly` object
      plotly::plotlyProxyInvoke(proxy, "addTraces", list(list(
        x = loop_data$x,
        y = loop_data$y,
        xaxis = "x1",
        yaxis = y_axis,
        mode = "markers",
        name = shapeId,
        marker = markers,
        hoverinfo = "none"
      )))
    }
  }
}




#' Add vertical lines to plotly object that can be used for cropping
#'
#' @param pl_plotly a `plotly` object
#' @inheritParams plot_wave_channel_fancy
#' @param color color of lines
#'
#' @keywords internal
add_crop_lines <- function(pl_plotly, audio_params, color = "#0BDA51"){

  crop_range <- audio_params$duration/60
  crop_start <- crop_range*(3/4)

  pl_plotly %>% plotly::layout(
    shapes = list(
      # vertical line
      list(type = "line",
           line = list(color = color, width = 3),
           x0 = crop_start[1], x1 = crop_start[1],
           y0 = 0, y1 = 1, yref = "paper"),
      list(type = "line",
           line = list(color = color, width = 3),
           x0 = crop_start[2], x1 = crop_start[2],
           y0 = 0, y1 = 1, yref = "paper")
    )
  )
}
