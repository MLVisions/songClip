

#' Make equalizer data
#'
#' @param starting_vals vector of starting values
make_equalizer_data <- function(starting_vals = rep(0, 6)){

  # Must be exactly 6 values (this is how presets will work)
  checkmate::assert_numeric(starting_vals, len = 6)

  data <- tibble::tibble(
    index = seq(1,6),
    frequency = c(60, 150, 400, 1000, 2400, 15000),
    shift = starting_vals #round(jitter(c(rep(0, 2), 2, rep(0, 3)), 4)) # for testing
  ) %>%
    dplyr::mutate(
      freq_fmt = format_freq(frequency),
      shift_fmt = format_shift(shift)
    )

  # Ordered factor
  data <- data %>% dplyr::mutate(
    freq_fmt = ordered(freq_fmt, levels = unique(data$freq_fmt))
  )


  return(data)
}


#' Update equalizer data based on values in sliders
#'
#' @param eq_data dataset in the form returned by `make_equalizer_data`
#' @param slider_updates table of values returned by vertical sliders
update_equalizer_data <- function(eq_data, slider_updates){

  # Take opposite value to invert vertical sliders
  update_data <- eq_data %>% dplyr::mutate(
    shift = -slider_updates$value,
    shift_fmt = format_shift(shift)
  )

  return(update_data)
}

#' Make Equalizer plot
#'
#'
#' @param eq_data equalizer data with expected columns returned from
#'        `make_equalizer_data()`
#' @param shift_bounds y-axis limits.
#' @param avg_line Logical (`TRUE`/`FALSE`). If `TRUE`, plot additional polynomial fit
#'
#' @details
#'
#' TODO: add mechanism for moving the points (drag and drop)
#' # worst case can use sliders or something, but drag and drop is much more
#' user friendly.
#'
#' potential resource: https://stackoverflow.com/questions/47280032/draggable-line-chart-in-r-shiny
#'
#' @keywords internal
make_equalizer_plot <- function(eq_data = make_equalizer_data(),
                                shift_bounds = c(-12, 12),
                                avg_line = FALSE
){

  labeller_func <- function(shift_bounds){
    purrr::map_chr(shift_bounds, ~ format_shift(.x))
  }

  # Grey lines color
  grey_line_color <- rgb(235, 235, 235, 100, maxColorValue = 255)

  # Ensure formatted frequency is ordered factor
  data_pl <- eq_data %>%
    dplyr::mutate(
      freq_fmt = ordered(freq_fmt, levels = unique(eq_data_pl$freq_fmt))
    )

  # Base plot
  pl <- ggplot(data = data_pl, aes(x = index, y = shift, group = 1)) +
    # Fix Scales
    scale_x_continuous(breaks = data_pl$index, labels = data_pl$freq_fmt) +
    scale_y_continuous(limits = shift_bounds, breaks = shift_bounds,
                       labels = labeller_func) +
    # Add custom line for the center
    geom_hline(yintercept = 0, linetype = "solid", color = grey_line_color, linewidth = 0.5) +
    theme_dark() +
    labs(x = NULL, y = NULL) +
    ggtitle("Equalizer")

  if(isTRUE(avg_line)){
    eq_data_pl <- eq_smooth_line(data_pl, nsteps = 50, jitter = 200)
    eq_data_pl <- eq_data_pl %>%
      dplyr::mutate(
        freq_fmt = ordered(freq_fmt, levels = unique(eq_data_pl$freq_fmt))
      )
    pl <- pl +
      # geom_point(data = eq_data_pl, group = 1) +
      geom_smooth(data = eq_data_pl, color = "lightblue", linetype = 3, se = TRUE,
                  linewidth = 0.5,fill = "lightblue", alpha = 0.25)
  }

  # Add fake variability (negative only) to create shadow effect seen in Spotify
  pl <- stagger_eq_ribbon(pl, data_pl, 20, color = "#50C878")

  # Plot smoothed line to connect points
  pl <- pl + geom_smooth(color = "#0BDA51", linewidth = 1, se = FALSE, na.rm=TRUE)

  # Add main points on top
  pl <- pl + geom_point(data = eq_data, color = "white", size = 2)


  # Add theme
  pl <- pl +
    theme(
      # Center
      plot.title = element_text(hjust = 0.5),
      # Background
      panel.background = element_rect(fill = "#252525"),
      # remove the vertical grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank() ,
      # Horizontal lines grey
      panel.grid.major.x = element_line(linewidth=.3, color=grey_line_color),
      axis.text = element_text(size = 14),
      # margin
      axis.text.y = element_text(margin = margin(t = 0, r = 35, b = 0, l = 0)),
      axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )

  pl_plotly <- suppressWarnings(plotly::ggplotly(pl)) %>%
    config(edits = list(shapePosition = TRUE),
           showTips = FALSE, displayModeBar = FALSE)

  return(pl_plotly)
}







#' Add geom_ribbon to create shadow effect
#'
#' @param data_pl plot data
#' @param ntimes number of ribbons to make
#' @param color string. Color of ribbon fill.
#' @param alpha numeric. Value of alpha, passed to `ggplot::geom_ribbon()`
#'
#' @keywords internal
stagger_eq_ribbon <- function(pl,
                              data_pl,
                              ntimes = 12,
                              color = "green",
                              alpha = 0.05){

  # Add fake geom_smooth line to get data
  pl_fake <- pl + geom_smooth(se = FALSE)
  main_line <- suppressWarnings(ggplot_build(pl_fake)$data[[2]]) %>%
    dplyr::select(index = x, shift = y)


  stagger_eq_data <- function(data_pl, ntimes = 12){
    shifts <- purrr::map_dfc(seq(ntimes), ~ {
      shift_name <- paste0("r_shift_", .x)
      data_pl %>% dplyr::mutate(!!sym(shift_name) := shift - .x/4) %>%
        dplyr::select(all_of(shift_name))
    })
    cbind(data_pl, shifts)
  }

  data_stagger <- main_line %>% stagger_eq_data(ntimes = ntimes)

  shifts <- names(data_stagger)[grepl("r_shift", names(data_stagger))]

  if(!rlang::is_empty(shifts)){
    for(i in seq_along(shifts)){
      shifts_i <- shifts[i]
      pl <- pl + geom_ribbon(
        data = data_stagger,
        aes(ymin = !!sym(shifts_i), ymax = shift, group = 1),
        fill = color, alpha = alpha
      )
    }
  }

  return(pl)
}



#' Create smoothed line
#'
#' @param data_pl plot data
#' @param xvar x variable to perform `lm()` with (y is always `shift`).
#' @param nsteps number of steps to make in between points
#' @param jitter amount to jitter values to increase fake variability ribbon
#'
#' @keywords internal
eq_smooth_line <- function(data_pl, xvar = "index", nsteps = 50, jitter = 100){


  # fit polynomial for every three points
  # average overlapping estimates
  groups <- list(c(1:3), c(2:4), c(3:5), c(4:6))
  group_freqs <- purrr::map(groups, ~ data_pl$frequency[.x])

  data_new <- purrr::map_dfr(groups, function(group){
    data1 <- data_pl[group,]
    f1 <- as.formula(glue::glue("shift ~ poly({xvar}, 2)"))
    m1 <- lm(f1, data = data1)

    point_range_i <- data1$index
    point_range_f <- data1$frequency

    # Set new x axis and corresponding frequency
    x1 <- data.frame(
      index = unique(c(
        seq(point_range_i[1], point_range_i[2], length.out = nsteps/2),
        seq(point_range_i[2], point_range_i[3], length.out = nsteps/2)
      )),
      frequency = unique(c(
        seq(point_range_f[1], point_range_f[2], length.out = nsteps/2),
        seq(point_range_f[2], point_range_f[3], length.out = nsteps/2)
      ))
    )

    # Predict shift (round before jittering to increase 'd')
    predict_y <- round(predict(m1, x1), 2)
    jitter_factor <- exp(rnorm(length(predict_y), 1, sd = jitter/100))

    tibble::tibble(
      index = x1$index,
      frequency = x1$frequency,
      predict_y = predict_y,
      shift = round(predict_y * log(jitter_factor), 2)
    )
  })

  data_sum <- data_new %>%
    dplyr::group_by(frequency, index) %>%
    dplyr::summarize(shift = round(mean(shift), 2)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(frequency) %>%
    dplyr::mutate(
      freq_fmt = format_freq(frequency),
      shift_fmt = format_shift(shift)
    )

  return(data_sum)

}


format_freq <- function(freq){
  freq <- round(freq)
  ifelse(freq >= 1000, paste0(freq/1000, "KHz"), paste0(freq, "Hz"))
}

format_shift <- function(shift){
  if(shift >= 0){
    paste0("+",shift, "dB")
  }else{
    paste0(shift, "dB")
  }
}
