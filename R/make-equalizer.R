

format_freq <- function(freq){
  freq <- round(freq)
  ifelse(freq >= 1000, paste0(freq/1000, "KHz"), paste0(freq, "Hz"))
}

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
      freq_fmt = ordered(freq_fmt, levels = unique(eq_data$freq_fmt)),
      shift_fmt = paste0(shift, "dB")
    )

  return(data)
}


#' Update equalizer data based on values in sliders
#'
#' @param eq_data dataset in the form returned by `make_equalizer_data`
#' @param slider_updates table of values returned by vertical sliders
update_equalizer_data <- function(eq_data, slider_updates){

  update_data <- eq_data %>% dplyr::mutate(
    shift = slider_updates$value,
    shift_fmt = paste0(shift, "dB")
  )

  return(update_data)
}

#' Make Equalizer plot
#'
#' TODO: add mechanism for moving the points (drag and drop)
#' # worst case can use sliders or something, but drag and drop is much more
#' user friendly.
#'
#' potential resource: https://stackoverflow.com/questions/47280032/draggable-line-chart-in-r-shiny
make_equalizer_plot <- function(eq_data = make_equalizer_data(),
                                shift_bounds = c(-12, 12)
){

  labeller_func <- function(shift_bounds){
    purrr::map_chr(shift_bounds, ~ {
      if(.x >= 0){
        paste0("+",.x, "dB")
      }else{
        paste0(.x, "dB")
      }
    })
  }

  data_pl <- eq_data %>%
    dplyr::mutate(
      freq_fmt = ordered(freq_fmt, levels = unique(eq_data$freq_fmt))
    ) %>%
    dplyr::rename(Frequency = freq_fmt)

  # Base plot
  pl <- ggplot(data = data_pl, aes(x = index, y = shift, group = 1)) +
    # Fix Scales
    scale_x_continuous(breaks = data_pl$index, labels = data_pl$Frequency) +
    scale_y_continuous(limits = shift_bounds, breaks = shift_bounds,
                       labels = labeller_func) +
    # Add custom lines for the center
    geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey", size = 0.5) +
    # Plot Data
    # geom_line(color = "green", group = 1, linewidth = 2) +
    geom_smooth(color = "green") +
    theme_dark() +
    labs(x = NULL, y = NULL) +
    ggtitle("Equalizer")

  # Add fake variability (negative only) to create shadow effect seen in Spotify
  pl <- stagger_eq_ribbon(pl, data_pl, 100)

  # Add main marker on top
  pl <- pl + geom_point(color = "white", size = 3.5)

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
      panel.grid.major.x = element_line(linewidth=.3, color="lightgrey"),
      axis.text = element_text(size = 14),
      # margin
      axis.text.y = element_text(margin = margin(t = 0, r = 35, b = 0, l = 0)),
      axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )

  pl_plotly <- plotly::ggplotly(pl)

  return(pl_plotly)
}





stagger_eq_data <- function(eq_data, ntimes = 12){

  shifts <- purrr::map_dfc(seq(ntimes), ~{
    shift_name <- paste0("r_shift_", .x)
    shift <- eq_data %>% dplyr::mutate(!!sym(shift_name) := shift - .x/4) %>%
      dplyr::select(all_of(shift_name))
    shift
  })

  cbind(eq_data, shifts)

}

#' Add geom_ribbon to create shadow effect
#'
#'
stagger_eq_ribbon <- function(pl, data_pl, nsteps, ntimes){

  data_stagger <- eq_smooth_line(data_pl, nsteps = 20) %>% stagger_eq_data(ntimes = 20)

  shifts <- names(data_stagger)[grepl("r_shift", names(data_stagger))]

  if(!rlang::is_empty(shifts)){
    for(i in seq_along(shifts)){
      shifts_i <- shifts[i]
      pl <- pl + geom_ribbon(data = data_stagger, aes(ymin = !!sym(shifts_i), ymax = shift, group = 1), fill = "green", alpha=0.05)
    }
  }

  return(pl)
}



#' Create smoothed line
#'
#'
eq_smooth_line <- function(data_pl, yvar = "index", nsteps = 20){


  # fit polynomial for every three points
  # average overlapping estimates
  groups <- list(c(1:3), c(2:4), c(3:5), c(4:6))
  group_freqs <- purrr::map(groups, ~ data_pl$frequency[.x])

  data_new <- purrr::map_dfr(groups, function(group){
    data1 <- data_pl[group,]
    f1 <- as.formula(glue::glue("shift ~ poly({yvar}, 2)"))
    m1 <- lm(f1, data = data1)

    point_range_i <- data1$index
    point_range_f <- data1$frequency

    x1 <- data.frame(
      index = c(
        seq(point_range_i[1], point_range_i[2], length.out = nsteps/2),
        seq(point_range_i[2], point_range_i[3], length.out = nsteps/2)
      ),
      frequency = c(
        seq(point_range_f[1], point_range_f[2], length.out = nsteps/2),
        seq(point_range_f[2], point_range_f[3], length.out = nsteps/2)
      )
    )

    tibble::tibble(
      index = x1$index,
      frequency = x1$frequency,
      shift = round(predict(m1, x1), 1)
    )
  })

  data_sum <- data_new %>%
    dplyr::group_by(frequency, index) %>%
    dplyr::summarize(shift = mean(shift)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(frequency) %>%
    dplyr::mutate(
      Frequency = format_freq(frequency)
    )

  return(data_sum)

}
