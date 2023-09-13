
#' Make Equalizer plot
#'
#' TODO: add mechanism for moving the points (drag and drop)
#' # worst case can use sliders or something, but drag and drop is much more
#' user friendly.
#'
#' potential resource: https://stackoverflow.com/questions/47280032/draggable-line-chart-in-r-shiny
make_equalizer_plot <- function(shift_bounds = c(-12, 12)){
  data <- tibble::tibble(
    frequency = c(60, 150, 400, 1000, 2400, 15000),
    shift = rep(0, 6) #round(jitter(c(rep(0, 2), 2, rep(0, 3)), 4)) # for testing
  ) %>%
    dplyr::mutate(
      freq_fmt = ifelse(frequency >= 1000, paste0(frequency/1000, "KHz"), paste0(frequency, "Hz")),
      shift_fmt = paste0(shift, "dB")
    )

  labeller_func <- function(shift_bounds){
    purrr::map_chr(shift_bounds, ~ {
      if(.x >= 0){
        paste0("+",.x, "dB")
      }else{
        paste0(.x, "dB")
      }
    })
  }

  data_pl <- data %>%
    dplyr::mutate(freq_fmt = ordered(freq_fmt, levels = unique(data$freq_fmt))) %>%
    dplyr::rename(Frequency = freq_fmt)

  # Base plot
  pl <- ggplot(data = data_pl, aes(x = Frequency, y = shift)) +
    # Fix Scales
    scale_x_discrete(limits = data_pl$Frequency) +
    scale_y_continuous(limits = shift_bounds, breaks = shift_bounds,
                       labels = labeller_func) +
    # Add custom lines for the center
    geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey", size = 0.5) +
    # Plot Data
    geom_line(color = "green", group = 1, size = 2) +
    # TODO: add fake variability (negative only) to create shadow effect in spotify
    # geom_smooth(method='gam', se = TRUE, alpha=0.2, formula=y~s(x,bs="cr")) +
    geom_point(color = "white", size = 3.5) +
    theme_dark() +
    labs(x = NULL, y = NULL) +
    ggtitle("Equalizer")

  # Add theme
  pl <- pl +
    theme(
      # Center
      plot.title = element_text(hjust = 0.5),
      # Background
      # panel.background = element_rect(fill = "#252525"),
      # remove the vertical grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank() ,
      # Horizontal lines grey
      panel.grid.major.x = element_line(size=.3, color="lightgrey"),
      axis.text = element_text(size = 14),
      # margin
      axis.text.y = element_text(margin = margin(t = 0, r = 35, b = 0, l = 0)),
      axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )

  plot <- plotly::ggplotly(pl)

  return(
    list(
      plot_data = data,
      plot = plot
    )
  )
}






