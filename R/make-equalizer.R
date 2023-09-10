
#' Make Equalizer plot
make_equalizer_plot <- function(shift_bounds = c(-12, 12)){
  data <- tibble::tibble(
    frequency = c(60, 150, 400, 1000, 2400, 15000),
    shift = rep(0, 6)
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

  data_pl <- data %>% dplyr::rename(Frequency = freq_fmt)

  # Base plot
  pl <- ggplot(data = data_pl, aes(x = Frequency, y = shift)) +
    # Fix Scales
    scale_x_discrete(limits = factor(data_pl$Frequency)) +
    scale_y_continuous(limits = shift_bounds, breaks = shift_bounds,
                       labels = labeller_func) +
    # Add custom lines for the center
    geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey", size = 0.5) +
    # Plot Data
    geom_line(color = "green", group = 1, size = 2) +
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
  return(
    list(
      plot_data = data,
      plot = pl
    )
  )
}



make_equalizer <- function(shift_bounds = c(-12, 12)){


  ui <- fluidPage(
    plotOutput("equalizer_plot", height = "500px", dblclick = "resetPlot"),
    tags$script("
      Shiny.addCustomMessageHandler('resetPlot', function(message) {
        Plotly.relayout('equalizer_plot', {
          'xaxis.range': [-10, 10],
          'yaxis.range': [-12, 12]
        });
      });
    ")
  )

  server <- function(input, output, session) {
    output$equalizer_plot <- plotly::renderPlotly({
      equalizer_plot <- make_equalizer_plot(shift_bounds = shift_bounds)
      equalizer_plot$plot %>%
      plotly::ggplotly() %>%
       plotly::config(editable = TRUE)
        plotly::layout(
          shapes = list(
            type = "circle",
            fillcolor = "white",
            line = list(color = "gray"),
            x0 = -2, x1 = 1,
            y0 = -2, y1 = 2,
            xsizemode = "pixel",
            ysizemode = "pixel",
            xref = "paper",  # Use paper for x-axis reference
            yref = "y"      # Use y for y-axis reference
          )
        )
    })
  }

  shinyApp(ui, server)
}
