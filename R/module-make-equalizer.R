# Module UI ---------------------------------------------------------------

#' Create shiny module for modifying the equalizer
#' @describeIn make_equalizer_ui Create module UI for modifying the equalizer
#'
#' @keywords internal
make_equalizer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      style = "background-color: #252525; padding-bottom: 1em;
      margin-right: 0px; margin-left: 0px;",
      plotly::plotlyOutput(ns("equalizer_plot"), height = "325px"),
      fluidRow(
        style = "margin-right: 0px; margin-left: 0px;",
        column(
          width = 2, offset = 10, align = "right",
          shinyWidgets::actionBttn(
            ns("reset"), "Reset",
            style = "bordered", color = "success"
          ),
          br()
        )
      )
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn make_equalizer_ui Create module server for modifying the equalizer
#'
#'
#' @keywords internal
make_equalizer_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      .rv <- reactiveValues(reset = FALSE)
      ns <- session$ns

      # TODO: make list of presets

      # Cached dataset - this one updates as soon as plotly does
      equalizer_data_cache <- reactiveVal(make_equalizer_data())

      # reactive dataset - updates less frequently
      # TODO: fix reset button (complicated reactivity)
      equalizer_data <- eventReactive(list(equalizer_data_cache(), input$reset),{
        # browser()
        if(isTRUE(.rv$reset)){
          .rv$reset <- FALSE
          df <- make_equalizer_data()
        }else{
          df <- shiny::req(equalizer_data_cache())
        }
        df
      }) #%>% throttle(1000)


      # not currently used or needed (just in case)
      # model <- reactive({
      #   eq_data <- shiny::req(equalizer_data())
      #   lm(shift ~ poly(index,2), eq_data)
      # })

      equalizer_plot_reac <- reactive({
        eq_data <- shiny::req(equalizer_data())
        make_equalizer_plot(eq_data = eq_data)
      })

      output$equalizer_plot <- plotly::renderPlotly({
        shiny::req(equalizer_plot_reac())
      })

      # Get new location
      event_data <- reactive({
        plotly::event_data("plotly_relayout")
      })

      # update equalizer data in response dragging points
      observe({
        shiny::req(equalizer_plot_reac())
        eq_data <- shiny::req(equalizer_data())
        ed <- shiny::req(event_data())

        update <- update_equalizer_data(eq_data, ed)
        equalizer_data_cache(update)
      }) %>% throttle(2000)


      # Reset values
      observeEvent(input$reset, {
        .rv$reset <- TRUE
        # equalizer_data_cache(make_equalizer_data())
      }, priority = 2, ignoreInit = TRUE)

      return(
        list(
          eq_data = reactive(equalizer_data())
        )
      )
    }
  )
}



# Plotting Function -------------------------------------------------------



#' Make Equalizer plot
#'
#'
#' @param eq_data equalizer data with expected columns returned from
#'        `make_equalizer_data()`
#' @param shift_bounds y-axis limits.
#'
#'
#' @examples
#'
#' \dontrun{
#' data_pl <- make_equalizer_data(round(jitter(c(rep(0, 2), 2, rep(0, 3)), 4)))
#' pl_plotly <- make_equalizer_plot(data_pl)
#' pl_plotly
#' }
#'
#' @return `plotly` object with draggable points
#'
#' @keywords internal
make_equalizer_plot <- function(eq_data = make_equalizer_data(),
                                shift_bounds = c(-12, 12)
){



  # Ensure formatted frequency is ordered factor
  data_pl <- eq_data %>%
    dplyr::mutate(
      freq_fmt = ordered(freq_fmt, levels = unique(eq_data$freq_fmt))
    )


  # Create datapoints
  # These have to be created in a particular way for them to be movable
  circles <- purrr::map2(
    data_pl$index, eq_data$shift,
    ~ list(
      type = "circle",
      # anchor circles at (mpg, wt)
      xanchor = .x,
      yanchor = .y,
      # give each circle a 2 pixel diameter
      x0 = -5, x1 = 5,
      y0 = -5, y1 = 5,
      xsizemode = "pixel",
      ysizemode = "pixel",
      # other visual properties
      fillcolor = "white",
      line = list(color = "transparent")
    )
  )

  # font styling
  t1 <- list(size = 15, color = "lightgrey")
  # Grey lines color
  grey_line_color <- rgb(235, 235, 235, 100, maxColorValue = 255)

  # plot the shapes and fitted line
  pl_plotly <-
    plotly::plot_ly(data_pl, x = ~index, y= ~shift) %>%
    # add_lines(y = predict(model(), eq_data), color = I("red")) %>%
    plotly::layout(
      shapes = circles,
      yaxis = list(
        range = shift_bounds,
        fixedrange = TRUE, title = '',
        ticktext = purrr::map_chr(shift_bounds, ~ format_shift(.x)),
        tickvals = shift_bounds,
        zerolinecolor = grey_line_color,
        gridcolor = grey_line_color,
        tickfont = list(size = 18)
      ),
      xaxis = list(
        fixedrange = TRUE, title = '',
        ticktext = data_pl$freq_fmt,
        tickvals = data_pl$index,
        tickmode = "array",
        gridcolor = grey_line_color
      ),
      # styling
      font = t1,
      paper_bgcolor = '#252525',
      plot_bgcolor = '#252525',
      # margin
      margin = list(pad = 30),
      # Legend
      showlegend = FALSE
    ) %>%
    stagger_eq_ribbon(data_pl = data_pl) %>%
    config_plotly()

  return(pl_plotly)
}



# Data setup and helper functions -----------------------------------------




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


#' Update equalizer data in response to changes in plotly shape anchors
#'
#' @param eq_data dataset in the form returned by `make_equalizer_data`
#' @param event_data data returned by `plotly::event_data()`
update_equalizer_data <- function(eq_data, event_data){

  shape_anchors <- event_data[grepl("^shapes.*anchor$", names(event_data))]
  if (length(shape_anchors) != 2) return(NULL)
  row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
  pts <- as.numeric(shape_anchors)

  # update values (y-axis only)
  eq_data$shift[row_index] <- round(pts[2], 2)
  update_data <- eq_data %>% dplyr::mutate(
    shift_fmt = format_shift(shift)
  )

  return(update_data)
}


config_plotly <- function(pl){
  pl %>% plotly::config(
    edits = list(shapePosition = TRUE),
    showTips = FALSE, displayModeBar = FALSE
  ) %>%
    plotly::style(hoverinfo = "none")
}

#' Add lines to create shadow effect
#'
#' @param data_pl plot data
#' @param ntimes number of ribbons to make
#' @param color string. Color of ribbon fill.
#'
#' @keywords internal
stagger_eq_ribbon <- function(pl,
                              data_pl,
                              ntimes = 12,
                              color = "#0BDA51"){

  # Add fake geom_smooth line to get data
  pl_fake <- ggplot2::ggplot(data = data_pl, ggplot2::aes(x = index, y = shift, group = 1)) +
    ggplot2::geom_smooth(se = FALSE)
  main_line <- suppressWarnings(ggplot2::ggplot_build(pl_fake)$data[[1]]) %>%
    dplyr::select(index = x, shift = y) %>% tibble::as_tibble()

  stagger_eq_data <- function(data_pl, ntimes){
    shifts <- purrr::map_dfc(seq(ntimes), ~ {
      shift_name <- paste0("r_shift_", .x)
      step <- ifelse(.x==1, 0.5, 0.75)
      step2 <- ifelse(.x==1, 0, 0.25)
      data_pl %>% dplyr::mutate(!!sym(shift_name) := shift - step*.x + step2) %>%
        dplyr::select(all_of(shift_name))
    })
    cbind(data_pl, shifts) %>% tibble::as_tibble()
  }

  data_stagger <- main_line %>% stagger_eq_data(ntimes = ntimes)
  shifts <- names(data_stagger)[grepl("r_shift", names(data_stagger))]

  # Add shaded band
  if(!rlang::is_empty(shifts)){
    for(i in seq_along(shifts)){
      shifts_i <- shifts[i]
      data_stagger_i <- data_stagger %>%
        dplyr::transmute(index, shift = !!sym(shifts_i))
      opacity <- (length(shifts) - i + 1)/(length(shifts)+4) * 0.6
      pl <- plotly::add_lines(
        pl,
        data = data_stagger_i, opacity = opacity,
        line = list(shape = 'spline', smoothing = 1.5, width = 16), color = I(color))
    }
  }

  # Add main line
  pl <- plotly::add_lines(
    pl,
    data = main_line,
    line = list(shape = 'spline', smoothing = 1.5, width = 5.75),
    color = I("#0BDA51")
  )

  return(pl)
}



format_freq <- function(freq){
  freq <- round(freq)
  ifelse(freq >= 1000, paste0(freq/1000, "KHz"), paste0(freq, "Hz"))
}

format_shift <- function(shift){
  ifelse(shift >= 0, paste0("+",shift, "dB"), paste0(shift, "dB"))
}