
# Module UI ---------------------------------------------------------------

#' Create shiny module for tuning an audio file
#' @describeIn tune_audio_ui Create module UI for tuning an audio file
#'
#' @keywords internal
tune_audio_ui <- function(id){

  ns <- shiny::NS(id)
  link_youtube <- tags$a(shiny::icon("youtube"), "Youtube", href = "https://www.youtube.com/", target = "_blank")
  link_posit <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")

  tagList(
    shinydashboardPlus::box(
      title = "Tune Audio",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      collapsed = FALSE,
      status = "primary",
      fluidRow(
        column(
          width = 12,
          shinyWidgets::pickerInput(ns("audio_select"), "Select an audio file", choices = c()),
          bslib::navset_card_pill(
            placement = "above",
            ### Cropping, looping, and speed ###
            bslib::nav_panel(
              # TODO: make better title for this
              title = "Cropping, Looping, & Speed",
              plotOutput(ns("audio_plot")) %>% withSpinner(color="#086A87")
            ),
            ### Equalizer ###
            bslib::nav_panel(
              title = "Equalizer",
              # TODO: add mechanism for moving the points
              # also add labels for adjusting Trebble, Bass, etc.
              plotly::plotlyOutput(ns("equalizer_plot")) %>% withSpinner(color="#086A87"),
              # plotOutput(ns("equalizer_plot")),
              fluidRow(
                column(
                  width = 10, offset = 2,
                  make_vertical_sliders(
                    id_prefix = "equalizer_freq", ns = ns,
                    rep(0, 6), labels = NULL, height="100px", use_columns = TRUE
                  )
                )
              )
            ),
            bslib::nav_spacer(),
            bslib::nav_item(link_youtube),
            bslib::nav_menu(
              title = "Other links",
              align = "right",
              bslib::nav_panel(
                title = "Three",
                p("Third tab content")
              ),
              bslib::nav_item(link_posit)
            )
          )
        )
      )
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn tune_audio_ui Create module server for tuning an audio file
#'
#' @param audio_choices reactive list of audio files and their parameters
#' @param audio_dir reactive audio directory (selected)
#'
#' @keywords internal
tune_audio_server <- function(id, audio_choices, audio_dir) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()

    # TODO: use shinyjs to disable play button if tuneR::getWavPlayer() is not set
    observe({
      audio_player_set <- nzchar(tuneR::getWavPlayer()) && fs::file_exists(tuneR::getWavPlayer())

      if(isFALSE(audio_player_set)){
        # shinyjs::disable()
      }
    })

    # Set audio choices with their info based on audio library
    observeEvent(audio_choices(), {
      audio_choices <- shiny::req(audio_choices())
      # Update Choices
      shinyWidgets::updatePickerInput(
        session = session, "audio_select", choices = audio_choices$choice_name,
        selected = audio_choices$choice_name[1],
        choicesOpt = list(icon = audio_choices$icon, subtext = audio_choices$subtext),
        options = shinyWidgets::pickerOptions(container = "body",iconBase = "fas"),
      )
    })


    # Load Chosen audio object
    audio_obj <- reactive({
      audio_select <- shiny::req(input$audio_select, audio_dir())
      audio_path <- file.path(audio_dir(), audio_select)
      tuneR::readMP3(here::here(audio_path))
    })



    # Cropping, Looping, & Speed ----------------------------------------------


    # Audio inspection plot
    audio_plot <- reactive({
      audio_obj <- shiny::req(audio_obj())
      assign("audio_obj", audio_obj, envir = .GlobalEnv)
      plot_wave_audio(audio_obj)
    })

    output$audio_plot <- renderPlot({
      audio_plot()
    })


    # Equalizer ---------------------------------------------------------------

    equalizer_data <- reactiveVal(make_equalizer_data())

    # Equalizer Plot
    equalizer_plot_reac <- reactive({
      data_pl <- shiny::req(equalizer_data())
      make_equalizer_plot(data_pl, shift_bounds = c(-12, 12))
    })

    output$equalizer_plot <- plotly::renderPlotly({
      equalizer_plot_reac()
    })

    # Equalizer Input
    observe({

      # Vertical slider inputs
      input_ids <- paste0("equalizer_freq_", seq(1,6))
      equalizer_freqs <- purrr::map_dfr(input_ids, function(id){
        data.frame(input = id, value = shiny::req(input[[id]]))
      }) %>% tibble::as_tibble()

      # Starting table or preset
      eq_data <- shiny::req(equalizer_data())
      update_eq <- update_equalizer_data(eq_data, equalizer_freqs)
      equalizer_data(update_eq)
    })

    return(
      list(

      )
    )

  })
}



make_vertical_sliders <- function(id_prefix = "slider",
                                  ns = shiny::NS(id_prefix),
                                  values = rep(0, 6),
                                  labels = c("60Hz", "150Hz", "400Hz", "1KHz", "2.4KHz", "15KHz"),
                                  min = -12,
                                  max = 12,
                                  step = 0.2,
                                  height = "150px",
                                  use_columns = FALSE
){

  # values cannot be named
  if(!is.null(names(values))) names(values) <- NULL

  # Column sizing
  l <- length(values)
  col_size <- floor(12/l)

  slider_uis <- purrr::imap(values, function(value, index){
    label_i <- if(is.null(labels)) NULL else labels[index]
    ui <- shinyWidgets::noUiSliderInput(
      inputId = ns(paste0(id_prefix,"_", index)), label = label_i,
      min = min, max = max, step = step,
      value = value, margin = 0,
      orientation = "vertical",
      height = height, width = "30px",
      color = "#27ae60", inline = TRUE,
      tooltips = FALSE
    )
    if(isTRUE(use_columns)){
      ui <- column(
        width = col_size,
        ui
      )
    }
    return(ui)
  })

  slider_uis
}


