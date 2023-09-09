
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
            bslib::nav_panel(
              title = "Cropping, Looping, & Speed",
              plotOutput(ns("audio_plot"))
            ),
            bslib::nav_panel(title = "Tuning", p("Adjust Trebble, Bass, etc.")),
            bslib::nav_spacer(),
            bslib::nav_item(link_youtube),
            bslib::nav_menu(
              title = "Other links",
              align = "right",
              bslib::nav_panel("Three", p("Third tab content")),
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


    audio_obj <- reactive({
      audio_select <- shiny::req(input$audio_select, audio_dir())
      audio_path <- file.path(audio_dir(), audio_select)
      tuneR::readMP3(here::here(audio_path))
    })

    audio_plot <- reactive({
      audio_obj <- shiny::req(audio_obj())
      tuneR::plot(audio_obj)
    })

    output$audio_plot <- renderPlot({
      audio_plot()
    })

    return(
      list(

      )
    )

  })
}
