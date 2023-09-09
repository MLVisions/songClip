
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
          bslib::navset_card_pill(
            placement = "above",
            bslib::nav_panel(title = "One", p("First tab content.")),
            bslib::nav_panel(title = "Two", p("Second tab content.")),
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
#' @param audio_files reactive vector of audio files
#'
#' @keywords internal
tune_audio_server <- function(id, audio_files) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()


    audio_obj <- reactive({
      shiny::req(audio_files())
      audio_path <- audio_files()[1]
      tuneR::readMP3(here::here(audio_path))
    })

    return(
      list(

      )
    )

  })
}
