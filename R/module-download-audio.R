# Module UI ---------------------------------------------------------------

#' Create shiny module for saving youtube videos as audio files
#' @describeIn download_audio_ui Create module UI for saving youtube videos as audio files
#'
#' @keywords internal
download_audio_ui <- function(id){

  ns <- shiny::NS(id)
  tagList(
    shinydashboardPlus::box(
      title = "Search Youtube",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      collapsed = FALSE,
      status = "danger",
      fluidRow(
        column(
          width = 6
        ),
        column(
          width = 6
        )
      )
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn download_audio_ui Create module server for saving youtube videos as audio files
#'
#'
#' @keywords internal
download_audio_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()

    return(
      list(

      )
    )

  })
}
