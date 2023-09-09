
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
#' @param imported_audio reactive vector of audio files
#'
#' @keywords internal
tune_audio_server <- function(id, imported_audio) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()

    return(
      list(

      )
    )

  })
}
