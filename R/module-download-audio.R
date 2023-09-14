# Module UI ---------------------------------------------------------------

#' Create shiny module for saving youtube videos as audio files
#' @describeIn download_audio_ui Create module UI for saving youtube videos as audio files
#'
#' @keywords internal
download_audio_ui <- function(id){

  ns <- shiny::NS(id)
  link_youtube <- tags$a(shiny::icon("youtube"), "Search Youtube",
                         href = "https://www.youtube.com/",
                         target = "_blank")
  link_online_converter <- tags$a(shiny::icon("video"),
                                  tags$span("Online YouTube to MP3", br(),
                                            "Converter and Downloader"),
                                  href = "https://wave.video/convert/youtube-to-mp3-240",
                                  target = "_blank")

  tagList(
    shinydashboardPlus::box(
      title = "YouTube to MP3",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "danger",
      fluidRow(
        column(
          width = 12,
          bslib::navset_tab(
            bslib::nav_panel(
              title = "Convert & Dowload YouTube to MP3",
              # This likely requires a system dependency. There is a chance
              # this will fail for some users. We will have to point them to
              # `link_online_converter` if that happens (pop-up message).
              fluidRow(
                style = "text-align: center;",
                column(
                  width = 8, offset = 2,
                  align = "center",
                  br(),
                  textInput(ns("youtube_url"), label = NULL, placeholder = "Paste any video URL")
                )
              )
            ),
            bslib::nav_spacer(),
            bslib::nav_item(link_youtube),
            bslib::nav_menu(
              title = "Other Links",
              align = "right",
              bslib::nav_item(link_online_converter),
            )
          )
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
