clip_song_ui <- function(
    id,
    box_title = "Box Title",
    status = "primary",
    collapsible = FALSE,
    collapsed = FALSE,
    box_width = 12){

  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = box_title,
      solidHeader = TRUE,
      width = box_width,
      collapsible = collapsible,
      collapsed = collapsed,
      status = status#,

    )
  )
}

clip_song_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()

    return(
      list(

      )
    )

  })
}


#' songClip shiny app
clip_song <- function(){

  ui <- fluidPage(
    clip_song_ui("songClip")
  )

  server <- function(input, output, session) {
    clip_song_server("songClip")
  }

  shinyApp(ui, server)
}
