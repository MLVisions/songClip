
#' songClip shiny app
#'
#' @param browser Logical (`TRUE`/`FALSE`). If `TRUE`, launch the app in a browser.
#' @param audio_dir a directory containing audio files.
#'
#' @returns a shiny instance
#' @importFrom htmltools tags p HTML tagList
#' @import shiny
#' @importFrom shinydashboardPlus box
#'
#'
#'
#' @export
clip_song <- function(browser = FALSE, audio_dir = EXAMPLE_AUDIO_DIR){

  options("songClip.audio_dir" = audio_dir)

  ui <-
    shinydashboardPlus::dashboardPage(
      freshTheme = song_clip_theme,
      options = list(sidebarExpandOnHover = TRUE),
      header = shinydashboardPlus::dashboardHeader(
        title = "Song Clip"
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        collapsed = TRUE,
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Download Songs", tabName = "tab_download", icon = icon("download")),
          shinydashboard::menuItem("Song Clip", tabName = "tab_clipsong", icon = icon("scissors"))
        )
      ),
      body = shinydashboard::dashboardBody(
        tags$head(
          tags$style(HTML(jsHeader)),
          tags$script(HTML(jqueryHeader)),
          tags$style(HTML(noWrap_css))
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem("tab_download",
                                  import_audio_ui("import_audio"),
                                  download_audio_ui("download_audio")
          ),
          shinydashboard::tabItem("tab_clipsong",
                                  tune_audio_ui("tune_audio")
          )
        )
      ),
      controlbar = shinydashboardPlus::dashboardControlbar(),
      title = "DashboardPage"
    )


  server <- function(input, output, session) {

    # TODO: This should be a nested module (inside import_audio_server)
    dwnld_audio <- download_audio_server("download_audio")

    imported_audio <- import_audio_server("import_audio", audio_dir = getOption("songClip.audio_dir"))

    tune_audio_server("tune_audio", imported_audio = imported_audio)
  }

  app <- shiny::shinyApp(ui, server)

  # Launch app
  if(isTRUE(browser)){
    withr::with_options(
      list(shiny.launch.browser = .rs.invokeShinyWindowExternal),
      shiny::runApp(app)
    )
  }else{
    withr::with_options(
      list(shiny.launch.browser = .rs.invokeShinyPaneViewer),
      shiny::runApp(app)
    )
  }

}


#' Set theme for songClip app
#' @keywords internal
song_clip_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#086A87"
  ),
  fresh::adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  fresh::adminlte_global(
    content_bg = "#FFFFFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

# song_clip_styles <- bslib::bs_add_rules(bslib::bs_theme(
#   version = 5,
#   "well-bg" = "#FFF",
#   base_font = bslib::font_google("Poppins")
# ), rules = c(
#   ".nav-link.active {@extend .text-light }"
# ))
