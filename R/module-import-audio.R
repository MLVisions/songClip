
# Module UI ---------------------------------------------------------------

#' Create shiny module for choosing an audio directory and selecting which files to import
#' @describeIn import_audio_ui Create module UI for choosing an audio directory
#' and selecting which files to import
#'
#' @param id module id
#' @param title label fed to `shinyDirButton`
#' @param box_title label of the first `shinydashboardPlus::box`
#' @param status the color of the box. See `shinydashboardPlus::box` status
#' @param collapsible whether the module can be collapsed/hidden
#' @param collapsed whether the module should start off as collapsed
#'
#' @importFrom htmltools h3 h5
#' @importFrom shiny verbatimTextOutput uiOutput
#'
#' @keywords internal
import_audio_ui <- function(id,
                            title = "Select a folder containing audio files",
                            box_title = "Import Audio Files",
                            collapsible = FALSE,
                            collapsed = FALSE,
                            status = "primary") {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = box_title,
      solidHeader = TRUE,
      width = 12,
      collapsible = collapsible,
      collapsed = collapsed,
      status = status,
      h3("Select the folder that contains all your audio files"),
      h5("You will then choose which files to import from a list."),
      shinyFiles::shinyDirButton(ns("audio_dir"), label = title, title = title),
      br(),
      h5("Audio library currently set to:"),
      verbatimTextOutput(ns("dir_selected"), placeholder = TRUE),
      uiOutput(ns("audio_import_ui"))
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn import_audio_ui Create module server for choosing an audio
#' directory and selecting which files to import
#'
#' @param audio_dir an initial audio directory
#'
#' @keywords internal
import_audio_server <- function(id,
                                audio_dir = NULL,
                                title = "Starting Directory") {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Directory Options
    root_opts <- set_root_opts(audio_dir, title)

    shinyFiles::shinyDirChoose(
      input,
      'audio_dir',
      roots = root_opts,
      filetypes = c('', 'mp4','mp3', 'wav')
    )

    global <- reactiveValues(audio_dir = audio_dir)

    output$dir_selected <- renderText({
      global$audio_dir
    })

    observeEvent(input$audio_dir, {
      if (!"path" %in% names(input$audio_dir)) return()
      global$audio_dir <- shinyFiles::parseDirPath(root_opts, input$audio_dir)
    }, ignoreNULL = TRUE)


    # Find audio files
    observeEvent(global$audio_dir,{
      shiny::req(global$audio_dir)
      # filter to relevant files only
      files <- list.files(global$audio_dir, full.names = FALSE)
      choices <- files[grep("mp4|mp3|wav", files)]
      updateSelectInput(session = session, "audio_imports", choices = choices, selected = choices[1])
    })


    output$audio_import_ui <- renderUI({
      shiny::req(global$audio_dir)
      tagList(
        hr(),
        h3("Select audio files to import"),
        selectInput(ns("audio_imports"), "Files to Import", choices = c())
      )
    })
    outputOptions(output, "audio_import_ui", suspendWhenHidden = FALSE)

    audio_obj <- reactive({
      shiny::req(input$audio_imports)
      audio_path <- file.path(global$audio_dir, input$audio_imports)
      tuneR::readMP3(here::here(audio_path))
    })

    return(
      list(
        audio_obj = audio_obj,
        audio_dir = reactive({global$audio_dir})
      )
    )
  }
  )
}


#' Set root directories for use with `shinyDirChoose`
#'
#' @inheritParams import_audio_server
#' @param title label of root directory for `audio_dir`
#'
#' @keywords internal
set_root_opts <- function(audio_dir, title){
  # Look for Desktop
  desktop_path <- file.path(path.expand('~'),'Desktop')
  if(fs::dir_exists(desktop_path)){
    root_opts <- c(
      "Desktop" = desktop_path,
      "Home" = path.expand('~'),
      `Working Directory` = getwd()
    )
  }else{
    root_opts <- c(
      "Home" = path.expand('~'),
      `Working Directory` = getwd()
    )
  }

  # Add specified audio_dir if it's not part of the current list
  if(!is.null(audio_dir) && !(fs::path_norm(audio_dir) %in% fs::path_norm(unname(root_opts)))){
    root_opts <- c(eval(parse(text = glue::glue("c('{title}' = audio_dir)"))), root_opts)
  }

  return(root_opts)
}
