
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
#' @importFrom htmltools h1 h2 h3 h4 h5
#'
#' @keywords internal
import_audio_ui <- function(id,
                            title = "Select a folder containing audio files",
                            box_title = "Set Audio Library") {
  ns <- NS(id)
  tagList(
    easy_row(
      easy_col = TRUE,
      color = "white",
      htmlOutput(ns("dir_selected_rel")),
      br(),
      shinyWidgets::dropdown(
        label = "Set Library",
        right = TRUE,
        easy_row(
          easy_col = TRUE,
          bg_color = "#086A87",
          color = "white",
          class = "shadow-wrapper",
          h3("Set Audio Library"),
          h5("Select the folder that contains all your audio files"),
          shinyFiles::shinyDirButton(
            ns("audio_dir"), label = "Choose Directory",
            title = "Select a folder containing your audio files"
          ),
          br(),
          h5("Audio library currently set to:"),
          htmlOutput(ns("dir_selected")),
          br()
        )
      )
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
    global <- reactiveValues(audio_dir = audio_dir, audio_choices = NULL)

    # Handling for specified audio directory
    observeEvent(audio_dir, {
      if(is.null(audio_dir)){
        global$audio_dir <- EXAMPLE_AUDIO_DIR
      }else if(!fs::dir_exists(audio_dir)){
        msg <- paste0("
        Please select the appropriate location using the
        `Set Library` dropdown or re-launch the app. Defaulting to example
        library.") %>% gsub("\n", " ", .)
        shinyalert::shinyalert(
          title = "The audio library you specified does not exist", text = msg,
          type = "warning", session = session
        )
        global$audio_dir <- EXAMPLE_AUDIO_DIR
      }
    }, priority = 2, ignoreNULL = FALSE)


    # Directory Options
    root_opts <- set_root_opts(audio_dir, title)

    shinyFiles::shinyDirChoose(
      input,
      'audio_dir',
      roots = root_opts,
      filetypes = c('', 'mp4','mp3', 'wav')
    )


    output$dir_selected <- renderUI({
      path_abs <- fs::path_real(shiny::req(global$audio_dir))
      HTML(
        paste0(
          "<span class='folder-path-small' style = 'word-wrap: break-word;
          max-width: 320px; width: fit-content;'>", path_abs, "</span>")
      )
    })

    output$dir_selected_rel <- renderUI({
      path <- shiny::req(global$audio_dir)
      if(fs::path_real(path) == path.expand("~")){
        path_rel <- "Home"
      }else{
        path_rel <- file.path("~", fs::path_rel(path, "~"))
      }
      HTML(
        paste0(
          "Audio library: <span class='folder-path-small'
          style = 'word-wrap: break-word; max-width: 250px; width: fit-content;'>",
          path_rel, "</span>"
        )
      )
    })

    observeEvent(input$audio_dir, {
      if (!"path" %in% names(input$audio_dir)) return()
      global$audio_dir <- shinyFiles::parseDirPath(root_opts, input$audio_dir)
    }, ignoreNULL = TRUE)


    # Find audio files
    observeEvent(global$audio_dir,{
      shiny::req(global$audio_dir)

      # filter to relevant files only
      files <- list.files(global$audio_dir, full.names = TRUE)
      choices <- files[grep("mp3|wav|mp4", fs::path_ext(files))]
      choice_names <- basename(choices)

      # Set Icons
      icons <- gsub(".*mp4.*", "fa-video", choice_names) %>%
        gsub(".*mp3|wav.*", "fa-music", .)

      # Set subtext
      subtext <- purrr::map_chr(choices, function(choice_path){
        info <- av::av_media_info(choice_path)
        duration <- floor(info$duration)
        duration <- strftime(as.POSIXct("00:00:00", format="%H:%M:%S") +
                               duration, format="%H:%M:%S")
        if(grepl("^00:", duration)){
          duration <- gsub("^00\\:", "", duration)
        }
        paste("~", duration)
      })

      global$audio_choices <- tibble::tibble(
        choice_name = choice_names,
        choice = choices,
        icon = icons,
        subtext = subtext
      )

    })

    return(
      list(
        audio_choices = reactive({global$audio_choices}),
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

  # Add example directory if not set to audio_dir
  if(is.null(audio_dir) || audio_dir != EXAMPLE_AUDIO_DIR){
    root_opts <- c(
      eval(parse(text = glue::glue("c('Examples' = EXAMPLE_AUDIO_DIR)"))),
      root_opts
    )
  }

  # Add specified audio_dir if it's not part of the current list
  valid_dir <- !is.null(audio_dir) && nzchar(audio_dir) && fs::dir_exists(audio_dir)
  already_specified <- valid_dir && fs::path_real(audio_dir) %in% fs::path_real(unname(root_opts))

  if(isTRUE(valid_dir) && !already_specified){
    root_opts <- c(
      eval(parse(text = glue::glue("c('{title}' = audio_dir)"))),
      root_opts
    )
  }else if(isTRUE(already_specified)){
    # Reorder to specified
    selected <- which(fs::path_real(audio_dir) == fs::path_real(unname(root_opts)))
    root_opts <- c(root_opts[selected], root_opts[-selected])
  }

  return(root_opts)
}

