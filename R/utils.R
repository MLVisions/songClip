

#' Format error message that shouldn't happen
#'
#' @param error_msg text to error with
#'
#' @keywords internal
dev_bug <- function(error_msg){
  cli::cli_abort(
    c(
      "i"="USER SHOULD NOT SEE THIS ERROR. Please file an issue on Github.",
      "",
      ">" = "Error message:",
      "x"= error_msg
    )
  )
}

#' Format warning message that shouldn't happen
#'
#' @param warn_msg text to warn with
#'
#' @keywords internal
dev_warning <- function(warn_msg){
  cli::cli_warn(
    c(
      "i"="USER SHOULD NOT SEE THIS WARNING. Please file an issue on Github.",
      "",
      ">" = "Warning message:",
      "!"= warn_msg
    )
  )
}


#' Format seconds as `{minutes:seconds}`
#'
#' @param time time in seconds
#' @param as_char Logical (`TRUE`/`FALSE`). If `TRUE`, coerce date to character
#'        string.
#'
#' @examples
#'
#' format_seconds(80, as_date = FALSE)
#'
#' @return character string formatted as `{minutes:seconds}`
#' @keywords internal
format_seconds <- function(time, as_date = TRUE, as_char = FALSE){
  if(isTRUE(as_date)){
    op <- options(digits.secs = 6)
    on.exit(options(op), add = TRUE)
    date <- as.POSIXct(time, origin = as.Date("1970-01-01"), tz='UTC',
               format = "%Y-%m-%d %H:%M:%OS")
    if(as_char) as.character(date) else date
  }else{
    sprintf("%02d:%02.0f", time %/% 60, time %% 60)
  }
}


format_freq <- function(freq){
  freq <- round(freq)
  ifelse(freq >= 1000, paste0(freq/1000, "KHz"), paste0(freq, "Hz"))
}


format_shift <- function(shift){
  ifelse(shift >= 0, paste0("+",shift, "dB"), paste0(shift, "dB"))
}



#' Substitute for \code{\link{fluidRow}} with no horizontal margins
#'
#' `plotly` behaves a little funky in `fluidRow`s, causing horizontal scrollbars to
#' be added when they aren't necessary. Manually removing the left and right margins
#' was the easiest method for removing these scrollbars.
#'
#' @param ... HTML tags
#' @param style html formatted string to append to the `style` argument of a `fluidRow`.
#' @param color font color of the `fluidRow`.
#' @param bg_color background color of the `fluidRow`.
#'
#' @keywords internal
no_margin_row <- function(
    ...,
    easy_col = FALSE,
    align = c("left", "right", "center", "justify"),
    style = NULL,
    color = NULL,
    bg_color = NULL
){
  style <- if(is.null(style)){
    glue::glue("margin-right: 0px; margin-left: 0px;")
  }else{
    glue::glue("margin-right: 0px; margin-left: 0px; {style};")
  }

  easy_row(
    ...,
    easy_col = easy_col,
    align = align,
    style = style,
    color = color,
    bg_color = bg_color
  )
}


easy_row <- function(
    ...,
    easy_col = FALSE,
    align = c("left", "right", "center", "justify"),
    style = NULL,
    color = NULL,
    bg_color = NULL
){

  align <- match.arg(align)
  style <- if(is.null(style)) "" else glue::glue("{style};")

  if(!is.null(color)) style <- glue::glue("{style} color: {color};")
  if(!is.null(bg_color)) style <- glue::glue("{style} background-color: {bg_color};")

  if(isTRUE(easy_col)){
    shiny::fluidRow(
      style = style,
      shiny::column(
        width = 12,
        align = align,
        ...
      )
    )
  }else{
    shiny::fluidRow(
      style = style,
      ...
    )
  }
}

