

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
