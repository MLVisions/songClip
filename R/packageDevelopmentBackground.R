

# Useful Commands ---------------------------------------------------------

# Sections like the one above can be created with `CMD + SHIFT + r`

# `CMD + f` will search the script

# When in an R project, `CMD + SHIFT + f` will search the entire R project for that file
#   - you can choose to just search a directory too

# `CMD + Enter` (or `CMD + return`), will run either A) the current call your cursor is at, or
# B) run what you have highlighted


# If you hold `control + option`, and then it the up or down arrow, you can edit multiple lines at once
# (try it, it's cool)



# Background & Best Practices for Package Development ---------------------





# Loading all functions and variables defined in R/ directory during package development:

# `devtools::load_all()`

# Loading exported functions from a package you installed:

# library(dplyr)
# library(tuneR)
# library(songClip)

# Notes:
# `library()` calls are used when scripting, such as for specific client projects, modeling project,
# shiny app, Rmarkdown, etc.
# However in most circumstances you *dont* want `library()` calls in an R package. Instead,
# you either A) import the package functions you need and it to the `NAMESPACE` (more on this later), or
# B) use qualifiers (<package_name>::<package function>, e.g, `tuneR::readMP3()`)

# Packages that are needed for the package, are added to the `Imports` section of the `DESCRIPTION` file.

# The `DESCRIPTION`, `NAMESPACE`, and `.Rbuildignore` files are the 3 files that decide how an R package
# gets installed

# The `.Rprofile` sets the development environment. It can be used for all the scripting examples, as well
# as an R package development environment. You often use one to set `options()` or for static file paths
# (e.g., a path to an installation or tarball).
# Here, we are using it to set where to grab the packages from.



# Package Documentation and Roxygen ---------------------------------------


# TL;DR of section below: If you added roxygen (comments that start with #')
# this will generate helper files (stored in `man/` directory) and update the NAMESPACE:

# `devtools::document()`

# The **NAMESPACE** file defines which functions in your package are **exported**, along with what other packages
# were needed to be **imported**. This **must be set up correctly** for other users to be able to use your package.



### Intro ###

# When you normally install packages, and are not developing a package (such as a specific client project),
# you would call `library(tuneR)`, `library(songClip)`, etc. to load the functions you need. This will load
# all functions *exported* by the package into your current environment.
# Example:
# library(dplyr)
# data(mtcars)
# mtcars %>% mutate(new_column = gear + 2) # point being, you didnt have to write `dplyr::mutate()` because you
# *called the library* via `library()`

# However in R package development, we *only* have access to the specific functions we *import* when we go to
# *release* a package. The `main` branch is meant to signify a state where the code works, but doesnt have to match the latest release.
# You can call `library(songClip)` for testing things during development, but when you go to write
# the final function, you have to do one of the following two things:
#
# A) Use qualifiers (double colon prefix (::)) for the function (e.g., dplyr::mutate() or tuneR::readMP3("file_path"))
#       Note: You can reference installed packages' internal (unexported) functions, using the triple colon (:::)
#
# B) Use roxygen to add the function to the namespace:
#       Format:   #' @importFrom <package> <functions separated by a space>
#       Example:  #' @importFrom tuneR readMP3 writeWave
#    Upon running `devtools::document()`, this will add those functions to the NAMESPACE file as imports. You can then
#       use the functions `readMP3()`, and `writeWave()` in *your* functions without needing qualifiers.
#    Use can also use `#' @import` to import *all functions exported by the package*. This makes the package binary bigger though,
#       and is generally not recommended. It's mainly used if you have to use most or many of the functions in the package.
#       Packages that rely on shiny for instance, usually do this (such as `shinydashboard`, an R package for making dashboard apps)
#


###  Creating Helper Files ###

# `man/` files are helper files. This is how they work:
# you document the function using roxygen above the function declaration.
# Running `devtools::document()` will create a helper file, and upon reloading
# the package (`devtools::load_all()`), you will be able to type `?my_function`
# in the console, and it will pop open a helper doc for that function with whatever
# you commented above the function.

# Core Roxygen Definitions:

# roxygen: [#' @@title] ; meaning: adds a title. The First line that starts with `#'` will become the title by default
# roxygen: [#' @description] ; meaning: adds a description below the title. The second line, separated by a newline, will be the
#                                       description by default.
# roxygen: [#' @details] ; meaning: adds a details section to the helper file
# roxygen: [#' @param ] ; meaning: document function parameter. Add a description to the parameter so users/developers know how to
#                                  use the function.
# roxygen: [#' @keywords internal] ; meaning: makes the function an internal function. When a user installs this package, and runs
#                                             `library(thisPackage)`, only *exported* functions will be available to them.
#                                             This is technically not necessary (can be omitted), but is considered good practice.
# roxygen: [#' @export ] ; meaning: exports the function, making it available to the user upon installation.

# roxygen: [#' @importFrom <package> <function>] ; meaning: The function requires these functions to be imported to work



### Full Example ###
# You can change the lines below, delete or add roxygen to experiment, and then
# run `devtools::document()`, followed by `?my_function` to see the changes to the helper file.
# If you remove a `@importFrom`, run `devtools::document()` followed by `devtools::load_all()`,
# you will notice an error when you try to use the function. Although you have the function installed,
# it must be directly imported to be able to be used, *without qualifiers (::)*.


#' Append a symbol to a dataframe. This is the title.
#'
#' Adds the chosen symbol as a column to the end of a dataframe, and optionally create
#' an ID column. This will be the description.
#'
#' @param data a dataset. Defaults to `mtcars`.
#' @param symbol the symbol to append
#' @param add_id_col Logical (`TRUE`/`FALSE`). If `TRUE`, add an ID column and
#' append it to the front of the dataframe.
#' @param group_by_col column to group by if creating an ID column.
#'
#' @importFrom rlang syms sym
#' @importFrom dplyr n
#'
#' @details
#' This is a terrible, disgusting function. Why would you ever want to do this.
#' Im mainly trying to illustrate the different types of arguments/defaults you
#' can have, and how roxygen/the loaded function will work.
#'
#' The `@importFrom` statements above will not show up in the helper file (this text will),
#' however the `NAMESPACE` file will update when you run `devtools::document()` if you modify them.
#'
#' The `@importFrom` statements above translate to:
#' - "import `syms` and `sym` from the package `rlang`
#' - "import `n` from the package `dplyr`
#'
#'
#' **bolded** word
#'
#' *italizized phrase*
#'
#' `this phrase` will appear as code
#'
#' This section will be a code block:
#' ```
#' # Adds a new column to a dataset, repeating 'A' all the way down (see ?dplyr::mutate for more details):
#' data %>% dplyr::mutate(new_column = "A")
#' ```
#'
#' @note
#' `mtcars` is an example dataset exported by the `datasets` package, and
#' comes with the base installation of R.
#'
#'
#' @examples
#'
#' \dontrun{
#' # Defaults to symbol 'a'
#' my_function(mtcars)
#'
#' # Defaults to `mtcars` dataset
#' my_function(symbol = "b")
#'
#' # Create ID and row_number columns
#' my_function(add_id_col = TRUE, group_by_col = "gear")
#'
#' # Would error out in a controlled way:
#' my_function(add_id_col = TRUE)
#' my_function(add_id_col = TRUE, group_by_col = "columnDoesntExist")
#' my_function(symbol = "c")
#' }
#'
#'
#' @returns a dataframe
#'
#' @keywords internal
my_function <- function(
    data = mtcars,
    symbol = c("a","b"),
    add_id_col = FALSE,
    group_by_col
){

  # Checks the class of data object (`class(data)`)
  checkmate::assert_true(inherits(data, c("data.frame", "tibble")))

  # `match.arg` forces the user to choose one of the options defined by the argument.
  # This will also choose the first element of a function argument *if* it wasn't directly specified by the user.
  # It is a base R function, so it doesnt need to be imported or qualified (::).
  symbol <- match.arg(symbol) # users can only choose 'a' or 'b' (would error if you tried 'c')

  # Add symbol column
  data_new <- data %>% dplyr::mutate(new_column = symbol)

  # Add ID column
  if(isTRUE(add_id_col)){

    # Will error if this returns false (must be a string; cannot be a vector)
    checkmate::assert_string(group_by_col)

    # error with *informative message* if group_by_col is not found within the column names of the dataset
    # You could error via a `checkmate::` function, but this will be more informative for users.
    if(!(group_by_col %in% names(data))){
      cli::cli_abort(glue::glue("The column {group_by_col} was not found in data"))
    }else{
      # Create ID column, grouped by `group_by_col`
      data_new <- data_new %>% dplyr::group_by(!!sym(group_by_col)) %>%
        dplyr::mutate(
          row_number = 1:n(), # Create row number column
          ID = dplyr::cur_group_id() # Create ID column
          ) %>%
        # Bring the new columns to the front of the dataframe
        dplyr::relocate(c(row_number, ID, !!sym(group_by_col)))
    }
  }

  return(data_new)
}
