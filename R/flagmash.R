#' Create mash-ups of different flags
#' Given two ISO country codes (for country A and country B), `flagmash(A, B)`
#' will take the colours of the flag for country A and apply them to the flag
#' of country B. It will give the flag a name using the adjective of country A
#' with the name of country B.
#'
#' For example `flagmash("IE", "US")` will take the colours of the Republic of
#' Ireland's flag and apply them to the design of the flag of the United States
#' of America, it will call this flag "Irish United States of America".
#'
#' @param country1 ISO 2 letter code of the flag to extract colours from
#' @param country2 ISO 2 letter code of the flag to replace colours in
#'
#' @return An representation of the flag and the flag name in the RStudio viewer.
#'
#' @export
flagmash <- function(country1, country2) {

  flag1 <- get_flag(country1)
  flag2 <- get_flag(country2)

  flag1_colours <- get_flag_colours(flag1)

  new_flag <- replace_flag_colours(flag2, flag1_colours)

  country1_adjective <- countries$adjective[countries$iso2c == toupper(country1)]
  country2_name <- countries$country[countries$iso2c == toupper(country2)]

  new_flag_name <- paste(country1_adjective, country2_name, collapse = " ")

  flag_html <- htmltools::HTML(
    "<h1>",new_flag_name,"</h1>",
    paste0(new_flag, collapse = "\n")
  )

  htmltools::html_print(flag_html)

}


# get the svg flag
get_flag <- function(code) {

  code <- tolower(code)

  flag <- file.path("inst", "extdata", "flag-icon-css", "flags", "4x3",
                    paste0(code, ".svg"))

  readLines(flag)

}

# get the colours from a flag
get_flag_colours <- function(flag) {

  flag_colours <- gsub("(.*)(#[a-zA-Z0-9]+)(.*)", "\\2", flag)[grepl("#", flag)]

  return(unique(flag_colours))

}

# switch the colours
replace_flag_colours <- function(flag, replacement_colours) {

  # get current flag colours
  my_colours <- get_flag_colours(flag)

  # randomise order of the replacement colours
  new_colours <- runif(length(replacement_colours)) |>
    setNames(replacement_colours) |>
    sort() |>
    names()

  if (length(my_colours) < length(new_colours)) {
    #subset to smaller amount
    new_colours <- new_colours[1:length(my_colours)]
  } else if (length(my_colours) > length(new_colours)) {

    replicate <- length(my_colours) %/% length(new_colours)
    remainder <- length(my_colours) %% length(new_colours)

    new_colours <- rep(new_colours, replicate)

    if (remainder > 0) {
      new_colours <- c(new_colours, new_colours[1:remainder])
    }

  }

  new_flag <- flag

  for (i in seq_along(my_colours)) {
    new_flag <- gsub(my_colours[i], new_colours[i], new_flag)
  }

  return(new_flag)

}

