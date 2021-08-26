# internal functions

# get adjectives from wikipedia
# optionally open an editor to manually edit (likely v necessary)
get_adjectives <- function(open_file = FALSE) {

  flags <- dir("inst/extdata/flag-icon-css/flags/1x1/", pattern = ".svg") |>
    {\(x) gsub(".svg", "", x)}() |>
    toupper()

  adjective_wiki_url <- "https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations"

  adjectives_read <- xml2::read_html(adjective_wiki_url)

  adjectives <- adjectives_read |>
    rvest::html_node("table") |>
    rvest::html_table() |>
    stats::setNames(c("country", "adjective", "demonym")) |>
    dplyr::select(-demonym) |>
    dplyr::mutate(
      dplyr::across(tidyselect:::where(is.character),
                    ~gsub("\\[.*\\]", "", .)),
      adjective = gsub("\\bor\\b", "and", adjective),
      iso2c = countrycode::countrycode(country, "country.name", "iso2c",
                                       warn = FALSE),
      iso2c = dplyr::case_when(
        country == "Bonaire" ~ "BQ",
        country == "European Union" ~ "EU",
        country == "England" ~ "GB-ENG",
        country == "Scotland" ~ "GB-SCT",
        country == "Wales" ~ "GB-WLS",
        country == "Northern Ireland" ~ "GB-NIR",
        country == "Saint Martin" ~ "MF",
        country == "Virgin Islands, United States" ~ "VI",
        country == "Kosovo" ~ "XK",
        TRUE ~ iso2c
      )
    ) |>
    dplyr::bind_rows(tibble::tibble(
      country = c("Catalonia", "Galicia", "United Nations"),
      adjective = c("Catalan", "Galician", "Global"),
      iso2c = c("ES-CT", "ES-GA", "UN"))) |>
    dplyr::filter((iso2c %in% flags) & country != "Myanmar" & country != "East Timor")

  readr::write_csv(adjectives, "inst/extdata/adjectives.csv")

  if (open_file) {
    usethis::edit_file("inst/extdata/adjectives.csv")
  }

}

# use the adjectives csv to build a country dataset inside the package
build_dataset <- function(save_to_package = FALSE) {

  countries <- readr::read_csv("inst/extdata/adjectives.csv", na = character()) |>
    dplyr::mutate(
      country2 = countrycode::countrycode(country,
                                               "country.name",
                                               "cow.name",
                                               warn = FALSE),
      country2 = dplyr::case_when(
        iso2c == "VG" ~ "British Virgin Islands",
        is.na(country2) ~ country,
        TRUE ~ country)
    ) |>
    dplyr::select(iso2c, country = country2, adjective)

  if (save_to_package) {
    usethis::use_data(countries, overwrite = TRUE)
  } else {
    countries <<- countries
  }

}
