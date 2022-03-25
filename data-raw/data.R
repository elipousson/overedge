## code to prepare `DATASET` dataset goes here

library(httr)
library(dplyr)
library(stringr)

get_repo_svg <- function(repo, branch = "main") {
  req <- GET(
    paste0("https://api.github.com/repos/", repo, "/git/trees/", branch, "?recursive=1")
  )

  data.frame(
    "path" = unlist(lapply(content(req)$tree, function(x) x$path))
  ) |>
    filter(str_detect(path, "\\.svg$")) |>
    transmute(
      repo = repo,
      name = str_extract(path, "(?<=/)[:graph:]+(?=.svg$)"),
      url = paste0("https://raw.githubusercontent.com/", repo, "/", branch, "/", path)
    )
}

maki <-
  get_repo_svg(repo = "mapbox/maki") |>
  mutate(
    size = 15,
    style = ""
  )

temaki <-
  get_repo_svg(repo = "ideditor/temaki") |>
  mutate(
    size = 40,
    style = ""
  )

wu_icons <-
  get_repo_svg(repo = "manifestinteractive/weather-underground-icons", branch = "master") |>
  mutate(
    size = 64,
    style = str_extract(name, "(?<=icons/).+(?=/svg)"),
    name = str_extract(name, "(?<=svg/).+")
  )

calcite <-
  get_repo_svg(repo = "Esri/calcite-point-symbols", branch = "master") |>
  mutate(
    size = case_when(
      str_detect(name, "13") ~ 13,
      str_detect(name, "17") ~ 17,
      str_detect(name, "21") ~ 21
    ),
    style = "",
    name = str_remove(name, "-[:digit:]+$")
  ) |>
  arrange(name, desc(size))

lane_icons <-
  get_repo_svg(repo = "openstreetmap/lane-icons", branch = "master") |>
  mutate(
    size = 40,
    style = ""
  )

osm_map_icons <-
  get_repo_svg(repo = "openstreetmap/map-icons", branch = "master") |>
  filter(str_detect(url, "/svg/")) |>
  mutate(
    size = 40,
    style = ""
  )

map_icons <-
  bind_rows(
    maki,
    temaki,
    wu_icons,
    lane_icons,
    osm_map_icons,
    calcite
  ) |>
  relocate(
    repo,
    .after = everything()
  )

usethis::use_data(map_icons, overwrite = TRUE)


# Derived from https://raw.githubusercontent.com/visioguy/PaperSizes/master/PaperSizes.json
paper_sizes <-
  tibble::tribble(
    ~name, ~series, ~size, ~standard, ~units, ~width, ~height, ~orientation, ~type,
    "9 in. x 12 in.", "Architecture", NA, "ANSI", "in", 9, 12, "portrait", "paper",
    "18 in. x 24 in.", "Architecture", NA, "ANSI", "in", 18, 24, "portrait", "paper",
    "Arch A", "Architecture", NA, "ANSI", "in", 9, 12, "portrait", "paper",
    "Arch C", "Architecture", NA, "ANSI", "in", 18, 24, "portrait", "paper",
    "Arch E", "Architecture", NA, "ANSI", "in", 36, 48, "portrait", "paper",
    "30 in. x 42 in.", "Architecture", NA, "ANSI", "in", 30, 42, "portrait", "paper",
    "Arch E1", "Architecture", NA, "ANSI", "in", 30, 42, "portrait", "paper",
    "12 in. x 18 in.", "Architecture", NA, "ANSI", "in", 12, 18, "portrait", "paper",
    "24 in. x 36 in.", "Architecture", NA, "ANSI", "in", 24, 36, "portrait", "paper",
    "Arch B", "Architecture", NA, "ANSI", "in", 12, 18, "portrait", "paper",
    "Arch D", "Architecture", NA, "ANSI", "in", 24, 36, "portrait", "paper",
    "A", "Engineering", NA, "ANSI", "in", 8.5, 11, "portrait", "paper",
    "C", "Engineering", NA, "ANSI", "in", 17, 22, "portrait", "paper",
    "E", "Engineering", NA, "ANSI", "in", 34, 44, "portrait", "paper",
    "B+", "Engineering", NA, "ANSI", "in", 13, 19, "portrait", "paper",
    "Super B", "Engineering", NA, "ANSI", "in", 13, 19, "portrait", "paper",
    "B", "Engineering", NA, "ANSI", "in", 11, 17, "portrait", "paper",
    "D", "Engineering", NA, "ANSI", "in", 22, 34, "portrait", "paper",
    "Letter", NA, NA, "ANSI", "in", 8.5, 11, "portrait", "paper",
    "Folio", NA, NA, "ANSI", "in", 8.5, 13, "portrait", "paper",
    "Ledger", NA, NA, "ANSI", "in", 11, 17, "portrait", "paper",
    "Half Letter", NA, NA, "ANSI", "in", 5.5, 8.5, "portrait", "paper",
    "Tabloid", NA, NA, "ANSI", "in", 11, 17, "portrait", "paper",
    "Junior Legal", NA, NA, "ANSI", "in", 5, 8, "portrait", "paper",
    "Legal", NA, NA, "ANSI", "in", 8.5, 14, "portrait", "paper",
    "Large Post Quarto", NA, NA, "British Imperial", "in", 10, 10, "square", "paper",
    "Foolscap Quarto", NA, NA, "British Imperial", "in", 6.5, 8, "portrait", "paper",
    "Duke", NA, NA, "British Imperial", "in", 5.5, 7, "portrait", "paper",
    "Small Post Quarto", NA, NA, "British Imperial", "in", 7, 9, "portrait", "paper",
    "Duchess", NA, NA, "British Imperial", "in", 4.5, 6, "portrait", "paper",
    "Albert", NA, NA, "British Imperial", "in", 4, 6, "portrait", "paper",
    "Small Post Octavo", NA, NA, "British Imperial", "in", 4.5, 7, "portrait", "paper",
    "Large Post Octavo", NA, NA, "British Imperial", "in", 5, 8, "portrait", "paper",
    "Foolscap Folio", NA, NA, "British Imperial", "in", 8, 13, "portrait", "paper",
    "Facebook cover photo", NA, "cover", "Facebook", "px", 820, 312, "landscape", "social",
    "Facebook post", NA, "post", "Facebook", "px", 1200, 630, "landscape", "social",
    "Facebook story", NA, "story", "Facebook", "px", 1080, 1920, "portrait", "social",
    "Instagram post", NA, "post", "Instagram", "px", 1080, 1080, "square", "social",
    "Instagram story", NA, "story", "Instagram", "px", 1080, 1920, "portrait", "social",
    "A0", "A", "0", "ISO", "mm", 841, 1189, "portrait", "paper",
    "A1", "A", "1", "ISO", "mm", 594, 841, "portrait", "paper",
    "A2", "A", "2", "ISO", "mm", 420, 594, "portrait", "paper",
    "A3", "A", "3", "ISO", "mm", 297, 420, "portrait", "paper",
    "A4", "A", "4", "ISO", "mm", 210, 297, "portrait", "paper",
    "A5", "A", "5", "ISO", "mm", 148, 210, "portrait", "paper",
    "A6", "A", "6", "ISO", "mm", 105, 148, "portrait", "paper",
    "A7", "A", "7", "ISO", "mm", 74, 105, "portrait", "paper",
    "A8", "A", "8", "ISO", "mm", 52, 74, "portrait", "paper",
    "A9", "A", "9", "ISO", "mm", 37, 52, "portrait", "paper",
    "A10", "A", "10", "ISO", "mm", 26, 37, "portrait", "paper",
    "B0", "B", "0", "ISO", "mm", 1000, 1414, "portrait", "paper",
    "B1", "B", "1", "ISO", "mm", 707, 1000, "portrait", "paper",
    "B2", "B", "2", "ISO", "mm", 500, 707, "portrait", "paper",
    "B3", "B", "3", "ISO", "mm", 353, 500, "portrait", "paper",
    "B4", "B", "4", "ISO", "mm", 250, 353, "portrait", "paper",
    "B5", "B", "5", "ISO", "mm", 176, 250, "portrait", "paper",
    "B6", "B", "6", "ISO", "mm", 125, 176, "portrait", "paper",
    "B7", "B", "7", "ISO", "mm", 88, 125, "portrait", "paper",
    "B8", "B", "8", "ISO", "mm", 62, 88, "portrait", "paper",
    "B9", "B", "9", "ISO", "mm", 44, 62, "portrait", "paper",
    "B10", "B", "10", "ISO", "mm", 31, 44, "portrait", "paper",
    "C0", "C", "0", "ISO", "mm", 917, 1297, "portrait", "paper",
    "C1", "C", "1", "ISO", "mm", 648, 917, "portrait", "paper",
    "C2", "C", "2", "ISO", "mm", 458, 648, "portrait", "paper",
    "C3", "C", "3", "ISO", "mm", 324, 458, "portrait", "paper",
    "C4", "C", "4", "ISO", "mm", 229, 324, "portrait", "paper",
    "C5", "C", "5", "ISO", "mm", 162, 229, "portrait", "paper",
    "C6", "C", "6", "ISO", "mm", 114, 162, "portrait", "paper",
    "C7", "C", "7", "ISO", "mm", 81, 114, "portrait", "paper",
    "C8", "C", "8", "ISO", "mm", 57, 81, "portrait", "paper",
    "C9", "C", "9", "ISO", "mm", 40, 47, "portrait", "paper",
    "C10", "C", "10", "ISO", "mm", 28, 28, "square", "paper",
    "0", NA, "0", "JIS", "mm", 1030, 1456, "portrait", "paper",
    "1", NA, "1", "JIS", "mm", 728, 1030, "portrait", "paper",
    "2", NA, "2", "JIS", "mm", 515, 728, "portrait", "paper",
    "3", NA, "3", "JIS", "mm", 364, 515, "portrait", "paper",
    "4 Kiku", NA, "4", "JIS", "mm", 227, 306, "portrait", "paper",
    "4", NA, "4", "JIS", "mm", 257, 364, "portrait", "paper",
    "4 Shiroku ban", NA, "4", "JIS", "mm", 264, 379, "portrait", "paper",
    "5 Shiroku ban", NA, "5", "JIS", "mm", 189, 262, "portrait", "paper",
    "5", NA, "5", "JIS", "mm", 182, 257, "portrait", "paper",
    "5 Kiku", NA, "5", "JIS", "mm", 151, 227, "portrait", "paper",
    "6", NA, "6", "JIS", "mm", 128, 182, "portrait", "paper",
    "6 Shiroku ban", NA, "6", "JIS", "mm", 127, 188, "portrait", "paper",
    "7", NA, "7", "JIS", "mm", 91, 128, "portrait", "paper",
    "8", NA, "8", "JIS", "mm", 64, 91, "portrait", "paper",
    "9", NA, "9", "JIS", "mm", 45, 64, "portrait", "paper",
    "10", NA, "10", "JIS", "mm", 32, 45, "portrait", "paper",
    "11", NA, "11", "JIS", "mm", 22, 32, "portrait", "paper",
    "12", NA, "12", "JIS", "mm", 16, 22, "portrait", "paper",
    "Twitter cover photo", NA, "cover", "Twitter", "px", 1500, 500, "landscape", "social",
    "Twitter image and link post", NA, "post", "Twitter", "px", 1200, 628, "landscape", "social",
    "Twitter single image post", NA, "post", "Twitter", "px", 1200, 675, "landscape", "social",
    "Twitter multiple image post", NA, "post", "Twitter", "px", 700, 800, "portrait", "social",
    "8.5 in. x 7 in.", "EDDM", NA, "USPS", "in", 8.5, 7, "landscape", "postcard",
    "8.5 in. x 11 in.", "EDDM", NA, "USPS", "in", 8.5, 11, "portrait", "postcard",
    "6.25 in. x 9 in.", "EDDM", NA, "USPS", "in", 6.25, 9, "portrait", "postcard",
    "6.25 in. x 11 in.", "EDDM", NA, "USPS", "in", 6.25, 11, "portrait", "postcard",
    "6 in. x 12 in.", "EDDM", NA, "USPS", "in", 6, 12, "portrait", "postcard",
    "4.25 in. x 11 in.", "EDDM", NA, "USPS", "in", 4.25, 11, "portrait", "postcard",
    "USPS postcard (min)", NA, NA, "USPS", "in", 5, 3.5, "landscape", "postcard",
    "USPS postcard (max)", NA, NA, "USPS", "in", 6, 4.25, "landscape", "postcard",
    "6 in. x 11 in.", NA, NA, NA, "in", 11, 6, "landscape", "postcard",
    "5.5 in. x 8.5 in.", NA, NA, NA, "in", 8.5, 5.5, "landscape", "postcard",
    "4 in. x 6 in.", NA, NA, NA, "in", 6, 4, "landscape", "postcard",
    "6 in. x 9 in.", NA, NA, NA, "in", 9, 6, "landscape", "postcard",
    "6.5 in. x 9.5 in.", NA, NA, NA, "in", 9.5, 6.5, "landscape", "postcard",
    "5 in. x 7 in.", NA, NA, NA, "in", 7, 5, "landscape", "postcard",
    "3 in. x 4 in.", NA, NA, NA, "in", 4, 3, "landscape", "postcard",
    "4.25 in. x 5.5 in.", NA, NA, NA, "in", 5.5, 4.25, "landscape", "postcard",
    "8 x 10 inches", NA, NA, NA, "in", 8, 10, "portrait", "print",
    "8.5 x 11 inches", NA, NA, NA, "in", 8.5, 11, "portrait", "print",
    "18 x 24 inches", NA, NA, NA, "in", 18, 24, "portrait", "print",
    "5 x 7 inches", NA, NA, NA, "in", 5, 7, "portrait", "print",
    "4 x 6 inches", NA, NA, NA, "in", 4, 6, "portrait", "print",
    "12 x 18 inches", NA, NA, NA, "in", 12, 18, "portrait", "print",
    "24 x 36 inches", NA, NA, NA, "in", 24, 36, "portrait", "print"
  )

usethis::use_data(paper_sizes, overwrite = TRUE)

osm_building_tags <-
  osmdata::available_tags("building")

osm_building_tags <-
  stringr::str_remove(osm_building_tags, "(?<=&)[:graph:]+") |>
  stringr::str_remove("&")

usethis::use_data(
  osm_building_tags,
  overwrite = TRUE
)

dist_unit_options <-
  c(
    "arc_degree", "arc_minute", "arc_second", "cm", "m", "metre", "meter", "meters", "km", "kilometer", "kilometers",
    "inch", "in", "ft", "foot", "feet", "yard", "yards", "mi", "mile", "miles", "nautical_mile", "radian"
  )

add_metric_units <-
  tibble::tribble(
    ~symbol, ~symbol_aliases, ~name_singular, ~name_singular_aliases, ~name_plural, ~name_plural_aliases, ~def, ~definition, ~comment, ~dimensionless, ~source_xml,
    "km", "", "kilometer", "kilometre", "kilometers", "kilometres", "m * 1000", "length equivalent to 1000 meters", NA, FALSE, NA,
    "cm", "", "centimeter", "centimetre", "centimeters", "centimetres", "m/100", "length equivalent to 0.01 meter", NA, FALSE, NA
  )

valid_units <-
  units::valid_udunits()

valid_units <-
  valid_units |>
  dplyr::bind_rows(add_metric_units)

units_filter_cols <- c("symbol", "symbol_aliases", "name_singular", "name_singular_aliases", "name_plural", "name_plural_aliases")

dist_units <-
  valid_units |>
  filter(
    if_any(
      any_of(units_filter_cols),
      ~ (.x %in% dist_unit_options)
    )
  )

dist_units <-
  naniar::replace_with_na_if(dist_units, is.character, ~ .x == "")

usethis::use_data(
  dist_units,
  overwrite = TRUE
)

area_units <-
  valid_units |>
  filter(
    if_any(
      any_of(units_filter_cols),
      ~ (.x %in% c("hectare", "acre", "acre_foot"))
    )
  )

area_units <-
  naniar::replace_with_na_if(area_units, is.character, ~ .x == "")

usethis::use_data(
  area_units,
  overwrite = TRUE
)

dist_unit_options <-
  unique(c(
    dist_units$name_singular,
    dist_units$symbol,
    dist_units$symbol_aliases,
    dist_units$name_singular,
    stringr::str_split(dist_units$name_singular_aliases, ", ", simplify = TRUE),
    dist_units$name_plural,
    stringr::str_split(dist_units$name_plural_aliases, ", ", simplify = TRUE)
  ))


usethis::use_data(
  dist_unit_options,
  overwrite = TRUE,
  internal = FALSE
)

standard_scales <-
  tibble::tribble(
    ~scale, ~standard, ~series, ~actual_ft, ~actual_ft_unit, ~scale_in, ~scale_in_unit, ~scale_in_accuracy, ~scale_cm, ~scale_cm_unit, ~scale_cm_accuracy, ~size_latlon, ~size_latlon_unit, ~area_approx, ~area_approx_unit, ~series_status,
    "1:20,000", "USGS", "Puerto Rico 7.5 minute", 6e-04, "in", 1670, "ft", "approximate", 200, "m", "exact", "7.5 by 7.5 minute", "minute", "7.10E+01", "sq mi", NA,
    "1:24,000", "USGS", "7.5 minute", 5e-04, "in", 2000, "ft", "exact", 240, "m", "exact", "7.5 by 7.5 minute", "minute", "49 to 70", "sq mi", NA,
    "1:25,000", "USGS", "7.5 minute", 0.00048, "in", 2080, "ft", "approximate", 250, "m", "exact", "7.5 by 7.5 minute", "minute", "49 to 70", "sq mi", NA,
    "1:25,000", "USGS", "7.5 by 15 minute", 0.00048, "in", 2080, "ft", "approximate", 250, "m", "exact", "7.5 by 15 minute", "minute", "98 to 140", "sq mi", NA,
    "1:50,000", "USGS", "USGS-DMA 15 minute", 0.00024, "in", 4170, "ft", "approximate", 500, "m", "exact", "15 by 15 minute", "minute", "197 to 282", "sq mi", NA,
    "1:62,500", "USGS", "15 minute*", NA, NA, 1, "mi", "approximate", 625, "m", "exact", "15 by 15 minute", "minute", "197 to 282", "sq mi", "abandoned",
    "1:63,360", "USGS", "Alaska Maps", NA, NA, 1, "mi", "exact", 634, "m", "exact", "15 by 20 to 36 minute", "minute", "207 to 281", "sq mi", NA,
    "1:50,000", "USGS", "County Maps", 0.00024, "in", 4170, "ft", "approximate", 500, "m", "exact", "County area", NA, "Varies", NA, NA,
    "1:100,000", "USGS", "County Maps", NA, NA, 1.6, "mi", "approximate", 1, "km", "exact", "County area", NA, "Varies", NA, NA,
    "1:100,000", "USGS", "30 by 60 minute", NA, NA, 1.6, "mi", "approximate", 1, "km", "exact", "30 by 60 minute", "minute", "1,568 to 2,240", "sq mi", NA,
    "1:125,000", "USGS", "30 minute*", NA, NA, 2, "mi", "approximate", 1.25, "km", "exact", "30 by 30 minute", "minute", "786 to 1,124", "sq mi", "abandoned",
    "1:250,000", "USGS", "1 degree by 2 degrees or 3 degrees", NA, NA, 4, "mi", "approximate", 2.5, "km", "exact", "1° by 2° or 3°", "degree", "4,580 to 8,669", "sq mi", NA,
    "1:500,000", "USGS", "State Maps", NA, NA, 8, "mi", "approximate", 5, "km", "exact", "State area", NA, "Varies", NA, NA,
    "1:1,000,000", "USGS", "State Maps", NA, NA, 16, "mi", "approximate", 10, "km", "exact", "State area", NA, "Varies", NA, NA,
    "1:2,000,000", "USGS", "U.S. Sectional Maps", NA, NA, 32, "mi", "approximate", 20, "km", "exact", "State groups", NA, "Varies", NA, NA,
    "1:250,000", "USGS", "Antarctica Maps", NA, NA, 4, "mi", "approximate", 2.5, "km", "exact", "1° by 3° to 15°", "degree", "4,089 to 8,336", "sq mi", NA,
    "1:500,000", "USGS", "Antarctica Maps", NA, NA, 8, "mi", "approximate", 5, "km", "exact", "2° by 7.5°", "degree", "28,174 to 30,462", "sq mi", NA,
    "1 in = 10 feet", "Engineering", NA, 0.1, "in", 10, "feet", "exact", 3.05, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 20 feet", "Engineering", NA, 0.05, "in", 20, "feet", "exact", 6.1, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 30 feet", "Engineering", NA, 0.0333, "in", 30, "feet", "exact", 9.14, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 40 feet", "Engineering", NA, 0.025, "in", 40, "feet", "exact", 12.2, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 50 feet", "Engineering", NA, 0.02, "in", 50, "feet", "exact", 15.2, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 60 feet", "Engineering", NA, 0.0167, "in", 60, "feet", "exact", 18.3, "m", "approximate", NA, NA, NA, NA, "common",
    "3/32 in = 1 foot", "Architectural", NA, 0.0938, "in", 10.7, "feet", "approximate", 3.25, "m", "approximate", NA, NA, NA, NA, "common",
    "3/16 in = 1 foot", "Architectural", NA, 0.188, "in", 5.33, "feet", "approximate", 1.63, "m", "approximate", NA, NA, NA, NA, "common",
    "1/8 in = 1 foot", "Architectural", NA, 0.125, "in", 8, "feet", "exact", 2.44, "m", "approximate", NA, NA, NA, NA, "common",
    "1/4 in = 1 foot", "Architectural", NA, 0.25, "in", 4, "feet", "exact", 1.22, "m", "approximate", NA, NA, NA, NA, "common",
    "3/8 in = 1 foot", "Architectural", NA, 0.375, "in", 2.67, "feet", "approximate", 0.813, "m", "approximate", NA, NA, NA, NA, "common",
    "1/2 in = 1 foot", "Architectural", NA, 0.5, "in", 2, "feet", "exact", 0.61, "m", "approximate", NA, NA, NA, NA, "common",
    "3/4 in = 1 foot", "Architectural", NA, 0.75, "in", 1.33, "feet", "approximate", 0.406, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 1 foot", "Architectural", NA, 1, "in", 1, "feet", "exact", 0.305, "m", "approximate", NA, NA, NA, NA, "common",
    "1½ in = 1 foot", "Architectural", NA, 1.5, "in", 0.667, "feet", "approximate", 0.203, "m", "approximate", NA, NA, NA, NA, "common"
  )


usethis::use_data(
  standard_scales,
  overwrite = TRUE
)

us_states <-
  tigris::states(
    cb = TRUE,
    resolution = "5m"
  )

us_states <-
  us_states %>%
  janitor::clean_names("snake") %>%
  sf::st_transform(3857)

nest_states <-
  us_states %>%
  dplyr::group_by(geoid) %>%
  dplyr::group_nest(keep = TRUE)

us_states <-
  purrr::map_dfr(
    .x = nest_states$data,
    ~ tibble::tibble(
      name = unique(.x$name),
      geoid = unique(.x$geoid),
      statefp = unique(.x$statefp),
      abb = unique(.x$stusps),
      bbox = list(sf::st_bbox(.x)),
      wkt = sf::st_as_text(sf::st_as_sfc(.x))
    )
  )


get_state_pop <-
  function(x) {
    tidycensus::get_acs(
      geography = "state",
      variables = "B01001_001",
      year = 2019,
      state = x,
      cache_table = TRUE
    ) %>%
      janitor::clean_names("snake")
  }


possibly_get_state_pop <-
  purrr::possibly(
    ~ get_state_pop(.x),
    NULL,
    quiet = FALSE
  )

us_states_census <-
  purrr::map_dfr(
  us_states$statefp,
  ~ possibly_get_state_pop(.x)
)

us_states_census <-
us_states_census %>%
  dplyr::select(-c(name, variable, moe)) %>%
  dplyr::rename(est_pop = estimate)

us_states <- us_states %>%
  dplyr::left_join(
    us_states_census,
    by = "geoid"
  ) %>%
  dplyr::relocate(
    abb,
    est_pop,
    .after = geoid
  )

names(us_states$geoid) <- tolower(us_states$abb)
names(us_states$wkt) <- tolower(us_states$abb)

usethis::use_data(
  us_states,
  overwrite = TRUE,
  internal = FALSE
)

us_counties <-
  purrr::map_dfr(
    us_states$statefp,
    ~ tigris::counties(
      state = .x,
      cb = TRUE,
      resolution = "5m"
    )
  )

us_counties <-
  us_counties %>%
  janitor::clean_names("snake") %>%
  sf::st_transform(3857)

us_counties <-
  us_counties %>%
  dplyr::left_join(
    dplyr::select(us_states, state_abb = abb, statefp),
    by = "statefp"
  )

nest_counties <- us_counties %>%
  dplyr::group_by(geoid) %>%
  dplyr::group_nest(keep = TRUE)

us_counties <-
  purrr::map_dfr(
    .x = nest_counties$data,
    ~ tibble::tibble(
      name_short = unique(.x$name),
      geoid = unique(.x$geoid),
      countyfp = unique(.x$countyfp),
      statefp = unique(.x$statefp),
      abb_state = unique(.x$state_abb),
      bbox = list(sf::st_bbox(.x)),
      wkt = sf::st_as_text(sf::st_as_sfc(.x))
    )
  )

get_county_pop <-
  function(x, y) {
    tidycensus::get_acs(
      geography = "county",
      variables = "B01001_001",
      year = 2019,
      state = x,
      county = y,
      cache_table = TRUE
    )
  }

possibly_get_county_pop <-
  purrr::possibly(
    ~ get_county_pop(.x, .y),
    NULL,
    quiet = FALSE
  )


us_counties_census <-
  purrr::map2_dfr(
    us_counties$statefp,
    us_counties$countyfp,
    ~ possibly_get_county_pop(
      .x,
      .y
    )
  ) %>%
  janitor::clean_names("snake")

us_counties <-
  us_counties %>%
  dplyr::left_join(us_counties_census, by = "geoid") %>%
  dplyr::select(-c(moe, variable)) %>%
  dplyr::rename(
    est_pop = estimate
  ) %>%
  dplyr::relocate(
    abb_state, est_pop, .after = name_short
  ) %>%
  dplyr::relocate(
    name, .before = dplyr::everything()
  ) %>%
  dplyr::mutate(
    name = dplyr::case_when(
      (is.na(name) && state_abb == "VI") ~ paste0(name_short, ", ", "U.S. Virgin Islands"),
      (is.na(name) && state_abb == "GU") ~ paste0(name_short, ", ", "Guam"),
      (is.na(name) && state_abb == "MP") ~ paste0(name_short, ", ", "Northern Mariana Islands"),
      (is.na(name) && state_abb == "AS") ~ paste0(name_short, ", ", "American Samoa"),
      TRUE ~ name
    )
  ) %>%
  dplyr::mutate(
  label = stringr::str_extract(name, ".+(?=,)"),
  label = janitor::make_clean_names(paste0(label, ", ", abb_state))
) %>%
  dplyr::mutate(
    label = stringr::str_replace(
      label, "_borough_", "_bor_"
    ),
    label = stringr::str_replace(
      label, "_county_", "_co_"
    ),
    label = stringr::str_replace(
      label, "_parish_", "_par_"
    ),
    label = stringr::str_replace(
      label, "_township_", "_twp_"
    ),
    label = stringr::str_replace(
      label, "_census_area_", "_"
    ),
    label = stringr::str_replace(
      label, "_municipio_", "_muni_"
    )
  )

names(us_counties$wkt) <- us_counties$label
names(us_counties$geoid) <- us_counties$label

us_counties <- dplyr::select(
  us_counties,
  -label
)

usethis::use_data(
  us_counties,
  overwrite = TRUE,
  internal = FALSE
)
