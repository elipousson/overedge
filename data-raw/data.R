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
    ~name, ~series, ~size, ~standard, ~units, ~width, ~height, ~asp_portrait, ~asp_landscape, ~asp_text, ~type,
    "Letter", NA, NA, "ANSI", "in", 8.5, 11, 0.7727272727, 1.294117647, "8.5:11", "paper",
    "Folio", NA, NA, "ANSI", "in", 8.5, 13, 0.6538461538, 1.529411765, "8.5:13", "paper",
    "Legal", NA, NA, "ANSI", "in", 8.5, 14, 0.6071428571, 1.647058824, "8.5:14", "paper",
    "Ledger", NA, NA, "ANSI", "in", 11, 17, 0.6470588235, 1.545454545, "11:17", "paper",
    "Half Letter", NA, NA, "ANSI", "in", 5.5, 8.5, 0.6470588235, 1.545454545, "5.5:8.5", "paper",
    "Junior Legal", NA, NA, "ANSI", "in", 5, 8, 0.625, 1.6, "5:8", "paper",
    "Tabloid", NA, NA, "ANSI", "in", 11, 17, 0.6470588235, 1.545454545, "11:17", "paper",
    "A5", "A", "5", "ISO", "mm", 148, 210, 0.7047619048, 1.418918919, "148:210", "paper",
    "A4", "A", "4", "ISO", "mm", 210, 297, 0.7070707071, 1.414285714, "210:297", "paper",
    "A3", "A", "3", "ISO", "mm", 297, 420, 0.7071428571, 1.414141414, "297:420", "paper",
    "A2", "A", "2", "ISO", "mm", 420, 594, 0.7070707071, 1.414285714, "420:594", "paper",
    "A1", "A", "1", "ISO", "mm", 594, 841, 0.7063020214, 1.415824916, "594:841", "paper",
    "A0", "A", "0", "ISO", "mm", 841, 1189, 0.7073170732, 1.413793103, "841:1189", "paper",
    "A6", "A", "6", "ISO", "mm", 105, 148, 0.7094594595, 1.40952381, "105:148", "paper",
    "A7", "A", "7", "ISO", "mm", 74, 105, 0.7047619048, 1.418918919, "74:105", "paper",
    "A8", "A", "8", "ISO", "mm", 52, 74, 0.7027027027, 1.423076923, "52:74", "paper",
    "A9", "A", "9", "ISO", "mm", 37, 52, 0.7115384615, 1.405405405, "37:52", "paper",
    "A10", "A", "10", "ISO", "mm", 26, 37, 0.7027027027, 1.423076923, "26:37", "paper",
    "A", "Engineering", NA, "ANSI", "in", 8.5, 11, 0.7727272727, 1.294117647, "8.5:11", "paper",
    "B", "Engineering", NA, "ANSI", "in", 11, 17, 0.6470588235, 1.545454545, "11:17", "paper",
    "C", "Engineering", NA, "ANSI", "in", 17, 22, 0.7727272727, 1.294117647, "17:22", "paper",
    "D", "Engineering", NA, "ANSI", "in", 22, 34, 0.6470588235, 1.545454545, "22:34", "paper",
    "E", "Engineering", NA, "ANSI", "in", 34, 44, 0.7727272727, 1.294117647, "34:44", "paper",
    "B+", "Engineering", NA, "ANSI", "in", 13, 19, 0.6842105263, 1.461538462, "13:19", "paper",
    "Super B", "Engineering", NA, "ANSI", "in", 13, 19, 0.6842105263, 1.461538462, "13:19", "paper",
    "9 in. x 12 in.", "Architecture", NA, "ANSI", "in", 9, 12, 0.75, 1.333333333, "9:12", "paper",
    "12 in. x 18 in.", "Architecture", NA, "ANSI", "in", 12, 18, 0.6666666667, 1.5, "12:18", "paper",
    "18 in. x 24 in.", "Architecture", NA, "ANSI", "in", 18, 24, 0.75, 1.333333333, "18:24", "paper",
    "24 in. x 36 in.", "Architecture", NA, "ANSI", "in", 24, 36, 0.6666666667, 1.5, "24:36", "paper",
    "30 in. x 42 in.", "Architecture", NA, "ANSI", "in", 30, 42, 0.7142857143, 1.4, "30:42", "paper",
    "Arch A", "Architecture", NA, "ANSI", "in", 9, 12, 0.75, 1.333333333, "9:12", "paper",
    "Arch B", "Architecture", NA, "ANSI", "in", 12, 18, 0.6666666667, 1.5, "12:18", "paper",
    "Arch C", "Architecture", NA, "ANSI", "in", 18, 24, 0.75, 1.333333333, "18:24", "paper",
    "Arch D", "Architecture", NA, "ANSI", "in", 24, 36, 0.6666666667, 1.5, "24:36", "paper",
    "Arch E", "Architecture", NA, "ANSI", "in", 36, 48, 0.75, 1.333333333, "36:48", "paper",
    "Arch E1", "Architecture", NA, "ANSI", "in", 30, 42, 0.7142857143, 1.4, "30:42", "paper",
    "B0", "B", "0", "ISO", "mm", 1000, 1414, 0.7072135785, 1.414, "1000:1414", "paper",
    "B1", "B", "1", "ISO", "mm", 707, 1000, 0.707, 1.414427157, "707:1000", "paper",
    "B2", "B", "2", "ISO", "mm", 500, 707, 0.7072135785, 1.414, "500:707", "paper",
    "B3", "B", "3", "ISO", "mm", 353, 500, 0.706, 1.416430595, "353:500", "paper",
    "B4", "B", "4", "ISO", "mm", 250, 353, 0.7082152975, 1.412, "250:353", "paper",
    "B5", "B", "5", "ISO", "mm", 176, 250, 0.704, 1.420454545, "176:250", "paper",
    "B6", "B", "6", "ISO", "mm", 125, 176, 0.7102272727, 1.408, "125:176", "paper",
    "B7", "B", "7", "ISO", "mm", 88, 125, 0.704, 1.420454545, "88:125", "paper",
    "B8", "B", "8", "ISO", "mm", 62, 88, 0.7045454545, 1.419354839, "62:88", "paper",
    "B9", "B", "9", "ISO", "mm", 44, 62, 0.7096774194, 1.409090909, "44:62", "paper",
    "B10", "B", "10", "ISO", "mm", 31, 44, 0.7045454545, 1.419354839, "31:44", "paper",
    "C0", "C", "0", "ISO", "mm", 917, 1297, 0.7070161912, 1.414394766, "917:1297", "paper",
    "C1", "C", "1", "ISO", "mm", 648, 917, 0.7066521265, 1.415123457, "648:917", "paper",
    "C2", "C", "2", "ISO", "mm", 458, 648, 0.7067901235, 1.414847162, "458:648", "paper",
    "C3", "C", "3", "ISO", "mm", 324, 458, 0.7074235808, 1.413580247, "324:458", "paper",
    "C4", "C", "4", "ISO", "mm", 229, 324, 0.7067901235, 1.414847162, "229:324", "paper",
    "C5", "C", "5", "ISO", "mm", 162, 229, 0.7074235808, 1.413580247, "162:229", "paper",
    "C6", "C", "6", "ISO", "mm", 114, 162, 0.7037037037, 1.421052632, "114:162", "paper",
    "C7", "C", "7", "ISO", "mm", 81, 114, 0.7105263158, 1.407407407, "81:114", "paper",
    "C8", "C", "8", "ISO", "mm", 57, 81, 0.7037037037, 1.421052632, "57:81", "paper",
    "C9", "C", "9", "ISO", "mm", 40, 47, 0.8510638298, 1.175, "40:47", "paper",
    "C10", "C", "10", "ISO", "mm", 28, 28, 1, 1, "28:28", "paper",
    "Albert", NA, NA, "British Imperial", "in", 4, 6, 0.6666666667, 1.5, "4:6", "paper",
    "Duchess", NA, NA, "British Imperial", "in", 4.5, 6, 0.75, 1.333333333, "4.5:6", "paper",
    "Duke", NA, NA, "British Imperial", "in", 5.5, 7, 0.7857142857, 1.272727273, "5.5:7", "paper",
    "Foolscap Quarto", NA, NA, "British Imperial", "in", 6.5, 8, 0.8125, 1.230769231, "6.5:8", "paper",
    "Foolscap Folio", NA, NA, "British Imperial", "in", 8, 13, 0.6153846154, 1.625, "8:13", "paper",
    "Small Post Octavo", NA, NA, "British Imperial", "in", 4.5, 7, 0.6428571429, 1.555555556, "4.5:7", "paper",
    "Small Post Quarto", NA, NA, "British Imperial", "in", 7, 9, 0.7777777778, 1.285714286, "7:9", "paper",
    "Large Post Octavo", NA, NA, "British Imperial", "in", 5, 8, 0.625, 1.6, "5:8", "paper",
    "Large Post Quarto", NA, NA, "British Imperial", "in", 10, 10, 1, 1, "10:10", "paper",
    "0", NA, "0", "JIS", "mm", 1030, 1456, 0.7074175824, 1.413592233, "1030:1456", "paper",
    "1", NA, "1", "JIS", "mm", 728, 1030, 0.7067961165, 1.414835165, "728:1030", "paper",
    "2", NA, "2", "JIS", "mm", 515, 728, 0.7074175824, 1.413592233, "515:728", "paper",
    "3", NA, "3", "JIS", "mm", 364, 515, 0.7067961165, 1.414835165, "364:515", "paper",
    "4", NA, "4", "JIS", "mm", 257, 364, 0.706043956, 1.416342412, "257:364", "paper",
    "4 Shiroku ban", NA, "4", "JIS", "mm", 264, 379, 0.6965699208, 1.435606061, "264:379", "paper",
    "4 Kiku", NA, "4", "JIS", "mm", 227, 306, 0.7418300654, 1.348017621, "227:306", "paper",
    "5", NA, "5", "JIS", "mm", 182, 257, 0.7081712062, 1.412087912, "182:257", "paper",
    "5 Shiroku ban", NA, "5", "JIS", "mm", 189, 262, 0.7213740458, 1.386243386, "189:262", "paper",
    "5 Kiku", NA, "5", "JIS", "mm", 151, 227, 0.6651982379, 1.503311258, "151:227", "paper",
    "6", NA, "6", "JIS", "mm", 128, 182, 0.7032967033, 1.421875, "128:182", "paper",
    "6 Shiroku ban", NA, "6", "JIS", "mm", 127, 188, 0.6755319149, 1.480314961, "127:188", "paper",
    "7", NA, "7", "JIS", "mm", 91, 128, 0.7109375, 1.406593407, "91:128", "paper",
    "8", NA, "8", "JIS", "mm", 64, 91, 0.7032967033, 1.421875, "64:91", "paper",
    "9", NA, "9", "JIS", "mm", 45, 64, 0.703125, 1.422222222, "45:64", "paper",
    "10", NA, "10", "JIS", "mm", 32, 45, 0.7111111111, 1.40625, "32:45", "paper",
    "11", NA, "11", "JIS", "mm", 22, 32, 0.6875, 1.454545455, "22:32", "paper",
    "12", NA, "12", "JIS", "mm", 16, 22, 0.7272727273, 1.375, "16:22", "paper",
    "USPS postcard (max)", NA, NA, "USPS", "in", 6, 4.25, 1.411764706, 0.7083333333, "6:4.25", "postcard",
    "USPS postcard (min)", NA, NA, "USPS", "in", 5, 3.5, 1.428571429, 0.7, "5:3.5", "postcard",
    "4 in. x 6 in.", NA, NA, NA, "in", 6, 4, 1.5, 0.6666666667, "6:4", "postcard",
    "4.25 in. x 5.5 in.", NA, NA, NA, "in", 5.5, 4.25, 1.294117647, 0.7727272727, "5.5:4.25", "postcard",
    "5 in. x 7 in.", NA, NA, NA, "in", 7, 5, 1.4, 0.7142857143, "7:5", "postcard",
    "5.5 in. x 8.5 in.", NA, NA, NA, "in", 8.5, 5.5, 1.545454545, 0.6470588235, "8.5:5.5", "postcard",
    "6 in. x 9 in.", NA, NA, NA, "in", 9, 6, 1.5, 0.6666666667, "9:6", "postcard",
    "6 in. x 11 in.", NA, NA, NA, "in", 11, 6, 1.833333333, 0.5454545455, "11:6", "postcard",
    "3 in. x 4 in.", NA, NA, NA, "in", 4, 3, 1.333333333, 0.75, "4:3", "postcard",
    "6.5 in. x 9.5 in.", NA, NA, NA, "in", 9.5, 6.5, 1.461538462, 0.6842105263, "9.5:6.5", "postcard",
    "8.5 in. x 7 in.", "EDDM", NA, "USPS", "in", 8.5, 7, 1.214285714, 0.8235294118, "8.5:7", "postcard",
    "6.25 in. x 9 in.", "EDDM", NA, "USPS", "in", 6.25, 9, 0.6944444444, 1.44, "6.25:9", "postcard",
    "4.25 in. x 11 in.", "EDDM", NA, "USPS", "in", 4.25, 11, 0.3863636364, 2.588235294, "4.25:11", "postcard",
    "6.25 in. x 11 in.", "EDDM", NA, "USPS", "in", 6.25, 11, 0.5681818182, 1.76, "6.25:11", "postcard",
    "8.5 in. x 11 in.", "EDDM", NA, "USPS", "in", 8.5, 11, 0.7727272727, 1.294117647, "8.5:11", "postcard",
    "6 in. x 12 in.", "EDDM", NA, "USPS", "in", 6, 12, 0.5, 2, "6:12", "postcard",
    "4 x 6 inches", NA, NA, NA, "in", 4, 6, 0.6666666667, 1.5, "4:6", "print",
    "5 x 7 inches", NA, NA, NA, "in", 5, 7, 0.7142857143, 1.4, "5:7", "print",
    "8 x 10 inches", NA, NA, NA, "in", 8, 10, 0.8, 1.25, "8:10", "print",
    "8.5 x 11 inches", NA, NA, NA, "in", 8.5, 11, 0.7727272727, 1.294117647, "8.5:11", "print",
    "12 x 18 inches", NA, NA, NA, "in", 12, 18, 0.6666666667, 1.5, "12:18", "print",
    "18 x 24 inches", NA, NA, NA, "in", 18, 24, 0.75, 1.333333333, "18:24", "print",
    "24 x 36 inches", NA, NA, NA, "in", 24, 36, 0.6666666667, 1.5, "24:36", "print",
    "Instagram post", NA, "post", "Instagram", "px", 1080, 1080, 1, 1, "1080:1080", "social",
    "Instagram story", NA, "story", "Instagram", "px", 1080, 1920, 0.5625, 1.777777778, "1080:1920", "social",
    "Facebook post", NA, "post", "Facebook", "px", 1200, 630, 1.904761905, 0.525, "1200:630", "social",
    "Facebook story", NA, "story", "Facebook", "px", 1080, 1920, 0.5625, 1.777777778, "1080:1920", "social",
    "Facebook cover photo", NA, "cover", "Facebook", "px", 820, 312, 2.628205128, 0.3804878049, "820:312", "social",
    "Twitter single image post", NA, "post", "Twitter", "px", 1200, 675, 1.777777778, 0.5625, "1200:675", "social",
    "Twitter multiple image post", NA, "post", "Twitter", "px", 700, 800, 0.875, 1.142857143, "700:800", "social",
    "Twitter image and link post", NA, "post", "Twitter", "px", 1200, 628, 1.910828025, 0.5233333333, "1200:628", "social",
    "Twitter cover photo", NA, "cover", "Twitter", "px", 1500, 500, 3, 0.3333333333, "1500:500", "social"
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

valid_dist_units <-
  valid_units |>
  filter(
    if_any(
      any_of(units_filter_cols),
      ~ (.x %in% dist_unit_options)
    )
  )

valid_dist_units <-
  naniar::replace_with_na_if(valid_dist_units, is.character, ~ .x == "")

usethis::use_data(
  valid_dist_units,
  overwrite = TRUE
)

dist_unit_options <-
  unique(c(
    valid_dist_units$name_singular,
    valid_dist_units$symbol,
    valid_dist_units$symbol_aliases,
    valid_dist_units$name_singular,
    stringr::str_split(valid_dist_units$name_singular_aliases, ", ", simplify = TRUE),
    valid_dist_units$name_plural,
    stringr::str_split(valid_dist_units$name_plural_aliases, ", ", simplify = TRUE)
  ))


usethis::use_data(
  dist_unit_options,
  overwrite = TRUE,
  internal = TRUE
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
