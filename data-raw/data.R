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
                ~name,        ~series, ~size,          ~standard, ~units, ~width, ~height, ~asp_portrait, ~asp_landscape,   ~asp_text,
             "Letter",             NA,    NA,             "ANSI",   "in",    8.5,      11,  0.7727272727,    1.294117647,    "8.5:11",
              "Folio",             NA,    NA,             "ANSI",   "in",    8.5,      13,  0.6538461538,    1.529411765,    "8.5:13",
              "Legal",             NA,    NA,             "ANSI",   "in",    8.5,      14,  0.6071428571,    1.647058824,    "8.5:14",
             "Ledger",             NA,    NA,             "ANSI",   "in",     11,      17,  0.6470588235,    1.545454545,     "11:17",
        "Half Letter",             NA,    NA,             "ANSI",   "in",    5.5,     8.5,  0.6470588235,    1.545454545,   "5.5:8.5",
       "Junior Legal",             NA,    NA,             "ANSI",   "in",      5,       8,         0.625,            1.6,       "5:8",
            "Tabloid",             NA,    NA,             "ANSI",   "in",     11,      17,  0.6470588235,    1.545454545,     "11:17",
                 "A5",            "A",    5L,              "ISO",   "mm",    148,     210,  0.7047619048,    1.418918919,   "148:210",
                 "A4",            "A",    4L,              "ISO",   "mm",    210,     297,  0.7070707071,    1.414285714,   "210:297",
                 "A3",            "A",    3L,              "ISO",   "mm",    297,     420,  0.7071428571,    1.414141414,   "297:420",
                 "A2",            "A",    2L,              "ISO",   "mm",    420,     594,  0.7070707071,    1.414285714,   "420:594",
                 "A1",            "A",    1L,              "ISO",   "mm",    594,     841,  0.7063020214,    1.415824916,   "594:841",
                 "A0",            "A",    0L,              "ISO",   "mm",    841,    1189,  0.7073170732,    1.413793103,  "841:1189",
                 "A6",            "A",    6L,              "ISO",   "mm",    105,     148,  0.7094594595,     1.40952381,   "105:148",
                 "A7",            "A",    7L,              "ISO",   "mm",     74,     105,  0.7047619048,    1.418918919,    "74:105",
                 "A8",            "A",    8L,              "ISO",   "mm",     52,      74,  0.7027027027,    1.423076923,     "52:74",
                 "A9",            "A",    9L,              "ISO",   "mm",     37,      52,  0.7115384615,    1.405405405,     "37:52",
                "A10",            "A",   10L,              "ISO",   "mm",     26,      37,  0.7027027027,    1.423076923,     "26:37",
                  "A",  "Engineering",    NA,             "ANSI",   "in",    8.5,      11,  0.7727272727,    1.294117647,    "8.5:11",
                  "B",  "Engineering",    NA,             "ANSI",   "in",     11,      17,  0.6470588235,    1.545454545,     "11:17",
                  "C",  "Engineering",    NA,             "ANSI",   "in",     17,      22,  0.7727272727,    1.294117647,     "17:22",
                  "D",  "Engineering",    NA,             "ANSI",   "in",     22,      34,  0.6470588235,    1.545454545,     "22:34",
                  "E",  "Engineering",    NA,             "ANSI",   "in",     34,      44,  0.7727272727,    1.294117647,     "34:44",
                 "B+",  "Engineering",    NA,             "ANSI",   "in",     13,      19,  0.6842105263,    1.461538462,     "13:19",
            "Super B",  "Engineering",    NA,             "ANSI",   "in",     13,      19,  0.6842105263,    1.461538462,     "13:19",
     "9 in. x 12 in.", "Architecture",    NA,             "ANSI",   "in",      9,      12,          0.75,    1.333333333,      "9:12",
    "12 in. x 18 in.", "Architecture",    NA,             "ANSI",   "in",     12,      18,  0.6666666667,            1.5,     "12:18",
    "18 in. x 24 in.", "Architecture",    NA,             "ANSI",   "in",     18,      24,          0.75,    1.333333333,     "18:24",
    "24 in. x 36 in.", "Architecture",    NA,             "ANSI",   "in",     24,      36,  0.6666666667,            1.5,     "24:36",
    "30 in. x 42 in.", "Architecture",    NA,             "ANSI",   "in",     30,      42,  0.7142857143,            1.4,     "30:42",
             "Arch A", "Architecture",    NA,             "ANSI",   "in",      9,      12,          0.75,    1.333333333,      "9:12",
             "Arch B", "Architecture",    NA,             "ANSI",   "in",     12,      18,  0.6666666667,            1.5,     "12:18",
             "Arch C", "Architecture",    NA,             "ANSI",   "in",     18,      24,          0.75,    1.333333333,     "18:24",
             "Arch D", "Architecture",    NA,             "ANSI",   "in",     24,      36,  0.6666666667,            1.5,     "24:36",
             "Arch E", "Architecture",    NA,             "ANSI",   "in",     36,      48,          0.75,    1.333333333,     "36:48",
            "Arch E1", "Architecture",    NA,             "ANSI",   "in",     30,      42,  0.7142857143,            1.4,     "30:42",
                 "B0",            "B",    0L,              "ISO",   "mm",   1000,    1414,  0.7072135785,          1.414, "1000:1414",
                 "B1",            "B",    1L,              "ISO",   "mm",    707,    1000,         0.707,    1.414427157,  "707:1000",
                 "B2",            "B",    2L,              "ISO",   "mm",    500,     707,  0.7072135785,          1.414,   "500:707",
                 "B3",            "B",    3L,              "ISO",   "mm",    353,     500,         0.706,    1.416430595,   "353:500",
                 "B4",            "B",    4L,              "ISO",   "mm",    250,     353,  0.7082152975,          1.412,   "250:353",
                 "B5",            "B",    5L,              "ISO",   "mm",    176,     250,         0.704,    1.420454545,   "176:250",
                 "B6",            "B",    6L,              "ISO",   "mm",    125,     176,  0.7102272727,          1.408,   "125:176",
                 "B7",            "B",    7L,              "ISO",   "mm",     88,     125,         0.704,    1.420454545,    "88:125",
                 "B8",            "B",    8L,              "ISO",   "mm",     62,      88,  0.7045454545,    1.419354839,     "62:88",
                 "B9",            "B",    9L,              "ISO",   "mm",     44,      62,  0.7096774194,    1.409090909,     "44:62",
                "B10",            "B",   10L,              "ISO",   "mm",     31,      44,  0.7045454545,    1.419354839,     "31:44",
                 "C0",            "C",    0L,              "ISO",   "mm",    917,    1297,  0.7070161912,    1.414394766,  "917:1297",
                 "C1",            "C",    1L,              "ISO",   "mm",    648,     917,  0.7066521265,    1.415123457,   "648:917",
                 "C2",            "C",    2L,              "ISO",   "mm",    458,     648,  0.7067901235,    1.414847162,   "458:648",
                 "C3",            "C",    3L,              "ISO",   "mm",    324,     458,  0.7074235808,    1.413580247,   "324:458",
                 "C4",            "C",    4L,              "ISO",   "mm",    229,     324,  0.7067901235,    1.414847162,   "229:324",
                 "C5",            "C",    5L,              "ISO",   "mm",    162,     229,  0.7074235808,    1.413580247,   "162:229",
                 "C6",            "C",    6L,              "ISO",   "mm",    114,     162,  0.7037037037,    1.421052632,   "114:162",
                 "C7",            "C",    7L,              "ISO",   "mm",     81,     114,  0.7105263158,    1.407407407,    "81:114",
                 "C8",            "C",    8L,              "ISO",   "mm",     57,      81,  0.7037037037,    1.421052632,     "57:81",
                 "C9",            "C",    9L,              "ISO",   "mm",     40,      47,  0.8510638298,          1.175,     "40:47",
                "C10",            "C",   10L,              "ISO",   "mm",     28,      28,             1,              1,     "28:28",
             "Albert",             NA,    NA, "British Imperial",   "in",      4,       6,  0.6666666667,            1.5,       "4:6",
            "Duchess",             NA,    NA, "British Imperial",   "in",    4.5,       6,          0.75,    1.333333333,     "4.5:6",
               "Duke",             NA,    NA, "British Imperial",   "in",    5.5,       7,  0.7857142857,    1.272727273,     "5.5:7",
    "Foolscap Quarto",             NA,    NA, "British Imperial",   "in",    6.5,       8,        0.8125,    1.230769231,     "6.5:8",
     "Foolscap Folio",             NA,    NA, "British Imperial",   "in",      8,      13,  0.6153846154,          1.625,      "8:13",
  "Small Post Octavo",             NA,    NA, "British Imperial",   "in",    4.5,       7,  0.6428571429,    1.555555556,     "4.5:7",
  "Small Post Quarto",             NA,    NA, "British Imperial",   "in",      7,       9,  0.7777777778,    1.285714286,       "7:9",
  "Large Post Octavo",             NA,    NA, "British Imperial",   "in",      5,       8,         0.625,            1.6,       "5:8",
  "Large Post Quarto",             NA,    NA, "British Imperial",   "in",     10,      10,             1,              1,     "10:10",
                  "0",             NA,    0L,              "JIS",   "mm",   1030,    1456,  0.7074175824,    1.413592233, "1030:1456",
                  "1",             NA,    1L,              "JIS",   "mm",    728,    1030,  0.7067961165,    1.414835165,  "728:1030",
                  "2",             NA,    2L,              "JIS",   "mm",    515,     728,  0.7074175824,    1.413592233,   "515:728",
                  "3",             NA,    3L,              "JIS",   "mm",    364,     515,  0.7067961165,    1.414835165,   "364:515",
                  "4",             NA,    4L,              "JIS",   "mm",    257,     364,   0.706043956,    1.416342412,   "257:364",
      "4 Shiroku ban",             NA,    4L,              "JIS",   "mm",    264,     379,  0.6965699208,    1.435606061,   "264:379",
             "4 Kiku",             NA,    4L,              "JIS",   "mm",    227,     306,  0.7418300654,    1.348017621,   "227:306",
                  "5",             NA,    5L,              "JIS",   "mm",    182,     257,  0.7081712062,    1.412087912,   "182:257",
      "5 Shiroku ban",             NA,    5L,              "JIS",   "mm",    189,     262,  0.7213740458,    1.386243386,   "189:262",
             "5 Kiku",             NA,    5L,              "JIS",   "mm",    151,     227,  0.6651982379,    1.503311258,   "151:227",
                  "6",             NA,    6L,              "JIS",   "mm",    128,     182,  0.7032967033,       1.421875,   "128:182",
      "6 Shiroku ban",             NA,    6L,              "JIS",   "mm",    127,     188,  0.6755319149,    1.480314961,   "127:188",
                  "7",             NA,    7L,              "JIS",   "mm",     91,     128,     0.7109375,    1.406593407,    "91:128",
                  "8",             NA,    8L,              "JIS",   "mm",     64,      91,  0.7032967033,       1.421875,     "64:91",
                  "9",             NA,    9L,              "JIS",   "mm",     45,      64,      0.703125,    1.422222222,     "45:64",
                 "10",             NA,   10L,              "JIS",   "mm",     32,      45,  0.7111111111,        1.40625,     "32:45",
                 "11",             NA,   11L,              "JIS",   "mm",     22,      32,        0.6875,    1.454545455,     "22:32",
                 "12",             NA,   12L,              "JIS",   "mm",     16,      22,  0.7272727273,          1.375,     "16:22"
  )


usethis::use_data(paper_sizes, overwrite = TRUE)

osm_building_tags <-
  osmdata::available_tags("building")

osm_building_tags <-
  stringr::str_remove(osm_building_tags, "(?<=&)[:graph:]+") |>
  stringr::str_remove("&")

usethis::use_data(osm_building_tags, internal = TRUE, overwrite = TRUE)

