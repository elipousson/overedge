path <- system.file("extdata", package = "overedge")

read_sf_exif(
  path = file.path(path, "photos"),
  filetype = "jpeg"
)
