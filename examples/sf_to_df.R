nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

nc_coords <-
  suppressWarnings(sf::st_coordinates(sf::st_centroid(nc)))

nc_df <-
  dplyr::bind_cols(sf::st_drop_geometry(nc), nc_coords)

nc_df <- janitor::clean_names(nc_df)
df_to_sf(nc_df, coords = c("x", "y"), remove_coords = TRUE)
nc_df$xy <- paste(nc_df$x, nc_df$y, sep = ",")
df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
