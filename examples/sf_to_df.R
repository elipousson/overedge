nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
nc_df <- ggspatial::df_spatial(nc)
df_to_sf(nc_df, coords = c("x", "y"))
nc_df$xy <- paste(nc_df$x, nc_df$y, sep = ",")

df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
