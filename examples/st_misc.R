   library(ggplot2)

   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
   nc_rotated <- st_scale_rotate(nc, scale = 0.5, rotate = 15)

   ggplot() +
     geom_sf(data = nc) +
     geom_sf(data = nc_rotated, fill = NA, color = "red")
