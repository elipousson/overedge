nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
nc_crs <- sf::st_crs(nc)
get_area(nc[1:2,])$area
get_area(nc[1:2,], units = "acres")$area
get_area(nc[1:2,], units = "acres", .id = "acreage")$acreage

get_dist(nc[1,], to = c("xmax", "ymax"), units = "mile")$dist
get_dist(nc[1,], to = nc[30,], units = "km")$dist

nc_line <- as_line(c(as_point(nc[1,]), as_point(nc[30,])), crs = nc_crs)

get_length(nc_line)
get_bearing(nc_line)
