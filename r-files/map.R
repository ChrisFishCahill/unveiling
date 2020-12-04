# install this version of ggalt that Elio Campitelli figured out cleverly:
# devtools::install_github("eliocamp/ggalt@new-coord-proj") #ggalt for TTM projection 
# devtools::install_github("ropensci/rnaturalearthhires") # provinces
# devtools::install_github("seananderson/ggsidekick") # theme_sleek()

library(ggplot2)
theme_set(theme_light())
library(tidyr)
library(INLA)
library(dplyr)
library(grid)
library(gridExtra)
library(ggalt)
library(ggpubr)
library(ggforce)
library(rnaturalearth)
library(raster)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(maps)
library(maptools)
library(grid)

# devtools::source_url("https://raw.githubusercontent.com/ChrisFishCahill/fish-map/master/alberta-map.R")
# alberta_map(data = data, filename = "analysis2/Fig_1_map")

# data must have "Long_c" and "Lat_c" to match ggplot--> locations to plot
# alberta_map(data=data, filename="map") --> prints a saves resolution "map.png"

alberta_map <- function(data = data, filename = filename, not_surveyed=not_surveyed) {
  canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
  provinces <- ne_states(country = "canada", returnclass = "sf")
  alberta <- provinces %>% filter(name %in% "Alberta")
  
  inset <- ggplot(data = provinces) +
    geom_sf(fill = "white", colour = "black") +
    geom_sf(data = alberta, fill = "gray66") +
    ggsidekick::theme_sleek() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      panel.border = element_blank()
    ) +
    coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77
    +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +
    y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # inset
  
  provinces <- ne_states(country = "canada", returnclass = "sp")
  alberta <- provinces[provinces$name %in% "Alberta", ]
  
  alta <- spTransform(
    alberta,
    CRS("+proj=longlat +datum=WGS84")
  )
  
  alta.fort <- fortify(alberta)
  
  names(alta.fort)[1] <- "Long_c"
  names(alta.fort)[2] <- "Lat_c"
  
  # library(googleway)
  # require(ggmap)
  # register_google(apiKey)
  # set_key(key = apiKey)
  # cities <- data_frame(cities = c(
  #   "Edmonton, alta", "Calgary, alta")) %>%
  #   mutate_geocode(cities)
  
  cities <- tibble::tibble(
    cities = c("Edmonton", "Calgary"),
    Long_c = c(-113.4938, -114.0719),
    Lat_c = c(53.54612, 51.04473)
  )
  
  study_map <- ggplot(NULL) +
    geom_polygon(
      colour = "black", fill = "white", data = alta.fort,
      aes(x = Long_c, y = Lat_c, group = id)
    ) +
    geom_point(pch = 21, size = 1.5, data = data, aes(Long_c, Lat_c), fill = "black") +
    geom_point(pch = 24, size = 3, data = cities, aes(Long_c, Lat_c), fill = "steelblue") + 
    geom_point(pch = 1, size = 1.5, data = not_surveyed, aes(Long_c, Lat_c)) +
    scale_x_continuous(breaks = c(-120, -115, -110)) +
    scale_y_continuous(breaks = c(49, 52, 56, 60)) +
    annotate("text", x = cities$Long_c[1], y=cities$Lat_c[1]+0.25, label = "Edmonton") +
    annotate("text", x = cities$Long_c[2], y=cities$Lat_c[2]+0.25, label = "Calgary") +
    ylab("Latitude") +
    xlab("Longitude") +
    ggsidekick::theme_sleek() +
    theme(
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      panel.border = element_blank(), 
      axis.ticks = element_blank()
    ) +
    ggalt::coord_proj(
      paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
    ) 
  # study_map
  study_map <- study_map + scale_bar(
    lon = -120, lat = 51.1, distance_lon = 100,
    distance_lat = 10, distance_legend = 50, dist_unit = "km", orientation = F#, orientation = TRUE,
    #arrow_length = 55, arrow_distance = 85, arrow_north_size = 0.75
  )
  # study_map
  rotation <- 15
  #png(filename = paste0(filename, ".png"), 
  #    height = 11, width = 7, units = "in", res = 2000)
  pdf(file = paste0(filename, ".pdf"),
     height = 11, width = 7)
  vp_inset <- grid::viewport(angle=rotation, width = 0.3, height = 0.3, 
                             x = 0.14, y = -0.06, just = c("left", "bottom"))
  print(study_map)
  print(inset, vp = vp_inset)
  dev.off()
}

# Extra mapping stuff for scalebar and arrow:
# minor changes from devtools::install_github("3wen/legendMap")
create_scale_bar <- function(lon, lat, distance_lon, distance_lat,
                             distance_legend, dist_units = "km") {
  # First rectangle
  bottom_right <- gcDestination(
    lon = lon, lat = lat, bearing = 90,
    dist = distance_lon, dist.units = dist_units, model = "WGS84"
  )
  
  topLeft <- gcDestination(
    lon = lon, lat = lat, bearing = 0,
    dist = distance_lat, dist.units = dist_units, model = "WGS84"
  )
  rectangle <- cbind(
    lon = c(lon, lon, bottom_right[1, "long"], bottom_right[1, "long"], lon),
    lat = c(lat, topLeft[1, "lat"], topLeft[1, "lat"], lat, lat)
  )
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(
    lon = lon, lat = lat, bearing = 90,
    dist = distance_lon * 2, dist.units = dist_units, model = "WGS84"
  )
  rectangle2 <- cbind(
    lon = c(
      bottom_right[1, "long"], bottom_right[1, "long"],
      bottom_right2[1, "long"], bottom_right2[1, "long"],
      bottom_right[1, "long"]
    ), lat = c(
      lat, topLeft[1, "lat"],
      topLeft[1, "lat"], lat, lat
    )
  )
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(
    lon = lon, lat = lat, bearing = 0,
    dist = distance_legend, dist.units = dist_units, model = "WGS84"
  )
  on_top2 <- on_top3 <- on_top
  on_top2[1, "long"] <- bottom_right[1, "long"]
  on_top3[1, "long"] <- bottom_right2[1, "long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend,
                             text = c(0, distance_lon, distance_lon * 2)
  ),
  stringsAsFactors = FALSE, row.names = NULL
  )
  return(list(
    rectangle = rectangle,
    rectangle2 = rectangle2,
    legend = legend
  ))
}

create_orientation_arrow <- function(scale_bar, length,
                                     distance = 1, dist_units = "km") {
  lon <- scale_bar$rectangle2[1, 1]
  lat <- scale_bar$rectangle2[1, 2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(
    lon = lon, lat = lat,
    bearing = 0, dist = distance, dist.units = dist_units,
    model = "WGS84"
  )
  lon <- beg_point[1, "long"]
  lat <- beg_point[1, "lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(
    lon = lon, lat = lat, bearing = 0,
    dist = length, dist.units = dist_units, model = "WGS84"
  )
  
  left_arrow <- gcDestination(
    lon = on_top[1, "long"],
    lat = on_top[1, "lat"], bearing = 225, dist = length / 5,
    dist.units = dist_units, model = "WGS84"
  )
  
  right_arrow <- gcDestination(
    lon = on_top[1, "long"],
    lat = on_top[1, "lat"], bearing = 135, dist = length / 5,
    dist.units = dist_units, model = "WGS84"
  )
  
  res <- rbind(
    cbind(
      x = lon, y = lat, xend = on_top[1, "long"],
      yend = on_top[1, "lat"]
    ),
    cbind(
      x = left_arrow[1, "long"], y = left_arrow[1, "lat"],
      xend = on_top[1, "long"], yend = on_top[1, "lat"]
    ),
    cbind(
      x = right_arrow[1, "long"], y = right_arrow[1, "lat"],
      xend = on_top[1, "long"], yend = on_top[1, "lat"]
    )
  )
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1, "lat"]) / 2)
  
  return(list(res = res, coords_n = coords_n))
}

scale_bar <- function(lon, lat, distance_lon, distance_lat,
                      distance_legend, dist_unit = "km", rec_fill = "white",
                      rec_colour = "black", rec2_fill = "black", rec2_colour = "black",
                      legend_colour = "black", legend_size = 3, orientation = TRUE,
                      arrow_length = 500, arrow_distance = 300, arrow_north_size = 6) {
  the_scale_bar <- create_scale_bar(
    lon = lon, lat = lat,
    distance_lon = distance_lon, distance_lat = distance_lat,
    distance_legend = distance_legend, dist_unit = dist_unit
  )
  # First rectangle
  rectangle1 <- geom_polygon(
    data = the_scale_bar$rectangle,
    aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour
  )
  
  # Second rectangle
  rectangle2 <- geom_polygon(
    data = the_scale_bar$rectangle2,
    aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour
  )
  
  # Legend
  scale_bar_legend <- annotate("text",
                               label = paste(the_scale_bar$legend[, "text"], dist_unit, sep = ""),
                               x = the_scale_bar$legend[, "long"], y = the_scale_bar$legend[, "lat"],
                               size = legend_size, colour = legend_colour
  )
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if (orientation) { # Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(
      scale_bar = the_scale_bar,
      length = arrow_length, distance = arrow_distance, dist_unit = dist_unit
    )
    arrow <- list(geom_segment(
      lineend = "butt", linejoin = "mitre",
      size = arrow_north_size, data = coords_arrow$res,
      aes(x = x, y = y, xend = xend, yend = yend + 0.1)
    ))
    res <- c(res, arrow)
  }
  return(res)
}
