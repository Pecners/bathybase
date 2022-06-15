library(tidyverse)
library(terra)
library(rayshader)
library(MetBrewer)
library(scico)
library(magick)
library(glue)
library(sf)
library(tigris)
library(jsonlite)

d <- rast("data/BathybaseDb/100-199/100/bathy.tiff")
lake_info <- fromJSON("data/BathybaseDb/100-199/100/info.json")
max_depth <- lake_info$`Max Depth` * 3.281
volume <- lake_info$Volume / 1233

dd <- raster_to_matrix(d)

small <- rayshader::resize_matrix(dd, .25)

colors <- rev(met.brewer("Manet"))
colors <- scico(n = 10, palette = "romaO")
colors <- c("#ffffff", "#C5002B")

hm <- abs(dd)

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

if (min(c(wr, hr)) < .5) {
  if (wr < .5) {
    wr <- .5
  } else {
    hr <- .5
  }
}

rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = .2,
          windowsize = c(800*wr, 800*hr),
          shadowwidth = 100, shadowcolor = colors[1],
          phi = 90, zoom = .7, theta = 0, background = "white")

render_camera(phi = 90, theta = 0, zoom = .8, fov = 0)


render_highquality(
  "mendota_badgers.png", parallel = TRUE, 
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "env/phalzer_forest_01_4k.hdr",
  intensity_env = 1, samples = 300,
  rotate_env = 100, 
  width = round(6000 * wr), height = round(6000 * hr)
)


# Hiro palette
lab <- "badger"

text_color <- colors[2]

img <- image_read(glue("mendota_badgers.png"))

# Title
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 125, gravity = "north",
                       location = "-1700+150")
# Subtitle
img_ <- image_annotate(img_, "Lake Mendota", weight = 700, 
                       font = "Cinzel Decorative", location = "-1700+350",
                       color = text_color, size = 200, gravity = "north")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from Wisconsin DNR", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

# Max Depth
img_ <- image_annotate(img_, glue("Max Depth: {round(max_depth)} ft"),
                       font = "Cinzel Decorative", location = "-1700+700",
                       color = text_color, size = 100, gravity = "north")

# Volume
img_ <- image_annotate(img_, glue("Volume: {round((volume)/1000, 1)}K ac-ft"), 
                       font = "Cinzel Decorative", location = "-1700+900",
                       color = text_color, size = 100, gravity = "north")



states <- spData::us_states 

b <- st_point(c(lake_info$Lon, lake_info$Lat)) %>%
  st_sfc(crs = 4326) %>% 
  st_buffer(dist = 100000)

loc_plot <- ggplot() + 
  geom_sf(data = states, fill = "transparent", color = text_color, size = 0.1) + 
  geom_sf(data = b, fill = NA, color = text_color) +
  theme_void() + 
  coord_sf(crs = 3347) 

ggsave(loc_plot, filename = glue("mendota_inset.png"), w = 4, h = 3)

inset <- image_read(glue("mendota_inset.png"))
new_inset <- image_scale(inset, "x1200")

img_mosaic <- image_composite(img_, new_inset, gravity = "north",
                              offset = "-1700+1050")

magick::image_write(img_mosaic, glue("mendota_titled_badgers.png"))

smimage <- image_scale(img_mosaic, "x3000")
image_write(smimage, "mendota_titled_badgers_small.png")
