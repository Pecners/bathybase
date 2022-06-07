library(tidyverse)
library(terra)
library(rayshader)
library(MetBrewer)
library(scico)
library(magick)
library(glue)
library(sf)
library(tigris)

d <- rast("data/BathybaseDb/400-499/478/bathy.tiff")
j <- jsonlite::fromJSON("data/BathybaseDb/400-499/478/info.json")

outline <- as.data.frame(d, xy = TRUE)
out_sf <- outline %>%
  st_as_sf(coords = c("x", "y"), crs = crs(d)) %>%
  st_convex_hull() %>%
  summarise(do_union = FALSE) %>%
  st_cast(to = "POLYGON")

dd <- raster_to_matrix(d)

small <- rayshader::resize_matrix(dd, .25)

colors <- rev(met.brewer("Hiroshige"))
colors <- scico(n = 10, palette = "bamako")

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
  plot_3d(heightmap = hm, solid = FALSE, 
          windowsize = c(800*wr, 800*hr), shadowdepth = -100,
          shadowwidth = 100, shadowcolor = colors[1],
          phi = 90, zoom = .7, theta = 0, background = "white")

render_camera(phi = 90, theta = 0, zoom = .5, fov = 0)


render_highquality(
  "loon_steep_hiro.png", parallel = TRUE, 
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "env/phalzer_forest_01_4k.hdr",
  intensity_env = 2,
  rotate_env = 100, 
  width = round(6000 * wr), height = round(6000 * hr)
)


# Hiro palette
lab <- "hiro"

colors <- rev(met.brewer("Hiroshige"))
text_color <- colors[1]

img <- image_read(glue("loon_steep_{lab}.png"))

# Title
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 125, gravity = "north",
                       location = "-2200+150")
# Subtitle
img_ <- image_annotate(img_, "Loon Lake", weight = 700, 
                       font = "Cinzel Decorative", location = "-2200+300",
                       color = text_color, size = 200, gravity = "north")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from Minnesota DNR", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 50, gravity = "south")

# Max Depth
img_ <- image_annotate(img_, glue("Max Depth: {round(214.9)} ft"),
                       font = "Cinzel Decorative", location = "-2200+700",
                       color = text_color, size = 100, gravity = "north")

# Volume
img_ <- image_annotate(img_, glue("Volume: {round((309921500 * 7.481)/1000000000, 1)}B gal"), 
                       font = "Cinzel Decorative", location = "-2200+900",
                       color = text_color, size = 100, gravity = "north")



counties <- tigris::counties(state = "MN") %>%
  st_transform(crs = st_crs(out_sf))

states <- spData::us_states 

b <- st_bbox(out_sf) %>%
  st_as_sfc() %>% 
  st_buffer(dist = 100000)

loc_plot <- ggplot() + 
  geom_sf(data = states, fill = "white", color = text_color, size = 0.1) + 
  geom_sf(data = b, fill = NA, color = colors[length(colors)]) +
  theme_void() + 
  coord_sf(crs = 3347)

ggsave(loc_plot, filename = "inset.png", w = 4, h = 3)

inset <- image_read("inset.png")
new_inset <- image_scale(inset, "x50")

img_mosaic <- image_composite(img_, inset, gravity = "north",
                              offset = "-800+250")
print(img_mosaic)

magick::image_write(img_mosaic, glue("loon_titled_{lab}.png"))
s