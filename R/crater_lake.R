library(tidyverse)
library(terra)
library(rayshader)
library(MetBrewer)
library(scico)
library(magick)
library(glue)

d <- rast("data/BathybaseDb/1-99/1/bathy.tiff")
dd <- raster_to_matrix(d)

small <- rayshader::resize_matrix(dd, .25)

colors <- rev(met.brewer("Hiroshige"))
colors <- scico(n = 10, palette = "bamako")

hm <- dd

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, 
          windowsize = c(800*wr, 800*hr), shadowdepth = -200,
          shadowwidth = 100, shadowcolor = colors[1],
          phi = 90, zoom = .7, theta = 0, background = "white")

render_camera(phi = 90, theta = 0, zoom = 1, fov = 0)


render_highquality(
  "crater_hiro.png", parallel = TRUE, 
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "env/phalzer_forest_01_4k.hdr",
  intensity_env = 2,
  rotate_env = 100, 
  width = round(6000 * wr), height = round(6000 * hr)
)


# Batlow palette
lab <- "bamako"

colors <- scico(n = 10, palette = lab)
text_color <- colors[1]

img <- image_read(glue("crater_{lab}.png"))

# Title
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 150, gravity = "northwest",
                       location = "+500+400")
# Subtitle
img_ <- image_annotate(img_, "Crater Lake", weight = 700, 
                       font = "Cinzel Decorative", location = "+350+600",
                       color = text_color, size = 200, gravity = "northwest")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from Bathybase", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

magick::image_write(img_, glue("crater_titled_{lab}.png"))

# Hiro palette
lab <- "hiro"

colors <- rev(met.brewer("Hiroshige"))
text_color <- colors[1]

img <- image_read(glue("crater_{lab}.png"))

# Title
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 150, gravity = "northwest",
                       location = "+500+400")
# Subtitle
img_ <- image_annotate(img_, "Crater Lake", weight = 700, 
                       font = "Cinzel Decorative", location = "+350+600",
                       color = text_color, size = 200, gravity = "northwest")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from Bathybase", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

magick::image_write(img_, glue("crater_titled_{lab}.png"))


