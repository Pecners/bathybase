library(tidyverse)
library(terra)
library(rayshader)
library(MetBrewer)
library(scico)
library(magick)
library(glue)
library(jsonlite)
library(scales)

lake_info <- fromJSON("data/BathybaseDb/1-99/2/info.json")
max_depth <- lake_info$`Max Depth` * 3.281
volume <- lake_info$Volume / 1233

d <- rast("data/BathybaseDb/1-99/2/bathy.tiff")
dd <- raster_to_matrix(d)

small <- rayshader::resize_matrix(dd, .25)

pal <- "bilbao"

colors <- rev(met.brewer("Benedictus"))
colors <- scico(n = 10, palette = pal)

hm <- dd

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}


rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(rev(colors))(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, 
          windowsize = c(800*wr, 800*hr), shadowdepth = -300,
          shadowwidth = 100, shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white")

#render_camera(phi = 90, theta = 0, zoom = 1, fov = 0)


render_highquality(
  glue("tahoe_{pal}.png"), parallel = TRUE, 
  samples = 300,
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "env/phalzer_forest_01_4k.hdr",
  intensity_env = 2,
  rotate_env = 100, 
  width = round(6000 * wr), height = round(6000 * hr)
)


add_stuff <- function(lab, lake, c, c_fun, t) {
  if (c_fun == "scico") {
    colors <- scico(n = 10, palette = lab)
  } else {
    colors <- rev(met.brewer(lab))
  }
  
  text_color <- colors[t]
  
  img <- image_read(glue("tahoe_{lab}.png"))
  
  # Title
  img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                         color = text_color, size = 125, gravity = "north",
                         location = "-1400+150")
  # Subtitle
  img_ <- image_annotate(img_, "Lake Tahoe", weight = 700, 
                         font = "Cinzel Decorative", location = "-1400+325",
                         color = text_color, size = 200, gravity = "north")
  
  # Caption
  img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from USGS", 
                         font = "Cinzel Decorative", location = "+0+50",
                         color = alpha(text_color, .5), size = 75, gravity = "south")
  
  # Max Depth
  img_ <- image_annotate(img_, glue("Max Depth: {label_comma()(round(max_depth))} ft"),
                         font = "Cinzel Decorative", location = "-1400+650",
                         color = text_color, size = 90, gravity = "north")
  
  # Volume
  img_ <- image_annotate(img_, glue("Volume: {round((volume)/1000000, 1)}M ac-ft"), 
                         font = "Cinzel Decorative", location = "-1400+850",
                         color = text_color, size = 90, gravity = "north")
  
  
  
  states <- spData::us_states 
  
  b <- st_point(c(lake_info$Lon, lake_info$Lat)) %>%
    st_sfc(crs = 4326) %>% 
    st_buffer(dist = 100000)
  
  loc_plot <- ggplot() + 
    geom_sf(data = states, fill = "white", color = text_color, size = 0.1) + 
    geom_sf(data = b, fill = NA, color = colors[c]) +
    theme_void() + 
    coord_sf(crs = 3347)
  
  ggsave(loc_plot, filename = glue("{lake}_inset.png"), w = 4, h = 3)
  
  inset <- image_read(glue("{lake}_inset.png"))
  new_inset <- image_scale(inset, "x50")
  
  img_mosaic <- image_composite(img_, inset, gravity = "north",
                                offset = "-1400+1050")
  
  magick::image_write(img_mosaic, glue("{lake}_titled_{lab}.png"))
}

add_stuff(lab = "lapaz", lake = "tahoe", c = 8, c_fun = "scico")
add_stuff(lab = "Benedictus", lake = "tahoe", c = length(colors), c_fun = "mb")  
add_stuff(lab = "nuuk", lake = "tahoe", c = 3, c_fun = "scico")  
add_stuff(lab = "bilbao", lake = "tahoe", c = 7, c_fun = "scico", t = 10)  

