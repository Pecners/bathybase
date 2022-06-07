
dir <- "data/BathybaseDb"

f <- list.files(dir)
ff <- list.files(glue("{dir}/{f[1]}"))

j <- jsonlite::fromJSON(glue("{dir}/{f[1]}/{ff[1]}/info.json"))

all_lakes <- map_df(1:length(f), function(i) {
  ff <- list.files(glue("{dir}/{f[i]}"))
  
  map_df(1:length(ff), function(k) {
    j <- jsonlite::fromJSON(glue("{dir}/{f[i]}/{ff[k]}/info.json"))
    
    tibble(
      name = j$Name,
      lat = j$Lat,
      long = j$Lon,
      mean_depth = j$`Mean Depth`,
      max_depth = j$`Max Depth`,
      volume = j$Volume,
      main_dir = f[i],
      sub_dir = ff[k],
      source = j$Source
    )
  })
})
