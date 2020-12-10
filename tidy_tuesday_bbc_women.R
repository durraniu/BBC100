
# Load Libraries ----------------------------------------------------------

library(tidyverse)
# library(RCurl)
library(magick)
library(tidygeocoder)
library(leaflet)
library(htmltools)
# library(leafpop)
library(echarts4r)
library(echarts4r.assets)
library(parallaxr)
library(highcharter)




# Read Data ---------------------------------------------------------------

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')


women <-  women %>% tail(-1)




# Find Coordinates of Countries -------------------------------------------

women <- women %>%
  geocode(country, method = "osm")



write_csv(x = women, path = "women_new.csv")


# Leaflet map -------------------------------------------------------------

pops <- purrr::map(.x = seq(nrow(women)),
                   .f = function(i) {
                     paste0("<figure>", "<img src = ", women[i, "img"], 
                            ' width="150" height="150" ', ">", "<figcaption>",
                            '<p>', "Name: ", "<b>", women[i, "name"], "</b>", '<p></p>', 
                            "Role: ", women[i, "role"], '<p></p>',
                            # women[i, "role"],'</p><p>', 
                            "Country: ", women[i, "country"], '</p>',
                            "</figcaption>", "</figure>")
                   })




wmap <- leaflet(data = women) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(lng = ~long, lat = ~lat, 
                   label = purrr::map(.x = pops, .f = htmltools::HTML),
                   popup = ~htmlEscape(description),
                   labelOptions = labelOptions(noHide = FALSE, textsize = "15px"),
                   clusterOptions = markerClusterOptions())
             

htmlwidgets::saveWidget(wmap, file="wmap.html")





# Count Globe -------------------------------------------------------------

women_counts <- women %>% 
  count(country, sort = T, name = "value") %>% 
  left_join(x=., y = women %>% 
              select(country, lat, long) %>% 
              unique(), by = "country") 

cglobe <- women_counts %>% 
  e_charts(long) %>% 
  e_globe(
    # environment = ea_asset("starfield"),
    base_texture = ea_asset("world topo"),
    # height_texture = ea_asset("world topo"),
    displacementScale = 0.01
  ) %>% 
  e_bar_3d(lat, value, coord_system = "globe") %>% 
  e_visual_map(value)

htmlwidgets::saveWidget(cglobe, file = "globe.html")




# Flipping image-----------------------------

pic <- image_read("bbc100.jpg")
pic2 <- image_flop(pic)
image_write(image = pic2, path = "pic2.jpg")




# Drilldown ---------------------------------------------------------------

women_cat_cnts <- women %>% 
  group_by(category) %>% 
  summarise(counts = n()) %>% 
  ungroup()

women_cat_cnts2 <- women %>% 
  group_by(category, country) %>% 
  summarise(counts = n()) %>% 
  ungroup()

women_drilldown <- women_cat_cnts2 %>% 
  group_nest(category) %>% 
  mutate(
    id = category,
    type = "column",
    # in the drilldown we'll give the mapping via creating the columns
    data = purrr::map(data, mutate, name = country, y = counts),
    data = purrr::map(data, list_parse)
  )



bar_chart <- hchart(
  women_cat_cnts,
  "column",
  hcaes(x = category, y = counts, name = category, drilldown = category),
  name = "Number of Women",
  colorByPoint = TRUE
) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(women_drilldown)
  ) %>% 
  hc_yAxis(
    title = list(text = "Number of Women"),
    minorTickInterval = 'auto'
  ) %>% 
  hc_xAxis(
    title = ""
  )


htmlwidgets::saveWidget(bar_chart, "bar_chart.html")








##  MD files-----------------------------
all_md_str <- list.files(pattern=".md", full.names = FALSE)
# all_md_str <- all_md_str[-2] 

## Loop through each MD file, parse, and return a single tibble
md_tibble <-
  all_md_str %>%
  purrr::map_dfr(parse_md) 

## Output HTML file

generate_scroll_doc(path = "index.html",
                    inputs = md_tibble)





