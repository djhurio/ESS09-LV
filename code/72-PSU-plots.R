# ESS9-LV PSU plots

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages ####
require(data.table)
require(leaflet)


# Reset ####
rm(list = ls())
gc()


# Load sample ####

load("results/sample_majo_2.Rdata")
sample_majo_2[psu == min(psu)]

sample_majo_2[, popup_label := paste0("<b>", idno, "</b>; ", adrese)]

map_data <- sample_majo_2[, .(n = .N, popup_label = paste(popup_label,
                                                          collapse = "<br>")),
                          keyby = .(psu, adr_kods_eka, lon, lat)]

map_data

anyDuplicated(map_data, by = "adr_kods_eka")

m <- leaflet(data = map_data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~popup_label,
                   radius = ~10 * n, stroke = T,
                   clusterOptions = markerClusterOptions())
m  # Print the map
