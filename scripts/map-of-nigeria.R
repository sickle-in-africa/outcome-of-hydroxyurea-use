library(cartography)
library(sp)
library(spData)
library(sf)
library(tidyverse)

source('./scripts/modules/importing.R')
media_directory_path <- '/home/jack/computer/genemap/hydroxyurea-study/media/publication-ready-plots/'

nigeria_plot <- function() {

    get_study_data() -> study_data

    study_data %>% group_by(hosp_name) %>% summarise(count=n()) -> hospital_counts

    epsilon <- 0.0

    data.frame(
        id=c("Jo",      "Am",      "Ir",   "Gw", "Ab"),
        lon=c(8.89794, 8.530365,   6.219014,  7.085914,     7.489297), 
        lat=c(9.917513, 11.991867, 6.739321,  8.936026,     9.064331),
        VAL=c(34,        4,        15,        60,           7),
        hosp_name=c(
            "Jos University Teaching Hospital",
            "Aminu Kano Teaching Hospital",
            "Irua Specialist Teaching Hospital",
            "University of Abuja Teaching Hospital, Gwagwalada ",
            "National Hospital  Abuja")) %>%
        as_tibble -> hospital_locations

        merge(hospital_counts, hospital_locations, by="hosp_name") %>%
            mutate(label_lon = lon - epsilon) %>%
            mutate(label_lat = lat + epsilon) %>%
            pivot_longer(cols=c(lon,label_lon), names_to="label_or_point",values_to="lon") -> 
            hospital_data

    hsf <- st_as_sf(hospital_data, coords=c('lon', 'lat'))
    st_crs(hsf) <- 4326

    data(world)

    africa <- world[world$continent == "Africa",]
    nigeria <- world[world$name_long == "Nigeria",]

    png(paste(media_directory_path,"map-of-nigeria.png"), width = 8, height = 8, units = 'in', res = 300)

    plot(st_geometry(nigeria), expandBB = c(0, 0.1, 0.1, 0.1), border = NA, col = NA, bg = "#A6CAE0")
    plot(st_geometry(africa), col = "#E3DEBF", border = NA, add = TRUE)
    plot(st_geometry(nigeria), col = "#D1914D", border = "grey80", add=TRUE)

    # Add circles proportional to the total population
    propSymbolsLayer(
        x = hsf[hsf$label_or_point=="lon",], 
        var = "count", 
        inches = 0.4,
        col = "brown4",
        legend.pos = "bottomright",  
        legend.title.txt = "Number of patients")

    labelLayer(
        x = hsf[hsf$label_or_point=="label_lon",], 
        txt = "id", 
        cex = 0.8,
        overlap=FALSE)
    dev.off()
}