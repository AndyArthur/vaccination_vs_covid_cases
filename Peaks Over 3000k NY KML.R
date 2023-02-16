library(vroom)
library(tidyverse)
library(sf)
library(plotKML)

rm(list = ls())
gnis <- vroom("/media/hd2/draft.redistricting/NationalGNIS.zip")

gnis %>%
    filter(STATE_ALPHA == "NY", FEATURE_CLASS == "Summit", ELEV_IN_FT >= 3000) %>%
    arrange(-ELEV_IN_FT) %>%
    mutate(Tallest_Peak = row_number()) %>%
    st_as_sf(coords = c("PRIM_LONG_DEC", "PRIM_LAT_DEC"), crs = 4326, agr = "constant") %>%
    st_transform(26918) -> gnis


fn <- "/tmp/peaksover3000k.kml"

kml_open(fn)
kml_layer.SpatialPoints(as(gnis, Class = "Spatial"),
    subfolder.name = str_c("Highest Peaks in NYS"),
    labels = str_c(gnis$Tallest_Peak, ". ", gnis$FEATURE_NAME),
    altitude = gnis$ELEV_IN_FT + 1000,
    altitudeMode = "relativeToGround",
    html.table = str_c(
        "<h3>", gnis$FEATURE_NAME,
        '</h3> <table><tr><td><b>Tallest Peak: </b></td><td style="text-align: right"># ', gnis$Tallest_Peak, "</td></tr><tr><td>",
        '<b>Elevation: </b></td><td style="text-align: right">', scales::comma(gnis$ELEV_IN_FT), " ft</td></tr><tr><td>",
        '<b>Topographic Map: </b></td><td style="text-align: right">', gnis$MAP_NAME, "</td></tr></table>"
    ),
    outline = TRUE,
    alpha = 1,
    width = 0.2,
    fill = "black",
    colour = gnis$Tallest_Peak,
    colour_scale = terrain.colors(n = 8, rev = T)
)
kml_close(fn)

kml <- read_file(fn)

kml %>%
    str_replace("<Folder>\n", "") %>%
    str_replace_all("</Folder>\n", "") %>%
    str_replace_all('<kml xmlns:xsd="https://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" xmlns:xmlns="https://www.opengis.net/kml/2.2/" version="1.0">', "<kml>") %>%
    str_replace_all("</PolyStyle>", "</PolyStyle>\n\t\t<LineStyle>\n\t\t\t<color>#40FFFFFF</color>\n\t\t\t<width>1</width>\n\t\t</LineStyle>") %>%
    str_replace_all("https://plotkml.r-forge.r-project.org/icon3.png", "http://maps.google.com/mapfiles/kml/shapes/mountains.png") %>%
    str_replace_all("<scale>0.5</scale>", "<scale>0.8</scale>") %>%
    write(fn)

kml_compress(fn, rm = T)

unzip("~/Documents/GIS.Data/dec.lands/DEC_lands.zip", exdir = "/tmp/")
unzip("~/Documents/GIS.Data/dec.lands/DEC_roadstrails.zip", exdir = "/tmp/")


nytrails <- read_sf("/tmp/DECroadstrails.shp") %>% st_transform(26918)
nylands <- read_sf("/tmp/DecLands.shp") %>% st_transform(26918)


library(gt)
library(gtExtras)

gnis %>%
    mutate(
        State_Land = nylands[st_intersects(., nylands) %>% as.numeric(), ]$FACILITY %>% str_to_title(),
        Nearest_Trail =
            ifelse(
                st_distance(., nytrails[st_nearest_feature(., nytrails), ]) < units::set_units(0.1, "mi"),
                NA,
                nytrails[st_nearest_feature(., nytrails) %>% as.numeric(), ]$NAME %>% str_to_title()
            )
    ) %>%
    st_transform(4326) %>%
    mutate(
        Location = str_c(
            "<a href=\"https://mapper.acme.com/?ll=",
            st_coordinates(geometry)[, 2],
            ",",
            st_coordinates(geometry)[, 1],
            "&z=17&t=M&marker0=",
            st_coordinates(geometry)[, 2],
            "%2C",
            st_coordinates(geometry)[, 1],
            "\">",
            st_coordinates(geometry)[, 2],
            ",<br />",
            st_coordinates(geometry)[, 1],
            "</a>"
        ),
        Location = map(Location, gt::html)
    ) %>%
    st_drop_geometry() %>%
    arrange(Tallest_Peak) %>%
    select(Tallest_Peak, FEATURE_NAME, ELEV_IN_FT, COUNTY_NAME, MAP_NAME, State_Land, Nearest_Trail, Location) %>%
    gt() %>%
    gt::fmt_integer(3) %>%
    gtsave("/tmp/output.html")
