
# 0. Preamble -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(ggmap)
library(osmdata)
library(showtext)

font_add("Gill Sans", "GillSans.ttc")
showtext_auto()
# Import data from CETSP by INFORMSP http://www.respeitoavida.sp.gov.br/relatorios/
# http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_fatais.xlsx
# http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_naofatais.csv

cetsp <- "http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_fatais.xlsx"

download.file(cetsp, here("data", "acidentes_fatais.xlsx"), mode = "wb")

fatal <- readxl::read_excel(here("data/acidentes_fatais.xlsx"))

# Import city shapefile (for spatial join)
shp_sp <- geobr::read_municipality(3550308, simplified = F)
# 1. Data Clean -----------------------------------------------------------

## Fatal Accidents ---------------------------------------------------------

fatal <- fatal %>%
  # Clean names using janitor
  janitor::clean_names() %>%
  # Rename some variables manually
  rename(
    date = data_do_acidente,
    lat = lat_geo,
    lng = long_geo,
    climate_conditions = condicoes_climaticas_siopm,
    daytime = turno,
    victims = quantidade_de_vitimas,
  ) %>%
  select(
    date, lat, lng, victims, climate_conditions, daytime
  ) %>%
  mutate(
    date = ymd(as.Date(date)),
    # Latitude and longitude values stores with , as decimal
    lat = as.numeric(str_replace(lat, ",", ".")),
    lng = as.numeric(str_replace(lng, ",", ".")),
    # Convert daytime into ordered factor
    daytime = factor(daytime,
                     levels = c("MADRUGADA", "MANHA", "TARDE", "NOITE"),
                     labels = c("Madrugada", "Manhã", "Tarde", "Noite")),
    # Weekday and year using lubridate
    weekday = wday(date, label = T),
    year = year(date)
  )

geofatal <- fatal %>%
  filter(!is.na(lat) | !is.na(lng)) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4674) %>%
  st_join(shp_sp) %>%
  filter(!is.na(code_muni))

## OSMdata -----------------------------------------------------------------

# Define bbox
bbox <- getbb("Sao Paulo Brazil")
# Base query
qr <- opq(bbox)
# Add feature requests to query
qr_road <- add_osm_feature(qr, key = "highway",
                           value = c("motorway", "primary",
                                     "motorway_link", "primary_link"))
# Download
big_streets <- osmdata_sf(q = qr_road, quiet = FALSE)
# Add feature requests to query
qr_med_streets <- add_osm_feature(
  qr,
  key = "highway",
  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")
)
# Download
med_streets <- osmdata_sf(q = qr_med_streets, quiet = FALSE)
# Add feature requests to query
qr_small_streets <- add_osm_feature(
  qr,
  key = "highway",
  value = c("residential", "living_street", "unclassified", "service",
            "footway")
)
# Download
small_streets <- osmdata_sf(q = qr_small_streets, quiet = FALSE)

# Plot --------------------------------------------------------------------

p_fatal <- ggplot() +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "gray20") +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "gray60") +
  geom_sf(data = filter(geofatal, year == 2019),
          aes(color = victims, size = victims)) +
  scale_colour_viridis_c(
    name = "Number of\nfatal victims",
    limits = c(1, 5),
    breaks = 1:5
  ) +
  scale_size_continuous(
    name = "Number of\nfatal victims",
    limits = c(1, 5),
    breaks = 1:5
  ) +
  coord_sf(
    ylim = c(-23.7, -23.475),
    xlim = c(-46.75, -46.55)
    ) +
  labs(title = "Fatal Accidents in Sao Paulo",
       subtitle = "Total number of fatal car accidents in Sao Paulo (BR)") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, family = "Gill Sans", face = "bold", hjust=.5),
    plot.subtitle = element_text(size = 12, family = "Gill Sans"),
    legend.title = element_text(size = 8, family = "Gill Sans")
  )

ggsave(file = here("graphics", "1_road_accidents", "map.pdf"),
       p_fatal,
       units = "in",
       width = 6,
       height=7)
