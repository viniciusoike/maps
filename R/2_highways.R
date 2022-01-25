# Classificar o nome das ruas de Poa #

library(osmdata)
library(sf)
library(geobr)
library(ggtext)
library(tidyverse)



# Import Data -------------------------------------------------------------

## geobr -------------------------------------------------------------------

# City border
poa <- read_municipality(4314902)
# City neighborhoods
nbs <- read_neighborhood()
nbspoa <- filter(nbs, code_muni == 4314902)


## osmdata -----------------------------------------------------------------

# Define bbox
bbox <- getbb("Porto Alegre Brazil")
# Base query
qr <- opq(bbox)



# Add feature requests to query

# All roads
qr_roads <- add_osm_feature(qr, key = "highway")
# Only big roads
qr_big_streets <- add_osm_feature(
  qr,
  key = "highway",
  value = c("motorway", "primary", "motorway_link", "primary_link")
)
# Only medium roads
qr_med_streets <- add_osm_feature(
  qr,
  key = "highway",
  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")
)
# Only small roads
qr_small_streets <- add_osm_feature(
  qr,
  key = "highway",
  value = c("residential", "living_street", "unclassified", "service",
            "footway")
)
# Download
roads <- osmdata_sf(q = qr_roads)
big_streets <- osmdata_sf(q = qr_big_streets)
med_streets <- osmdata_sf(q = qr_med_streets)
small_streets <- osmdata_sf(q = qr_small_streets)

snames <- roads$osm_lines$name

# Remove duplicate names
snames <- unique(snames)

# Remove missing values
snames <- na.omit(snames)

#snames[str_detect(snames, "(Rua [0-9])|(Beco [0-9])|(Travessa [0-9])|(Via [0-9])|(Viela [0-9])")]
#snames[str_detect(snames, "Profess")]

# Convert to tibble
dictionary <- tibble(
  street_name = snames
)

# Get names of all cities and states in Brazil
muni <- read_municipality()
state <- read_state()
geonames <- c(muni$name_muni, state$name_state)
geostring <- paste(geonames, collapse = "|")

# Common titles for army position, liberal professions, and religious
# personalities
posto_exercito <- c("Marechal", "Almirante", "General", "Comandante", "Coronel",
                    "Cabo", "Capitão", "(Castello Branco)", "(Costa e Silva)",
                    "(Ernesto Geisel)")

profissao <- c("Engenheir", "Doutor", "Profess", "Desembargador")

santidade <- c("Frei", "Santo", "Santa", "São", "Padre")

posto_exercito <- paste(posto_exercito, collapse = "|")
profissao <- paste(profissao, collapse = "|")
santidade <- paste(santidade, collapse = "|")
# Proxy for closed condominiums / favelas
rua_sem_nome <- c(
  "(Rua [0-9])|(Beco [0-9])|(Travessa [0-9])|(Via [0-9])|(Viela [0-9])|(^Acesso)|(Rua [A-Z] )")

dictionary <- dictionary %>%
  mutate(
    class = case_when(
      str_detect(street_name, posto_exercito) ~ "Exército",
      str_detect(street_name, profissao) ~ "Profissão",
      str_detect(street_name, geostring) ~ "Nome de Cidade/UF",
      str_detect(street_name, santidade) ~ "Figura Religiosa",
      str_detect(street_name, "[0-9] de") ~ "Feriado",
      str_detect(street_name, rua_sem_nome) ~ "Rua sem nome",
      str_count(street_name, "\\w+") > 2 ~ "Personalidade Histórica",
      str_count(street_name, "\\w+") <= 2 ~ "Coisa",
      TRUE ~ "Outro"
    )
  )

s1 <- big_streets$osm_lines %>%
  st_transform(crs = 4674) %>%
  left_join(dictionary, by = c("name" = "street_name"))

s2 <- med_streets$osm_lines %>%
  st_transform(crs = 4674) %>%
  left_join(dictionary, by = c("name" = "street_name"))

s3 <- small_streets$osm_lines %>%
  st_transform(crs = 4674) %>%
  left_join(dictionary, by = c("name" = "street_name"))


sub <- "
<p style='line-height:0.8'> A maior parte das ruas (~38%) têm nome de personalidades históricas (e.g. <span style='color: #fdb462'><strong>Rua Vicente da Fontoura</strong></span>) ou de outras cidades/UFs (~28%) (e.g. <span style='color: #80b1d3'><strong>Av. Bento Goncalves</strong></span>).<br/>
H&aacute; 208 ruas nomeadas em homenagem a membros do ex&eacute;rcito (e.g. <span style='color: #ffffb3;'><strong>Av. Presidente Castelo Branco</strong></span>).<br/>
As ruas sem nome (e.g. <span style='color: #e78ac3;'><strong>Beco A<strong></span>, <span style='color: #e78ac3;'><strong>Rua 2<strong></span>) majoritariamente se concentram em aglomerados subnormais.<br/>
A categoria profiss&atilde;o re&uacute;ne as ruas que usam um t&iacute;tulo (e.g. <span style='color: #b3de69';><strong>Doutor Flores<strong></span>). H&aacute; 103 ruas com nomes de professores (e.g. <span style='color: #b3de69';><strong>Rua Professor Fitzgerald<strong></span>).
<br/></p>"


p1 <- ggplot() +
  geom_sf(
    data = s1,
    aes(color = class),
    key_glyph = draw_key_rect
  ) +
  geom_sf(
    data = s2,
    aes(color = class),
    key_glyph = draw_key_rect,
    size = 0.55
  ) +
  geom_sf(
    data = s3,
    aes(color = class),
    key_glyph = draw_key_rect,
    size = 0.4
  ) +
  coord_sf(
    ylim = c(-30.07, -30),
    xlim = c(-51.245, -51.135)
  ) +
  scale_color_brewer(
    name = "",
    type = "qual",
    palette = 8
  ) +
  scale_fill_brewer(
    name = "",
    type = "qual",
    palette = 8
  ) +
  labs(
    title = "**Nomes de Ruas em Porto Alegre**",
    subtitle = sub,
    caption = "Fonte: osmdata. Cores: ColorBrewer. Autor: @viniciusoike"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray70", colour = "gray70"),
    panel.background = element_rect(fill = "gray70", colour = "gray70"),
    legend.position = c(0.1, 0.8),
    
    plot.margin = margin(t = 1.5, b = 1, unit = "cm"),
    
    text = element_text(family = "Gill Sans", size = 8),
    
    plot.title = element_markdown(
      family = "Gill Sans",
      size = 16,
      hjust = 0.5
    ),
    
    plot.subtitle = element_markdown(
      family = "Gill Sans",
      size = 10,
      colour = "gray20"
    ),
    
    panel.border = element_rect(colour = "gray20", fill = "transparent"),
    
    legend.title = element_blank(),
    legend.background = element_rect(fill = "gray70", colour = "gray70"),
    legend.box.background = element_rect(fill = "gray70", colour = "gray70"),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 0.5, b = 0.5, unit = "cm")
  )

cowplot::save_plot(here::here("test.png"),
                   p1,
                   base_height = 10,
                   dpi = 300)

