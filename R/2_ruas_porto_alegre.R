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
muni <- filter(muni, code_state == 43)
state <- read_state()
geonames <- c(muni$name_muni, state$name_state)
geostring <- paste(geonames, collapse = "|")

# Common titles for army position, liberal professions, and religious
# personalities
posto_exercito <- c("Marechal", "Almirante", "General", "Comandante", "Coronel",
                    "Cabo", "Capitão", "Brigadeiro", "Tenente",
                    "(Castello Branco)", "(Costa e Silva)",
                    "(Ernesto Geisel)")

profissao <- c("Engenheir", "Doutor", "Profess", "Desembargador")

santidade <- c("Frei", "Santo", "Santa", "São", "Padre", "Papa")

posto_exercito <- paste(posto_exercito, collapse = "|")
profissao <- paste(profissao, collapse = "|")
santidade <- paste(santidade, collapse = "|")
# Proxy for closed condominiums / favelas

cardinais <- c("Um", "Dois", "Três", "Quatro", "Cinco", "Seis", "Sete", "Oito",
               "Nove", "Dez", "Onze", "Doze", "Treze", "Catorze", "Quatorze",
               "Quinze", "Dezesseis", "Dezesete", "Dezoito", "Dezenove", "Vinte",
               "Vinte e Um", "Vinte e Dois", "Vinte e Três", "Vinte e Quatro",
               "Vinte e Cinco", "Vinte e Seis", "Vinte e Sete", "Vinte e Oito",
               "Vinte e Nove", "Trinta", "Quarenta", "Cinquenta", "Sessenta",
               "Setenta", "Oitenta", "Noventa", "Cem")

cardinais <- paste(paste0("(Rua ", cardinais, ")"), collapse = "|")

str_detect("Acesso 2", rx_acesso)

rx_acesso <- "(^Acesso+)"
rx_alameda <- "(Alameda [0-9])|(Alameda [A-Z][0-9])|(Alameda [0-9]$)|(Alameda [0-9][0-9]$)|(Alameda [0-9][0-9][0-9]$)|(Alameda [0-9][0-9][0-9][A-Z]$)|(Alameda [A-Z]$)|(Alameda [A-Z][A-Z]$)|(Alameda [A-Z][A-Z][A-Z]$)"
rx_beco <- "(^Beco+)"
rx_caminho <- "(^Caminho+)"
rx_rua <- "(Rua [0-9])|(Rua [A-Z][0-9])|(Rua [0-9]$)|(Rua [0-9][0-9]$)|(Rua [0-9][0-9][0-9]$)|(Rua [0-9][0-9][0-9][A-Z]$)|(Rua [A-Z]$)|(Rua [A-Z][A-Z]$)|(Rua [A-Z][A-Z][A-Z]$)"
rx_travessa <- "(Travessa [0-9])|(Travessa [A-Z][0-9])|(Travessa [0-9]$)|(Travessa [0-9][0-9]$)|(Travessa [A-Z]$)|(Travessa [A-Z][A-Z]$)"
rx_avenida <- "(Avenida [0-9])|(Avenida [A-Z][0-9])|(Avenida [0-9]$)|(Avenida [0-9][0-9]$)|(Avenida [A-Z]$)|(Avenida [A-Z][A-Z]$)"
rx_via <- "(Via [0-9])|(Via [A-Z][0-9])|(Via [0-9]$)|(Via [0-9][0-9]$)|(Via [A-Z]$)|(Via [A-Z][A-Z]$)"
rx_viela <- "(Viela [0-9])|(Viela [A-Z][0-9])|(Viela [0-9]$)|(Viela [0-9][0-9]$)|(Viela [A-Z]$)|(Viela [A-Z][A-Z]$)"

rua_sem_nome <- paste(
  rx_acesso, rx_alameda, rx_beco, rx_caminho,
  rx_rua, rx_travessa, rx_avenida, rx_via, rx_viela,
  sep = "|")

rua_sem_nome <- paste(rua_sem_nome, cardinais, sep = "|")

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

# Ajustes manuais
dictionary <- dictionary %>%
  mutate(
    class = if_else(str_detect(street_name, "Carlos Gomes"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Getúlio Vargas"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Protásio Alves"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Salgado Filho"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Salgado Filho"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Mário Tavares Haussen"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Donário Braga"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "Joracy Camargo"), "Personalidade Histórica", class),
    class = if_else(str_detect(street_name, "(Beira Rio)|(Beira Rio)"), "Coisa", class),
    class = if_else(str_detect(street_name, "Ipiranga"), "Coisa", class)
    
  )

write_csv(dictionary, here::here("data/dictionary_street_names_poa.csv"))

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
<p style='line-height:0.9'> A maior parte das ruas (~38%) têm nome de personalidades históricas (e.g. <span style='color: #fdb462'><strong>Rua Vicente da Fontoura</strong></span>) ou de outras cidades/UFs (~28%) (e.g. <span style='color: #80b1d3'><strong>Av. Paraná</strong></span>).<br/>
H&aacute; 208 ruas nomeadas em homenagem a membros do ex&eacute;rcito (e.g. <span style='color: #b15928;'><strong>Av. Presidente Castelo Branco</strong></span>).<br/>
As ruas sem nome (e.g. <span style='color: #e78ac3;'><strong>Beco A<strong></span>, <span style='color: #e78ac3;'><strong>Rua 2<strong></span>) majoritariamente se concentram em aglomerados subnormais.<br/>
A categoria profiss&atilde;o re&uacute;ne as ruas que usam um t&iacute;tulo (e.g. <span style='color: #b3de69';><strong>Doutor Flores<strong></span>). H&aacute; 103 ruas com nomes de professores (e.g. <span style='color: #b3de69';><strong>Rua Professor Fitzgerald<strong></span>).<br/>
Algumas ruas podem ser classificadas em mais do que um  grupo como a <span><strong>Av. Bento Gonçalves<strong></span> (município ou personalidade histórica) <br/>
ou <span><strong>Av. Senador Salgado Filho<strong></span> (município, personalidade histórica ou profissão) </span>.<br/>
</p>"

cores <- RColorBrewer::brewer.pal(8, "Set3")
cores[2] <- "#b15928"
cores[8] <- "#e78ac3"
   
p1 <- ggplot() +
  geom_sf(
    data = filter(s1, !is.na(class)),
    aes(color = class),
    key_glyph = draw_key_rect
  ) +
  geom_sf(
    data = filter(s2, !is.na(class)),
    aes(color = class),
    key_glyph = draw_key_rect,
    size = 0.6
  ) +
  geom_sf(
    data = filter(s3, !is.na(class)),
    aes(color = class),
    key_glyph = draw_key_rect,
    size = 0.45
  ) +
  coord_sf(
    ylim = c(-30.07, -30),
    xlim = c(-51.245, -51.135)
  ) +
  scale_colour_manual(
    name = "",
    values = cores
  ) +
  scale_fill_manual(
    name = "",
    values = cores
  ) +
  labs(
    title = "**Origem do Nome de Ruas em Porto Alegre**",
    subtitle = sub,
    caption = "Fonte: osmdata. Cores: ColorBrewer. Autor: @viniciusoike"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
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
      size = 11,
      colour = "gray20"
    ),
    
    panel.border = element_rect(colour = "gray20", fill = "transparent"),
    
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    legend.box.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 0.5, b = 0.5, unit = "cm")
  )

cowplot::save_plot(here::here("graphics/2_porto_alegre_streets/street_maps.png"),
                   p1,
                   base_height = 10,
                   dpi = 300)