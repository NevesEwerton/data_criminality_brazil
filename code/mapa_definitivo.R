library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(geobr)
library(ggrepel)
library(tidyverse)
library(showtext)
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(grid)
library(ggtext)


# Definindo a fonte utilizada
font_add_google("Chakra Petch")
showtext_auto()

estados_gestao <- c("PA", "CE", "PB", "PE", "AL", "MG", "ES", "RJ", "SP", "RS", "DF")
datas_gestao <- c(
  "Jun/2022", "Mar/2014", "Jan/2011", "Mai/2007",
  "Jan/2015", "Jan/2005", "Mai/2011", "Jun/2009",
  "Jan/2014", "Fev/2019", "Dez/2019"
)
nomes_gestao <- c(
  "Pará", "Ceará", "Paraíba", "Pernambuco", "Alagoas", "Minas Gerais",
  "Espírito Santo", "Rio de Janeiro", "São Paulo", "Rio Grande do Sul", "Distrito Federal"
)

# Criando dataframe com as informações das políticas de gestão por resultados
gestao_df <- data.frame(sigla_uf = estados_gestao, data = datas_gestao)

# Carregando os dados para criar o mapa 
estados <- read_state(code_state = 'all', year = 2020)

# Fazendo o cruzamento entre os dataframes
estados <- estados %>%
  rename(sigla_uf = abbrev_state) %>%
  mutate(gestao = sigla_uf %in% estados_gestao) %>%
  left_join(gestao_df, by = "sigla_uf") %>%
  st_as_sf()

# Paleta de cores inspirada no mapa de referência
cor_participante <- "#964F4C" # verde escuro
cor_nao_participante <- "#E7E6D8" # bege claro

# Centroide de cada estado
estados_centroides <- estados %>%
  mutate(centroide = st_centroid(geom)) %>%
  
  # Coordenadas do centroide para colunas separadas
  mutate(
    lon = st_coordinates(centroide)[,1],
    lat = st_coordinates(centroide)[,2]
  )

# 3. Código do mapa com melhorias
mapa <- ggplot() +
  geom_sf(
    data = estados, 
    aes(fill = gestao), 
    color = "grey40", 
    size = 0.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = cor_participante, "FALSE" = cor_nao_participante),
    labels = c("Participant", "Non-participant"),
    guide = guide_legend(title = NULL,override.aes = list(color = NA))
  ) +
  geom_text(
    data = estados_centroides,
    aes(x = lon, y = lat, label = sigla_uf),
    size = 10,
    fontface = "bold",
    color = "black",
    check_overlap = TRUE # Evita sobreposição manual
  ) +
  theme_void(base_family = "sans") +
  labs(
    title = "The Brazilian Experience with Compstat-like Programs",
    subtitle = "<span style='display: block; text-align: center;'>States in <span style='color:#964F4C'><b>red</b></span> implemented programs similar to Compstat.</span>",
    caption = "Source: Instituto Sou da Paz | Visualization: Ewerton Neves Cardoso"
  ) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      hjust = 0.5,
      size = 55,
      family = "Chakra Petch",
      margin = margin(b = 20)
    ),
    plot.background = element_rect(fill = "#F6F5ED", color = NA),
    plot.title = element_text(
      hjust = 0.5, 
      size = 70, 
      face = "bold",
      family = 'Chakra Petch',
      margin = margin(b = 20., t=10)
    ),
    plot.caption = element_text(
      size = 30, 
      hjust = 0.95,
      family = 'Chakra Petch',
      color = "grey40"
    ),
    legend.position = "none",
    legend.title = element_text(
      size = 11, 
      face = "bold",
      family = 'Chakra Petch'
    ),
    legend.text = element_text(
      size = 10,
      family = 'Chakra Petch'
      )
  )

# Pegando os centroides de MG para posicionar a seta e a caixa de texto
coord_mg <- estados_centroides %>% filter(sigla_uf == "MG")
x_mg <- coord_mg$lon
y_mg <- coord_mg$lat

mapa_final <- mapa +
  
  # Criando a caixa de texto
  annotate(
    "label",
    x = -38,  # Ajuste conforme a longitude desejada
    y = -32.5,   # Ajuste conforme a latitude desejada
    label = "IGESP was Brazil's first Compstat-like program, \nlaunched in Minas Gerais State, and the first \nto be formally evaluated for impact.",
    size = 12,
    fill = "#FFFBEA",
    color = "black",
    label.size = 0.5,
    fontface = "italic",
    lineheight = .5    # Ajuste aqui também
  ) +
  
  #Criando a seta que liga o estado a caixa de texto
  annotate(
    "curve",
    x = -36,
    y = -29,
    xend = x_mg + 1,
    yend = y_mg,
    curvature = 0.2,     # Curvatura da seta (negativo = curva para a esquerda)
    angle = 90, 
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    colour = "black",
    linewidth = 1
  )

ggsave("grafico/mapa/mapa_brazil_compstat1.png", plot = mapa_final, bg = "#F6F5ED")





