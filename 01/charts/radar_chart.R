# https://rstudio.github.io/renv/articles/use.html
# https://rstudio.github.io/renv/reference/use.html
# https://github.com/ricardo-bion/ggradar/blob/master/DESCRIPTION

renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "see@0.6.9",
  "ricardo-bion/ggradar@568537ab057b333f628c1087d1a8b268eb490de8",
  "svglite@2.1.0"
)

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(see)
library(ggradar)
library(tidyr)
library(here)
library(stringr)
library(forcats)

# https://tibble.tidyverse.org/articles/types.html
# https://readr.tidyverse.org/reference/parse_datetime.html#format-specification
# https://github.com/dssgPT/Plotting-Good-DSSG/tree/main/desafios/001_Seca_Em_Portugal_SNIRH

df <- read_csv(
  "https://raw.githubusercontent.com/dssgPT/Plotting-Good-DSSG/main/desafios/001_Seca_Em_Portugal_SNIRH/snirh_clean.csv",
  col_types = cols_only(
    nome_infraestrutura = col_character(),
    resumo_infraestrutura = col_double(),
    data = col_date(format = "%Y-%m-%d"),
    medida = col_character()
  ),
  na = c("n/d")
)
df

data <- iris %>%
  group_by(Species) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

data

convert_month <- function(x) {
  paste0(str_to_lower(x), ".")
}

vigia <- df %>%
  filter(
    nome_infraestrutura == "Vigia",
    medida == "percentagem",
    data %within% interval(ymd("2021-01-01"), ymd("2021-12-01"))
  ) %>%
  # `system("locale -a")`
  # https://ciberduvidas.iscte-iul.pt/consultorio/perguntas/as-abreviaturas-dos-meses-dos-anos-segundo-o-novo-acordo-ortografico/30221
  mutate(mes = month(data, label = TRUE, locale = "pt_PT")) %>%
  mutate(mes = fct_relabel(mes, convert_month))
vigia

# https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R#L69
# https://github.com/easystats/see/blob/master/R/coord_radar.R
# https://stackoverflow.com/a/42572133

vigia %>%
  select(nome_infraestrutura, mes, resumo_infraestrutura) %>%
  pivot_wider(names_from = mes, values_from = resumo_infraestrutura) %>%
  ggradar(
    grid.min = 0,
    grid.mid = 50,
    grid.max = 100,
    label.gridline.min = FALSE,
    label.gridline.mid = FALSE,
    label.gridline.max = FALSE
  )

gridlines_df <-
  data.frame(
    x = rep(vigia$mes, 4),
    y = rep(c(25, 50, 75, 100), each = 12)
  )
gridlines_df

labels_df <- data.frame(
  x = rep(vigia$mes[1], 4),
  y = c(25, 50, 75, 100),
  label = c("25%", "50%", "75%", "100%")
)
labels_df

# https://easystats.github.io/see/reference/coord_radar.html
# https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html
# https://ggplot2.tidyverse.org/reference/geom_polygon.html

vigia %>%
  ggplot() +
  geom_polygon(
    aes(x = mes, y = resumo_infraestrutura, group = nome_infraestrutura),
    fill = "lightblue",
  ) +
  geom_polygon(
    aes(x = x, y = y, group = y),
    gridlines_df,
    color = "lightgrey",
    fill = NA,
    size = 0.25
  ) +
  scale_y_continuous(breaks = c(25, 50, 75, 100), limits = c(0, 100)) +
  coord_radar(start = -pi / 12) +
  # coord_polar(start = -pi / 12) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.ontop = FALSE,
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

# ggsave(here("radar_chart.svg"))
# ggsave(here("radar_chart.png"))

vigia %>%
  ggplot() +
  geom_polygon(
    aes(x = x, y = y, group = y),
    gridlines_df,
    color = "lightgrey",
    fill = NA,
    size = 0.25
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c("", as.character(vigia$mes[-1]))) +
  coord_radar(start = -pi / 12) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    # Warning:
    # axis.text.x = element_text(color = c(NA, rep("gray", 11))),
    axis.ticks = element_blank(),
    panel.ontop = FALSE,
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      size = 0.5,
      color = c("lightgray", rep(NA, 11))
    ),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  ) +
  geom_text(
    aes(x = x, y = y, label = label),
    labels_df,
    size = 2, hjust = "middle", vjust = "center"
  ) +
  # https://ggplot2.tidyverse.org/reference/geom_text.html
  geom_label(
    aes(x = "jan.", y = 100, label = "jan."),
    vjust = -0.25,
    label.size = NA,
    label.padding = unit(0.25, "lines"),
    fill = "white",
    label.r = unit(0, "lines")
  )
