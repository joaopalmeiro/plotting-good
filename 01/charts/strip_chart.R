renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "svglite@2.1.0",
  "scico@1.3.0",
  "scales@1.1.1",
  "ggrepel@0.9.1"
)

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)
library(scico)
library(scales)
library(here)
library(ggrepel)

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

convert_month <- function(x) {
  paste0(str_to_lower(x), ".")
}

vigia <- df %>%
  filter(
    nome_infraestrutura == "Vigia",
    medida == "percentagem",
    data %within% interval(ymd("2000-01-01"), ymd("2021-12-01"))
  ) %>%
  mutate(mes = month(data, label = TRUE, abbr = TRUE, locale = "pt_PT")) %>%
  mutate(mes = fct_relabel(mes, convert_month)) %>%
  mutate(ano = year(data))
vigia
vigia %>% glimpse()
n_distinct(vigia$nome_infraestrutura)
n_distinct(vigia$medida)

missing_vigia <- vigia %>%
  filter(is.na(resumo_infraestrutura))
missing_vigia %>% glimpse()

vigia <- vigia %>% na.exclude()
vigia %>% glimpse()

n_anos <- n_distinct(vigia$ano)
n_anos

# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html#sec:shape-spec
# https://wilkelab.org/ungeviz/index.html
# https://mjskay.github.io/ggdist/
# https://stackoverflow.com/a/42749526
# https://ggplot2.tidyverse.org/reference/geom_spoke.html
# https://github.com/thomasp85/scico
# https://www.fabiocrameri.ch/colourmaps/
# https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html
# https://altair-viz.github.io/gallery/strip_plot.html
# https://ggplot2.tidyverse.org/reference/sec_axis.html

deg2rad <- function(deg) {
  (deg * pi) / (180)
}

# https://dplyr.tidyverse.org/reference/mutate.html
# https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
# https://css-tricks.com/footnote-characters/

vigia_to_plot <- vigia %>%
  select(resumo_infraestrutura, ano, mes) %>%
  group_by(mes) %>%
  mutate(left_spoke = if_else(
    resumo_infraestrutura == min(resumo_infraestrutura),
    0.5,
    0.3
  )) %>%
  mutate(right_spoke = if_else(
    resumo_infraestrutura == max(resumo_infraestrutura),
    0.5,
    0.3
  )) %>%
  mutate(annotation = case_when(
    mes == "jul." & resumo_infraestrutura == min(resumo_infraestrutura) ~ "Mínimo mensal",
    mes == "jul." & resumo_infraestrutura == max(resumo_infraestrutura) ~ "Máximo mensal",
    TRUE ~ ""
  ))
vigia_to_plot

black_color <- "black"
gray_color <- "gray"

vigia_to_plot %>%
  ggplot(aes(x = mes, y = resumo_infraestrutura, label = annotation)) +
  # geom_point() +
  geom_vline(aes(xintercept = mes), colour = gray_color) +
  # Right:
  geom_spoke(
    angle = 0,
    # radius = 0.5,
    # radius = 1,
    # radius = 0.3,
    aes(colour = resumo_infraestrutura, radius = right_spoke),
    show.legend = FALSE,
    size = 0.5
  ) +
  # Left:
  geom_spoke(
    angle = deg2rad(180),
    aes(colour = resumo_infraestrutura, radius = left_spoke),
    show.legend = FALSE,
    size = 0.5
  ) +
  geom_text_repel(
    # min.segment.length = 0,
    # segment.size = 0.25,
    size = 12 / .pt,
    colour = black_color
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100),
    labels = label_percent(scale = 1),
    sec.axis = dup_axis()
  ) +
  scale_colour_scico(
    palette = "lapaz",
    limits = c(0, 100),
    direction = -1
  ) +
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # axis.text = element_text(
    #   size = 14,
    #   colour = "black"
    # ),
    axis.text.x = element_text(
      size = 14,
      colour = black_color
    ),
    axis.text.y = element_text(
      size = 12,
      colour = black_color
    ),
    panel.grid.major.y = element_line(
      colour = gray_color,
      linetype = "dashed"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

px_per_inch <- 72
width <- 667 / px_per_inch
height <- 375 / px_per_inch

ggsave(
  here("strip_chart.svg"),
  width = width,
  height = height
)
