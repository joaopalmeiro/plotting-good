renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "svglite@2.1.0",
  "scico@1.3.0",
  "scales@1.1.1"
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

vigia <- df %>%
  filter(
    nome_infraestrutura == "Vigia",
    medida == "percentagem",
    data %within% interval(ymd("2000-01-01"), ymd("2021-12-01"))
  ) %>%
  mutate(mes = month(data, label = TRUE, abbr = FALSE, locale = "pt_PT")) %>%
  mutate(mes = fct_relabel(mes, str_to_lower)) %>%
  mutate(ano = year(data))
vigia
vigia %>% glimpse()

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
  ))
vigia_to_plot

vigia_to_plot %>%
  ggplot(aes(x = mes, y = resumo_infraestrutura)) +
  # geom_point() +
  geom_vline(aes(xintercept = mes), colour = "gray") +
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
    axis.text = element_text(
      size = 12,
      colour = "black"
    ),
    panel.grid.major.y = element_line(
      colour = "gray",
      linetype = "dashed"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# ggsave(here("strip_plot.svg"))
