# https://rstudio.github.io/renv/articles/use.html
# https://rstudio.github.io/renv/reference/use.html
# https://github.com/ricardo-bion/ggradar/blob/master/DESCRIPTION

renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "see@0.6.9",
  "ricardo-bion/ggradar@568537ab057b333f628c1087d1a8b268eb490de8",
  "svglite@2.1.0",
  "ggtext@0.1.1",
  "ragg@1.2.2",
  "cowplot@1.1.1",
  "patchwork@1.1.1"
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
library(ggtext)
library(ragg)
library(cowplot)
library(patchwork)

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

convert_month <- function(x) {
  paste0(str_to_lower(x), ".")
}

vigia <- df %>%
  filter(
    nome_infraestrutura == "Vigia",
    medida == "percentagem",
    data %within% interval(ymd("2012-01-01"), ymd("2021-12-01"))
  ) %>%
  # `system("locale -a")`
  # https://ciberduvidas.iscte-iul.pt/consultorio/perguntas/as-abreviaturas-dos-meses-dos-anos-segundo-o-novo-acordo-ortografico/30221
  mutate(mes = month(data, label = TRUE, locale = "pt_PT")) %>%
  mutate(mes = fct_relabel(mes, convert_month)) %>%
  mutate(ano = year(data))
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

vigia_one_year <-
  vigia %>% filter(ano == 2021)
vigia_one_year

gridlines_df <-
  data.frame(
    x = rep(vigia_one_year$mes, 4),
    y = rep(c(25, 50, 75, 100), each = 12)
  )
gridlines_df

labels_df <- data.frame(
  x = rep(vigia_one_year$mes[1], 4),
  y = c(25, 50, 75, 100),
  label = c("25%", "50%", "75%", "100%")
)
labels_df

# strrep(" ", nchar(as.character(vigia$mes[1])))

legend <- vigia_one_year %>%
  ggplot() +
  geom_polygon(
    aes(x = x, y = y, group = y),
    gridlines_df,
    color = "lightgray",
    fill = NA,
    size = 0.25
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  # scale_x_discrete(labels = c("", as.character(vigia$mes[-1]))) +
  coord_radar(start = -pi / 12) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    # https://ggplot2.tidyverse.org/articles/faq-customising.html#how-can-i-change-the-default-font-size-in-ggplot2
    # https://ggplot2.tidyverse.org/reference/ggtheme.html (`base_size = 11`)
    # https://github.com/tidyverse/ggplot2/blob/v3.3.5/R/theme-defaults.r#L113
    # axis.text.x = element_text(size = 3.88 * 0.8 * .pt),
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
  geom_label(
    aes(x = mes[1], y = 100, label = mes[1]),
    hjust = "middle",
    label.size = NA,
    label.r = unit(0, "lines"),
    label.padding = unit(0.5, "lines"),
    fill = "white",
    size = (11 / .pt) * 0.8,
    vjust = "bottom",
    color = "white"
  ) +
  # https://ggplot2.tidyverse.org/reference/geom_text.html#alignment
  # https://github.com/tidyverse/ggplot2/blob/v3.3.5/R/geom-label.R#L54
  # https://github.com/tidyverse/ggplot2/blob/v3.3.5/R/geom-.r#L193
  # https://ggplot2.tidyverse.org/articles/ggplot2-specs.html#font-size (mm)
  # https://ggplot2.tidyverse.org/reference/element.html#arguments (pt)
  geom_label(
    aes(x = x, y = y, label = label),
    labels_df,
    label.size = NA,
    label.r = unit(0, "lines"),
    label.padding = unit(0.25, "lines"),
    fill = "white",
    hjust = "middle",
    vjust = 0.75,
    # vjust = "top",
    # vjust = "center",
    size = (11 / .pt) * 0.8,
    color = "black"
  )
legend

# ggsave(here("legend.svg"))

# 3.88 * 0.8
# (11 / .pt) * 0.8

# https://easystats.github.io/see/reference/coord_radar.html
# https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html
# https://ggplot2.tidyverse.org/reference/geom_polygon.html

chart <- vigia %>%
  ggplot() +
  geom_polygon(
    aes(x = mes, y = resumo_infraestrutura, group = nome_infraestrutura),
    fill = "#0284C7",
  ) +
  geom_polygon(
    aes(x = x, y = y, group = y),
    gridlines_df,
    color = "#E2E8F0",
    fill = NA,
    # size = 0.25
    size = 1
  ) +
  scale_y_continuous(breaks = c(25, 50, 75, 100), limits = c(0, 100)) +
  coord_radar(start = -pi / 12) +
  # coord_polar(start = -pi / 12) +
  facet_wrap(vars(ano), ncol = 5, strip.position = "top") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      size = 12, # pt
      color = "#475569"
    ),
    axis.ticks = element_blank(),
    panel.ontop = FALSE,
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    strip.background = element_blank(),
    plot.subtitle = element_markdown()
  ) +
  labs(
    # title = "",
    # subtitle = "<span style = 'color:lightblue;'>Percentagem (%) mensal de água</span> armazenada na albufeira da **Vigia** entre 2012 e 2021",
    # caption = "Fonte: SNIRH (dados tratados pela DSSG) • Gráfico: João Palmeiro"
  )
chart
# ggdraw(chart)

# layout <- "
# ####A
# BBBBB
# "
# legend + chart + plot_layout(design = layout)

# <br>
ggsave(here("radar_chart.svg"))
# ggsave(here("radar_chart.png"))
# https://ragg.r-lib.org/reference/agg_png.html
# https://github.com/z3tt/TidyTuesday/blob/master/R/2021_22_MarioKart.Rmd
# https://ragg.r-lib.org/index.html#use-ragg-in-rstudio
# ggsave(
#   here("radar_chart.png"),
#   unit = "px",
#   scaling = 1,
#   device = agg_png,
#   limitsize = FALSE
# )

single_chart <- vigia %>%
  filter(ano == 2021) %>%
  ggplot() +
  geom_polygon(
    aes(x = mes, y = resumo_infraestrutura, group = nome_infraestrutura),
    fill = "lightblue",
  ) +
  geom_polygon(
    aes(x = x, y = y, group = y),
    gridlines_df,
    color = "lightgray",
    fill = NA,
    size = 0.25
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  coord_radar(start = -pi / 12) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_blank(),
    panel.ontop = FALSE,
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )
single_chart

# ggsave(here("single_radar_chart.svg"))
