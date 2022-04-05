renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "svglite@2.1.0",
  "waterfalls@0.1.2",
  "scales@1.1.1"
)

library(readr)
library(dplyr)
library(here)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(waterfalls)
library(scales)

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

# https://dplyr.tidyverse.org/reference/lead-lag.html
# https://github.com/HughParsonage/waterfalls/blob/master/R/waterfall.R#L38
# https://r-charts.com/flow/waterfall-chart/
# https://dplyr.tidyverse.org/reference/case_when.html

year <- 2021

vigia <- df %>%
  filter(
    nome_infraestrutura == "Vigia",
    medida == "percentagem",
    data %within% interval(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-12-01")))
  ) %>%
  mutate(mes = month(data, label = TRUE, abbr = TRUE, locale = "pt_PT")) %>%
  mutate(mes = fct_relabel(mes, convert_month)) %>%
  mutate(ano = year(data)) %>%
  arrange(data)
vigia

# https://pt.wikipedia.org/wiki/Porcentagem#Ponto_percentual
# https://en.wikipedia.org/wiki/Percentage_point
# https://pt.wiktionary.org/wiki/ponto_percentual
# https://github.com/tidyverse/dplyr/issues/6206
# https://www.amcharts.com/demos/waterfall-chart/

# pp_suffix <- " pp"
pp_suffix <- "pp"

vigia_to_plot <- vigia %>%
  mutate(
    waterfall = round(resumo_infraestrutura - lag(resumo_infraestrutura), 1)
  ) %>%
  mutate(waterfall = coalesce(waterfall, resumo_infraestrutura)) %>%
  mutate(bar_color = case_when(
    row_number() == 1 ~ "green",
    sign(waterfall) == 1 ~ "blue",
    sign(waterfall) == -1 ~ "red"
  )) %>%
  # mutate(label = ifelse(
  #   mes %in% c("jan."),
  #   paste0(waterfall, "%"),
  #   paste0(waterfall, " pp")
  # )) %>%
  mutate(label = case_when(
    mes == "jan." ~ paste0(abs(waterfall), "%"),
    waterfall != 0 ~ paste0(abs(waterfall), pp_suffix),
    TRUE ~ as.character(abs(waterfall))
  )) %>%
  mutate(mes = ifelse(row_number() == 1, paste0(mes, "*"), as.character(mes)))
vigia_to_plot %>% glimpse()

last_label <- paste0(tail(vigia_to_plot, 1)$mes, "*")
last_label

# https://github.com/HughParsonage/waterfalls/blob/master/R/waterfall.R#L43
# multiplier <- 0.25
multiplier <- 0.05
label_threshold <- multiplier * (max(cumsum(vigia_to_plot$waterfall)) - min(cumsum(vigia_to_plot$waterfall)))

vigia_to_plot %>%
  select(mes, waterfall) %>%
  waterfall(
    calc_total = TRUE,
    # draw_lines = TRUE,
    draw_lines = FALSE,
    linetype = "solid",
    total_rect_color = "orange",
    total_rect_text_color = "black",
    # total_axis_text = "dez.",
    # total_axis_text = "",
    total_axis_text = last_label,
    fill_by_sign = FALSE,
    fill_colours = vigia_to_plot$bar_color,
    rect_border = NA,
    # rect_border = "black",
    # https://github.com/HughParsonage/waterfalls/blob/master/R/waterfall.R#L223
    rect_text_size = (12 / .pt) * (5 / 14),
    draw_axis.x = NA,
    # draw_axis.x = "behind",
    # rect_width = 0.7
    # rect_width = 1
    rect_width = 0.95,
    put_rect_text_outside_when_value_below = label_threshold,
    rect_text_labels = vigia_to_plot$label,
    # https://github.com/HughParsonage/waterfalls/blob/master/R/waterfall.R#L46
    total_rect_text = paste0(sum(vigia_to_plot$waterfall), "%")
  ) +
  # geom_hline(yintercept = 0) +
  # geom_hline(yintercept = 100) +
  scale_y_continuous(
    breaks = c(0, 50, 100),
    limits = c(0, 100),
    labels = label_percent(scale = 1),
    # sec.axis = dup_axis()
  ) + theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(
      size = 14,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 12,
      colour = "black",
      # hjust = 0.5
    ),
    panel.grid.major.y = element_line(
      colour = "gray",
      linetype = "solid"
    ),
    panel.grid.major.x = element_line(
      colour = "lightgray",
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank()
  )

px_per_inch <- 72
# width <- 568 / px_per_inch
# height <- 320 / px_per_inch
width <- 640 / px_per_inch
height <- 360 / px_per_inch

ggsave(
  here("waterfall_chart.svg"),
  width = width,
  height = height
)

# vigia_to_plot <- vigia %>%
#   mutate(start = lag(resumo_infraestrutura, default = 0))

# vigia_to_plot %>%
#   ggplot(aes(x=mes, y=start)) +
#   geom_segment(aes(xend = mes, yend = resumo_infraestrutura), size=10)
