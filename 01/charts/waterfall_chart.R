renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "svglite@2.1.0",
  "waterfalls@0.1.2"
)

library(readr)
library(dplyr)
library(here)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(waterfalls)

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

vigia <- df %>%
  filter(
    nome_infraestrutura == "Vigia",
    medida == "percentagem",
    data %within% interval(ymd("2021-01-01"), ymd("2021-12-01"))
  ) %>%
  mutate(mes = month(data, label = TRUE, abbr = TRUE, locale = "pt_PT")) %>%
  mutate(mes = fct_relabel(mes, convert_month)) %>%
  mutate(ano = year(data))
vigia

vigia_to_plot <- vigia %>%
  mutate(waterfall = round(resumo_infraestrutura - lag(resumo_infraestrutura), 1)) %>%
  mutate(waterfall = coalesce(waterfall, resumo_infraestrutura)) %>%
  mutate(bar_color = case_when(
    row_number() == 1 ~ "green",
    sign(waterfall) == 1 ~ "blue",
    sign(waterfall) == -1 ~ "red"
  ))

vigia_to_plot %>%
  select(mes, waterfall) %>%
  waterfall(
    calc_total = TRUE,
    draw_lines = TRUE,
    linetype = "solid",
    total_rect_color = "orange",
    total_rect_text_color = "white",
    fill_by_sign = FALSE,
    fill_colours = vigia_to_plot$bar_color
  )

ggsave(here("waterfall_chart.svg"))

# vigia_to_plot <- vigia %>%
#   mutate(start = lag(resumo_infraestrutura, default = 0))

# vigia_to_plot %>%
#   ggplot(aes(x=mes, y=start)) +
#   geom_segment(aes(xend = mes, yend = resumo_infraestrutura), size=10)
