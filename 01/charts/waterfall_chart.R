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
  mutate(waterfall = resumo_infraestrutura - lag(resumo_infraestrutura)) %>%
  mutate(waterfall = coalesce(waterfall, resumo_infraestrutura))

vigia_to_plot %>%
  select(mes, waterfall) %>%
  waterfall(calc_total = TRUE)

# vigia_to_plot <- vigia %>%
#   mutate(start = lag(resumo_infraestrutura, default = 0))

# vigia_to_plot %>%
#   ggplot(aes(x=mes, y=start)) +
#   geom_segment(aes(xend = mes, yend = resumo_infraestrutura), size=10)

# ggsave(here("waterfall_chart.svg"))
