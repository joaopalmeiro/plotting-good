renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "svglite@2.1.0"
)

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)

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

vigia %>%
  ggplot()
