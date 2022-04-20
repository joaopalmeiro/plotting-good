renv::use(
  "styler@1.7.0",
  "here@1.0.1",
  "tidyverse@1.3.1",
  "svglite@2.1.0",
  "ggstream@0.1.0",
  "ggrepel@0.9.1",
  "showtext@0.9-5"
)

library(readr)
library(dplyr)
library(here)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(ggstream)
library(ggrepel)
library(showtext)

# https://github.com/yixuan/showtext#a-quick-example
# https://r-graph-gallery.com/custom-fonts-in-R-and-ggplot2.html
# font_add_google("Rubik", "rubik")
# showtext_auto()

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

min(df$data)
max(df$data)

albufeiras <- c(
  "Alvito",
  "Campilhas",
  "Fonte Serne",
  "Monte Gato",
  "Monte Migueis",
  "Monte da Rocha",
  "Odivelas",
  "Pego do Altar",
  "Roxo",
  "Vale do Gaio"
)

start_year <- 2000
end_year <- 2021

df_to_plot <- df %>%
  filter(
    nome_infraestrutura %in% albufeiras,
    medida == "metro_cubico",
    data %within% interval(ymd(paste0(start_year, "-01-01")), ymd(paste0(end_year, "-12-01")))
  ) %>%
  mutate(annotation = case_when(
    year(data) == end_year & month(data) == 12 ~ nome_infraestrutura,
    TRUE ~ ""
  )) %>%
  arrange(data, nome_infraestrutura)
df_to_plot

missing_df_to_plot <- df_to_plot %>%
  filter(is.na(resumo_infraestrutura))
missing_df_to_plot %>% glimpse()

# df_to_plot <- df_to_plot %>% na.exclude()
# df_to_plot %>% glimpse()

# https://rdrr.io/r/grDevices/palettes.html
# https://riatelab.github.io/mapsf/reference/mf_get_pal.html
# http://riatelab.github.io/cartography/docs/reference/carto.pal.html
# https://emilhvitfeldt.github.io/paletteer/
# https://packages.tesselle.org/khroma/reference/index.html
# https://windicss.org/utilities/general/colors.html
hcl.pals()
# pal <- hcl.colors(length(albufeiras), palette = "Blues")
# pal <- rep(hcl.colors(2, palette = "Mint"), length(albufeiras) / 2)
pal <- rep(c("#93c5fd", "#60a5fa"), length(albufeiras) / 2)
pal
# black <- "#171717"

# Based on: https://github.com/z3tt/TidyTuesday/blob/master/R/2020_27_ClaremontRunXMen.Rmd
# https://ggplot2.tidyverse.org/reference/geom_contour.html
# https://github.com/davidsjoberg/ggstream/blob/master/R/geom_stream.R#L284
# https://github.com/davidsjoberg/ggstream

# https://rawgraphs.io/learning/how-to-make-a-streamgraph/
# https://r-charts.com/evolution/ggstream/
# https://github.com/tidyverse/ggplot2/blob/main/R/geom-.r#L204

gridline_width_px <- 1

df_to_plot %>%
  ggplot(aes(
    x = data,
    y = resumo_infraestrutura,
    fill = nome_infraestrutura,
    label = annotation
  )) +
  # geom_stream(
  #   type = "mirror",
  #   # geom = "contour",
  #   geom = "contour_filled",
  #   color = "lightgray",
  #   show.legend = FALSE,
  #   size = 1.25,
  #   # extra_span = 0.1,
  #   # bw = 1
  #   # bw = 0.1
  # ) +
  geom_stream(
    type = "mirror",
    geom = "polygon",
    # show.legend = TRUE,
    show.legend = FALSE,
    # extra_span = 0.1,
    # bw = 1,
    # bw = 0.1,
    # color = "black",
    # lwd = 0.25,
  ) +
  scale_fill_manual(values = pal) +
  # scale_fill_manual(values = hcl.colors(10)) +
  # scale_x_date(date_labels = "%y", date_breaks = "1 year") +
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(
      # family = "rubik",
      face = "plain",
      size = 14
    ),
    legend.text = element_text(
      # family = "rubik",
      face = "plain",
      size = 14
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "lightgray",
      linetype = "solid",
      # size = 1,
      size = (gridline_width_px / ggplot2::.pt) / 0.75,
    ),
    panel.grid.minor = element_blank()
  )

px_per_inch <- 72
width <- 568 / px_per_inch
height <- 320 / px_per_inch

ggsave(
  here("streamgraph.svg"),
  width = width,
  height = height
)
