# FWISE priority countries for future eradications (MAPS)
# made 13.09.26

# clean up

rm(list = ls())

# not in

'%ni%' <- Negate('%in%')

# not in

'%ni%' <- Negate('%in%')

# claude functions to fix country errors

source("scripts/claudeCountry.R")

# load packages

library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)

# read in data

world <- ne_countries(scale = "medium", returnclass = "sf") %>% dplyr::select(country = admin)
deja <- read_csv("./outputs/fwise_successful_countries_2026.csv")$country
future  <- read_csv("./outputs/fwise_priority_countries_shoal_gfid.csv")$country

#map

world_status <- world %>% 
  mutate(
    status = case_when(
      country %in% deja   ~ "deja",
      country %in% future ~ "future",
      .default = NA_character_
    )
  )

sum(!is.na(world_status$status))

fwise_now_next <- ggplot(world_status) +
  geom_sf(aes(fill = status, colour = status), linewidth = 0.2) +
  scale_fill_manual(
    values = c("deja" = "#54A1CB", "future" = "#EEBF54"),
    na.value = "gray92"
  ) +
  scale_colour_manual(
    values = c("deja" = "#0072B2", "future" = "#E69F00"),
    na.value = "gray62"
  ) +
  coord_sf(crs = "+proj=moll", ylim = c(-6000000, NA)) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.margin = margin(0, 0, 0, 0))

fwise_now_next

fwise_now <- fwise_now_next %+% 
  mutate(world_status, status = if_else(status == "deja", status, NA_character_))

fwise_next <- fwise_now_next %+% 
  mutate(world_status, status = if_else(status == "future", status, NA_character_))

fwise_now
fwise_next
fwise_now_next

plots <- list(
  fwise_now      = fwise_now,
  fwise_next     = fwise_next,
  fwise_now_next = fwise_now_next
)

for (nm in names(plots)) {
  
  ggsave(paste0("outputs/web/", nm, "_300dpi.png"), plots[[nm]], width = 14, height = 8, dpi = 300)

  ggsave(paste0("outputs/web/", nm, "_600dpi.png"), plots[[nm]], width = 14, height = 8, dpi = 600)
  
  ggsave(paste0("outputs/web/pdf_uncropped/", nm, ".pdf"), plots[[nm]], width = 14, height = 8, device = cairo_pdf)
  
  ggsave(paste0("outputs/web/svg_uncropped", nm, ".svg"), plots[[nm]], width = 14, height = 8, device = svg)
  
}
#fini