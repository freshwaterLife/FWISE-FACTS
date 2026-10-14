# FWISE priority countries for future eradications
# made 13.09.26

# clean up

rm(list = ls())

# not in

'%ni%' <- Negate('%in%')

# claude functions to fix country errors

source("scripts/claudeCountry.R")

# load packages

library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)

# gfid v2
# https://doi.org/10.5281/zenodo.22694143
# data to join

gfid_main <- read_excel("./inputs/GFID_v2.xlsx", sheet = "Main_data")
gfid_impact <- read_excel("./inputs/GFID_v2.xlsx", sheet = "Impacts")
gfid_iucn <- read_excel("./inputs/GFID_v2.xlsx", sheet = "Ranges")

# main data

colnames(gfid_main)

# fix some country stuff 
# some species duplicated across countries cause different sources
# check returns every row in a duplicate group; distinct() keeps one survivor per...
# group, so rows removed = flagged rows - number of groups

gfid_main_raw <- gfid_main

dcs <- dupe_country_species_check(gfid_main_raw)
dcs %>% print(n = Inf)

dcs_n <- nrow(dcs) - nrow(distinct(dcs, country, scientificName)); dcs_n

dupe_country_species_check(gfid_main_raw) %>% 
  count(country, scientificName) %>% 
  count(n, name = "n_groups")

nrow(gfid_main_raw)

gfid_main <- gfid_main %>% 
  distinct(country, scientificName, .keep_all = TRUE)
  
identical(nrow(gfid_main_raw) - nrow(gfid_main), dcs_n)

# 1 species in Svalbard also in Norway, 4 species labelled Swaziland also in those labelled eSwatini
  
gfid_main_raw2 <- gfid_main

gfid_main_raw2 %>% 
  filter(country %in% c("Norway", "Svalbard and Jan Mayen")) %>% 
  count(scientificName) %>% 
  filter(n > 1)

gfid_main_raw2 %>% 
  filter(country %in% c("eSwatini", "Swaziland")) %>% 
  count(scientificName) %>% 
  filter(n > 1)

gfid_check_before <- gfid_main %>% 
    filter(country %in% c("Swaziland", "eSwatini", "Svalbard and Jan Mayen", "Norway")) %>% 
    count(country)

gfid_check_before
  
gfid_main <- gfid_main %>% 
  mutate(
    country = replace_values(
      country,
      "Swaziland" ~ "eSwatini",
      "Svalbard and Jan Mayen" ~ "Norway"
    )
  ) %>% 
  distinct(country, scientificName, .keep_all = TRUE)

gfid_check_after <- gfid_main %>% 
  filter(country %in% c("eSwatini", "Norway")) %>% 
  count(country)

gfid_check_after 

nrow(gfid_main_raw2) - nrow(gfid_main)

#now proceed

gfid_main <- gfid_main %>% 
  dplyr::select(country, countryID, continent, continentID, 
                scientificName,GFIDIdent,
                habitat, degreeOfEstablishment
                )

sum_n <- function (df,cols) {
  
  df %>% 
    group_by({{cols}}) %>% 
    summarize(n = n()) %>%
    arrange(desc(n))
  
  }

sum_n(gfid_main, degreeOfEstablishment)
sum_n(gfid_main, habitat)

# impact data

colnames(gfid_impact)

gfid_impact <- gfid_impact %>% 
  dplyr::select(GFIDIdent,
                impactEconomic, impactEnvironmental_ecol,impactSocial
                )

sum_n(gfid_impact, impactEconomic)
sum_n(gfid_impact, impactEnvironmental_ecol)
sum_n(gfid_impact, impactSocial)

# iucn freshwater

colnames(gfid_iucn)

gfid_iucn <- gfid_iucn %>% 
  dplyr::select(GFIDIdent, 
                freshwater)

sum_n(gfid_iucn, freshwater)

# join them
# interestingly main vs. iucn habitat do not perfectly agree
# ~4.5k fw non-natives, ~1k established and just ~150 less impactful

fw_habitat <- c("FRESHWATER", "FRESHWATER|MARINE", "FRESHWATER|MARINE|MARINE")

gfid <- left_join(left_join(gfid_main, gfid_impact), gfid_iucn) 

gfid %>% filter(habitat %in% fw_habitat & freshwater != TRUE)
gfid %>% filter(freshwater == TRUE & habitat %ni% fw_habitat)

gfid_fw <- gfid %>% 
  filter(habitat %in% fw_habitat | freshwater == TRUE) 

gfid_fw_est <- gfid_fw %>% 
  filter(degreeOfEstablishment == 1)

gfid_fw_inv <- gfid_fw_est %>% 
  filter(impactEconomic == 1 | impactEnvironmental_ecol == 1 | impactSocial == 1)

nrow(gfid_fw)
nrow(gfid_fw_est)
nrow(gfid_fw_inv)

gfid_n <- function (df) {
  
  df %>%
    group_by(country) %>% 
    summarize(nfish = n()) %>% 
    arrange(desc(nfish))
  
  }

gfid <- left_join(
  gfid_n(gfid_fw) %>% rename(nfish_nn = nfish), #non-native
  gfid_n(gfid_fw_inv) %>% rename(nfish_inv = nfish) 
  ) %>% 
  mutate(nfish_inv = replace_na(nfish_inv, 0))

dupe_country_check(gfid)

gfid

# shoal priority fishes
# https://shoal.org/1000-fishes/

shoal <- read_xlsx("inputs/shoal1000fishes_2026-09-12.xlsx")

colnames(shoal)

shoal_long <- shoal %>% 
  filter(str_detect(iucn_threat, "Invasive & other problematic species, genes & diseases")) %>% 
  dplyr::select(scientific_name, realm, country, country_subunit) %>% 
  separate_longer_delim(country, delim = ", ") %>% 
  mutate(country = str_squish(country))

shoal_long %>% 
  count(country, scientific_name) %>% 
  filter(n > 1)

shoal <- shoal %>% 
  filter(str_detect(iucn_threat, "Invasive & other problematic species, genes & diseases")) %>% 
  dplyr::select(scientific_name, realm, country, country_subunit) %>% 
  separate_longer_delim(country, delim = ", ") %>% 
  mutate(country = str_squish(country)) %>% 
  count(country, name = "nfish_shoal", sort = TRUE)

dupe_country_check(shoal)

shoal %>% filter(country == "eSwatini")
shoal %>% filter(country == "Swaziland")
shoal %>% filter(country == "Svalbard and Jan Mayen")

# put them together and prioritize
# first see if country names match

gfid %>% filter(country %ni% shoal$country)
shoal %>% filter(country %ni% gfid$country)

# claude function to fix them
# use world at medium scale as ref

world <- ne_countries(scale = "medium", returnclass = "sf") 

world <- world %>% 
  dplyr::select(country = admin) %>% 
  mutate(area_km2 = as.numeric(units::set_units(st_area(.), km^2)))

setdiff(shoal_long$country, world$country)
setdiff(gfid$country, world$country)

gfid <- fix_country(gfid,  country, ref = world$admin) %>% 
  rename(country_gfid = country_original)

gfid %>% filter(is.na(country))

gfid <- gfid %>% 
  filter(!is.na(country))

gfid %>% filter(country %ni% world$country)

shoal <- fix_country(shoal,  country, ref = world$admin) %>% 
  rename(country_shoal = country_original)

shoal %>% 
  filter(is.na(country))

shoal <- shoal %>% 
  filter(!is.na(country))

shoal %>% filter(country %ni% world$country)

world_fish <- gfid %>% 
  full_join(shoal, by = "country") %>% 
  mutate(across(starts_with("nfish"), ~ replace_na(.x, 0))) %>% 
  arrange(desc(nfish_shoal))

world_fish <- left_join(world_fish, world) %>% 
  relocate(all_of(c("country_gfid", "country_shoal", "area_km2")), .after = country)

world_fish <- world_fish %>% 
  mutate(across(starts_with("nfish"), ~ .x / area_km2, .names = "{.col}_km2")) %>% 
  relocate(nfish_nn_km2, .after = nfish_nn) %>% 
  relocate(nfish_inv_km2, .after = nfish_inv) %>% 
  relocate(nfish_shoal_km2, .after = nfish_shoal)

identical(nrow(world_fish), length(unique(world_fish$country)))

# add continent

world_fish <- add_continent(world_fish) %>% 
  relocate(continent, .before = country)

world_fish

# add fwise country wins as of september 2026 (21 countries)
# win defined as outcome == "Successful" &
# reason %in% c("Ecosystem/habitat restoration", "Native/endangered species", "Unknown"),
# exact definition may be different spelling in column and factors in final cleaned data set

world_fish <- world_fish %>% 
  mutate(fwise_win_2026 = if_else(country %in% fwise_countries_wins_2026, "yes", "no")) %>% 
  relocate(fwise_win_2026, .after = country_shoal)

# ok now let's do some prioritization !

fwise_countries_deja <- world_fish %>% 
  filter(fwise_win_2026 == "yes")

world_fish %>% group_by(continent) %>% summarize(n = n())

# 131 potential countries

countries_next <- world_fish %>% 
  filter(continent %ni% c("europe", "north_america", "oceania")
        & country %ni% fwise_countries_wins_2026    
           )

# split by region

continents_next <- countries_next %>% 
  group_split(continent) %>% 
  set_names(paste0(map_chr(., ~ .x$continent[1]), "_next"))

continents_next <- continents_next[c("latin_america_next", 
                                     "sub_saharan_africa_next",
                                     "north_africa_next", 
                                     "west_central_asia_next", "east_asia_next")]

list2env(continents_next, envir = .GlobalEnv)

nrow(latin_america_next) #41 countires
nrow(sub_saharan_africa_next) #39
nrow(north_africa_next) #39
nrow(west_central_asia_next) #22
nrow(east_asia_next) #24

plot_pairs <- tribble(
  ~y,             ~x,
  "nfish_nn",     "nfish_nn_km2",
  "nfish_inv",    "nfish_inv_km2",
  "nfish_inv",    "nfish_nn",
  "nfish_inv_km2","nfish_nn_km2",
  "nfish_shoal",  "nfish_nn",
  "nfish_shoal",  "nfish_inv",
  "nfish_shoal",  "nfish_nn_km2",
  "nfish_shoal",  "nfish_inv_km2"
)

pdf("outputs/continent_scatterplots.pdf", width = 7, height = 7)

for (nm in names(continents_next)) {
  
  d <- continents_next[[nm]]
  
  for (k in seq_len(nrow(plot_pairs))) {
    
    yv <- d[[plot_pairs$y[k]]]
    xv <- d[[plot_pairs$x[k]]]
    r  <- cor(xv, yv, use = "complete.obs")
    
    plot(
      yv ~ xv,
      pch  = 19,
      xlab = plot_pairs$x[k],
      ylab = plot_pairs$y[k],
      main = paste0(
        nm, " (n = ", sum(complete.cases(xv, yv)), ")\n",
        plot_pairs$y[k], " ~ ", plot_pairs$x[k],
        "   r = ", round(r, 2)
      )
    )
    
  }
  
}

dev.off()

# NOTES ON THE ABOVE PLOTS
# latin_america
# no relationship between nn or inv raw and km2
# inv generally rises with nn (raw and km2)
# shoal generally rises with nn raw and but NOT with inv raw
# no relationship between shoal and km2
# 
# sub_saharan_africa
# no relationship between nn or inv raw and km2
# inv strongly rises with nn (raw and km2)
# shoal weakly rises with nn and inv raw
# no relationship between shoal and km2
# 
# north_africa
# no relationship between nn or inv raw and km2
# inv rises with nn (weakly in raw and strongly in km2)
# shoal weakly rises with nn and inv raw
# no relationship between shoal and km2
# 
# west_central_asia
# no relationship between nn or inv raw and km2
# inv strongly rises with nn (raw and km2)
# shoal weakly rises with nn and inv raw
# no relationship between shoal and km2
# 
# east_asia
# no relationship between nn or inv raw and km2
# inv strongly rises with nn (raw and km2)
# shoal generally rises with nn raw and but NOT with inv raw
# no relationship between shoal and km2

# given the above we should prioritize countries across regions that...
# EITHER have lots of priority fishes threatened by invasives 
# OR lots of invasive fish (ecological, social, or economic - impact)
# NOTE THAT FOR EXAMPLE CHILE HAS 30 NON-NATIVE FISH YET 0 WITH IMPACT... SEEMS IMPLAUSIBLE
# likely due to geographical bias in impact data
# so incorporate that into prioritization too...
# if discrepancy between nn and inv is huge, consider nn as well...

continents_prioritize <- continents_next %>% 
  map(
    ~ .x %>% 
      dplyr::select(continent:country_shoal, nfish_shoal, nfish_inv, nfish_nn, geometry) %>% 
      mutate(
        diff_inv_nn     = nfish_inv - nfish_nn,
        prop_nn_inv     = if_else(nfish_nn == 0, NA_real_, round(nfish_inv / nfish_nn, 2)),
        shoal_inv_sum   = nfish_shoal + nfish_inv,
        shoal_inv_ratio = if_else(nfish_inv == 0, NA_real_, nfish_shoal / nfish_inv)
      ) %>% 
      relocate(geometry, .after = last_col())
  )

continents_prioritize <- continents_prioritize %>% 
  set_names(str_replace(names(.), "_next$", "_prioritize"))

list2env(continents_prioritize, envir = .GlobalEnv)

# looking at each region there are clear cut offs to prioritize across the three criteria

View(continents_prioritize[[1]])
View(continents_prioritize[[2]])
View(continents_prioritize[[3]])
View(continents_prioritize[[4]])
View(continents_prioritize[[5]])

# use claude function to summarize the cutoffs assessed by eye
# check by hand confirmed that this is correct

prioritize_fwise_future <- function(continent_name = NULL, min_shoal = NA, min_inv = NA, min_nn = NA,
                                    data = continents_prioritize) {
  
  d <- if (is.data.frame(data)) {
    
    data
    
  } else {
    
    if (is.null(continent_name)) stop("give a continent_name when passing the list")
    
    nm <- if (continent_name %in% names(data)) continent_name else paste0(continent_name, "_prioritize")
    
    if (!nm %in% names(data)) stop("no element named '", continent_name, "' in data")
    
    data[[nm]]
    
  }
  
  if (all(is.na(c(min_shoal, min_inv, min_nn)))) stop("set at least one threshold")
  
  flag <- function(x, min) {
    
    if (is.na(min)) return(0L)
    
    as.integer(!is.na(x) & x >= min)
    
  }
  
  d %>% 
    mutate(
      shoal_priority    = flag(nfish_shoal, min_shoal),
      gfid_inv_priority = flag(nfish_inv,   min_inv),
      gfid_nn_priority  = flag(nfish_nn,    min_nn),
      n_priority_reason = shoal_priority + gfid_inv_priority + gfid_nn_priority
    ) %>% 
    filter(n_priority_reason > 0) %>% 
    dplyr::select(
      continent:country_shoal,
      nfish_shoal,
      nfish_gfid_inv = nfish_inv,
      nfish_gfid_nn  = nfish_nn,
      shoal_priority,
      gfid_inv_priority,
      gfid_nn_priority,
      n_priority_reason,
      geometry
    ) %>% 
    arrange(desc(n_priority_reason), desc(nfish_shoal))
  
}

latin_america_priority <- prioritize_fwise_future(continent_name = "latin_america", 
                                                  data = latin_america_prioritize,
                                                  min_shoal = 12,
                                                  min_inv = 34,
                                                  min_nn = 30
                                                  )

sub_saharan_africa_priority <- prioritize_fwise_future(continent_name = "sub_saharan_africa", 
                                                  data = sub_saharan_africa_prioritize,
                                                  min_shoal = 20
                                                  )

north_africa_priority <- prioritize_fwise_future(continent_name = "north_africa", 
                                                  data = north_africa_prioritize,
                                                  min_shoal = 6,
                                                  min_inv = 9,
                                                  min_nn = 27
                                                  )

west_central_asia_priority <- prioritize_fwise_future(continent_name = "west_central_asia", 
                                                  data = west_central_asia_prioritize,
                                                  min_shoal = 43,
                                                  min_inv = 15
                                                  )

east_asia_priority <- prioritize_fwise_future(continent_name = "east_asia", 
                                                  data = east_asia_prioritize,
                                                  min_shoal = 50,
                                                  min_inv = 32,
                                                  min_nn = 102
                                                  )

# put them together and remove shoal and gfid names because all same here
# utter coincidence that the number of future priorities is EXACTLY SAME as fwise wins already...
# 21 is a magic number donc?

fwise_future_priority_countries <- bind_rows(
  latin_america_priority,
  sub_saharan_africa_priority,
  north_africa_priority,
  west_central_asia_priority,
  east_asia_priority,
  ) %>% 
  dplyr::select(-country_gfid,-country_shoal)

length(unique(fwise_future_priority_countries$country)) #21 countries
sum(fwise_future_priority_countries$nfish_shoal) #501 shoal priority fish iucn threatened by invasives
sum(fwise_future_priority_countries$nfish_gfid_inv) #205 gfid known invasive fish (-impact)
sum(fwise_future_priority_countries$nfish_gfid_nn) #837 gfid non-native fish

# export country csvs

continent_order <- c("north_america", "latin_america", 
                     "sub_saharan_africa", "north_africa", 
                     "europe",
                     "west_central_asia", "east_asia", 
                     "oceania")

fwise_countries_deja <- fwise_countries_deja %>% 
  rename(nfish_gfid_inv = nfish_inv, nfish_gfid_nn  = nfish_nn) %>% 
  dplyr::select(colnames(fwise_future_priority_countries)[1:5]) %>% 
  arrange(match(continent, continent_order), desc(nfish_shoal))

write_csv(fwise_countries_deja, "./outputs/fwise_successful_countries_2026.csv")

fwise_countries_prochain <- fwise_future_priority_countries %>% 
  dplyr::select(-geometry)

write_csv(fwise_countries_prochain, "./outputs/fwise_priority_countries_shoal_gfid.csv")

# map time

deja  <- fwise_countries_deja$country
future <- fwise_future_priority_countries$country

world_status <- world %>% 
  mutate(
    status = case_when(
      country %in% deja   ~ "Countries with successful restorations of invaded freshwaters",
      country %in% future ~ "Priority countries for future restorations",
      .default = NA_character_
    )
  )

sum(!is.na(world_status$status))

lab_deja   <- "Countries with successful restorations of invaded freshwaters"
lab_future <- "Priority countries for future restorations"

fwise_now_next <- ggplot(world_status) +
  geom_sf(aes(fill = status, colour = status), linewidth = 0.2) +
  scale_fill_manual(
    values = c(lab_deja = "#54A1CB", lab_future = "#EEBF54") %>% 
      setNames(c(lab_deja, lab_future)),
    na.value = "gray92",
    breaks = c(lab_deja, lab_future),
    name = NULL
  ) +
  scale_colour_manual(
    values = c("#0072B2", "#E69F00") %>% 
      setNames(c(lab_deja, lab_future)),
    na.value = "gray62",
    breaks = c(lab_deja, lab_future),
    name = NULL
  ) +
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.text = element_text(size = 15),
    legend.key.spacing.y = unit(6, "pt")
  )

fwise_now <- fwise_now_next %+% 
  mutate(world_status, status = if_else(status == lab_deja, status, NA_character_))

fwise_next <- fwise_now_next %+% 
  mutate(world_status, status = if_else(status == lab_future, status, NA_character_))

fwise_now
fwise_next
fwise_now_next

plots <- list(
  fwise_now      = fwise_now,
  fwise_next     = fwise_next,
  fwise_now_next = fwise_now_next
)

for (nm in names(plots)) {
  
  ggsave(paste0("outputs/", nm, "_600dpi.png"), plots[[nm]], width = 14, height = 8, dpi = 600)
  
  ggsave(paste0("outputs/", nm, "_900dpi.png"), plots[[nm]], width = 14, height = 8, dpi = 900)
  
  ggsave(paste0("outputs/", nm, ".pdf"), plots[[nm]], width = 14, height = 8, device = cairo_pdf)
  
}
#et c'est enfin fini @ 600 degreez