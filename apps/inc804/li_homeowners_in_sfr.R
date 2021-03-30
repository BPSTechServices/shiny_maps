##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, data.table, sf, mapview, tigris)
options(tigris_use_cache = T); options(tigris_class = "sf"); options("scipen"=100, "digits"=4) # options(scipen = 999)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("N:/work/district_planning/Economic Development/NICK/projects/r/chasr/R/chasr_development_functions.R")


# get_li_sfr_tenure('4159000', chas.plc.2015.t18a, chas.plc.2015.t18b, chas.plc.2015.t18c)

chas.tct.t18a <- cleanup_chas_tract(read_csv('E:/data_files/CHAS/data/tract/2013thru2017-140-csv/140/Table18A.csv'))
chas.tct.t18b <- cleanup_chas_tract(read_csv('E:/data_files/CHAS/data/tract/2013thru2017-140-csv/140/Table18B.csv'))
chas.tct.t18c <- cleanup_chas_tract(read_csv('E:/data_files/CHAS/data/tract/2013thru2017-140-csv/140/Table18C.csv'))

chas.plc.2015.t18a <- cleanup_chas_plc(read_csv('E:/data_files/CHAS/data/place/2013thru2017-160-csv/160/Table18A.csv'))
chas.plc.2015.t18b <- cleanup_chas_plc(read_csv('E:/data_files/CHAS/data/place/2013thru2017-160-csv/160/Table18B.csv'))
chas.plc.2015.t18c <- cleanup_chas_plc(read_csv('E:/data_files/CHAS/data/place/2013thru2017-160-csv/160/Table18C.csv'))

msa_tracts <- chas.tct.t18a %>%
  mutate(stcnty = paste0(st,cnty_plc)) %>%
  filter(stcnty %in% c('41005', '41009', '41051', '41067', '41071', '53011', '53059')) %>%
  pull(geoid2) 

msa_tracts.sf <- rbind(tigris::tracts(state = "OR", cb = TRUE), tigris::tracts(state = "WA", cb = TRUE)) %>%
  filter(substr(GEOID, 1, 5) %in% c('41005', '41009', '41051', '41067', '41071', '53011', '53059'))

low_income_sfr <- msa_tracts %>%
  map_df(get_li_sfr_tenure, chas_t18a = chas.tct.t18a, chas_t18b = chas.tct.t18b, chas_t18c = chas.tct.t18c) %>%
  mutate(GEOID = msa_tracts,
         total_hh = total_rent + total_own_outright + total_own_mortgage,
         total_own_sfr_li = own_mortgage_sfr_li + own_outright_sfr_li,
         total_rent_sfr_li = rent_sfr_li,
         share_hh_total_own_sfr_li = round(total_own_sfr_li / total_hh * 100, 1),
         share_sfr_total_own_sfr_li = round(total_own_sfr_li / sfr_total * 100, 1),
         share_hh_total_rent_sfr_li = round(total_rent_sfr_li / total_hh * 100, 1),
         share_sfr_total_rent_sfr_li = round(total_rent_sfr_li / sfr_total * 100, 1),) %>% 
  select(GEOID, total_hh, sfr_total, total_own_sfr_li:share_sfr_total_rent_sfr_li)

# msa_tracts.sf %>% 
#   left_join(., low_income_sfr, by = "GEOID") %>% 
#   select(GEOID, TRACT = NAME, total_hh:share_sfr_total_own_sfr_li) %>% 
#   mapview(zcol = "share_sfr_total_own_sfr_li")

pdxlisfr <- get_li_sfr_tenure('4159000', chas.plc.2015.t18a, chas.plc.2015.t18b, chas.plc.2015.t18c) %>%
  mutate(GEOID = '4159000',
         total_hh = total_rent + total_own_outright + total_own_mortgage,
         total_own_sfr_li = own_mortgage_sfr_li + own_outright_sfr_li,
         total_rent_sfr_li = rent_sfr_li,
         share_hh_total_own_sfr_li = round(total_own_sfr_li / total_hh * 100, 1),
         share_sfr_total_own_sfr_li = round(total_own_sfr_li / sfr_total * 100, 1),
         share_hh_total_rent_sfr_li = round(total_rent_sfr_li / total_hh * 100, 1),
         share_sfr_total_rent_sfr_li = round(total_rent_sfr_li / sfr_total * 100, 1),) %>% 
  select(GEOID, total_hh, sfr_total, total_own_sfr_li:share_sfr_total_rent_sfr_li)

saveRDS(pdxlisfr, "apps/inc804/data/pdxlisfr_2017.rds")

msa_tracts.sf %>% 
  left_join(., low_income_sfr, by = "GEOID") %>%
  select(GEOID, TRACT = NAME, total_hh:share_sfr_total_rent_sfr_li) %>%
  st_transform(4326) %>%
  saveRDS(., "apps/inc804/data/low_income_sfr_2017.rds")

# rio::export(low_income_sfr_homeowners, "low_income_sfr_homeowners_inc177_20200210.xlsx")

# mapshot(msa_tracts.sf %>% 
#           left_join(., low_income_sfr_homeowners, by = "GEOID") %>% 
#           select(GEOID, TRACT = NAME, total_hh:share_sfr_total_own_sfr_li) %>% 
#           mapview(zcol = "share_sfr_total_own_sfr_li", layer.name = "% SFR Occupied by LI Owners"), 
#         "Low-income (0-80% MFI) single-family homeowners.html")
