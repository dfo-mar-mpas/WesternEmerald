##Anlaysis of RV data

# Load libraries ----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(ggspatial)
library(viridis)
library(purrr)
library(furrr)
library(progressr)

#source data scripting funciton
source("code/webca_fish_trend.R")

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load filtered RV data from 03_filteredRVStations.R
load("data/rv_data_processed.RData")

rv_df <- rv_df%>%
         filter(YEAR>1984,!is.na(distance_category))%>% # in 1984 the gear was changed. There is a significant amount of work required to 'adjust' for the catability differnces so to avoid those, we just remove the pre-1985 data
         mutate(period = case_when(YEAR < 1993 ~ "pre-collapse", #periods taken from Nancy's paper
                                   YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
                                   YEAR > 2005 ~ "recent"),
                dist_200 = ifelse(distance_category!=0,"outside","inside"), #aggregate the distance categories
                dist_100 = case_when(distance_category!=0 & distance_category>100 ~ NA,
                                     distance_category!=0 & distance_category<200 ~ "outside",
                                     TRUE ~ "inside"),
                dist_50 = case_when(distance_category!=0 & distance_category>50 ~ NA,
                                     distance_category!=0 & distance_category<100 ~ "outside",
                                     TRUE ~ "inside"),
                dist_10 = case_when(distance_category!=0 & distance_category>10 ~ NA,
                                    distance_category!=0 & distance_category<50 ~ "outside",
                                    TRUE ~ "inside"))

#'common' grounfish species -- from Shackell et al. 2021
focal_sp <- read.csv("data/focal_sp.csv")%>%
            mutate(bioclass_comp = ifelse(SCI_NAME == "AMBLYRAJA RADIATA",FALSE,TRUE))# there are no AMBLYRAJA RADIATA detected within the MPA for classification 

rv_fish <- rv_df%>%
           filter(SCI_NAME %in% focal_sp$SCI_NAME)

## run bootstrap analysis on the rv data

# Set up future for parallel processing
plan(multisession)  # Adjust this based on your machine

# Initialize progress handlers
handlers(global = TRUE)  # Use the default console handler

#This is as optimized as I can make it, but it still takes time. 
df_1 <- rv_df %>%
  filter(SCI_NAME %in% focal_sp$SCI_NAME, 
         !is.na(distance_category),
         classification %in% c("WSS/Outer BoF", "WSS: Banks/Inner BoF")) %>%
  data.frame() %>%
  mutate(period = case_when(YEAR < 1993 ~ "pre-collapse",
                            YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
                            YEAR > 2005 ~ "recent")) %>%
  select(species = SCI_NAME, classification, period, distance_category, TOTWGT, TOTNO) %>%
  group_by(species, distance_category, classification) %>%
  group_modify(~ {
    with_progress({ # this only sort of works - at least tells you if something is frozen
      boot_fun(.x)
    })
  }) %>%
  ungroup() %>%
  data.frame()

save(df_1,file="output/bootstrapped_differences2.RData")
