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
            left_join(.,rv_df%>%
                        data.frame()%>%
                        distinct(SCI_NAME,.keep_all = TRUE)%>%
                        dplyr::select(SCI_NAME,COMM))%>%
             mutate(comm = tolower(COMM), #clean up the names
                    comm = str_to_title(comm),
                    comm = case_when(grepl("ERINACEA",COMM) ~ "Little Skate",
                                     grepl("GOOSEFISH",COMM) ~ "Monkfish",
                                     grepl("LIMANDA",COMM) ~ "Yellowtail Flounder",
                                     grepl("UNSEPARATED",COMM) ~ "Redfish",
                                     grepl("Urophycis",COMM) ~ "Longfin Hake",
                                     grepl("SQUIRREL",COMM) ~ "Red Hake",
                                     TRUE ~ comm))

##example of how to pull some plots

cod_plots <- webca_fish_trend(rv_df,species="GADUS MORHUA","Atlantic cod")

cod_plots$point_plot #the plot you can make better

cod_plots$sp_df #the data you could use to make the plot better!

rv_fish <- rv_df%>%
           filter(SCI_NAME %in% focal_sp$SCI_NAME)

## run bootstrap analysis on the rv data

# # Set up future for parallel processing
# plan(multisession)  # Adjust this based on your machine
# 
# # Initialize progress handlers
# handlers(global = TRUE)  # Use the default console handler
# 
# #This is as optimized as I can make it, but it still takes time. 
# boot_dat <- rv_df %>%
#   filter(SCI_NAME %in% focal_sp$SCI_NAME, 
#          !is.na(distance_category),
#          classification %in% c("WSS/Outer BoF", "WSS: Banks/Inner BoF")) %>%
#   data.frame() %>%
#   mutate(period = case_when(YEAR < 1993 ~ "pre-collapse",
#                             YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
#                             YEAR > 2005 ~ "recent")) %>%
#   select(species = SCI_NAME, classification, period, distance_category, TOTWGT, TOTNO) %>%
#   group_by(species, distance_category, classification) %>%
#   group_modify(~ {
#     with_progress({
#       boot_fun(.x)
#     })
#   }) %>%
#   ungroup() %>%
#   data.frame()%>%
#   left_join(focal_sp%>%dplyr::select(species=SCI_NAME,comm))
# 
# save(boot_dat,file="output/bootstrapped_differences.RData")

#load the bootstrap analysis outputs
load("output/bootstrapped_differences.RData")

boot_df <- boot_dat%>%
           group_by(species,distance_category,classification,period)%>%
           summarise(mean_diff_wgt = mean(prec_wgt_diff,na.rm=T),
                     lower_diff_wgt = quantile(prec_wgt_diff,0.1,na.rm=T),
                     upper_diff_wgt = quantile(prec_wgt_diff,0.9,na.rm=T),
                     mean_diff_cnt = mean(prec_count_diff,na.rm=T),
                     lower_diff_cnt = quantile(prec_count_diff,0.1,na.rm=T),
                     upper_diff_cnt = quantile(prec_count_diff,0.9,na.rm=T))%>%
           ungroup()%>%
           data.frame()

 ggplot(boot_df%>%filter(distance_category == 0), aes(x = mean_diff_wgt / 100, y = species, shape = period, fill = value)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~classification, scales = "free_x") +
  scale_shape_manual(values = c(21, 22)) +  # Use open shapes that accept fill
  scale_fill_manual(values = c("Inside" = "blue", "Outside" = "red")) +  # Fill colors for value
  scale_x_continuous(labels = scales::percent_format()) +  # Format x-axis as a percentage
  labs(x = "% Change from Pre-collapse", y = "Distance (km) from refuge", 
       fill = "Value (Inside/Outside)", shape = "Period") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"), legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(shape = 21)),  # Ensure shapes appear in one legend
         shape = guide_legend(override.aes = list(fill = "black"))) 
