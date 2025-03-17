##Analysis of RV data

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
source("code/webca_fish_trend2.0.R")
source("code/theme_custom.R")

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load filtered RV data from 03_filteredRVStationsHV.R
load("data/rv_data_processed2.0.RData")

rv_df <- rv_df%>%
  filter(YEAR>1984,!is.na(distance_category))%>% # in 1984 the gear was changed. There is a significant amount of work required to 'adjust' for the catability differnces so to avoid those, we just remove the pre-1985 data
  mutate(period = case_when(YEAR < 1993 ~ "pre-collapse", #periods taken from Nancy's paper
                            YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
                            YEAR > 2005 ~ "recent"),
         #aggregate the distance categories
         dist_100 = case_when(distance_category!=0 & distance_category>100 ~ NA,
                              distance_category!=0 & distance_category<200 ~ "outside",
                              TRUE ~ "inside"),
         dist_50 = case_when(distance_category!=0 & distance_category>50 ~ NA,
                             distance_category!=0 & distance_category<100 ~ "outside",
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

#Make some plots for the ploter

#Haddock
haddock_plot <- webca_fish_trend(x=rv_df,species ="MELANOGRAMMUS AEGLEFINUS",title="Haddock",poster=FALSE,point_size = 10)

#save plots

ggsave("output/haddock_comp_plot.png",haddock_plot$comp_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/haddock_point_plot.png")  

ggsave("output/haddock_comp_plot.png",haddock_plot$comp_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/haddock_comp_plot.png")  

ggsave("output/haddock_diff_plot.png",haddock_plot$diff_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/haddock_diff_plot.png")   

#silverhake
silverhake_plot <- webca_fish_trend(x=rv_df,species="MERLUCCIUS BILINEARIS",title="Silver hake",poster=FALSE,point_size = 10)

ggsave("output/silverhake_comp_plot.png",silverhake_plot$comp_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/silverhake_comp_plot.png")  

ggsave("output/silverhake_diff_plot.png",silverhake_plot$diff_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/silverhake_diff_plot.png")   

#red hake - shows a different outcome
redhake_plot <- webca_fish_trend(x=rv_df,species="UROPHYCIS CHUSS",title="Red hake",poster=FALSE,point_size = 10)

ggsave("output/redhake_comp_plot.png",redhake_plot$comp_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/redhake_comp_plot.png")   

ggsave("output/redhake_diff_plot.png",redhake_plot$diff_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/redhake_diff_plot.png")   

#thorny hake - shows a different outcome
thornyskate_plot <- webca_fish_trend(x=rv_df,species="AMBLYRAJA RADIATA",title="Thorny skate",poster=FALSE,point_size = 10)

ggsave("output/thornyskate_comp_plot.png",thornyskate_plot$comp_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/thornyskate_comp_plot.png")   

ggsave("output/thornyskate_diff_plot.png",thornyskate_plot$diff_plot+theme_big(),width=24,height=18,units = "in",dpi=600)
knitr::plot_crop("output/thornyskate_diff_plot.png")   

##example of how to pull some plots

cod_plots <- webca_fish_trend(rv_df,species="GADUS MORHUA",title = "Atlantic cod")

cod_plots$point_plot #the plot you can make better

cod_plots$sp_df #the data you could use to make the plot better!

rv_fish <- rv_df%>%
  #filter(SCI_NAME %in% focal_sp$SCI_NAME)
  filter(SCI_NAME == "MELANOGRAMMUS AEGLEFINUS",
         !is.na(dist_50),
         classification %in% c("WSS/Outer BoF","WSS: Banks/Inner BoF"))

boot_fish <- boot_dat%>%
  filter(species=="MELANOGRAMMUS AEGLEFINUS",
         distance_category == c(0,50))

boot_fish%>%
  group_by(classification,period,distance_category)%>%
  summarise(meanw=mean(TOTWGT,na.rm=T),
            meanc=mean(TOTNO,na.rm=T))

rv_fish%>%
  data.frame()%>%
  group_by(classification,period,dist_50)%>%
  summarise(meanw=mean(TOTWGT,na.rm=T),
            meanc=mean(TOTNO,na.rm=T))%>%
  ungroup()%>%data.frame()

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
  mutate(wgt_ratio = wgt_target/wgt_reference,
         count_ratio = count_target/count_reference)%>%
  group_by(species,distance_category,classification,period)%>%
  summarise(mean_wgt_ratio = mean(wgt_ratio,na.rm=T),
            sd_wgt_ratio = sd(wgt_ratio,na.rm=T),
            
            mean_count_ratio = mean(count_ratio,na.rm=T),
            sd_count_ratio = sd(count_ratio,na.rm=T))%>%
  ungroup()%>%
  data.frame()%>%
  filter(!is.na(mean_wgt_ratio),!is.na(mean_count_ratio))

test_df <- boot_df%>%filter(distance_category %in% c(0,50,100),
                            #species %in% c("MELANOGRAMMUS AEGLEFINUS","GADUS MORHUA")
)%>%
  mutate(distance_category = factor(distance_category))


ggplot(test_df, aes(x = mean_wgt_ratio, y = distance_category, group = interaction(distance_category, period))) +
  geom_errorbar(aes(xmin = mean_wgt_ratio - sd_wgt_ratio, xmax = mean_wgt_ratio + sd_wgt_ratio),
                width = 0, position = position_dodge(width = 0.3)) +  # Dodging the error bars horizontally
  geom_point(fill = "white", size = 3, shape = 21, position = position_dodge(width = 0.3)) +  # Dodging the first points horizontally
  geom_point(aes(fill = distance_category, alpha = period), size = 3, shape = 21, position = position_dodge(width = 0.3)) +  # Dodging the second points horizontally
  facet_grid(species ~ classification) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  scale_x_continuous(limits = c(NA, NA)) +  # Fill in the limits here as needed
  labs(x = "Mean Difference Weight (scaled)", y = "Buffer (km)",
       fill = "Period", shape = "") +
  geom_vline(xintercept = 1, lty = 2, lwd = 0.4) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white"))



boot_df <- boot_dat%>%
  group_by(species,distance_category,classification,period)%>%
  summarise(mean_diff_wgt = mean(prec_wgt_diff,na.rm=T)/100,
            sd_diff_wgt = sd(prec_wgt_diff,na.rm=T)/100, 
            lower_diff_wgt = quantile(prec_wgt_diff,0.1,na.rm=T)/100,
            upper_diff_wgt = quantile(prec_wgt_diff,0.9,na.rm=T)/100,
            
            mean_diff_cnt = mean(prec_count_diff,na.rm=T)/100,
            sd_diff_cnt = sd(prec_count_diff,na.rm=T)/100, 
            lower_diff_cnt = quantile(prec_count_diff,0.1,na.rm=T),
            upper_diff_cnt = quantile(prec_count_diff,0.9,na.rm=T))%>%
  ungroup()%>%
  data.frame()


test_df <- boot_df%>%filter(distance_category %in% c(0,50,100),classification =="WSS/Outer BoF",
                            species %in% c("MELANOGRAMMUS AEGLEFINUS","GADUS MORHUA"))%>%
  mutate(distance_category = paste(distance_category,"(km)",sep=" "))

dodge_width <- 0.3

ggplot(test_df, aes(x = mean_diff_wgt, y = distance_category)) +
  geom_line(aes(group = distance_category,col=distance_category), linewidth = 0.5,show.legend = FALSE) +
  geom_point(fill="white",size=3,shape=21)+
  geom_point(aes(fill = distance_category, alpha=period),size=3,shape=21)+
  facet_grid(species~.)+
  scale_alpha_manual(values = c(0.2,0.8))+
  #scale_fill_manual(values = c("white", "black")) + # Fill: white for one period, black for the other
  labs(x = "Mean Difference Weight (scaled)", y = "Buffer (km)",
       fill = "Buffer (km)", shape = "") +
  geom_vline(xintercept=1,lty=2,lwd=0.4)+
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_rect(fill="white"))

ggplot(test_df, aes(x = mean_diff_wgt, y = distance_category, group = interaction(distance_category, period))) +
  geom_errorbar(aes(xmin = mean_diff_wgt - sd_diff_wgt, xmax = mean_diff_wgt + sd_diff_wgt),
                width = 0, position = position_dodge(width = 0.3)) +  # Dodging the error bars horizontally
  geom_point(fill = "white", size = 3, shape = 21, position = position_dodge(width = 0.3)) +  # Dodging the first points horizontally
  geom_point(aes(fill = distance_category, alpha = period), size = 3, shape = 21, position = position_dodge(width = 0.3)) +  # Dodging the second points horizontally
  facet_grid(species ~ .) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  scale_x_continuous(limits = c(NA, NA)) +  # Fill in the limits here as needed
  labs(x = "Mean Difference Weight (scaled)", y = "Buffer (km)",
       fill = "Period", shape = "") +
  geom_vline(xintercept = 1, lty = 2, lwd = 0.4) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white"))







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
