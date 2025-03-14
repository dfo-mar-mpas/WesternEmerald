#load libraries
library(tidyverse)
library(sf)
library(raster)
library(MarConsNetData)
library(rnaturalearth)
library(scales)
library(patchwork)
library(ggspatial)
library(viridis)
library(ggimage)
library(rphylopic)
library(R.matlab)
library(zoo)
library(stars)

sf_use_s2(FALSE)

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

maritimes_network <- data_draft_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()%>%
  dplyr::select(Classification_E,SiteName_E)%>%
  rename(status=Classification_E,name=SiteName_E)%>%
  st_make_valid()

webca_poly <- maritimes_network%>%
  filter(name == "Western/Emerald Banks Marine Refuge")

#load the CMIP extractions. This data is from the Lewis et al analysis so is inclusive of 2022 Code below. 
load("output/cmip_extracts/cmip_rcp_2-6_extract.RData")
cmip_26 <- data_extract_output
rm(data_extract_output)

load("output/cmip_extracts/cmip_rcp_8-5_extract.RData")
cmip_85 <- data_extract_output
rm(data_extract_output)

cmip_df <- rbind(cmip_26,cmip_85)%>%
  group_by(climate_proj,year,month)%>%
  summarise(month_mean=weighted.mean(temp,cell_area))%>%
  ungroup()%>%
  group_by(climate_proj,year)%>%
  summarise(mean=mean(month_mean),
            se=sd(month_mean)/sqrt(12),
            sd=sd(month_mean))%>%
  ungroup()%>%
  data.frame()%>%
  mutate(climate_proj_fact = ifelse(climate_proj == 2.6,"RCP 2.6","RCP 8.5"))

cmip_df_smoothed <- cmip_df %>%
  group_by(climate_proj_fact) %>%
  arrange(year) %>%
  mutate(
    rolling_mean = rollmean(mean, k = 5, fill = NA, align = "center")
  ) %>%
  ungroup()

webca_timeseries <- ggplot(cmip_df_smoothed, aes(x = year, y = mean, color = climate_proj_fact, fill = climate_proj_fact)) +
  annotate("rect", # Add baseline period shading
           xmin = 2015,
           xmax = 2025,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.5,
           fill = "gray75") +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = rolling_mean), linewidth = 1) +
  theme_bw() +
  facet_wrap(~climate_proj_fact, ncol = 2) +
  labs(
    y = "Temperature (°C) ± se",
    x = ""
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    strip.background = element_rect(fill="white"),
    legend.position = "none"
  )+
  scale_fill_manual(values=c("cornflowerblue","orange"))+
  scale_color_manual(values=c("cornflowerblue","orange"))

ggsave("output/webca_temp_timeseries.png",webca_timeseries,height=6,width=6,units="in",dpi=600)


##CODE TO DO THE CMIP EXTRACTIONS

#Temperature timeseries ------

#store root directory
#root_dir <- getwd()

# setwd("c:/Users/stanleyr/Documents/Github/MAR_thermal_emerg/")
# 
# fls<-c(list.files("data/climate_projections/2.6/", full.names=T),list.files("data/climate_projections/8.5/",full.names=T)) ##climate projections for RCP 2.6; run again for the 4.5 folder
# fls <- fls[!grepl("CNRM",fls)] #remove the CNRM model from the analysis
# fls <- fls[!grepl("GFDL",fls)] #remove the GFDL model from the analysis
# 
# data <- readMat(fls[1]) #only do this for one projection because they are the same extent
# names(data) <- "datout" #this will update the ones with 'outt'
# 
# bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
# cmip_proj <- bdata@crs #projection of the CMIP
# 
# network_sp <- webca_poly%>%
#               st_transform(st_crs(bdata))%>%
#               as_Spatial()
# 
# network_extent <- extent(network_sp)
# 
# network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE) 
# network_raster_mask[network_raster_mask == 0] <- NA
# 
# out_masks <- network_raster_mask
# out_networks <- network_sp
# 
# emmission_scenarios <- c("2.6","8.5")

#loop over the modeled scenarios
# for(i in emmission_scenarios){
#   
#   cmip_files <- fls[grep(i,fls)]
#   
#   message(paste0("Loading data for RCP ",i))
#   
#   #read in data and create brick (month (12) x years (86))
#   for(j in 1:3){
#     
#     temp <- readMat(cmip_files[j])
#     names(temp) <- "datout"
#     temp <- brick(temp$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
#     assign(paste0("emmisiondata_",j),temp)
#     rm(temp)
#     
#   }
#   
#   #get the emsemble for each month/year combination
#   
#   message("Conducting model averaging to generate an ensemble.")
#   
#   ensemble.list <- list()
#   for(s in 1:dim(emmisiondata_1)[3]){
#     
#     temp <- stack(emmisiondata_1[[s]],emmisiondata_2[[s]],emmisiondata_3[[s]])
#     ensemble <- calc(temp,fun=mean,na.rm=T) # note that 'AWI' doesn't apply a land mask, so this will give us more data for coastal MPAs that are otherwise missed by the HAD/ISPL because of that 0.25 degree land mask
#     ensemble.list[[s]] <- ensemble
#     
#   }
#   
#   
#   #convert to a raster brick
#   bdata <- ensemble.list%>%stack()%>%brick(.,xmx=-41,ymn=38,ymx=85,crs=latlong)
#   cmip_proj <- proj4string(bdata)
#   
#   ## do the extracts on the ensembles ---
#   
#   message(paste0("Working on the climate extractions for RCP ",i))
#   
#   #loop through the network to extract the data. 
#   cmip_extracts<-list()
#   cmip_extracts_shapes <- list()
#   climate_proj <- i
#   mod <- "Ensemble"
#   
#     #load the depth-adjusted network (sf)
#     network_sf <- webca_poly%>%
#       mutate(area=st_area(.))%>%
#       st_transform(st_crs(bdata))
#     
#     network_raster_mask <- projectRaster(network_raster_mask,crs=cmip_proj)
#     
#     #extent of the network
#     network_extent <- extent(network_raster_mask)
#     
#     #apply the mask to the entire raster stack using raster  
#     bdata_processed <- bdata%>%
#       crop(.,network_extent,snap="out")%>%
#       raster::mask(.,network_raster_mask)
#     
#     spec = "all"
#     
#     #now use sf and stars to extract the data
#     data_extract <- bdata_processed%>% #crop the raster to the extent of the PA
#       st_as_stars()%>% #convert to stars raster brick
#       st_as_sf()%>% #convert to sf dataframe
#       st_transform(utm)%>% #convert to planar coordinates for a more appropriate overlay
#       st_intersection(.,network_sf%>%st_transform(utm)%>%dplyr::rename(site_area=area))%>%
#       st_transform(st_crs(network_raster_mask))%>% #transform back to the raster brick projection
#       mutate(cell_area=as.vector(st_area(.)/1000/1000))%>% #calculate the area of the raster cells that are overlaid 
#       gather(.,"layer","temp",starts_with("layer."))%>% #convert to the long-form    
#       mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
#              year=rep(2015:2100,each=length(layer)/86),
#              species=spec,
#              mod=mod,
#              climate_proj=climate_proj)%>%
#       dplyr::select(mod,climate_proj,species,year,month,name,site_area,cell_area,temp,geometry)%>%
#       suppressWarnings() #"attribute variables are assumed to be spatially constant throughout all geometries" - will clutter the output so it is suppressed
#     
#     #save the geometry information which is duplicated for each iteration (month x year)
#     data_extract_shape <- data_extract%>%
#       filter(month==1,year==2015)
#     
#     #output only the extracted information
#     data_extract_output <- data_extract%>%
#       data.frame()%>%
#       dplyr::select(-geometry)
#     
#     #save the output
#     
#     save(data_extract_output,file=paste0(root_dir,"/output/cmip_extracts/cmip_rcp_",gsub("\\.","-",i),"_extract.RData"))
#     save(data_extract_shape,file=paste0(root_dir,"/output/cmip_extracts/cmip_rcp_",gsub("\\.","-",i),"_shape.RData"))
# } #end of emmission loop

