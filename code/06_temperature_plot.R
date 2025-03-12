### Code to generate temperature plot based on the RV data

#load libraries ----
library(tidyverse)
library(sf)
library(zoo) 

#load filtered RV data from 03_filteredRVStationsHV.R
load("data/rv_data_processed2.0.RData")

rv_df <- rv_df%>%
  filter(!is.na(distance_category),
         distance_category<100)%>% # in 1984 the gear was changed. There is a significant amount of work required to 'adjust' for the catability differnces so to avoid those, we just remove the pre-1985 data
  mutate(period = case_when(YEAR < 1993 ~ "pre-collapse", #periods taken from Nancy's paper
                            YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
                            YEAR > 2005 ~ "recent"))

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


#get temperature records per set and year

temp_annual_df <- rv_df%>%
                  data.frame()%>%
                  mutate(id=paste(MISSION,SETNO,sep="-"))%>%
                  distinct(id,.keep_all = TRUE)%>%
                  group_by(YEAR)%>%
                  summarise(mean_bt=mean(BOTT_TEMP,na.rm=T),
                            sd_bt=sd(BOTT_TEMP,na.rm=T),
                            mean_st=mean(SURF_TEMP,na.rm=T),
                            sd_st=sd(SURF_TEMP,na.rm=T))%>%
                  ungroup()%>%
                  data.frame()%>%
                  pivot_longer(
                    cols = c(mean_bt, mean_st),
                    names_to = "temp_type",
                    values_to = "mean_temp"
                  ) %>%
                  mutate(
                    sd = ifelse(temp_type == "mean_bt", sd_bt, sd_st),
                    temp_type = ifelse(temp_type == "mean_bt", "Bottom", "Surface")
                  ) %>%
                  select(YEAR, temp_type, mean_temp, sd)


# get the rolling average temperature 
temp_wide <- temp_annual_df %>%
            pivot_wider(
              id_cols = YEAR,
              names_from = temp_type,
              values_from = c(mean_temp, sd)
            )

# Calculate 5-year moving averages for both temperature types
temp_wide <- temp_wide %>%
            arrange(YEAR) %>%
            mutate(
              rolling_bottom = rollmean(mean_temp_Bottom, k = 5, fill = NA, align = "right"),
              rolling_surface = rollmean(mean_temp_Surface, k = 5, fill = NA, align = "right")
            )

# Convert back to long format for plotting
temp_with_rolling <- temp_wide %>%
                      pivot_longer(
                        cols = c(mean_temp_Bottom, mean_temp_Surface, rolling_bottom, rolling_surface),
                        names_to = "variable",
                        values_to = "value"
                      ) %>%
                      mutate(
                        temp_type = case_when(
                          variable == "mean_temp_Bottom" ~ "Bottom",
                          variable == "mean_temp_Surface" ~ "Surface",
                          variable == "rolling_bottom" ~ "Bottom_5yr",
                          variable == "rolling_surface" ~ "Surface_5yr"
                        ),
                        is_rolling = variable %in% c("rolling_bottom", "rolling_surface"),
                        main_type = ifelse(grepl("Bottom", temp_type), "Bottom", "Surface")
                      ) %>%
                      # Merge back with standard deviations
                      left_join(
                        select(temp_annual_df, YEAR, temp_type, sd),
                        by = c("YEAR", "main_type" = "temp_type")
                      )

# Now plot with the rolling averages
temp_plot <- ggplot() +
  # Original data points and error bars (only for non-rolling data)
  geom_errorbar(
    data = subset(temp_with_rolling, !is_rolling),
    aes(x = YEAR, y = value, ymin = value - sd, ymax = value + sd, color = main_type),
    width = 0.3,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    data = subset(temp_with_rolling, !is_rolling),
    aes(x = YEAR, y = value, fill = main_type),
    size = 3, 
    shape = 21, 
    col = "black",
    position = position_dodge(width = 0.5)
  ) +
  geom_line(
    data = subset(temp_with_rolling, !is_rolling),
    aes(x = YEAR, y = value, color = main_type, group = main_type),
    show.legend = FALSE
  ) +
  
  # 5-year rolling average lines
  geom_line(
    data = subset(temp_with_rolling, is_rolling),
    aes(x = YEAR, y = value, color = main_type, group = temp_type),
    linewidth = 1.2,
    alpha = 0.8
  ) +
  
  # Linear trend line
  geom_smooth(
    data = temp_with_rolling,
    aes(x = YEAR, y = value, group = main_type),
    method = "lm", 
    col = "black", 
    linewidth = 0.5, 
    lty = 2
  ) +
  
  # Colors and labels
  scale_color_manual(values = c("Bottom" = "blue", "Surface" = "red")) +
  scale_fill_manual(values = c("Bottom" = "blue", "Surface" = "red")) +
  labs(
    x = "",
    y = "Temperature (°C) ± SD",
    color = "",
    fill = ""
  ) +
  theme_bw() +
  theme(
    # Transparent background
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    # Grey text and lines
    text = element_text(color = "grey40"),
    axis.text = element_text(color = "grey40"),
    axis.title = element_text(color = "grey40"),
    axis.ticks = element_line(color = "grey60"),
    axis.line = element_line(color = "grey60"),
    
    # Grid lines in grey
    panel.grid.major = element_line(color = "grey80", size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Legend styling
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.05),
    legend.title = element_text(color = "grey40"),
    legend.text = element_text(color = "grey40",size=18),
    
    # X-axis rotation for better readability
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("output/temperature_means.png",width = 14, height = 10, dpi = 300, bg = "transparent")














temp_annual_df <- rv_df%>%
                  data.frame()%>%
                  mutate(id=paste(MISSION,SETNO,sep="-"))%>%
                  distinct(id,.keep_all = TRUE)%>%
                  group_by(YEAR)%>%
                  summarise(mean_bt=mean(BOTT_TEMP,na.rm=T),
                            sd_bt=sd(BOTT_TEMP,na.rm=T),
                            mean_st=mean(SURF_TEMP,na.rm=T),
                            sd_st=sd(SURF_TEMP,na.rm=T))%>%
                  ungroup()%>%
                  data.frame()%>%
                pivot_longer(
                  cols = c(mean_bt, mean_st),
                  names_to = "temp_type",
                  values_to = "mean_temp"
                ) %>%
                mutate(
                  sd = ifelse(temp_type == "mean_bt", sd_bt, sd_st),
                  temp_type = ifelse(temp_type == "mean_bt", "Bottom", "Surface")
                ) %>%
                select(YEAR, temp_type, mean_temp, sd)

ggplot(temp_annual_df, aes(x = YEAR, y = mean_temp, fill=temp_type)) +
  geom_line(aes(color = temp_type,group=temp_type),show.legend = FALSE)+
  geom_errorbar(
    aes(ymin = mean_temp - sd, ymax = mean_temp + sd),
    width = 0.3,
    position = position_dodge(width = 0.5)
  ) +
  geom_smooth(method="lm",col="black",linewidth=0.5,lty=2)+
  geom_point(size = 3, position = position_dodge(width = 0.5),shape=21,col="black") +
  scale_color_manual(values = c("Bottom" = "blue", "Surface" = "red")) +
  scale_fill_manual(values = c("Bottom" = "blue", "Surface" = "red")) +
  labs(
    x = "",
    y = "Temperature (°C) ± SD",
    color = "",
    fill=""
  ) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9,0.1),
    legend.background = element_blank(),
    panel.grid.minor = element_blank()
  )
                

# Filter for the two species and calculate weighted averages
species_temp_df <- rv_df %>%
  filter(SCI_NAME %in% c("MELANOGRAMMUS AEGLEFINUS", "MERLUCCIUS BILINEARIS")) %>%
  # Group by year, species, and calculate weighted averages
  group_by(YEAR, SCI_NAME) %>%
  summarise(
    # Weighted average for bottom temperature
    mean_bt = weighted.mean(BOTT_TEMP, std_count, na.rm = TRUE),
    # Weighted standard deviation for bottom temperature
    sd_bt = sqrt(sum(std_count * (BOTT_TEMP - weighted.mean(BOTT_TEMP, std_count, na.rm = TRUE))^2, na.rm = TRUE) / 
                   sum(std_count, na.rm = TRUE)),
    
    # Weighted average for surface temperature
    mean_st = weighted.mean(SURF_TEMP, std_count, na.rm = TRUE),
    # Weighted standard deviation for surface temperature
    sd_st = sqrt(sum(std_count * (SURF_TEMP - weighted.mean(SURF_TEMP, std_count, na.rm = TRUE))^2, na.rm = TRUE) / 
                   sum(std_count, na.rm = TRUE)),
    
    # Total samples for reference
    total_samples = n()
  ) %>%
  ungroup() %>%
  # Reshape from wide to long format
  pivot_longer(
    cols = c(mean_bt, mean_st),
    names_to = "temp_type",
    values_to = "mean_temp"
  ) %>%
  mutate(
    sd = ifelse(temp_type == "mean_bt", sd_bt, sd_st),
    temp_type = ifelse(temp_type == "mean_bt", "Bottom", "Surface")
  ) %>%
  select(YEAR, SCI_NAME, temp_type, mean_temp, sd, total_samples) %>%
  # Rename species for better readability
  mutate(species = case_when(
    SCI_NAME == "MELANOGRAMMUS AEGLEFINUS" ~ "Haddock",
    SCI_NAME == "MERLUCCIUS BILINEARIS" ~ "Silver Hake",
    TRUE ~ SCI_NAME
  ))


# Create a faceted plot for both species
ggplot(species_temp_df, aes(x = YEAR, y = mean_temp, fill = temp_type)) +
  # Facet by species
  facet_wrap(~ species, ncol = 1) +
  
  # Lines
  geom_line(aes(color = temp_type, group = temp_type), show.legend = FALSE) +
  
  # Error bars
  geom_errorbar(
    aes(ymin = mean_temp - sd, ymax = mean_temp + sd),
    width = 0.3,
    position = position_dodge(width = 0.5)
  ) +
  
  # Points
  geom_point(size = 3, position = position_dodge(width = 0.5), shape = 21, col = "black") +
  
  # Smoothed line only from 2009 onward
  geom_smooth(
    data = subset(species_temp_df, YEAR >= 2009),
    method = "lm",
    aes(color = temp_type, group = temp_type),
    se = TRUE,
    linetype = "dashed"
  ) +
  
  # Color scales
  scale_color_manual(values = c("Bottom" = "blue", "Surface" = "red")) +
  scale_fill_manual(values = c("Bottom" = "blue", "Surface" = "red")) +
  
  # Labels and theme
  labs(
    x = "",
    y = "Weighted Temperature (°C) ± SD",
    title = "Temperature Preferences by Species (Weighted by Abundance)",
    color = "",
    fill = "Temperature"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )
