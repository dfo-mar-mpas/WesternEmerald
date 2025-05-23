#load libraries
library(tidyverse)
library(scales)
library(purrr)

#source basis functions
source("code/webca_fish_trend2.0.R")

#species to make plots form
focus_df <- data.frame(latin = c("MELANOGRAMMUS AEGLEFINUS","GADUS MORHUA","MERLUCCIUS BILINEARIS","SEBASTES","POLLACHIUS VIRENS"),
                       common = c("Haddock","Atlantic cod","Sliver hake","Redfish","Pollock"))

# Function to process a single species
process_single_species <- function(species_latin, species_common) {
  # Process the species data
  combo_data <- process_species(rv_df, species = species_latin, return_var = "sp_df") %>%
    mutate(
      period = case_when(
        YEAR < 1993 ~ "pre-collapse", 
        YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
        YEAR > 2005 ~ "recent"
      ),
      species = species_common  # Add common name
    ) %>%
    filter(!is.na(distance_category))
  
  # Step 1: Summarize first to handle multiple entries
  baseline_data <- combo_data %>%
    filter(distance_category == "0km") %>%
    group_by(YEAR, classification) %>%
    summarize(
      baseline_abund = mean(mean_abund, na.rm = TRUE),
      baseline_count = mean(mean_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  non_baseline_data <- combo_data %>%
    filter(distance_category %in% c("50km", "100km")) %>%
    group_by(YEAR, classification, distance_category) %>%
    summarize(
      mean_abund = mean(mean_abund, na.rm = TRUE),
      mean_count = mean(mean_count, na.rm = TRUE),
      species = first(species),  # Preserve the species name
      .groups = "drop"
    )
  
  diff_from_baseline <- non_baseline_data %>%
    left_join(baseline_data, by = c("YEAR", "classification")) %>%
    mutate(
      diff_abund = mean_abund - baseline_abund,
      diff_count = mean_count - baseline_count,
      pct_diff_abund = (mean_abund - baseline_abund) / baseline_abund * 100,
      pct_diff_count = (mean_count - baseline_count) / baseline_count * 100,
      period = case_when(
        YEAR < 1993 ~ "pre-collapse", 
        YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
        YEAR > 2005 ~ "recent"
      )
    ) %>%
    select(YEAR, classification, distance_category, species,
           diff_abund, diff_count, pct_diff_abund, pct_diff_count, period)
  
  # Return both the original data and difference data
  return(list(
    combo_data = combo_data,
    diff_data = diff_from_baseline
  ))
}

# Process all species
all_species_data <- map2(focus_df$latin, focus_df$common, process_single_species)

# Combine all the raw data for abundance boxplot
all_combo_data <- map_dfr(all_species_data, ~.x$combo_data)

# Combine all the difference data
all_diff_data <- map_dfr(all_species_data, ~.x$diff_data)

#gear change year filter
all_combo_data <- all_combo_data%>%filter(YEAR>1984)
all_diff_data <- all_diff_data%>%
                 filter(YEAR>1984)%>%
                  mutate(period = factor(period,levels=c("pre-collapse","post-collapse","recent")))

# Create boxplot for abundance
boxplot_abund <- ggplot(all_combo_data, aes(x = period, y = mean_abund, fill = distance_category)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7,outliers = FALSE) +
  # stat_summary(aes(group = distance_category), fun = mean, geom = "point", 
  #              position = position_dodge(width = 0.8), color = "black", size = 2, shape = 18) +
  facet_grid(species ~ classification,scales="free_y") +  # Two-way facet with species and classification
  scale_fill_brewer(palette = "Set1", name = "") +
  labs(
    x = "",
    y = "Mean annual abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )


# Create boxplot for percent difference
percent_boxplot <- ggplot(all_diff_data, aes(x = period, y = diff_abund, fill = distance_category)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_point(aes(color = distance_category), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 2, alpha = 0.6, show.legend = FALSE) +
  facet_grid(species ~ classification,scales="free_y") +  # Two-way facet with species and classification
  theme_bw() +
  labs(x = "", y = "% difference from WEBMR", fill = "") +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  scale_fill_brewer(palette = "Set1", name = "")

#save outputs
ggsave("output/abund_diff_webmr_all.png",boxplot_abund,height=10,width=7.5,units="in",dpi=300)
ggsave("output/percent_diff_webmr_all.png",percent_boxplot,height=10,width=7.5,units="in",dpi=300)



## Period abundance changes -------------- these only half make sense and I just used AI to adapte the code from above. So not sure these should be used keeping the code anyway

# Step 1: Calculate the baseline values (pre-collapse period)
period_baseline_data <- all_combo_data %>%
  filter(period == "pre-collapse") %>%
  group_by(species, classification, distance_category) %>%
  summarize(
    baseline_abund = mean(mean_abund, na.rm = TRUE),
    baseline_count = mean(mean_count, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Calculate differences between periods
period_diff_data <- all_combo_data %>%
  # Only include post-collapse and recent periods
  filter(period != "pre-collapse") %>%
  # Group by the relevant factors
  group_by(species, classification, distance_category, period) %>%
  # Calculate mean values for each group
  summarize(
    mean_abund = mean(mean_abund, na.rm = TRUE),
    mean_count = mean(mean_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Join with the baseline data
  left_join(period_baseline_data, 
            by = c("species", "classification", "distance_category")) %>%
  # Calculate absolute and percent differences
  mutate(
    # Absolute differences
    diff_abund = mean_abund - baseline_abund,
    diff_count = mean_count - baseline_count,
    
    # Percent differences
    pct_diff_abund = (mean_abund - baseline_abund) / baseline_abund * 100,
    pct_diff_count = (mean_count - baseline_count) / baseline_count * 100
  )

# Calculate reasonable y-axis limits for each facet for abundance difference
y_limits_abund <- period_diff_data %>%
  group_by(species, classification) %>%
  summarize(
    y_max = quantile(diff_abund, 0.95, na.rm = TRUE),
    y_min = quantile(diff_abund, 0.05, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate reasonable y-axis limits for each facet for percent difference
y_limits_pct <- period_diff_data %>%
  group_by(species, classification) %>%
  summarize(
    y_max = quantile(pct_diff_abund, 0.95, na.rm = TRUE),
    y_min = quantile(pct_diff_abund, 0.05, na.rm = TRUE),
    .groups = "drop"
  )

# Plot 1: Absolute abundance differences
period_boxplot_abund <- ggplot(period_diff_data, aes(x = distance_category, y = diff_abund, fill = period)) +
  # Add reference line at zero
  geom_hline(yintercept = 0, lty = 2, color = "darkgray") +
  # Add invisible points at the limit values to control the scale
  # geom_blank(data = y_limits_abund, aes(y = y_max, x = "0km")) +
  # geom_blank(data = y_limits_abund, aes(y = y_min, x = "0km")) +
  # # Actual boxplot
  geom_point(position = position_dodge(width = 0.8), size=3,shape=21) +
  # Add points for the mean
  facet_grid(species ~ classification, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Period") +
  labs(
    title = "Change in Abundance from Pre-Collapse Period",
    subtitle = "Comparing post-collapse and recent periods to pre-collapse baseline",
    x = "Distance Category",
    y = "Difference in Mean Abundance from Pre-Collapse"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

# Plot 2: Percent differences
period_boxplot_pct <- ggplot(period_diff_data, aes(x = distance_category, y = pct_diff_abund, fill = period)) +
  # Add reference line at zero
  geom_hline(yintercept = 0, lty = 2, color = "darkgray") +
  # Add invisible points at the limit values to control the scale
  # geom_blank(data = y_limits_pct, aes(y = y_max, x = "0km")) +
  # geom_blank(data = y_limits_pct, aes(y = y_min, x = "0km")) +
  # Actual boxplot
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7, outlier.shape = NA) +
  # Add points for the mean
  stat_summary(aes(group = period), fun = mean, geom = "point", 
               position = position_dodge(width = 0.8), color = "black", size = 2, shape = 18) +
  facet_grid(species ~ classification, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Period") +
  labs(
    title = "Percent Change in Abundance from Pre-Collapse Period",
    subtitle = "Comparing post-collapse and recent periods to pre-collapse baseline",
    x = "Distance Category",
    y = "% Difference in Abundance from Pre-Collapse"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

