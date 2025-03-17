process_species <- function(x, species, return_var = "all") {
  
  # Process the data
  sp_df <- x %>%
    filter(
      SCI_NAME == species, # Select the species of interest
      !is.na(distance_category), # Exclude rows with missing distance_category
      classification %in% c("WSS/Outer BoF", "WSS: Banks/Inner BoF") # Focus on two major classes
      ) %>%
    data.frame() %>% # Remove 'sf' dataframe syntax
    group_by(distance_category, classification, YEAR) %>%
    summarise(
      n_obs = n(),
      mean_abund = mean(std_wgt, na.rm = TRUE),
      sd_abund = sd(std_wgt, na.rm = TRUE),
      mean_count = mean(std_count, na.rm = TRUE),
      sd_count = sd(std_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    data.frame() %>%
    
    # Reclassify distance categories
    mutate(
      distance_category = case_when(
        distance_category == 0 ~ "0km",                  # 0 km buffer
        distance_category > 0 & distance_category <= 50 ~ "50km", # Combine 0-50 km into 50 km buffer
        distance_category > 50 & distance_category <= 100 ~ "100km", # 100 km buffer
        TRUE ~ NA_character_ # Exclude distances beyond 100 km
      ),
      distance_category = factor(
        distance_category, 
        levels = c("0km", "50km", "100km")
      ),
      
      # Period categorization
      period = case_when(
        YEAR < 1993 ~ "pre-collapse",
        YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
        YEAR > 2005 ~ "recent"
      ),
      
      # Additional classifications
      dist_50 = ifelse(distance_category == "50km", "Outside", ifelse(distance_category=="0km","Inside",NA)) ,
      dist_100 = ifelse(distance_category != "0km", "Outside", "Inside"), #combines 50 and 100
      species = species
    )

  plot_df <- sp_df%>%
    dplyr::select(classification,period,mean_abund,mean_count,period,dist_100,dist_50)%>%
    gather(dist_thresh,value,-c(classification,period,mean_abund,mean_count))%>%
    filter(!is.na(value))%>%
    group_by(period,classification,dist_thresh,value)%>%
    summarize(sd_abund=sd(mean_abund),
              mean_abund=mean(mean_abund),
              sd_count=sd(mean_count),
              mean_count=mean(mean_count))%>%
    ungroup()%>%
    data.frame()%>%
    mutate(period=factor(period,levels=c("pre-collapse","post-collapse","recent")),
           dist_cat = factor(as.numeric(gsub("dist_","",dist_thresh)),
                             levels=c(50,100)))%>%
    mutate(species=species)
  
  baseline_df <- plot_df %>%
    filter(period == "pre-collapse") %>%
    select(classification, value, dist_cat, baseline_mean_abund = mean_abund)
  
  diff_df <- plot_df %>%
    left_join(baseline_df, by = c("classification", "value", "dist_cat"))%>%
    filter(period != "pre-collapse") %>% # Exclude the baseline period
    mutate(perc_change_abund = 100 * (mean_abund - baseline_mean_abund) / baseline_mean_abund) %>%
    select(period, classification, value, dist_cat, perc_change_abund)%>%
    mutate(species=species)
  
#return outputs --- 

if(return_var == "all") {output = list()

output[["sp_df"]] <- sp_df
output[["diff_df"]] <- diff_df
output[["plot_df"]] <- plot_df

return(output)


}

if(return_var == "diff_df"){return(diff_df)}
if(return_var == "plot_df"){return(plot_df)}
if(return_var == "sp_df"){return(sp_df)}

}

webca_fish_trend <- function(x,species,title,poster=FALSE,point_size=1){
  
  require(scales)
  require(tidyverse)
  
  source("code/theme_custom.R")
  
  data_sets <- process_species(x=x,species=species,return_var = "all")
  list2env(data_sets, envir = .GlobalEnv)
  
  #comparison plots
  comp_plot <- ggplot(plot_df, aes(x = dist_cat, y = mean_abund, fill = value, shape = value, group = value)) +
    geom_hline(data=plot_df%>%filter(value=="Inside"),aes(yintercept = mean_abund),col="grey70",linewidth=0.5,lty=2)+
    geom_errorbar(aes(ymin = pmax(mean_abund - sd_abund, 0.001), ymax = mean_abund + sd_abund), width = 0,
                  position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5), size = point_size) +
   
    facet_grid(classification ~ period, scales = "free_y") +
    labs(x = "Distance (km) from refuge", y = "Mean abundance ± sd", fill = "", shape = "", group = "", title = title) +
    scale_shape_manual(values = c(21, 22)) +  # Distinguish between inside/outside
    scale_fill_manual(values = c("Inside" = "blue", "Outside" = "orange")) +  # Map "inside" to red and "outside" to blue
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))
  
  
  #difference plot
  diff_plot <- ggplot(diff_df, aes(x = perc_change_abund / 100, y = dist_cat, shape = period, fill = value)) +
    geom_point(position = position_dodge(width = 0.5), size = point_size) +
    facet_wrap(~classification, scales = "free_x") +
    scale_shape_manual(values = c(21, 22)) +  # Use open shapes that accept fill
    scale_fill_manual(values = c("Inside" = "blue", "Outside" = "orange")) +  # Fill colors for value
    scale_x_continuous(labels = scales::percent_format()) +  # Format x-axis as a percentage
    labs(x = "% Change from Pre-collapse", y = "Distance (km) from refuge", 
         fill = "Value (Inside/Outside)", shape = "Period") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"), legend.position = "right") +
    guides(fill = guide_legend(override.aes = list(shape = 21)),  # Ensure shapes appear in one legend
           shape = guide_legend(override.aes = list(fill = "black")))  # Single legend customization
  
  #messy point plot
  point_plot <- ggplot(data = sp_df, aes(x = YEAR, y = mean_count, col = classification, group = classification)) +
    geom_line() +
    geom_point(size=point_size) +
    facet_grid(classification ~ distance_category) +
    theme_bw() +
    scale_y_log10() +
    scale_color_manual(values = c("WSS/Outer BoF" = "lightblue", "WSS: Banks/Inner BoF" = "orange")) +  # Map classifications to colors
    theme(strip.background = element_rect(fill = "white"))
  
  #fitted trend line plot
  line_plot <- ggplot(data=sp_df,aes(x=YEAR,y=mean_count,col=distance_category,group=distance_category))+
    geom_smooth(method="lm",se=FALSE)+
    geom_smooth(data=sp_df%>%filter(distance_category==0),lwd=2,method="lm",alpha=0.3)+
    facet_wrap(~classification,ncol=1,scales="free_y")+
    theme_bw()+
    scale_y_log10()+
    geom_vline(xintercept=c(1987,2017),lty=2)+
    labs(col="Distance from MR",y="Mean abundance",x="",title=title)+
    theme(strip.background = element_rect(fill="white")) +
    scale_color_manual(values = c("0" = "lightblue", "50" = "orange", "100" ="darkgrey"))
  
  #generate the outputs as a list
  output=list()
  
  #datasets
  output[["sp_df"]] <- sp_df
  output[["diff_df"]] <- diff_df
  output[["plot_df"]] <- plot_df
  
  #plots
  if(!poster){output[["comp_plot"]] <- comp_plot}
  if(poster){output[["comp_plot"]] <- comp_plot + theme_custom_2()} #fancy poster theme
  
  if(!poster){output[["diff_plot"]] <- diff_plot}
  if(poster){output[["diff_plot"]] <- diff_plot+theme_custom_2()}
  
  if(!poster){output[["point_plot"]] <- point_plot}
  if(poster){output[["point_plot"]] <- point_plot+theme_custom_2()}
  
  if(poster){output[["line_plot"]] <- line_plot}
  if(!poster){output[["line_plot"]] <- line_plot+theme_custom_2()}
  
  return(output)
  
}

boot_fun <- function(x, max_boot = 1000, outlier_trim = TRUE) {
  reference_period <- "pre-collapse"
  target_periods <- c("post-collapse", "recent")
  
  # Initialize a list for results
  boot_results <- list()
  
  for (target in target_periods) {
    data_ref <- x %>% filter(period == reference_period)
    data_target <- x %>% filter(period == target)
    
    # Determine n_boot based on sample size
    n_ref <- nrow(data_ref)
    n_target <- nrow(data_target)
    n_boot <- min(max_boot, choose(n_ref, min(n_ref, 2)), choose(n_target, min(n_target, 2)))
    
    # Apply outlier trimming if enabled
    if (outlier_trim) {
      data_ref <- data_ref %>%
        mutate(
          TOTWGT = ifelse(TOTWGT < (quantile(TOTWGT, 0.25) - 1.5 * IQR(TOTWGT)) | 
                            TOTWGT > (quantile(TOTWGT, 0.75) + 1.5 * IQR(TOTWGT)), NA, TOTWGT),
          TOTNO = ifelse(TOTNO < (quantile(TOTNO, 0.25) - 1.5 * IQR(TOTNO)) |
                           TOTNO > (quantile(TOTNO, 0.75) + 1.5 * IQR(TOTNO)), NA, TOTNO)
        )
      
      data_target <- data_target %>%
        mutate(
          TOTWGT = ifelse(TOTWGT < (quantile(TOTWGT, 0.25) - 1.5 * IQR(TOTWGT)) | 
                            TOTWGT > (quantile(TOTWGT, 0.75) + 1.5 * IQR(TOTWGT)), NA, TOTWGT),
          TOTNO = ifelse(TOTNO < (quantile(TOTNO, 0.25) - 1.5 * IQR(TOTNO)) |
                           TOTNO > (quantile(TOTNO, 0.75) + 1.5 * IQR(TOTNO)), NA, TOTNO)
        )
    }
    
    # Bootstrap sampling with progress bar
    boot_out <- future_map_dfr(seq_len(n_boot), function(i) {
      set.seed(i)  # Set seed for reproducibility for each bootstrapping iteration
      ref_sample <- sample(n_ref, replace = TRUE)
      target_sample <- sample(n_target, replace = TRUE)
      
      data.frame(
        boot = i, 
        period = target,
        wgt_target = mean(data_target$TOTWGT[target_sample], na.rm = TRUE),
        wgt_reference = mean(data_ref$TOTWGT[ref_sample], na.rm = TRUE),
        count_target = mean(data_target$TOTNO[target_sample], na.rm = TRUE),
        count_reference = mean(data_ref$TOTNO[ref_sample], na.rm = TRUE)
      ) %>%
        mutate(
          prec_wgt_diff = ((wgt_reference - wgt_target) / wgt_reference) * 100,
          prec_count_diff = ((count_reference - count_target) / count_reference) * 100
        )
    }, .progress = TRUE)  # Remove .seed argument
    
    boot_results[[target]] <- boot_out
  }
  
  bind_rows(boot_results)
}


