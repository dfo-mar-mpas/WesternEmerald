
process_species <- function(x,species,return_var = all) {
  
  #first process
  sp_df <- x%>%
    filter(SCI_NAME == species, #Atlantic cod 
           !is.na(distance_category),
           classification %in% c("WSS/Outer BoF","WSS: Banks/Inner BoF"))%>% #two major classes
    data.frame()%>% #this will get rid of the 'sf' dataframe syntax
    group_by(distance_category,classification,YEAR)%>%
    summarise(n_obs=n(),
              mean_abund=mean(std_wgt,na.rm=T),
              sd_abund=sd(std_wgt,na.rm=T),
              mean_count=mean(std_count,na.rm=T),
              sd_count=sd(std_count,na.rm=T))%>%
    ungroup()%>%
    data.frame()%>%
    mutate(distance_cat = paste0(distance_category,"km"),
           distance_cat = factor(distance_cat,levels=c("0km","10km","50km","100km","200km")),
           period = case_when(YEAR < 1993 ~ "pre-collapse", #periods taken from Nancy's paper
                              YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
                              YEAR > 2005 ~ "recent"),
           dist_200 = ifelse(distance_category!=0,"Outside","Inside"), #aggregate the distance categories
           dist_100 = case_when(distance_category!=0 & distance_category>100 ~ NA,
                                distance_category!=0 & distance_category<200 ~ "Outside",
                                TRUE ~ "Inside"),
           dist_50 = case_when(distance_category!=0 & distance_category>50 ~ NA,
                               distance_category!=0 & distance_category<100 ~ "Outside",
                               TRUE ~ "Inside"),
           dist_10 = case_when(distance_category!=0 & distance_category>10 ~ NA,
                               distance_category!=0 & distance_category<50 ~ "Outside",
                               TRUE ~ "Inside"))%>%
          mutate(species=species)
  
  #second plot proess
  plot_df <- sp_df%>%
    dplyr::select(classification,period,mean_abund,mean_count,period,dist_200,dist_100,dist_50,dist_10)%>%
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
                             levels=c(10,50,100,200)))%>%
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

webca_fish_trend <- function(x,species,title){
  
  require(scales)
  require(tidyverse)
  
  data_sets <- process_species(x=x,species=species,return_var = "all")
  list2env(data_sets, envir = .GlobalEnv)
  
  #comparison plots
  comp_plot <- ggplot(plot_df, aes(x=dist_cat, y=mean_abund, fill=value, shape=value, group=value)) +
    geom_errorbar(aes(ymin=pmax(mean_abund - sd_abund, 0.001), ymax=mean_abund + sd_abund), width=0,
                  position=position_dodge(width=0.5)) +
    geom_point(position=position_dodge(width=0.5), size=3) +
    facet_grid(classification~period,scales="free_y") +
    labs(x="Distance Threshold", y="Mean Abundance",fill="",shape="",group="",title=title) +
    scale_shape_manual(values=c(21, 22)) +  # Distinguish between inside/outside
    theme_bw()+
    theme(strip.background = element_rect(fill="white"))
  
#difference plot
  diff_plot <- ggplot(diff_df, aes(x = perc_change_abund / 100, y = dist_cat, shape = period, fill = value)) +
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
           shape = guide_legend(override.aes = list(fill = "black")))  # Single legend customization
  
  #messy point plot
  point_plot <- ggplot(data=sp_df,aes(x=YEAR,y=mean_count,col=classification,group=classification))+
    geom_line()+
    geom_point()+
    facet_grid(classification~distance_category)+
    theme_bw()+
    scale_y_log10()+
    theme(strip.background = element_rect(fill="white"))
  
  #fitted trend line plot
  line_plot <- ggplot(data=sp_df,aes(x=YEAR,y=mean_count,col=distance_cat,group=distance_cat))+
    geom_smooth(method="lm",se=FALSE)+
    geom_smooth(data=sp_df%>%filter(distance_category==0),lwd=2,method="lm",alpha=0.3)+
    facet_wrap(~classification,ncol=1,scales="free_y")+
    theme_bw()+
    scale_y_log10()+
    geom_vline(xintercept=c(1987,2017),lty=2)+
    labs(col="Distance from MR",y="Mean abundance",x="",title="Atlantic cod")+
    theme(strip.background = element_rect(fill="white"))
  
  #generate the outputs as a list
  output=list()
  
  #datasets
  output[["sp_df"]] <- sp_df
  output[["diff_df"]] <- diff_df
  output[["plot_df"]] <- plot_df
  
  #plots
  output[["comp_plot"]] <- comp_plot
  output[["diff_plot"]] <- diff_plot
  output[["point_plot"]] <- point_plot
  output[["line_plot"]] <- line_plot
  
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


# #boot_fun <- function(x,n_boot=1000,outlier_trim=TRUE){
#   # Assuming x is your dataframe and you have already defined the reference period
#   reference_period <- "pre-collapse"
#   target_periods <- c("post-collapse", "recent")
#   
#   # Initialize an empty list to store results
#   boot_results <- list()
#   
#   # Loop through each target period
#   for (target in target_periods) {
#     # Filter data for the reference and target periods
#     data_ref <- x %>% filter(period == reference_period)
#     data_target <- x %>% filter(period == target)
#     
#     
#     if(outlier_trim){
#       
#       # totwgt <- c(data_ref$TOTWGT,data_target$TOTWGT)
#       # totno <- c(data_ref$TOTNO,data_target$TOTNO)
#       # 
#       # #identify outliers that are from the full data range of observations
#       # outlier_wgt <- data.frame(min = (quantile(totwgt, 0.25) - 1.5 * IQR(totwgt)),
#       #                           max = (quantile(totwgt, 0.25) + 1.5 * IQR(totwgt)))
#       # 
#       # outlier_no <- data.frame(min = (quantile(totno, 0.25) - 1.5 * IQR(totno)),
#       #                           max = (quantile(totno, 0.25) + 1.5 * IQR(totno)))
#       # 
#       # data_ref <- data_ref %>%
#       #   mutate(
#       #     # Calculate the IQR and replace outliers in TOTWGT with NA
#       #     TOTWGT = ifelse(
#       #       TOTWGT < outlier_wgt$min |
#       #         TOTWGT > outlier_wgt$max,
#       #       NA, 
#       #       TOTWGT
#       #     ),
#       #     # Calculate the IQR and replace outliers in TOTNO with NA
#       #     TOTNO = ifelse(
#       #       TOTNO < outlier_no$min |
#       #         TOTNO > outlier_no$max,
#       #       NA, 
#       #       TOTNO
#       #     )
#       #   )
#       # 
#       # data_target <- data_target%>%   
#       #   mutate(
#       #     # Calculate the IQR and replace outliers in TOTWGT with NA
#       #     TOTWGT = ifelse(
#       #       TOTWGT < outlier_wgt$min |
#       #         TOTWGT > outlier_wgt$max,
#       #       NA, 
#       #       TOTWGT
#       #     ),
#       #     # Calculate the IQR and replace outliers in TOTNO with NA
#       #     TOTNO = ifelse(
#       #       TOTNO < outlier_no$min |
#       #         TOTNO > outlier_no$max,
#       #       NA, 
#       #       TOTNO
#       #     )
#       #   )
#       
#       data_ref <- data_ref %>%
#         mutate(
#           # Calculate the IQR and replace outliers in TOTWGT with NA
#           TOTWGT = ifelse(
#             TOTWGT < (quantile(TOTWGT, 0.25) - 1.5 * IQR(TOTWGT)) |
#               TOTWGT > (quantile(TOTWGT, 0.75) + 1.5 * IQR(TOTWGT)),
#             NA,
#             TOTWGT
#           ),
#           # Calculate the IQR and replace outliers in TOTNO with NA
#           TOTNO = ifelse(
#             TOTNO < (quantile(TOTNO, 0.25) - 1.5 * IQR(TOTNO)) |
#               TOTNO > (quantile(TOTNO, 0.75) + 1.5 * IQR(TOTNO)),
#             NA,
#             TOTNO
#           )
#         )
#       
#       data_target <- data_target%>%
#         
#         mutate(
#           # Calculate the IQR and replace outliers in TOTWGT with NA
#           TOTWGT = ifelse(
#             TOTWGT < (quantile(TOTWGT, 0.25) - 1.5 * IQR(TOTWGT)) |
#               TOTWGT > (quantile(TOTWGT, 0.75) + 1.5 * IQR(TOTWGT)),
#             NA,
#             TOTWGT
#           ),
#           # Calculate the IQR and replace outliers in TOTNO with NA
#           TOTNO = ifelse(
#             TOTNO < (quantile(TOTNO, 0.25) - 1.5 * IQR(TOTNO)) |
#               TOTNO > (quantile(TOTNO, 0.75) + 1.5 * IQR(TOTNO)),
#             NA,
#             TOTNO
#           )
#         )
#     }
#     
#     dim_ref <- nrow(data_ref)
#     dim_target <- nrow(data_target)
#     
#     out <- list()
#     
#     for(i in 1:n_boot){
#       
#       data_ref_boot <- data_ref[sample(1:dim_ref,size = dim_ref,replace = TRUE),]
#       data_target_boot <- data_target[sample(1:dim_target,size = dim_target,replace = TRUE),]
#       
#       out[[i]] <- data.frame(boot=i,period=target,
#                              wgt_target = mean(data_target_boot$TOTWGT,na.rm=T),
#                              wgt_reference = mean(data_ref_boot$TOTWGT,na.rm=T),
#                              count_target = mean(data_target_boot$TOTNO,na.rm=T),
#                              count_reference = mean(data_ref_boot$TOTNO,na.rm=T))%>%
#         mutate(prec_wgt_diff = ((wgt_reference-wgt_target)/wgt_reference)*100,
#                prec_count_diff = ((count_reference-count_target)/count_reference)*100)
#     }
#     
#     out_df <- do.call('rbind',out)
#     
#     return(out_df)
#     
#   }
# }