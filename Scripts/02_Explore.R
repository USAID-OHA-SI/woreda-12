# PROJECT: Explore IIT 
# PURPOSE: Munge and Analysis of ITT Data
# AUTHOR: Lemlem Baraki | SI
# REF ID:   1229b41a
# LICENSE: MIT
# DATE: 2023-12-19
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(systemfonts)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(table1)
      
    
  # SI specific paths/functions  
load_secrets()
data_folder <- "Dataout/"

  
  # REF ID for plots
    ref_id <- "1229b41a"
    
  # Functions  
    #Compute p-values wit this function:
    
    pvalue <- function(x, ...) {
      # Construct vectors of data y, and groups (strata) g
      y <- unlist(x)
      g <- factor(rep(1:length(x), times=sapply(x, length)))
      if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
      } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
      }
      # Format the p-value, using an HTML entity for the less-than sign.
      # The initial empty string places the output on the line below the variable label.
      c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
    }

# LOAD DATA ============================================================================  

    df <- data_folder %>% 
      return_latest(pattern = "2023-12-11.") %>% 
      read_csv() %>% 
      janitor::clean_names()
    
    glimpse(df)
    names(df)
    
    

# MUNGE ============================================================================
  
    df_sample <- df %>% 
      dplyr::select(sex_binary, age_bands, year_art_init_gap, healthfac_type,
                    missed_ltfu_dur_grp, missed_custom_report_dur_grp, 
                    tracing_attempt_grp, rtt_binary, traced_binary,
                    region_cat, tracing_method_grp, woreda, zone, outcome_date) %>% 
      filter(tracing_attempt_grp!="0 attempts") %>% #exclude 0 attempts
      filter(region_cat == "Addis Ababa") #reduce sample size to one region  
    
    names(df_sample)
  
# VIZ ============================================================================

  #Recreate Table 1
    
    #Table 1: Outcome by RTT
    df_sample$rtt_bin <- factor(df_sample$rtt_binary, 
                             labels = c("Not RTT", "RTT"))
    
    df_sample$sex_bin <- factor(df_sample$sex_binary, 
                             labels = c("Females", "Males"))
    
    #add labels 
    label(df_sample$sex_bin)        <- "Sex"
    label(df_sample$age_bands)        <- "Age"
    label(df_sample$region_cat)        <- "Region"
    label(df_sample$zone)        <- "Zone"
    label(df_sample$healthfac_type)        <- "Health Facility"
    label(df_sample$missed_ltfu_dur_grp)        <- "LTFU duration"
    label(df_sample$missed_custom_report_dur_grp)        <- "Custom Report Duration"
    label(df_sample$tracing_attempt_grp)        <- "Number of tracing attempts"
    label(df_sample$tracing_method_grp)        <- "Tracing Method"
    
    names(df_sample)
    
    table1::table1(~sex_bin + age_bands + region_cat + zone + healthfac_type + 
                     missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
                     tracing_attempt_grp | rtt_bin, data = df_sample,
                   overall = F,
                   #extra.col=list(`P-value`=pvalue),#add in p-values
                   topclass = "Rtable1-grid Rtable1-shade Rtable1-times", 
                   caption = "Table 1: Baseline Cohort Demographics") 

# SPINDOWN ============================================================================
