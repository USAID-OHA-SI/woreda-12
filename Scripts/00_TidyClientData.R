# PROJECT: Ethiopia Community Program IIT Analysis 
# PURPOSE: Munge and Analysis of IIT Data 
# AUTHOR: Lemlem Baraki | SI
# REF ID:   69cbff36
# LICENSE: MIT
# DATE: 2023-05-04
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(gagglr)
  library(readxl)
  library(glue)
  library(gt)
  library(gtExtras)
  library(tameDP)
  
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    msd_path <- return_latest(folderpath = merdata, pattern = "_PSNU_IM_FY21-23.*Ethiopia")
    
  
  # Grab metadata
    get_metadata(msd_path)
    
    data_folder <- "Data/"
    
   #msd_source <- source_info(file_path)
   #curr_pd <- source_info(file_path, return = "period")
   #pd <- source_info(file_path, return = "period")
   #fy <- source_info(file_path, return = "fiscal_year")
   #qtr <- source_info(file_path, return = "quarter")  
  
  # REF ID for plots
    ref_id <- "69cbff36"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  #PEPFAR 
    df_msd <- read_psd(msd_path)
    
  #Patient level data 
    df_ptlevel <- data_folder %>% 
      return_latest("semi_processed _Dataset_Et f IIT .xlsx") %>% 
      read_xlsx(sheet = "Client Data fy 22& 23 as of Q2")
    
    

# MUNGE ============================================================================
  
    #pt level data
      #want to have columns of age, sex, ART duration, LTFU duration
        #age groups: <15, 15-24, 25-35, 36-45, 46-50, >50 (align with MER)
        #code sex: 0 for female, 1 for males 
        #ART duration in yrs: <1 , 1-3 yr, 3-6, 7-10, >10yrs
        #LTFU duration in months: <1month , 1-3 months, 4-6 months, >6 months
      #make several new variables from columns (K-J), (L-K), (L-I)
      #new columns (V-X): traced & located (yes/no),return to TX (yes/no), reason for not return to TX (comment) 
    
    view(df_ptlevel)
    
    pt_level <- df_ptlevel %>%
      #rename(Sex = Gender)%>%
      select(Region, Woreda, Age, Gender, `Client ART Start Date`, `Missed Appointment Date`, `LTFU Recorded Date`) %>%
      mutate(Sex = ifelse(Gender == "Male", "0", "1"))%>%
      mutate(age_bands = case_when(
        Age %in% c(1:4) ~ "1-4",
        Age %in% c(5:9) ~ "5-9",
        Age %in% c(10:14) ~ "10-14", 
        Age %in% c(15:19) ~ "15-19", 
        Age %in% c(20:24) ~ "20-24",
        Age %in% c(25:29) ~ "25-29",
        Age %in% c(30:34) ~ "30-34",
        Age %in% c(35:39) ~ "35-39",
        Age %in% c(40:44) ~ "40-44",
        Age %in% c(45:49) ~ "45-49",
        Age %in% c(50:100) ~ "50+"))%>%

                                    
                                   
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
    
                                