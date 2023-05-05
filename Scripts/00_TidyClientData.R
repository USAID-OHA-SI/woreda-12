# PROJECT: Ethiopia Community Program IIT Analysis 
# PURPOSE: Munge and Analysis of IIT Data 
# AUTHOR: Lemlem Baraki, Karishma Srikanth | SI
# REF ID:   69cbff36
# LICENSE: MIT
# DATE: 2023-05-04
# NOTES: Lemlem Baraki | SI

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

data_folder <- "Data/"


ref_id <- "09b4f778"

# IMPORT ------------------------------------------------------------------

df <- data_folder %>% 
  return_latest("IIT FY 22 & FY 23") %>% 
  read_excel() %>% 
  janitor::clean_names()

# MUNGE -------------------------------------------------------------------

    #pt level data
      #want to have columns of age, sex, ART duration, LTFU duration
        #age groups: <15, 15-24, 25-35, 36-45, 46-50, >50 (align with MER)
        #code sex: 0 for female, 1 for males 
        #ART duration in yrs: <1 , 1-3 yr, 3-6, 7-10, >10yrs
        #LTFU duration in months: <1month , 1-3 months, 4-6 months, >6 months
      #make several new variables from columns (K-J), (L-K), (L-I)
      #new columns (V-X): traced & located (yes/no),return to TX (yes/no), reason for not return to TX (comment) 
    
# Address age / sex recoding - align to MER
df_clean <- df %>% 
  #select(region, woreda, age, gender, client_art_start_date, missed_appointment_date, ltfu_recorded_date) %>%
  mutate(sex_binary = ifelse(gender == "Male", "1", "0")) %>%
  mutate(age_bands = case_when(
    age %in% c(1:4) ~ "1-4",
    age %in% c(5:9) ~ "5-9",
    age %in% c(10:14) ~ "10-14", 
    age %in% c(15:19) ~ "15-19", 
    age %in% c(20:24) ~ "20-24",
    age %in% c(25:29) ~ "25-29",
    age %in% c(30:34) ~ "30-34",
    age %in% c(35:39) ~ "35-39",
    age %in% c(40:44) ~ "40-44",
    age %in% c(45:49) ~ "45-49",
    age %in% c(50:100) ~ "50+"))

#Create Tracing variable and RTT variable

# If Column U == Return to Clinc, Active on Treatment, Hospitalized, Self Transfer Out → Returned to Treatment
# If Column U == Refused, Death → No for RTT


df_clean %>% 
  mutate(traced_binary = ifelse(ltfu_outcome_2 %in% c("Unknown Outcome", "No information found", "Other"), 0, 1)) %>% 
  mutate(traced = ifelse(traced_binary == 1, "Traced", "Not Traced")) %>% 
  mutate(rtt_binary = ifelse(ltfu_outcome_2 %in% c("Returned to clinic", "Active on treatment", "Hospitalized", "Self transfered out"), 1, 0)) %>% 
  mutate(rtt = ifelse(rtt_binary == 1, "RTT", "Not RTT")) 
                                   
  
# VIZ -------------------------------------------------------------------

# SPINDOWN -------------------------------------------------------------------

    
                                