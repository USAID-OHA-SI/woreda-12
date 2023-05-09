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
  return_latest("IIT FY 22 & FY 23 as of Q2 Data _ May 2_2023_defulat date removed") %>% 
  read_excel() %>% 
  janitor::clean_names()

# MUNGE -------------------------------------------------------------------

    #pt level data
      #want to have columns of age, sex, ART duration, LTFU duration
        #age groups: <15, 15-24, 25-35, <35 (align with MER)
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
    age %in% c(1:14) ~ "<15",
    age %in% c(15:24) ~ "15-24",
    age %in% c(25:35) ~ "25-35",
    age %in% c(36:100) ~ ">35"))
    

#Create Tracing variable and RTT variable
df_clean <- df_clean %>% 
  mutate(traced_binary = ifelse(ltfu_outcome_2 %in% c("Unknown Outcome", "No information found"), 0, 1)) %>% 
  mutate(traced = ifelse(traced_binary == 1, "Traced", "Not Traced")) %>% 
  mutate(rtt_binary = ifelse(ltfu_outcome_2 %in% c("Returned to clinic", "Active on treatment", "Hospitalized", "Self transfered out"), 1, 0)) %>% 
  mutate(rtt = ifelse(rtt_binary == 1, "RTT", "Not RTT")) 

#convert all date formats
df_clean <- df_clean %>% 
  #mutate(flag_date_issue = ifelse(client_art_start_date == "Monday, January 1, 1900", TRUE, FALSE)) %>% 
  mutate(across(contains("date"), ~str_replace(., "^\\S* ", ""))) %>% 
  mutate(across(contains("date"), ~mdy(.))) 

# create Year of ART initiation 
df_clean <- df_clean %>% 
  mutate(year_art_init = lubridate::year(client_art_start_date),
         month_art_init = lubridate::month(client_art_start_date)) %>% 
  mutate(year_art_init_grp = case_when(year_art_init <= 2018 ~ "Before October 2019",
                                       year_art_init == 2019 & month_art_init < 10  ~ "Before October 2019",
                                       year_art_init == 2019 & month_art_init >= 10 ~ "Between Oct  2019 and Sept 2021",
                                       year_art_init == 2020 ~ "Between Oct 2019 and Sept 2021",
                                       year_art_init == 2021 & month_art_init < 10 ~ "Between Oct  2019 and Sept 2021",
                                       year_art_init == 2021 & month_art_init >=10 ~ "After October 2021",
                                       year_art_init >= 2022 ~ "After October 2021")) 

# Create LTFU duration in months: <1 month , 1-3 months, 4-6 months, >6 months
df_clean <- df_clean %>% 
  mutate(record_ltfu_duration = ltfu_recorded_date - missed_appointment_date) %>% 
  mutate(missed_ltfu_dur_grp = case_when(record_ltfu_duration < 30 ~ "<30 days",
                                         record_ltfu_duration >= 30 & record_ltfu_duration < 60 ~ "30-60 days",
                                         record_ltfu_duration >= 60 & record_ltfu_duration <= 90 ~ "60-90 days",
                                         record_ltfu_duration > 90 ~ ">90 days")) %>% 
  mutate(custom_report_duration = custom_report_date - missed_appointment_date) %>% 
  mutate(missed_custom_report_dur_grp = case_when(custom_report_duration < 30 ~ "<30 days",
                                         custom_report_duration >= 30 & custom_report_duration < 60 ~ "30-60 days",
                                         custom_report_duration >= 60 & custom_report_duration <= 90 ~ "60-90 days",
                                         custom_report_duration > 90 ~ ">90 days"))
  

#Regroup regions: 5 groups - Addis Ababa, Amhara, Oromia, Gambela, Other regions (Sidama, SNNPR, South West)
#health_facility: create binary (health center/clinic/post vs general/tertiary/other)
#tracing_attempts: 0,1-2,3+
#tracing method: collapse informed by family and TX supporter


df_final <- df_clean %>%  
  mutate(region_cat = case_when(
    region %in% c("Sidama", "SNNPR", "South West") ~ "Other regions",
    TRUE ~ region)) %>% 
  mutate(healthfac_type = case_when(
    str_detect(health_facility, "Health Center|Clinic|Post|Health center|Heath Center|health Center") ~ "Health Center, Clinic, or Post",
    TRUE ~ "General/Tertiary/Other Hospital")) %>%
  mutate(tracing_attempt_grp = case_when(tracing_attempts == "---" ~ "0 attempts",
                                         tracing_attempts %in% c("1", "2") ~ "1-2 attempts",
                                         tracing_attempts %in% c("3", "4", "5") ~ ">3 attempts")) %>% 
  mutate(tracing_method_grp = case_when(ltfu_tracing_method_3 %in% c("informed_by_family", "informed_by_treatment_supporter") ~ "informed",
                                        TRUE ~ ltfu_tracing_method_3)) 

# SPINDOWN -------------------------------------------------------------------


today <- lubridate::today()

write_csv(df_final, glue("Dataout/newdata-ethiopia-rtt-patient-data-cleaned-{today}.csv"))
  




# create new variable for record of LTFU ( J-I column; Missed appointment date - LTFU Date)
# create new variable ( K-J) then create groups (LTFU date to CRP assigned for tracing)
# create new variable ( L-K) then create groups (Trace data minus CRP)
# create variable for L-I then create groups (Missed appointment → tracing date)

 # df_final <- df_group %>% 
 #    #time from missed appt to when LTFU recorded
 #    mutate(record_ltfu_duration = ltfu_recorded_date - missed_appointment_date) %>% 
 #    mutate(record_ltfu_duration = ifelse(record_ltfu_duration < 0, NA, record_ltfu_duration)) %>% #if art duration is negative, return NA
 #    #time from LTFU Recorded to when CRP assigned
 #    mutate(ltfu_crp_duration = crp_assigned_date_for_tracing -ltfu_recorded_date) %>%
 #    mutate(ltfu_crp_duration = ifelse(ltfu_crp_duration < 0, NA, ltfu_crp_duration)) %>% 
 #    #time from when CRp assigned to when traced
 #    mutate(trace_crp_duration = ltfu_tracing_date -crp_assigned_date_for_tracing) %>%
 #    mutate(trace_crp_duration = ifelse(trace_crp_duration < 0, NA, trace_crp_duration)) %>% 
 #    mutate(trace_crp_duration = ifelse(ltfu_tracing_date == as.Date("1900-01-01") | crp_assigned_date_for_tracing == as.Date("1900-01-01")
 #                                       , NA, trace_crp_duration)) %>% 
 #    #time from when missed appointmnet to when traced date
 #    mutate(trace_duration_from_missed = ltfu_tracing_date - missed_appointment_date) %>%
 #   # mutate(trace_duration_from_missed = ifelse(trace_duration_from_missed < 0, NA, trace_duration_from_missed)) %>% 
 #    mutate(trace_duration_from_missed = ifelse(ltfu_tracing_date == as.Date("1900-01-01") | crp_assigned_date_for_tracing == as.Date("1900-01-01")
 #                                       , NA, trace_duration_from_missed)) 

#create ART duration - no longer need these variables

# df_group <- df_clean %>% 
#    mutate(art_duration = missed_appointment_date - client_art_start_date) %>% 
#   mutate(art_duration = ifelse(art_duration < 0, NA, art_duration)) %>% #if art duration is negative, return NA
#   mutate(art_duration_grp = case_when(art_duration < 365 ~ "<1 year",
#                                       art_duration >= 365 & art_duration <1095 ~ "1-3 years",
#                                       art_duration >=1095 & art_duration <2190 ~ "3-6 years",
#                                       art_duration >=2190 & art_duration <3650 ~ "6-10 years",
#                                       art_duration >=3650 ~ ">10 years")) 

    


    
                                