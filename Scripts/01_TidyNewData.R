# PROJECT: Analyze new IIT data 
# PURPOSE: Munge and Analysis of IIT Data 
# AUTHOR: Lemlem Baraki, Karishma Srikanth| SI
# REF ID:   eabe7b34
# LICENSE: MIT
# DATE: 2023-12-04
# UPDATED: 2023-12-21 

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
    data_folder <- "Data/"
  
  # REF ID for plots
    ref_id <- "eabe7b34"
    
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
      return_latest(pattern = "IIT FY 22 & FY 23 as of Q4.") %>% 
      read_excel() %>% 
      janitor::clean_names()
    
    glimpse(df)
    names(df)

# MUNGE ============================================================================
  
  #Convert date format
    df_clean <- df%>% 
      mutate(across(contains("date"), ~str_replace(., "^\\S* ",  ""))) %>% 
      mutate(across(contains("date"), ~mdy(.)))
    
  #Recode age/sex to align with MER
    df_clean <- df_clean %>% 
      mutate(sex_binary = ifelse(gender == "Male", "1", "0")) %>% 
      mutate(age_bands = case_when(
        age %in% c(1:14) ~ "<15",
        age %in% c(15:24) ~ "15-24",
        age %in% c(25:35) ~ "25-35",
        age %in% c(36:100) ~ "35+"
      ))
  
  #Create Tracing & RTT variable 
    df_clean <- df_clean %>% 
      mutate(traced_binary = ifelse(ltfu_outcome %in% c("Unknown Outcome",
                                                          "No information found"), 0, 1)) %>% 
      mutate(traced = ifelse(traced_binary == 1, "Traced", "Not Traced")) %>% #Traced categorical 
      mutate(rtt_binary = ifelse(ltfu_outcome %in% c("Returned to clinic",
                                                       "Active on treatment",
                                                       "Hospitalized",
                                                       "Self transfered out"), 1, 0)) %>% 
      mutate(rtt = ifelse(rtt_binary == 1, "RTT", "Not RTT")) #RTT categorical
    
  #Create ART initiation year 
    df_clean <- df_clean %>% 
      mutate(year_art_init = lubridate::year(client_art_start_date),
             month_art_init = lubridate::month(client_art_start_date)) %>% 
      mutate(year_art_init_gap = case_when(year_art_init <= 2018 ~ "Before October 2019",
                                           year_art_init == 2019 & month_art_init < 10  ~ "Before October 2019",
                                           year_art_init == 2019 & month_art_init >= 10 ~ "Between Oct  2019 and Sept 2021",
                                           year_art_init == 2020 ~ "Between Oct 2019 and Sept 2021",
                                           year_art_init == 2021 & month_art_init < 10 ~ "Between Oct  2019 and Sept 2021",
                                           year_art_init == 2021 & month_art_init >=10 ~ "After October 2021",
                                           year_art_init >= 2022 ~ "After October 2021"))
    
  #Create LTFU duration in months
    df_clean <- df_clean %>%
      mutate(record_ltfu_duration = ltfu_recorded_date - missed_appointment_date) %>% 
      mutate(missed_ltfu_dur_grp = case_when(record_ltfu_duration < 30 ~ "<30 days",
                                             record_ltfu_duration >= 30 & record_ltfu_duration < 60 ~ "30-60 days",
                                             record_ltfu_duration >= 60 & record_ltfu_duration <= 90 ~ "60-90 days",
                                             record_ltfu_duration > 90 ~ "90+ days")) %>% 
      mutate(custom_report_duration = custom_report_date - missed_appointment_date) %>% 
      mutate(missed_custom_report_dur_grp = case_when(custom_report_duration < 30 ~ "<30 days",
                                                      custom_report_duration >= 30 & custom_report_duration < 60 ~ "30-60 days",
                                                      custom_report_duration >= 60 & custom_report_duration <= 90 ~ "60-90 days",
                                                      custom_report_duration > 90 ~ "90+ days"))
    
  #Collapse regions
    df_clean <- df_clean %>%  
      mutate(region_cat = case_when(
        region %in% c("Gambela", "Gambella") ~ "Gambela",
        region %in% c("Sidama", "SNNPR", "South West") ~ "Other regions",
        TRUE ~ region))
    
  #Health facility grouping (health center/clinic/post vs general/tertiary/other)
    df_clean <- df_clean %>% 
      mutate(healthfac_type = case_when(
        str_detect(health_facility, "Health Center|Clinic|Post|Health center|Heath Center|health Center") ~ "Health Center, Clinic, or Post",
        TRUE ~ "General, Tertiary, Other Hospital")) 
    
  #Tracing attempt grouping (0, 1-2, 3) 
    df_clean <- df_clean %>%
    mutate(tracing_attempt_grp = case_when(#tracing_attempts == "---" ~ "0 attempts",
                                           tracing_attempts %in% c("1", "2") ~ "1-2 attempts",
                                           tracing_attempts %in% c("3", "4", "5") ~ "3+ attempts",
                                           TRUE ~ "0 attempts"))  
    
  #create fiscal year based on 30 days after missed appt date
    df_clean <- df_clean %>% 
      mutate(missed_appt_30_days_after = missed_appointment_date + 30) %>% 
      mutate(fiscal_year = if_else(month(missed_appt_30_days_after) >= 10,
                                   year(missed_appt_30_days_after) + 1,
                                   year(missed_appt_30_days_after))) 
    
  #Tracing method grouping
    df_final <- df_clean %>%
    mutate(tracing_method_grp = case_when(ltfu_tracing_method_3 %in% c("informed_by_family",
                                                                       "informed_by_treatment_supporter") ~ "informed",
                                          TRUE ~ ltfu_tracing_method_3)) 
  
   df_final <- df_final %>% 
      filter(tracing_attempt_grp!="0 attempts") %>% #exclude 0 attempts
      filter(region_cat == "Addis Ababa") #reduce sample size to one region 


# VIZ ============================================================================
    #Limit df
    df_log <- df_final %>% 
      dplyr::select(sex_binary, age_bands, year_art_init_gap, healthfac_type, missed_ltfu_dur_grp,
             missed_custom_report_dur_grp, tracing_attempt_grp, rtt_binary, traced_binary,
             region_cat, tracing_method_grp) %>% 
      filter(tracing_attempt_grp!="0 attempts") %>% #exclude 0 attempts
      filter(region_cat == "Addis Ababa") #reduce sample size to one region 
    
 
  #Table 1: Outcome by RTT
    df_log$rtt_bin <- factor(df_log$rtt_binary, 
                             labels = c("Not RTT", "RTT"))
    
    df_log$sex_bin <- factor(df_log$sex_binary, 
                             labels = c("Females", "Males"))
    
    #add labels 
    label(df_log$sex_bin)        <- "Sex"
    label(df_log$age_bands)        <- "Age"
    label(df_log$region_cat)        <- "Region"
    label(df_log$healthfac_type)        <- "Health Facility"
    label(df_log$missed_ltfu_dur_grp)        <- "LTFU duration"
    label(df_log$missed_custom_report_dur_grp)        <- "Custom Report Duration"
    label(df_log$tracing_attempt_grp)        <- "Number of tracing attempts"
    label(df_log$tracing_method_grp)        <- "Tracing Method"
    
    names(df_log)
    
     table1::table1(~sex_bin + age_bands + region_cat + healthfac_type + 
                     missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
                     tracing_attempt_grp | rtt_bin, data = df_log,
                   #overall = F,
                   extra.col=list(`P-value`=pvalue),#add in p-values
                   topclass = "Rtable1-grid Rtable1-shade Rtable1-times", 
                   caption = "Table 1: Baseline Cohort Demographics") 
   
    
  #Table 2: Outcome by Traced 
    df_log$traced_bin <-factor(df_log$traced_binary, 
                              labels=c("Not Traced", "Traced"))
    
    table1::table1(~sex_bin + age_bands + region_cat + healthfac_type
                   + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
                     tracing_attempt_grp | traced_bin, data = df_log,
                   #overall = F,
                   #extra.col=list(`P-value`=pvalue),#add in p-values
                   topclass="Rtable1-grid Rtable1-shade Rtable1-times", #table style 
                   caption = "Table 2: Baseline Cohort Demographics")
    
    
# Regressions --------------------------------------------------------
 
    
    glimpse(df_log)
    names(df_log)
    
    library(mosaic)
    library(arm)
    
    #Outcome variables: rtt_binary, traced_binary
    tally(~rtt_bin, df_log, format = "count") 
      #Of 4453 participants, 3455 returned to treatment and 1001 did not 
    tally(~traced_bin, df_log, format = "count")
      #Of 44543 participants, 3720 were traced and 733 were not traced 
    
    
    #Predictor variables: age_bands, sex_binary, region_cat, healthfac_type
    tally(~sex_bin, df_log, format = "count")
      #Of 4453 participants, 2555 were female and 1898 were male 
    tally(~age_bands, df_log, format = "count")
      #Of 4453 participants, 67 were <15, 469 btwn 15-24, 1707 btwn 25-35, and 2210 were >35
    tally(~healthfac_type, df_log, format = "count")
      #Of 4453 participants, 1454 were seen at a hospital and 2999 were seen at a center/clinic/post
    
    
    #Log regression
    model_rtt <- glm(rtt_bin ~ sex_bin + age_bands + healthfac_type + missed_ltfu_dur_grp +
                       missed_custom_report_dur_grp + tracing_attempt_grp,
                     data = df_log, family = "binomial")
    
    summary(model_rtt)
    
    model_traced=glm(traced_bin ~ sex_bin + age_bands + healthfac_type + missed_ltfu_dur_grp +
                       missed_custom_report_dur_grp + tracing_attempt_grp,
                     data=df_log, family = "binomial")
    
    summary(model_traced)
    
    
# SPINDOWN ============================================================================

 # today <- lubridate::today()
  
  write_csv(df_final, glue("Dataout/newdata-ethiopia-rtt-patient-data-cleaned-{lubridate::today()}.csv"))  
  