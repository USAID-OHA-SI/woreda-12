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
library(table1)


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
    age %in% c(36:100) ~ "35+"))
    

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
                                         record_ltfu_duration > 90 ~ "90+ days")) %>% 
  mutate(custom_report_duration = custom_report_date - missed_appointment_date) %>% 
  mutate(missed_custom_report_dur_grp = case_when(custom_report_duration < 30 ~ "<30 days",
                                         custom_report_duration >= 30 & custom_report_duration < 60 ~ "30-60 days",
                                         custom_report_duration >= 60 & custom_report_duration <= 90 ~ "60-90 days",
                                         custom_report_duration > 90 ~ "90+ days"))
  

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
                                         tracing_attempts %in% c("3", "4", "5") ~ "3+ attempts")) %>% 
  mutate(tracing_method_grp = case_when(ltfu_tracing_method_3 %in% c("informed_by_family", "informed_by_treatment_supporter") ~ "informed",
                                        TRUE ~ ltfu_tracing_method_3)) 

# DATA ANALYSIS ------------------------------------------------------------------------

#view calculated variables - 15 of them --> using 11
df_viz <- df_final%>% 
  select(gender, sex_binary, age_bands, traced_binary, rtt_binary, 
                   year_art_init,month_art_init, year_art_init_grp,
                   record_ltfu_duration, missed_ltfu_dur_grp, custom_report_duration,
                   missed_custom_report_dur_grp, region_cat, healthfac_type, 
                   tracing_attempt_grp, tracing_method_grp)  #exclude rtt and traced
  #drop_na()

  #view(df_viz)


#Outcome variable: rtt_binary, traced_binary
  #RTT: 0 for no, 1 for yes 
tally(~rtt_binary, data = df_viz, format="count") #3421 not RTT vs 7797 RTT
tally(~rtt_binary, data = df_viz, format="percent") 

  #Traced: 0 for no, 1 for yes 
tally(~traced_binary, data = df_viz, format="count") #3019 not traced vs 8199 traced 

#Exposure/predictive variables: 
  #sex: 0 is for females, 1 is for males 
tally(~sex_binary, data = df_viz, format="count") #total of 6418 females and 4803 males 
tally(~sex_binary, data = df_viz, format="percent")
with(df_viz, table(rtt_binary, sex_binary))
with(df_viz, prop.table(table(rtt_binary, sex_binary), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$sex_binary)
a <- table(df_viz$sex_binary, df_viz$rtt_binary) #Of those who RTT, 4564 were female and 3232 were male
chisq.test(a) 

  #age bands >15, 15-24, 25-35, >35
tally(~age_bands, data = df_viz, margins=T)#total of 359 under 15, 1495 btwn 15-24, 4607 btwn 25-35, and 4756 above 35
b<- table(df_viz$age_bands, df_viz$rtt_binary) #Of those who RTT, 288 under 15, 983 15-24, 3189 25-35, and 3336 35+
with(df_viz, table(rtt_binary, age_bands))
with(df_viz, prop.table(table(rtt_binary, age_bands), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$age_bands)
chisq.test(b)

  #region: Addis, Amhara, Gambela, Oromia, Other
tally(~region_cat, data = df_viz)#total of 4254 in Addis, 3315 in Amhara, 681 in Gambela, 2077 in Oromia, 891 in Other regions
c<-table(df_viz$region_cat, df_viz$rtt_binary) #Of those who RTT, 2743 from AA, 2167 from Amhara, 457 from Gambela, 1715 Oromia, and 715 Other
with(df_viz, table(rtt_binary, region_cat))
with(df_viz, prop.table(table(rtt_binary, region_cat), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$region_cat)
chisq.test(c)

  #health facility type: Center/Clinic/Post vs General/Tertiary/Other
tally(~healthfac_type, data = df_viz)#total 7099 in Centers vs 4119 in Other
d<- table(df_viz$healthfac_type, df_viz$rtt_binary) #of those who RTT, 4890 in Centers/Clinics vs 2907 in Other Hospitals
with(df_viz, table(rtt_binary, healthfac_type))
with(df_viz, prop.table(table(rtt_binary, healthfac_type), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$healthfac_type)
chisq.test(d)

  #LTFU duration
tally(~missed_ltfu_dur_grp, data = df_viz) #total 7099 in Centers vs 4119 in Other
e<- table(df_viz$missed_ltfu_dur_grp, df_viz$rtt_binary) #of those who RTT, 4890 in Centers/Clinics vs 2907 in Other Hospitals
with(df_viz, table(rtt_binary, missed_ltfu_dur_grp))
with(df_viz, prop.table(table(rtt_binary, missed_ltfu_dur_grp), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$missed_ltfu_dur_grp)
chisq.test(e)

  #CRP duration
tally(~missed_custom_report_dur_grp, data = df_viz)#total 7099 in Centers vs 4119 in Other
f<- table(df_viz$missed_custom_report_dur_grp, df_viz$rtt_binary) #of those who RTT, 4890 in Centers/Clinics vs 2907 in Other Hospitals
with(df_viz, table(rtt_binary, missed_custom_report_dur_grp))
with(df_viz, prop.table(table(rtt_binary, missed_custom_report_dur_grp), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$missed_custom_report_dur_grp)
chisq.test(f)

  #Number of tracing attempts 
tally(~tracing_attempt_grp, data = df_viz)#total 7099 in Centers vs 4119 in Other
g<- table(df_viz$tracing_attempt_grp, df_viz$rtt_binary) #of those who RTT, 4890 in Centers/Clinics vs 2907 in Other Hospitals
with(df_viz, table(rtt_binary,tracing_attempt_grp))
with(df_viz, prop.table(table(rtt_binary, tracing_attempt_grp), margin = 2))
chisq.test(df_viz$rtt_binary, df_viz$tracing_attempt_grp)
chisq.test(g)
  

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


# MAKE TABLES -----------------------------------------------------------------
#Guide: https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html 

 #Table 1: outcome by RTT
  #Recode variable categories
df_viz$rtt_bin<-factor(df_viz$rtt_binary,
                          labels=c("Not RTT","RTT")) 

df_viz$sex_bin<-factor(df_viz$sex_binary,
                          labels=c("Females","Males"))

    #Add labels
label(df_viz$sex_bin)        <- "Sex"
label(df_viz$age_bands)        <- "Age"
label(df_viz$region_cat)        <- "Region"
label(df_viz$healthfac_type)        <- "Health Facility"
label(df_viz$missed_ltfu_dur_grp)        <- "LTFU duration"
label(df_viz$missed_custom_report_dur_grp)        <- "Custom report duration"
label(df_viz$tracing_attempt_grp)        <- "Number of tracing attempts"
label(df_viz$tracing_method_grp)        <- "Tracing method"

table1::table1(~sex_bin + age_bands + region_cat + healthfac_type
       + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
         tracing_attempt_grp | rtt_bin, data = df_viz,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", #table style 
       #footnote = "***",
       caption = "Table 1: Baseline Cohort Demographics")

si_save("Graphics/table1.svg")

#substitute in p-values
table1::table1(~sex_bin + age_bands + region_cat + healthfac_type
               + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
              tracing_attempt_grp | rtt_bin, data = df_viz,
              overall = F,
              extra.col=list(`P-value`=pvalue),
               topclass="Rtable1-grid Rtable1-shade Rtable1-times", #table style 
               #footnote = "***",
               caption = "Table 1: Baseline Cohort Demographics")


  #Table 2: outcome by tracing
df_viz$traced_bin<-factor(df_viz$traced_binary, 
                             labels=c("Not Traced", "Traced"))

table1::table1(~sex_bin + age_bands + region_cat + healthfac_type
               + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
                 tracing_attempt_grp| traced_bin, data = df_viz,
               topclass="Rtable1-grid Rtable1-shade Rtable1-times",
               #footnote = "***",
               caption = "Table 2: Baseline Cohort Demographics")

si_save("Graphics/table2.svg")

#substitute in p-values
table1::table1(~sex_bin + age_bands + region_cat + healthfac_type
               + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
                 tracing_attempt_grp | traced_bin, data = df_viz,
               overall = F,
               extra.col=list(`P-value`=pvalue),
               topclass="Rtable1-grid Rtable1-shade Rtable1-times", #table style 
               #footnote = "***",
               caption = "Table 2: Baseline Cohort Demographics")


  #alternative - using kable from kableExtra package
#Variables.to.compare=c("Sex", "Age bands", "Regions", "Health Facility Type", "LTFU duration", "CRP duration", "Number of tracing attempts")
#Comparison.table=matrix(nrow=length(Variables.to.compare), ncol=3)
#colnames(Comparison.table)=c("RTT", "not RTT", "p-value")
#rownames(Comparison.table)=Variables.to.compare

#Comparison.table[1:2,1:2]=c(100*chisq.test(a)$estimate, chisq.test(a)$p.value)
#names(c)
#Comparison.table[3:6,3:4]=c(100*chisq.test(b)$estimate, chisq.test(a)$p.value)
#Comparison.table[7:11,5:6]=c(100*chisq.test(c)$estimate, chisq.test(a)$p.value)
#Comparison.table[12:13,7:8]=c(100*chisq.test(d)$estimate, chisq.test(a)$p.value)

#kableExtra::kable(Comparison.table, digits=c(1,1,3), colnames=NULL, align="c")%>%
  #kableExtra::add_header_above(c("", "RTT\n (n=7797)", "not RTT\n (n=3421)", "P-value\n (t-test)"))

  #Alternative - using tabmulti from tab package: covariates on left + exposure on right 
#tab::tabmulti(sex_binary + age_bands + healthfac_type ~ ***, data = df_viz)
#tab::tabmulti(as.numeric(age_bands) + sex_binary + healthfac_type ~ ***, data = df_viz)


  #kable: do all the tests and enter into table object and format the table object
#kableExtra::kable(Comparison.table, digits=c(***), colnames = NULL, align="c")%>% 
  #kableExtra::add_header_above(c("", "", "", ))



# REGRESSION -------------------------------------------------------------------
#Helper packages for modeling 
library(rsample)
library(caret)
library(vip)
#library(glmnet)

#Split data into training and testing sets - intent is to predict the RTT response variable 
set.seed(123)
churn_split <- initial_split(df_viz, prop = .7, strata = "rtt_binary")
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

#log regression: need to set data to training for baseline 
model0 <- glm(rtt_binary ~ sex_bin + age_bands + region_cat + healthfac_type
           + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
             tracing_attempt_grp, data = churn_train, family = "binomial")

summary(model0)
#exp(coef(model0), confint(model0))
#exp(cbind(OR = coef(model0), confint(model0))) #for odds ratio 
  #display(model0, detail=T)

  #This model finds that controlling for ***, *** is associated with *** times the
  #odds of having RTT relative to those who were ***
  #This model finds that the odds of a PLHIV returning to treatment increases by
  #0.879 for every Male, 

model1=glm(traced_binary ~ sex_bin + age_bands + region_cat + healthfac_type
           + missed_ltfu_dur_grp + missed_custom_report_dur_grp + 
             tracing_attempt_grp,data=churn_train, family = "binomial")

summary(model1)
  #display(model0, detail=T)
  #exp(cbind(OR = coef(model1), confint(model1)))

#Note: want reference groups to be: male, <15, Other regions, Hospitals
  #relevel sex and region
  df_viz$sex_b <- relevel(df_viz$sex_bin, ref = "Males")
  df_viz$region_cat <- factor(df_viz$region_cat, ordered = FALSE,
                              levels = c("Addis Ababa", "Amhara", "Gambela",
                                         "Oromia", "Other regions"))
  df_viz$region_cat <- relevel(df_viz$region_cat, ref = "Other regions")

  
#Rerun log regressions 
    #include: age, sex, region, healthfac, record ltfu duration, missed ltfu duration, missed CRP duration
  model3=glm(rtt_binary ~ sex_bin + age_bands + region_cat + healthfac_type
             + record_ltfu_duration + missed_ltfu_dur_grp + 
               missed_custom_report_dur_grp + 
               tracing_attempt_grp,data=churn_train, family = "binomial")
  summary(model3)
#exp(cbind(OR = coef(model3), confint(model3)))

#Assess model accuracy 
#Guide: https://bradleyboehmke.github.io/HOML/logistic-regression.html#assessing-model-accuracy-1
  churn_train <- na.omit(churn_train)
  churn_train$rtt_binary = as.factor(churn_train$rtt_binary)
  
set.seed(123)
cv_model1 <- train(rtt_binary ~ sex_bin, 
                   data = churn_train,
                   method = "glm",
                   family = "binomial", 
                   trControl = trainControl(method = "cv", number = 10)) #conduct 10-fold validated log regression models 

cv_model2 <-train(rtt_binary ~ sex_bin + age_bands, 
                  data = churn_train,
                  method = "glm",
                  family = "binomial", 
                  trControl = trainControl(method = "cv", number = 10))

cv_model3 <-train(rtt_binary ~ sex_bin + age_bands + region_cat + healthfac_type
                  + record_ltfu_duration + missed_ltfu_dur_grp + 
                    missed_custom_report_dur_grp + 
                    tracing_attempt_grp, 
                  data = churn_train,
                  method = "glm",
                  family = "binomial", 
                  trControl = trainControl(method = "cv", number = 10))

  #extract sample performance measures 
summary(
  resamples(
    list(
      model1 = cv_model1,
      model2 = cv_model2
      #model3 = cv_model3
    )
  )
)$statistics$Accuracy

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

    


    
                                