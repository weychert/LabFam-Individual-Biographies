##################### BHPS/UKHLS employment history ############################
# This script processes employment history data from two major UK longitudinal household surveys: 
# the British Household Panel Survey (BHPS) and the UK Household Longitudinal Study (UKHLS). 
# It transforms raw employment records into a structured panel dataset suitable for longitudinal 
# and life-course analysis of work trajectories in the UK.
# Load necessary packages
# Sources of information on employment:
# BHPS
# 1. bletter_indresp_protect.dta (18 BHPS waves (1991–2008))
# 2. lifetime lifetime (Wave 2,11,12; years: 1992, 1993, 2002, 2003) BHPS retrospective lifetime employment history
# UKHLS
# 1. w_empstat (Wave 1, 5; years: 2009, 2014)
# 2. ukhls_indresp (14 UKHLS Waves (2009-2022))

# Load necessary objects 
source("00_setting_work_space.R")

# List of steps:
######### BHPS:
# 1. Process BHPS Employment Status (1991–2008) from indresp files 
# processes BHPS individual-level survey files (indresp) from 18 waves, 
# extracts relevant employment data, 
# constructs start dates for labor force status and current jobs, 
# labels employment status categories, and compiles all waves into a harmonized longitudinal dataset.
# list of steps:
# a. Define Variables of Interest
# b. List All BHPS indresp Files
# c. Loop Through Each Wave (1–18)
# d. Construct Start Dates for Labor Force Status
# e. Construct Start Dates for Current Job
# f. Recode Employment Status

# 2. Generate Monthly Employment Episodes from bhps indresp files 
# a. Prepare and Identify Spells Based on Employment Transitions
# b. Filter full respondents (ivfio == 1).
# c. Handle Missing Start Dates and Set End Dates
# d. Impute missing start dates using prior wave info or interview dates.
# e. Collapse repeated rows into single spells per person.
# f. Fix date inconsistencies (start date > end date). There are 13 undividuals like this 
# g. Expand each spell into monthly rows using neatRanges::expand_dates.

# 3.BHPS – Lifetime Employment History Processing
# a. Define Variables of Interest
# b. Loop Over Waves
# c. Standardize Month Codes - Special non-calendar codes (13–17) are converted to approximate calendar months:
# d. Generate Start/End Dates and Harmonize Employment Status
# e. Prepare Last Interview Date
# f. Merge Last Interview Date and lifemst to Adjust Dates
# g. Expand Data by Month - Use expand_dates() to generate a row for every month between start_date and end_date

# 4. Merge with main survey file lifemst and indresp
# This step merges the monthly-expanded lifetime employment history (lifemst_3) with monthly individual-level employment data (bhps_indresp_3) to create a comprehensive person-month employment panel:
# Full Join: Ensures all records from both sources are included (all = TRUE).
# Prioritization: Where overlapping, values from bhps_indresp_3 are preferred over lifemst_3.
# Coalescing: Uses coalesce() to fill in employment_status and spell from either source, prioritizing .y (indresp).
# ("Use employment_status.y (the indresp value) if it exists. If it doesn’t, use employment_status.x (from lifetime data)", Same thing for spell)

######### UKHLS
# 5. empstat from UKHLS - cleaning 
# a. List Files and variables
# b. Loop Through File
# c. Standardize Month Codes
# d. Handle Invalid Dates
# e. create start date
# f. Recode Employment Categories
# g. Generate End Date from Next Spell
# h. expands each employment spell into monthly records using the neatRanges::expand_dates() function:
# For every row in ukhls_empstat_2, it creates one row per month between start_date and end_date.
# It keeps key identifying variables: pidp, employment_status, and nr_emp_spell.
# The result is a person-month panel dataset, where each row represents a specific individual in a specific month, along with their employment status during that time.
# expand dates to dates here how many do we delete? one obs was deleted :)

# 6. indresp from UKHLS - cleaning
# a. List Files and Define Variables
# b. Loop Through Files
# c. Identify First/Last Interview per Person
# d. Construct Start/End Dates for Spells
# e. Detect Employment Spells
# f. Aggregate to Spell-Level
# g. Expand to Person-Month Format

# 7. Combine UKHLS Employment Data from Retrospective and Main Survey Sources
# Purpose: Merge the monthly-expanded retrospective employment history (ukhls_empstat_3) with employment information reconstructed from the main individual survey (ukhls_employment_5).
# Merge Type: A full join (all = TRUE) is used to include all available person-month records from both sources, ensuring no data is lost.
# Conflict Resolution:
# When the same pidp and month (Expanded) appears in both datasets:
# employment_status.y (from the main survey) is preferred over employment_status.x (from retrospective).
# Similarly, spell is taken from the main survey (spell) if available, or falls back to nr_emp_spell (from retrospective).
# Coalescing Explained Simply:
# "Use the value from the main survey if it exists. If it's missing, fall back on the retrospective version."
# merged_ukhls = merge(as.data.table(ukhls_empstat_3), as.data.table(ukhls_employment_5), by = c("pidp", "Expanded"), all = T)
# "Use the value from the main survey if it exists. If it's missing, fall back on the retrospective version"

##################### Combine BHPS & UKHLS #####################################
# 8. Combine and Finalize BHPS–UKHLS Employment Spells
# a.	Merge BHPS and UKHLS Person-Month Data
# Perform a full join on pidp and Expanded to combine BHPS (bhps_final) and UKHLS (merged_ukhls) monthly records.
# b.	Resolve Conflicting Values
# Use coalesce() to prioritize UKHLS values (.y) for employment_status and spell, falling back on BHPS values (.x) if needed.
# c.	Extract Last Interview Date Per Respondent
# Reshape interview dates from wide to long format.
# Keep only the latest interview per individual.
# Round interview dates down to the first of the month (Expanded format).
# d. Detect Employment Spells
# Identify changes in employment_status to flag the beginning of a new spell.
# Use cumsum() to number spells within each individual.
# e. Create Start and End Dates for Each Spell
# For each pidp and spell:
# Set the first month as the start_date.
# Tentatively define the end_date as the month before the next spell starts.
# f. Impute End Dates Using Interview Information
# Merge in the last interview date.
# If a spell has no end_date, use the interview date as a fallback.
# Round end_date up to the last day of the month.
# g. Fix Overlaps and Gaps
# Ensure that end_date is not before start_date.
# Prevent overlap between consecutive spells by adjusting start_date forward if needed.
# Snap all dates cleanly to month boundaries.
# h. Split Start and End Dates into Components
# Extract year (ENTRY_Y, EXIT_Y), month (ENTRY_M, EXIT_M), and day (ENTRY_D, EXIT_D) from start_date and end_date for easier downstream use.

################################################################################
# 1. Process BHPS Employment Status (1991–2008) from indresp files 
# processes BHPS individual-level survey files (indresp) from 18 waves, 
# extracts relevant employment data, 
# constructs start dates for labor force status and current jobs, 
# labels employment status categories, and compiles all waves into a harmonized longitudinal dataset.
# a. Define Variables of Interest
# b. List All BHPS indresp Files
# c. Loop Through Each Wave (1–18)
# d. Construct Start Dates for Labor Force Status
# e. Construct Start Dates for Current Job
# f. Recode Employment Status

# a. Define Variables of Interest
vlist_indresp<-data.frame(var = c(
  "pidp", 
  "jbstat",     # Current economic activity,
  "cjsbgm",     # cjsbgm Month current labour force status began
  "cjsbgy4",    # cjsbgy4 Year current labour force status began
  "jbbgm",      # jbbgm Month started current job
  "jbbgy"       # jbbgy4 Year started current job
))

# b. List All BHPS indresp Files
bhps_files<-list.files(paste0(folder_main_uk, "bhps"),pattern = "indresp") # 18 files it is finished survey in 2009

# c. Loop Through Each Wave (1–18)
bhps_indresp<-c()
for (i in 1:length(bhps_files)) {
  
  if (i==1) {
  x<-read_dta(paste0(folder_main_uk, "bhps","/",bhps_files[i]), 
              col_select = c(ends_with(vlist_indresp$var), 
                             ends_with("ivfio"),
                             paste0("b",letters[i],"_istrtdatd"),  # istrtdatd Date of interview: day
                             paste0("b",letters[i],"_istrtdatm")  # istrtdatm Date of interview: month
                             ))
  
  
  x[ , paste0("b", letters[i], "_istrtdaty")] <-1991
  
  }else{
    
    x<-read_dta(paste0(folder_main_uk, "bhps","/",bhps_files[i]), 
                col_select = c(ends_with(vlist_indresp$var), 
                               ends_with("ivfio"),
                               paste0("b",letters[i],"_istrtdatd"),  # istrtdatd Date of interview: day
                               paste0("b",letters[i],"_istrtdatm"),  # istrtdatm Date of interview: month
                               paste0("b",letters[i],"_istrtdaty")   # istrtdaty Date of interview: 4 digit year
                ))
    
  }
  
  # Remove Prefixes from Variable Names
  
  colnames(x)<-gsub(paste0("b", letters[i], "_"),"", colnames(x))
  
  x = x %>% mutate(
    # Add Wave Indicator
    Wave = 1990+i,
    # Convert Employment Status to Character
    across(c("jbstat"), ~ sjlabelled::as_character(.)))
    
  x  = x %>%   
    mutate(
      # d. Construct Start Dates for Labor Force Status
      cjsbgy4_1 = cjsbgy4,
      cjsbgm_1 = cjsbgm,
      # cjsbgy4 - Year labour force stat began: 4 digit
      cjsbgy4 = ifelse(cjsbgy4<0,NA,cjsbgy4),
      # cjsbgm - Month current labour force status began
      cjsbgm = ifelse(cjsbgm<0,NA,cjsbgm),
      start_date_l = if_else(!is.na(cjsbgy4) & !is.na(cjsbgm),as.Date(paste(cjsbgy4, cjsbgm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
      
      start_date_l = if_else(!is.na(cjsbgy4) & is.na(cjsbgm),as.Date(paste(cjsbgy4, istrtdatm-1, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(start_date_l))
      
      ) %>% 
      mutate(
        # e. Construct Start Dates for Current Job
        # jbbgm Month started current job 1 - 18
        jbbgm = ifelse(jbbgm<0,NA,jbbgm),
        # jbbgy4 Year started current job 1 - 18
        jbbgy = ifelse(jbbgy<0,NA,jbbgy),
        start_date_j = if_else(!is.na(jbbgy) & !is.na(jbbgm),as.Date(paste(jbbgy, jbbgm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
        
        start_date_j = if_else(!is.na(jbbgy) & is.na(jbbgm),as.Date(paste(jbbgy, istrtdatm-1, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(start_date_j)),
        # interview date
        istrtdaty = as.numeric(istrtdaty),
        istrtdatd = as.numeric(istrtdatd),
        istrtdatm = as.numeric(istrtdatm),
        interview_date = as.Date(paste(istrtdaty, istrtdatm, 1, sep = "-"), format = "%Y-%m-%d")
        
      ) %>% 
      select("pidp", "Wave","jbstat", "start_date_l","start_date_j", "ivfio", "cjsbgy4_1","cjsbgm_1", "interview_date")
  

  x = x %>% mutate(
    emp_status = tolower(sjlabelled::as_character(jbstat)),
  ) %>%  
    mutate(
      # f. Recode Employment Status
      employment_status =
        case_when(
          # [1] Working 
          emp_status %in% c("employed","self-employed","employed","self-employed", "employed","self-employed", "maternity leave")  ~ "Working",
          # [2] Unemployed
          emp_status %in% c("unemployed") ~ "Unemployed",
          # [3] Not in the Labor Force
          emp_status %in% c("lt sick, disabld", "family care", "ft studt, school","gvt trng scheme") ~ "Not in the Labor Force",
          # [4] Retired
          emp_status %in% c("retired") ~ "Retired",
          # [5] Other
          emp_status %in% c("other", "missing")  ~ "Other",
          TRUE ~ NA_character_ )) %>% 
    select("pidp", "Wave","interview_date","employment_status","jbstat", "start_date_l","start_date_j", "ivfio", "cjsbgy4_1","cjsbgm_1")
  
  bhps_indresp<-bind_rows(bhps_indresp, x)
  
  rm(x)

}

# End Result - harmonized dataset bhps_indresp with:
# One row per person per wave
# Derived start dates for labor force and job spell
# Cleaned and labeled employment status
# Interview dates and wave tracking

################################################################################
# 2. Generate Monthly Employment Episodes from bhps indresp files 
# a. Prepare and Identify Spells Based on Employment Transitions
# b. Filter full respondents (ivfio == 1).
# c. Handle Missing Start Dates and Set End Dates
# d. Impute missing start dates using prior wave info or interview dates.
# e. Collapse repeated rows into single spells per person.
# f. Fix date inconsistencies (start date > end date). There are 13 undividuals like this 
# g. Expand each spell into monthly rows using neatRanges::expand_dates.

bhps_indresp_1 = bhps_indresp %>% 
  # a. Prepare and Identify Spells Based on Employment Transitions
  group_by(pidp) %>% arrange(Wave, .by_group = T) %>% 
  # b. Filter full respondents (ivfio == 1).
  mutate(nr = 1:n()) %>% # only full respondents
  filter(ivfio==1) %>% 
  group_by(pidp) %>% arrange(Wave, .by_group = T) %>% 
  mutate(
    check = employment_status!=lag(employment_status),
    check = ifelse(is.na(check),T,check),
    spell= cumsum(check)) %>%
  # c. Handle Missing Start Dates and Set End Dates
  group_by(pidp, spell) %>% 
  # if no change in employment status then we fill down the first reported start_date_l (start of labor force status)
  fill(start_date_l, .direction = "down") %>% 
  select(pidp, Wave, nr, interview_date, spell, employment_status, start_date_l,start_date_j) %>% 
  mutate(end_date = interview_date) %>% group_by(pidp) %>% arrange(Wave, .by_group = T) %>%
  # d. Impute missing start dates using prior wave info or interview dates.
  # if start_date_l is missing we take interview date from previous Wave plus one month as start of the current spell 
  mutate(start_date_l = if_else(is.na(start_date_l), as.Date(lag(end_date %m+% months(1))), as.Date(start_date_l))) %>% 
  # if it is first Wave and start_date_l is missing then we assume first interview date 
  mutate(start_date_l = if_else(is.na(start_date_l) & nr==1, as.Date(interview_date), as.Date(start_date_l))) %>% 
  # correct if start date is bigger than end date
  mutate(
    start_date_l = if_else(start_date_l>end_date, as.Date(interview_date %m-% months(1)), as.Date(start_date_l))
  ) %>% ungroup() %>% 
  group_by(pidp) %>% arrange(Wave, .by_group = T) %>% 
  mutate(check_1 = lag(end_date)>start_date_l,
  ) %>% 
  # correct if date inconsistencies 
  mutate(
    start_date_l = if_else(check_1==T & !is.na(check_1), as.Date(lag(end_date %m+% months(1))),as.Date(start_date_l)),
    check_1_1 = lag(end_date)>start_date_l
  )

# e. Collapse repeated rows into single spells per person.
# from panel data where one row one year to spell data where one row one spell

bhps_indresp_2 = bhps_indresp_1 %>% group_by(pidp,spell) %>% 
  mutate(start_date = min(start_date_l, na.rm = T),
         end_date = max(end_date,na.rm = T)) %>% 
  select(pidp,employment_status,spell,start_date, end_date) %>% distinct() %>% 
  mutate(end_date = ceiling_date(end_date, "month") - days(1))

# f. Fix date inconsistencies (start date > end date). There are 18 individuals like this
bhps_indresp_3 = bhps_indresp_2 %>% filter(start_date<=end_date)

bhps_indresp_x = bhps_indresp_2 %>% filter(start_date>end_date)

# g.Expand each spell into monthly rows using neatRanges::expand_dates.
bhps_indresp_3 = neatRanges::expand_dates(bhps_indresp_3, 
                                          start_var = "start_date", 
                                          end_var = "end_date",
                                          vars_to_keep = c("pidp","employment_status", "spell"), unit = "month") %>% mutate(month = Expanded)

################################################################################
# 3.BHPS – Lifetime Employment History Processing
# a. Define Variables of Interest
# b. Loop Over Waves
# c. Standardize Month Codes - Special non-calendar codes (13–17) are converted to approximate calendar months:
# d. Generate Start/End Dates and Harmonize Employment Status
# e. Prepare Last Interview Date
# f. Merge Last Interview Date and lifemst to Adjust Dates
# g. Expand Data by Month - Use expand_dates() to generate a row for every month between start_date and end_date


# a. Define Variables of Interest
lifetime_vlist = c("leshno", "leshst", "leshey4", "leshsy4", "leshsm", "leshem", "leshne")

# b. Loop Over Waves
lifemst = c()
for (i in c("b","k","l")) {

  x = read_dta(paste0(folder_bhps_1,"b", i,"_lifemst_protect.dta"),col_select = c("pidp",ends_with(lifetime_vlist))) 
  
  colnames(x)<-gsub(paste0("b", i, "_"),"", colnames(x))
  
  x = x %>% mutate(
    spell = leshno,
    emp_status = leshst,
    END_M = leshem,        # month lifetime emp. hist. status changed
    START_M = leshsm,      # month lifetime emp. hist. status started
    END_Y = leshey4,       # year emp. history status changed: 4 digi
    START_Y = leshsy4      # year emp. history status started: 4 digi
    ) %>% 
    mutate(END_M = ifelse(END_M<0, NA, END_M),
           START_M  = ifelse(START_M<0, NA, START_M),       
           END_Y  = ifelse(END_Y<0, NA, END_Y),              
           START_Y = ifelse(START_Y<0, NA, START_Y),
          
    # c. Standardize Month Codes - Special non-calendar codes (13–17) are converted to approximate calendar months:
           START_M  = case_when(
             START_M  %in% c(13, 17) ~ 1,
             START_M  %in% c(14) ~ 4,
             START_M  %in% c(15) ~ 8,
             START_M  %in% c(16) ~ 10,
             TRUE ~ START_M 
             ),
           
           END_M  = case_when(
             END_M  %in% c(13, 17) ~ 1,
             END_M  %in% c(14) ~ 4,
             END_M  %in% c(15) ~ 8,
             END_M  %in% c(16) ~ 10,
             TRUE ~ END_M 
           )
           ) %>% 
    mutate(
    # d. Generate Start/End Dates and Harmonize Employment Status
      start_date = case_when(
        !is.na(START_Y) & !is.na(START_M) ~ as.Date(paste(START_Y, START_M, "01", sep = "-"), format = "%Y-%m-%d"),
        !is.na(START_Y) & is.na(START_M) ~ as.Date(paste(START_Y, "01-01", sep = "-"), format = "%Y-%m-%d"),
        TRUE ~ NA
      ),
      end_date = case_when(
        !is.na(END_Y) & !is.na(END_M) ~ as.Date(paste(END_Y, END_M, "01", sep = "-"), format = "%Y-%m-%d"),
        !is.na(END_Y) & is.na(END_M) ~ as.Date(paste(END_Y, "01-01", sep = "-"), format = "%Y-%m-%d"),
        TRUE ~ NA
      )) %>% select(pidp, spell,emp_status, start_date,end_date, leshsy4, leshsm, leshey4, leshem, leshne) %>% 
    mutate(
      emp_status = sjlabelled::as_character( emp_status),
      employment_status =
        case_when(
          # [1] Working 
          emp_status %in% c("f/t paid employment","self-employed", "p/t paid employment","self-employed","maternity leave")  ~ "Working",
          # [2] Unemployed
          emp_status %in% c("unemployed") ~ "Unemployed",
          # [3] Not in the Labor Force
          emp_status %in% c("lt sick, disabld", "family care", "ft studt, school","gvt trng scheme", "national/war service") ~ "Not in the Labor Force",
          # [4] Retired
          emp_status %in% c("retired") ~ "Retired",
          # [5] Other
          emp_status %in% c("other", "don't know", "missing", "refusal", "something else") ~ "Other",
          TRUE ~ NA_character_ )) %>% select(-emp_status)

  x$Wave<-1990+which(letters == i)
  
  lifemst = bind_rows(lifemst, x)

  rm(x)
  
}

# e. Prepare Last Interview Date
# last interview date
master_last = readRDS("output/master_bhps_ukhls_wide.rds") %>%
  select(pidp, starts_with("interview_date")) %>%
  reshape2::melt(
       id.vars = "pidp",
       variable.name = "Wave",
       value.name = "interview_date") %>%
  mutate(Wave = as.numeric(stringr::str_remove(Wave,"interview_date_"))) %>%
  filter(!is.na(interview_date)) %>% 
  # last interview date
  group_by(pidp) %>% arrange(Wave, .by_group = T) %>% slice(n()) %>% 
  mutate(interview_date = as.Date(paste0(format(as.Date(interview_date), "%Y-%m"),"-01") )) 

# f. Merge Last Interview Date and lifemst to Adjust Dates
lifemst_1 = merge(lifemst, master_last, by = c("pidp"), all.x = T) %>% 
  group_by(pidp) %>% arrange(start_date, .by_group = T) %>% 
  mutate( max = max(spell), 
          end_date = if_else(is.na(end_date)& max==spell,as.Date(interview_date), as.Date(end_date))
          ) %>% select(pidp, spell, employment_status, start_date,end_date, interview_date, leshne) %>% 
  mutate(end_date = floor_date(end_date %m-% months(1), unit = "month") + days_in_month(end_date  %m-% months(1)) - 1)

lifemst_2 = lifemst_1 %>% # we loose only 36 people 
  filter(!is.na(end_date) & !is.na(start_date)) %>% 
  filter(start_date<=end_date)

# g. Expand Data by Month - Use expand_dates() to generate a row for every month between start_date and end_date
lifemst_3 = neatRanges::expand_dates(lifemst_2, 
                                     start_var = "start_date", 
                                     end_var = "end_date",
                                     vars_to_keep = c("pidp","employment_status","spell"), unit = "month") %>% mutate(month = Expanded)

################################################################################
# 4. Merge with main survey file lifemst and indresp
# This step merges the monthly-expanded lifetime employment history (lifemst_3) with monthly individual-level employment data (bhps_indresp_3) to create a comprehensive person-month employment panel:
# Full Join: Ensures all records from both sources are included (all = TRUE).
# Prioritization: Where overlapping, values from bhps_indresp_3 are preferred over lifemst_3.
# Coalescing: Uses coalesce() to fill in employment_status and spell from either source, prioritizing .y (indresp).
# ("Use employment_status.y (the indresp value) if it exists. If it doesn’t, use employment_status.x (from lifetime data)", Same thing for spell)

bhps_final = merge(as.data.table(lifemst_3), as.data.table(bhps_indresp_3), by = c("pidp", "month","Expanded"), all = T) %>% 
  # we prioritize indresp file values 
  mutate(employment_status = coalesce(employment_status.y, employment_status.x),
         spell = coalesce(spell.y,spell.x)) %>% 
  select( pidp,spell,month,employment_status)%>% rename(Expanded = month)

rm(lifemst_3,lifemst_1,lifemst,lifetime_vlist, bhps_indresp_2, bhps_indresp_1,bhps_indresp,vlist_indresp,bhps_files)

################################################################################
# UKHLS
################################################################################
# 5. empstat from UKHLS - cleaning 
# a. List Files and variables
# b. Loop Through File
# c. Standardize Month Codes
# d. Handle Invalid Dates
# e. create start date
# f. Recode Employment Categories
# g. Generate End Date from Next Spell
# h. expands each employment spell into monthly records using the neatRanges::expand_dates() function:
# For every row in ukhls_empstat_2, it creates one row per month between start_date and end_date.
# It keeps key identifying variables: pidp, employment_status, and nr_emp_spell.
# The result is a person-month panel dataset, where each row represents a specific individual in a specific month, along with their employment status during that time.
# expand dates to dates here how many do we delete? one obs was deleted :)

# a. List Files and variables
vlist_empstat = c("spellno","leshst", "leshem", "leshsy", "leshsy4")

ukhls_emp = list.files(paste0(folder_main_uk, "ukhls/"),pattern = "empstat") 

# b. Loop Through Files
ukhls_empstat<-c()
for (i in 1:length(ukhls_emp)) {
  
  x<-read_dta(paste0(folder_main_uk, "ukhls","/",ukhls_emp[i]))
  
  x<-x %>% select("pidp", ends_with(vlist_empstat))
  
  letter_x = str_remove(ukhls_emp[i],"_empstat_protect.dta")
  
  colnames(x)<-str_remove(colnames(x), paste0(letter_x, "_"))
  
  x = x %>% mutate(across(c("leshst"), ~ sjlabelled::as_character(.))) %>% 
    mutate(
      # c. Standardize Month Codes
      leshem = case_when(
      leshem == 13 ~12, # winter/december	13
      leshem == 14 ~ 2, # winter/january/february	14
      leshem == 15 ~ 4, # spring	15
      leshem == 16 ~ 7, # summer	16
      leshem == 17 ~ 10,# autumn	17
      TRUE ~leshem 
      
    )) %>% 
    rename(nr_emp_spell =spellno,
           employment_status = leshst,
           START_M = leshem,
           START_Y =leshsy4 ) %>%
    mutate(
      # d. Handle Invalid Dates
      START_M = ifelse(START_M<0,NA,START_M),
      START_Y = ifelse(START_Y<0,NA,START_Y))
  
  x$Wave<-match(letter_x, letters)
  
  ukhls_empstat<-bind_rows(ukhls_empstat,x)
  rm(x)
}


ukhls_empstat = ukhls_empstat %>% 
  mutate(employment_status = ifelse(employment_status %in% c("don't know", "refused","refusal"),NA, employment_status))

ukhls_empstat_1 = ukhls_empstat %>%
  # e. create start date
  mutate(
         start_date = case_when(
           !is.na(START_Y) & !is.na(START_M) ~ as.Date(paste(START_Y, START_M, "01", sep = "-"), format = "%Y-%m-%d"),
           !is.na(START_Y) & is.na(START_M) ~ as.Date(paste(START_Y, "01-01", sep = "-"), format = "%Y-%m-%d"),
           TRUE ~ NA)) %>% 
  # no start date available for employment_status!="current status reached, no further changes" so we remove those spells 
  filter(employment_status!="current status reached, no further changes") %>% 
  mutate(
    # f. Recode Employment Categories
    employment_status = case_when(
    # [1] Working
    employment_status %in% c("full-time employed","self-employed", "part-time employed", "maternity leave") ~ "Working",
    # [2] Unemployed
    employment_status %in% c("unemployed") ~ "Unemployed",
    # [3] Not in the Labor Force
    employment_status %in% c("national service/war service","looking after family or home","long-term sick or disabled",
                             "full-time student/at school","on a government training scheme") ~ "Not in the Labor Force",
    # [4] Retired
    employment_status %in% c("retired") ~ "Retired",
    # [5] Other
    employment_status %in% c("something else") ~ "Other",
    employment_status %in% c("don't know", "refused","refusal") ~ NA,
    TRUE ~ NA_character_),
    nr_emp_spell = as.numeric(as.character(nr_emp_spell))
    
    ) %>% 
  filter(!is.na(start_date)) %>% 
  group_by(pidp) %>% 
  arrange(nr_emp_spell, .by_group = T) %>% 
  # g. Generate End Date from Next Spell
  mutate(
         end_date = as.Date(format(lead(start_date), "%Y-%m-01")),
         end_date = end_date %m-% months(1),
         ) %>% 
  # for last spell last interview date
  merge(select(master_last, pidp,interview_date), by = c("pidp"), all.x = T) %>%
  mutate(end_date = if_else(is.na(end_date), as.Date(interview_date), as.Date(end_date))) %>%
  arrange(start_date, .by_group = T) %>% 
  select(pidp, nr_emp_spell,employment_status,start_date,end_date) %>% 
  mutate(end_date = if_else(start_date  > end_date, as.Date(lubridate::ceiling_date(start_date, "month")- lubridate::days(1)), as.Date(end_date)))

# h. expands each employment spell into monthly records using the neatRanges::expand_dates() function:
# For every row in ukhls_empstat_2, it creates one row per month between start_date and end_date.
# It keeps key identifying variables: pidp, employment_status, and nr_emp_spell.
# The result is a person-month panel dataset, where each row represents a specific individual in a specific month, along with their employment status during that time.
# expand dates to dates here how many do we delete? one obs was deleted :)

ukhls_empstat_2 = ukhls_empstat_1 %>% filter(start_date<=end_date) %>% filter(!is.na(end_date))

ukhls_empstat_3 = neatRanges::expand_dates(ukhls_empstat_2, 
                         start_var = "start_date", 
                         end_var = "end_date",
                         vars_to_keep = c("pidp","employment_status", "nr_emp_spell"), 
                         unit = "month")

rm(ukhls_emp,vlist_empstat,ukhls_empstat_2,ukhls_empstat_1,ukhls_empstat)

################################################################################
# 6. indresp from UKHLS - cleaning
# a. List Files and Define Variables
# b. Loop Through Files
# c. Identify First/Last Interview per Person
# d. Construct Start/End Dates for Spells
# e. Detect Employment Spells
# f. Aggregate to Spell-Level
# g. Expand to Person-Month Format

# a. List Files and Define Variables
ukhls_files<-list.files(paste0(folder_main_uk, "ukhls"),pattern = "indresp") # 11 files

vlist_indresp_ukhls = c(
  "pidp",
  # technical to identify first interview 
  "ff_ivlolw",    # fed forward interview outcome prev wave
  "ff_everint",   # fed forward ever interviewed
  # interview information 
  "ivfio",        # individual interview outcome
  "intdaty_dv",   # Interview date: Year, derived
  "intdatm_dv",   # Interview date: Month, derived
  # employment flags
  "jbstat",       # Current labour force status
  "jbsamr",       # Same employer check
  "samejob",      # Check for same job within employer
  "jbhas",        # Did paid work last week
  "jboff",        # No work last week but has paid job
  "cjob",         # Current job indicator
  "empchk",       # Prev wave employed status check
  "notempchk",    # prev wave not employed check
  "ff_jbstat",    # respondent employment status at previous interview
  "ff_emplw",     # whether in paid employment at previous wave
  "nxtst",        # Next employment status
  "cstat",        # Current non-employment status
  # dates
  # job started date
  "jbbgm",        # month started current job
  "jbbgy",        # year started current job
  # job ended date
  "jbendm",       # month job ended
  "jbendy4",      # year job ended
  # employment status end date
  "empstendy4",	  # Employment status end: year	indresp	
  "empstendm",	  # Employment status end: month
  # next status end date
  "nxtstendy4",   # Next status end: year
  "nxtstendm",    # Next status end: month
  # next job end date
  "nxtjbendy4",   # Next job end: year
  "nxtjbendm",    # Next job end: month
  # number of spells
  "nmpsp_dv",	    # No. employment spells since last interview	
  "nnmpsp_dv"	    # No. non-employment spells since last interview
) 

# b. Loop Through Files
ukhls_indresp = c()
for (i in 1:length(ukhls_files)) {
  
  letter_x = str_remove(ukhls_files[i],"_indresp_protect.dta")
  
  x<-read_dta(paste0(folder_main_uk, "ukhls","/",ukhls_files[i]),
                col_select = c("pidp", ends_with(vlist_indresp_ukhls)))

  colnames(x)<-str_remove(colnames(x), paste0(letter_x, "_"))
  
  x$Wave<-2008+match(letter_x, letters)
  
  x$Wave_1<-match(letter_x, letters)
  
  ukhls_indresp<-bind_rows(ukhls_indresp,x)
  
  rm(x)
}  


ukhls_indresp_1 = ukhls_indresp %>% 
  # create employment status
  mutate(
    # encoding employment_status as we established in LIB
    emp_status = tolower(sjlabelled::as_character(jbstat)),
    employment_status =  
      case_when(
        # [1] Working 
        emp_status %in% c("paid employment(ft/pt)","self employed","self employed","paid employment(ft/pt)","on maternity leave", "self employed","self employed","paid employment(ft/pt)")  ~  "Working",
        # [2] Unemployed
        emp_status %in% c("unemployed") ~ "Unemployed",
        # [3] Not in the Labor Force
        emp_status %in% c("unpaid, family business","family care or home", "full-time student","govt training scheme","lt sick or disabled","on apprenticeship") ~ "Not in the Labor Force",
        # [4] Retired
        emp_status == "retired"~ "Retired",
        # [5] Other
        emp_status %in% c("doing something else") ~ "Other",
        TRUE ~ NA_character_ 
        )) %>% 
  mutate(
    # c. Identify First/Last Interview per Person
    intdaty_dv = ifelse(intdaty_dv<0,NA,intdaty_dv),
    intdatm_dv = ifelse(intdatm_dv<0,NA,intdatm_dv),
    date_interview = if_else(!is.na(intdaty_dv) & !is.na(intdatm_dv),as.Date(paste(intdaty_dv, intdatm_dv, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
    ############
    # d. Construct Start/End Dates for Spells
    ###########
    # jbbg - start current job date
    jbbgy = ifelse(jbbgy<0,NA,jbbgy),
    jbbgm = ifelse(jbbgm<0,NA,jbbgm),
    jbbgm = ifelse(is.na(jbbgm) & !is.na(jbbgy), 1,jbbgm),
    jbbg_date = if_else(!is.na(jbbgy) & !is.na(jbbgm) ,as.Date(paste(jbbgy, jbbgm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
    # jbend_date - job end date
    jbendy4 = ifelse(jbendy4<0,NA,jbendy4),
    jbendm  = ifelse(jbendm<0,NA,jbendm),
    jbendm  = ifelse(is.na(jbendm) & !is.na(jbendy4),1,jbendm),
    jbend_date = if_else(!is.na(jbendy4) & !is.na(jbendm),as.Date(paste(jbendy4, jbendm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
    # empstend - employment status end date
    empstendm = ifelse(empstendm<0,NA,empstendm),
    empstendy4 = ifelse(empstendy4<0,NA,empstendy4),
    empstend_date = if_else(!is.na(empstendy4) & !is.na(empstendm),as.Date(paste(empstendy4, empstendm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
    # next status end date
    nxtstendy4 = ifelse(nxtstendy4<0,NA, nxtstendy4), 
    nxtstendm = ifelse(nxtstendm<0,NA, nxtstendm),
    nxtstend_date = if_else(!is.na(nxtstendy4) & !is.na(nxtstendy4),as.Date(paste(nxtstendy4,  nxtstendm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
    # next job end date
    nxtjbendy4 = ifelse(nxtjbendy4<0,NA,nxtjbendy4),
    nxtjbendm = ifelse(nxtjbendm<0,NA,nxtjbendm),
    nxtjbend_date =  if_else(!is.na(nxtjbendy4) & !is.na(nxtjbendm),as.Date(paste(nxtjbendy4, nxtjbendm, "01", sep = "-"), format = "%Y-%m-%d"),as.Date(NA)),
    ) %>%
  filter(ivfio ==1) %>% # only those who were fully interviewed (full interview)
  select(pidp, Wave,employment_status,date_interview, 
         # flags
         jbsamr,samejob,empchk, jbhas ,jboff, cjob, empchk, notempchk, ff_jbstat,ff_emplw,nxtst,cstat,
         # dates
         jbbg_date, jbend_date, empstend_date, nxtstend_date, nxtjbend_date, ivfio) %>% 
  mutate(
    jbsamr = sjlabelled::as_character(jbsamr),
    samejob = sjlabelled::as_character(samejob),
    jbhas = sjlabelled::as_character(jbhas), 
    jboff = sjlabelled::as_character(jboff),
    cjob = sjlabelled::as_character(cjob),
    empchk = sjlabelled::as_character(empchk),
    notempchk = sjlabelled::as_character(notempchk),    
    ff_emplw = sjlabelled::as_character(ff_emplw),
    nxtst = sjlabelled::as_character(nxtst),
    cstat = sjlabelled::as_character(cstat)
         )

ukhls_indresp_2 = ukhls_indresp_1 %>% group_by(pidp) %>% 
  # first and last date interview
  mutate(
    first_interview_date = if_else(min(date_interview )==date_interview, as.Date(date_interview), as.Date(NA )),
    last_interview_date = if_else(max(date_interview )==date_interview, as.Date(date_interview), as.Date(NA ))
    )


ukhls_indresp_2 <- ukhls_indresp_2 %>%
  group_by(pidp) %>%
  arrange(Wave, .by_group = TRUE) %>%
  mutate(
    # if someone changed job or employer
    samejob_emp_1 = case_when((samejob =="no" | jbsamr=="no") & !is.na(jbend_date) ~ "diff", TRUE ~  "same"),

    start_date_1 = case_when(
      # Assign first_interview_date if the individual is not working and it is their first time in the sample.
      employment_status != "Working" & !is.na(first_interview_date) ~ first_interview_date,
      # Assign jbbg_date if the individual is working and it is their first time in the sample.
      employment_status == "Working" & !is.na(first_interview_date) ~ jbbg_date,
      TRUE ~ NA_Date_),
    end_date_1 = case_when(
      # Assign lead(empstend_date) if the individual is not working and was employed in the previous wave
      employment_status != "Working" & lead(notempchk) == "no" ~ lead(empstend_date),
      TRUE ~ NA_Date_),
    # if the individual is not working and was employed in the previous wave we can deduce also the start of this spell
    # by adding one month to end of previous employment (empstend_date)
    # so lead(empstend_date) is the end in perivous Wave and in current wave it is start but we need to add one month to it
    start_date_2 = case_when(
      notempchk == "no" ~ empstend_date %m+% months(1),TRUE ~ NA_Date_),
    # if it is last wave we assign last interview date as the end
    end_date_2 = if_else(!is.na(last_interview_date),as.Date(last_interview_date), as.Date(NA)),
    # if individual changed job or employer samejob_emp_1 =="diff" assign jbend_date - job end date as a lead for the previous wave
    end_date_3 = case_when(samejob_emp_1 =="diff" ~ jbend_date, TRUE ~ NA_Date_),
    end_date_4 = lead(end_date_3),
    # in currect Wave we need to add one more months so periods do not overlap
    start_date_4 = end_date_3 %m+% months(1),
    # collect all date cases in one columns using  coalesce - return the first non-missing (NA) value in a set of vectors
    start_date = coalesce(start_date_1, start_date_2, start_date_4),
    end_date = coalesce(end_date_1,end_date_2,end_date_4)) %>% ungroup()

# e. Detect Employment Spells
ukhls_indresp_3 = ukhls_indresp_2 %>%
  group_by(pidp) %>% arrange(Wave,.by_group = T) %>%
  mutate(check_1 = employment_status !=lag(employment_status),
         check_2 = samejob_emp_1 =="diff",
         spell = check_1 | check_2,
         spell = ifelse(is.na(spell), T,spell),
         spell = cumsum(spell)
         ) %>% group_by(pidp, spell) %>%
  # if spell do not change we can fill down the start_date
  fill(start_date, .direction = "down")


ukhls_indresp_4 = ukhls_indresp_3 %>%
  mutate(
    # in case of further missing we fill up the start and end using date_interview
    start_date = if_else(is.na(start_date), as.Date(date_interview), as.Date(start_date)),
    end_date = if_else(is.na(end_date), as.Date(date_interview), as.Date(end_date))
         ) %>%
  group_by(pidp, spell) %>%
  # f. Aggregate to Spell-Level
  # we calculate start and end for each spell suing min and max function
  summarise(employment_status = min(employment_status),
            start_date = min(start_date, na.rm = T),
            end_date = max(end_date, na.rm = T)
            ) %>%
  # the end dates are always the last days of the month
  mutate(end_date = lubridate::ceiling_date( end_date, "month") - lubridate::days(1))

ukhls_indresp_4_1 = filter(ukhls_indresp_4, end_date>=start_date)

# g. Expand to Person-Month Format
ukhls_employment_5 = neatRanges::expand_dates(ukhls_indresp_4_1,
                                              start_var = "start_date",
                                              end_var = "end_date",
                                              vars_to_keep = c("pidp","employment_status", "spell"),
                                              unit = "month") 


################################################################################
# 7. Combine UKHLS Employment Data from Retrospective and Main Survey Sources
# Purpose: Merge the monthly-expanded retrospective employment history (ukhls_empstat_3) with employment information reconstructed from the main individual survey (ukhls_employment_5).
# Merge Type: A full join (all = TRUE) is used to include all available person-month records from both sources, ensuring no data is lost.
# Conflict Resolution:
# When the same pidp and month (Expanded) appears in both datasets:
# employment_status.y (from the main survey) is preferred over employment_status.x (from retrospective).
# Similarly, spell is taken from the main survey (spell) if available, or falls back to nr_emp_spell (from retrospective).
# Coalescing Explained Simply:
# "Use the value from the main survey if it exists. If it's missing, fall back on the retrospective version."
# merged_ukhls = merge(as.data.table(ukhls_empstat_3), as.data.table(ukhls_employment_5), by = c("pidp", "Expanded"), all = T)
# "Use the value from the main survey if it exists. If it's missing, fall back on the retrospective version"

merged_ukhls = merge(as.data.table(ukhls_empstat_3), as.data.table(ukhls_employment_5), by = c("pidp", "Expanded"), all = T)

merged_ukhls = merged_ukhls %>% as.data.frame() %>% 
  mutate(employment_status = coalesce(employment_status.y,employment_status.x),
         spell = coalesce(spell, nr_emp_spell)) %>% 
  select(pidp, Expanded,spell, employment_status)


################################################################################
# 8. Combine and Finalize BHPS–UKHLS Employment Spells
# a.	Merge BHPS and UKHLS Person-Month Data
# Perform a full join on pidp and Expanded to combine BHPS (bhps_final) and UKHLS (merged_ukhls) monthly records.
# b.	Resolve Conflicting Values
# Use coalesce() to prioritize UKHLS values (.y) for employment_status and spell, falling back on BHPS values (.x) if needed.
# c.	Extract Last Interview Date Per Respondent
# Reshape interview dates from wide to long format.
# Keep only the latest interview per individual.
# Round interview dates down to the first of the month (Expanded format).
# d. Detect Employment Spells
# Identify changes in employment_status to flag the beginning of a new spell.
# Use cumsum() to number spells within each individual.
# e. Create Start and End Dates for Each Spell
# For each pidp and spell:
# Set the first month as the start_date.
# Tentatively define the end_date as the month before the next spell starts.
# f. Impute End Dates Using Interview Information
# Merge in the last interview date.
# If a spell has no end_date, use the interview date as a fallback.
# Round end_date up to the last day of the month.
# g. Fix Overlaps and Gaps
# Ensure that end_date is not before start_date.
# Prevent overlap between consecutive spells by adjusting start_date forward if needed.
# Snap all dates cleanly to month boundaries.
# h. Split Start and End Dates into Components
# Extract year (ENTRY_Y, EXIT_Y), month (ENTRY_M, EXIT_M), and day (ENTRY_D, EXIT_D) from start_date and end_date for easier downstream use.

# a. Merge BHPS and UKHLS Monthly Data
# Perform a full join on pidp and Expanded to combine BHPS (bhps_final) and UKHLS (merged_ukhls) monthly records.
final_bhps_ukhls = merge(bhps_final, merged_ukhls,by = c("pidp", "Expanded"), all = T) %>% 
  # b. Resolve Conflicts via Coalescing
  # Use coalesce() to prioritize UKHLS values (.y) for employment_status and spell, falling back on BHPS values (.x) if needed.
  mutate(employment_status = coalesce(employment_status.y,employment_status.x),
         spell = coalesce(spell.y, spell.x)
  ) %>% select(pidp, Expanded,spell, employment_status)


# c. Extract Last Interview Date Per Respondent
# Reshape interview dates from wide to long format.
# Keep only the latest interview per individual.
# Round interview dates down to the first of the month (Expanded format).
pivot_clean <- function(data, col_prefix, value_name, pid= "pidp") {
  data %>%
    select(pid, starts_with(col_prefix)) %>%
    pivot_longer(
      cols = starts_with(col_prefix),
      names_to = "spell",
      values_to = value_name
    ) %>%
    mutate(spell = str_remove(spell, paste0(col_prefix, "_"))) %>%
    filter(!is.na(!!sym(value_name)))
}

last_int = pivot_clean(readRDS("output/master_bhps_ukhls_wide.rds"), "interview_date", "interview_date") %>% 
  group_by(pidp) %>% arrange(spell, .by_group = T) %>% slice(n()) %>% 
  mutate(Expanded = floor_date(as.Date(interview_date), unit = "month")) %>% 
  select(pidp, Expanded)

# d. Detect Employment Spells
# Identify changes in employment_status to flag the beginning of a new spell.
# Use cumsum() to number spells within each individual.
bhps_ukhls_employment_spell = final_bhps_ukhls %>% group_by(pidp) %>% arrange(Expanded, .by_group = T) %>% 
  mutate(
    check = employment_status !=lag(employment_status),
    check  = ifelse(is.na(check), T, check),
    spell = cumsum(check)) %>% 
  group_by(pidp, spell) %>% arrange(Expanded, .by_group = T) %>% 
  # e. Create Start and End Dates for Each Spell
  # For each pidp and spell:
  # Set the first month as the start_date.
  # Tentatively define the end_date as the month before the next spell starts.
  summarise(
    employment_status = first(employment_status),
    start_date = min(Expanded),
         # end_date = max(Expanded)
    ) %>% 
  # f. Impute End Dates Using Interview Information
  # Merge in the last interview date.
  # If a spell has no end_date, use the interview date as a fallback.
  # Round end_date up to the last day of the month.
  group_by(pidp) %>%
  mutate(
    end_date = floor_date(lead(start_date) %m-% months(1), unit = "month") + days_in_month(start_date  %m-% months(1)) - 1,
         ) %>% 
  merge(last_int, by = c("pidp"), all.x = T) %>% group_by(pidp) %>% arrange(spell, .by_group = T) %>% 
  # g. Fix Overlaps and Gaps
  # Ensure that end_date is not before start_date.
  # Prevent overlap between consecutive spells by adjusting start_date forward if needed.
  # Snap all dates cleanly to month boundaries.
  mutate(
    end_date = ceiling_date(as.Date(end_date), unit = "month") - days(1),
    end_date = if_else(is.na(end_date), as.Date(Expanded), as.Date(end_date)),
    check = start_date<end_date,
    end_date = if_else(check==F, ceiling_date(as.Date(start_date), unit = "month") - days(1), as.Date(end_date)),
    check_1 = lag(end_date)>start_date,
    start_date = if_else(check_1==T & !is.na(lag(end_date)),ceiling_date(as.Date(start_date %m+% months(1)), unit = "month")- days(1), as.Date(start_date)),
    check_1_1 = start_date<end_date,
    check_1_2 = lag(end_date)<start_date,
    start_date = floor_date(start_date, unit = "month")
  )

# h. Split Start and End Dates into Components
# Extract year (ENTRY_Y, EXIT_Y), month (ENTRY_M, EXIT_M), and day (ENTRY_D, EXIT_D) from start_date and end_date for easier downstream use.
bhps_ukhls_employment_spell[c("ENTRY_Y","ENTRY_M","ENTRY_D")] <- str_split_fixed(bhps_ukhls_employment_spell$start_date, '-', 3)
bhps_ukhls_employment_spell[c("EXIT_Y","EXIT_M","EXIT_D")] <- str_split_fixed(bhps_ukhls_employment_spell$end_date, '-', 3)

# from months to spells (from one row one month to one row one spell)

to_many_spells = bhps_ukhls_employment_spell %>% 
  group_by(pidp) %>% summarise(max= max(spell)) %>% 
  filter(max<=100)

length(unique(to_many_spells$pidp)) - length(unique(bhps_ukhls_employment_spell$pidp)) # we loose 73 people 

bhps_ukhls_employment_spell  = bhps_ukhls_employment_spell[bhps_ukhls_employment_spell$pidp %in% to_many_spells$pidp,]

bhps_ukhls_employment_spell <- as.data.table(bhps_ukhls_employment_spell)

## 1) initial duration (exclusive; add +1 later if you want inclusive)
bhps_ukhls_employment_spell[, dur := as.integer(end_date - start_date)]

## 2) Fix the common “-1 day” pattern: set end_date to last day of that start month
bhps_ukhls_employment_spell[dur == -1L, end_date := ceiling_date(start_date, "month") - days(1)]

bhps_ukhls_employment_spell[, dur := as.integer(end_date - start_date)]

bhps_ukhls_employment_spell %>% group_by(dur) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  mutate(total = sum(n), percent = 100*(n/total))

bhps_ukhls_employment_spell[bhps_ukhls_employment_spell$pidp==49651,]


bhps_ukhls_employment_spell[, `:=`(
  start_date = as.IDate(start_date),
  end_date   = as.IDate(end_date)
)]

bhps_ukhls_employment_spell[, checkx := end_date - shift(start_date, type = "lead"), by = pidp]


bhps_ukhls_employment_spell[bhps_ukhls_employment_spell$pidp==49651,c("pidp", "spell","employment_status", "start_date","end_date", "dur", "checkx")]

bhps_ukhls_employment_spell[, c("start_date","end_date") := lapply(.SD, as.IDate), .SDcols = c("start_date","end_date")]

# if 'check' ended up as difftime, coerce to integer days
if (inherits(bhps_ukhls_employment_spell$check, "difftime")) bhps_ukhls_employment_spell[, check := as.integer(check, units = "days")]
bhps_ukhls_employment_spell[, `:=`(
  start_date = as.IDate(start_date),
  end_date   = as.IDate(end_date)
)]
bhps_ukhls_employment_spell[, start_date := fifelse(
  shift(checkx) %in% c(30L, 29L, 27L, 31L),
  shift(end_date) + 1L,     # add one day as an integer
  start_date
), by = pidp]

bhps_ukhls_employment_spell[, checky := end_date - shift(start_date, type = "lead"), by = pidp]

bhps_ukhls_employment_spell[bhps_ukhls_employment_spell$pidp==49651,c("pidp", "spell",      "employment_status", "start_date",   "end_date", "dur", "checky")]

bhps_ukhls_employment_spell %>% group_by(dur) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  mutate(total = sum(n), percent = 100*(n/total))

bhps_ukhls_employment_spell %>% group_by(checky) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  mutate(total = sum(n), percent = 100*(n/total))

################################################################################
# 9. Transform long format to wide format 

total_employment = bhps_ukhls_employment_spell %>% 
  mutate(
    across(c("ENTRY_Y", "ENTRY_M", "ENTRY_D", "EXIT_Y", "EXIT_M", "EXIT_D"), as.integer),
    EMPLOYMENT_STATUS = case_when(
      employment_status == "Working" ~ 1, 
      employment_status == "Unemployed" ~ 2, 
      employment_status == "Not in the Labor Force" ~ 3, 
      employment_status == "Retired" ~ 4, 
      employment_status == "Other" | is.na(employment_status) ~ 5, 
      TRUE ~ NA
    ),
    EMPLOYMENT_STATUS = as.integer(EMPLOYMENT_STATUS),
    EMPLOYMENT_STATUS = labelled(
      EMPLOYMENT_STATUS,
      c("working" = 1,
        "unemployed" = 2,
        "not in the labor force" = 3,
        "retired" = 4,
        "other" = 5)))

# to wide format 

employment_biography_bhps_ukhls_wide = total_employment %>% 
  pivot_wider(
    id_cols = c(pidp),
    names_from = spell ,
    values_from = c("EMPLOYMENT_STATUS","ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M", "EXIT_D"),
    names_glue = "{.value}_{spell}"
  )

###############################################################################
# 10. Order Variables 
ordering_variables_wide_format <- function(x_c) {
  lista_order<-c()
  for (i in 1:x_c) {
    x<-c(
      paste0("EMPLOYMENT_STATUS_", i),
      paste0("ENTRY_Y_",i),
      paste0("ENTRY_M_", i),
      paste0("ENTRY_D_", i),
      paste0("EXIT_Y_",i),
      paste0("EXIT_M_",i),
      paste0("EXIT_D_",i)
    )
    
    lista_order<- append(lista_order, x)
  }
  return(lista_order)
}

x = length(select(employment_biography_bhps_ukhls_wide, starts_with("ENTRY_Y_")) %>% names())-1

employment_biography_bhps_ukhls_wide<-employment_biography_bhps_ukhls_wide[,c("pidp",ordering_variables_wide_format(x))]

# add stata labels

# Apply labels to employment spell variables

for (i in 1:x) {
  var_status <- paste0("EMPLOYMENT_STATUS_", i)
  var_entry_y <- paste0("ENTRY_Y_", i)
  var_entry_m <- paste0("ENTRY_M_", i)
  var_entry_d <- paste0("ENTRY_D_", i)
  var_exit_y <- paste0("EXIT_Y_", i)
  var_exit_m <- paste0("EXIT_M_", i)
  var_exit_d <- paste0("EXIT_D_", i)
  
  if (var_status %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_status]]) <- paste("Employment status during spell", i)
  }
  if (var_entry_y %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_entry_y]]) <- paste("Year entry for spell", i)
  }
  if (var_entry_m %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_entry_m]]) <- paste("Month entry for spell", i)
  }
  if (var_entry_d %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_entry_d]]) <- paste("Day entry for spell", i)
  }
  
  if (var_exit_y %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_exit_y]]) <- paste("Year exit for spell", i)
  }
  if (var_exit_m %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_exit_m]]) <- paste("Month exit for spell", i)
  }
  if (var_exit_d  %in% names(employment_biography_bhps_ukhls_wide)) {
    var_label(employment_biography_bhps_ukhls_wide[[var_exit_d ]]) <- paste("Day exit for spell", i)
  }
}

################################################################################
# 11. save results
saveRDS(employment_biography_bhps_ukhls_wide , "output/bhps_ukhls_employment.rds")   

if (type_you_want==".csv") {
  write.csv(employment_biography_bhps_ukhls_wide, "output/bhps_ukhls_employment.csv")
}else if(type_you_want==".dta"){
  write_dta(employment_biography_bhps_ukhls_wide, "output/bhps_ukhls_employment.dta")

}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_fertility", "folder_main_uk", "folder_part_uk", "folder_family_files",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)































