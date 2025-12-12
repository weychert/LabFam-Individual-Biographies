################################# SHP Employment ###############################
# list of steps:

# A. Prepare base objects 
# download necessary packages and directories 
source("00_setting_work_space.R")

# create object to iterate thought -->number of folders 
folder_waves_available<-as.data.frame(list.files(folder_shp_1)) %>% rename(folder_name = `list.files(folder_shp_1)`)

folder_waves_available[c('wave_nr', 'year_wave')] <-str_split_fixed(folder_waves_available$folder_name, "_", 2)

folder_waves_available<-folder_waves_available %>% 
  mutate(year_wave = as.numeric(year_wave)) %>% 
  arrange(year_wave) %>% 
  mutate(wave_nr = ifelse(year_wave == 1999, "99", 
                          str_remove(year_wave, "20")))

# Function pivot_clean() is designed to reshape wide-format panel data into long format, 
# cleaning up the variable names in the process and removing missing values
pivot_clean <- function(data, col_prefix, value_name, pid = "pidp") {
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

################################################################################
# B. Prepare variables for active and non-active individuals about their changing job and employer
################################################################################
# a. read the data into R with selected variables in long format across waves
SHP_append_all_waves<-c()
for (wave in 1:length(folder_waves_available$year_wave)) {
  
  var = c("idpers",
          paste0("wstat",folder_waves_available$wave_nr[wave]),   # wstat - Working status
          paste0("p", folder_waves_available$wave_nr[wave],"w01"),# pw01  - Employment: work with payment last week:  yes, no
          paste0( "p",folder_waves_available$wave_nr[wave],"w06")  # pw06  - Job offered: Earliest starting date 
  )
  
  if (wave==1) {
    data_personal <- haven::read_dta(
      paste0(folder_shp_1, folder_waves_available[wave,"folder_name"], 
             "/","shp",
             folder_waves_available[wave,"wave_nr"],"_p_user.dta"),
      
      col_select = var)
  }else{
    data_personal <- haven::read_dta(
      paste0(folder_shp_1, folder_waves_available[wave,"folder_name"], 
             "/","shp",
             folder_waves_available[wave,"wave_nr"],"_p_user.dta"),
      
      col_select = c(var,
                     paste0("p",folder_waves_available$wave_nr[wave], "w177"),# pw177 - Active persons: Change in job: yes, no
                     paste0( "p",folder_waves_available$wave_nr[wave],"w154") # pw154 - Non-active persons: Paid Job Last year: yes,no
      ))
  }
  
  colnames(data_personal) <- sub(folder_waves_available$wave_nr[wave], "", colnames(data_personal))
  
  data_personal$year_wave<-folder_waves_available[wave,"year_wave"]
  
  SHP_append_all_waves<-bind_rows(SHP_append_all_waves,data_personal)
  
}

# b. At the time of interview create variable employemnt_code_current variable
# The employment_code_current variable classifies individuals into three employment 
# status categories—Working, Unemployed, and Not in the Labor Force—based on their 
# recent activity and survey responses. If a person reported working for pay in 
# the previous week (pw01 == 1), they are classified as Working. If they did not 
# work (pw01 == 2) but indicated they were unemployed (pw06 == 1) or 
# their working status (wstat) shows they are unemployed (wstat == 2), 
# they are classified as Unemployed. Conversely, if they did not work 
# and either reported being inactive (pw06 > 1) or their working status 
# was not in the labor force (wstat == 3), they are classified as Not in 
# the Labor Force. An edge case also treats individuals who did not work
# but are listed as employed in wstat (wstat == 1) as Unemployed, assuming 
# a temporary absence from work. If none of these conditions apply, their 
# status is marked as missing (NA). This logic helps ensure each respondent 
# is consistently classified based on a combination of self-reported employment and labor force status.
SHP_append_all_waves<-SHP_append_all_waves %>% 
  mutate(
  had_change = pw154 == 1 | pw177 == 1,
  employment_code_current = 
    case_when(
    pw01 == 1 ~ "Working",
    pw01 == 2 & pw06 == 1  ~ "Unemployed",
    pw01 == 2 & pw06 >1 ~ "Not in the Labor Force",  
    pw01 == 2 & wstat == 2  ~ "Unemployed",
    pw01 == 2 & wstat == 3 ~ "Not in the Labor Force", 
    pw01 == 2 & wstat == 1  ~ "Unemployed",
    TRUE  ~ NA_character_
  )
)

SHP_append_all_waves %>% group_by(pw06,employment_code_current) %>% summarise(n()) %>% filter(!is.na(employment_code_current))

################################################################################
# C. Prepare calendar data 
################################################################################
# Voorpostel, M., Tillmann, R., Lebert, F., Kuhn, U., Lipps, O., Ryser, V.-A., Antal, E., Monsch, 
# G.-A., Dasoki, N., Klaas, H. S., & Wernli, B. (2021). Swiss Household Panel User Guide (1999–2020), Wave 22, January 2022. Lausanne: FORS.
# page 36-37
# Using the answers in the individual questionnaire, the calendar file contains for every
# person the labour market participation - shp_ca.dta file
# status in each month. For a person who completed the individual
# questionnaire in wave x, information on his/her activity is contained for:
# - the last 12 months if the person has not answered the individual questionnaire in
# the preceding wave;
# - the period between the individual interview in wave x-1 and the individual interview in wave x if the person has answered the individual interview both in wave x
# and in the preceding wave.
# The activity calendar is empty for waves in which a respondent did not answer the individual questionnaire

shp_ca <- read_dta(paste0(folder_shp_2, "shp_ca.dta")) %>% 
  select(-"filter22")

# The dataset tracks individual-level monthly employment activity from 
# September 1999 (sep99) to at least March 2023 (mar23). 
# Each row represents a unique individual, identified by the idpers variable, 
# and each subsequent column corresponds to a specific month, containing 
# a numeric code that reflects the individual's labour market status for 
# that month. The current format is wide, meaning there is one row per 
# individual, and each month is stored as a separate column. 
# For analytical purposes—such as tracking labour market transitions over time—we aim to reshape the dataset into a long format. 
# In this new format, each row will represent a single month for a given individual, and a new column 
# (e.g., employment_status) will capture that individual’s labour force status in that month. 
# This structure is more suitable for longitudinal analysis and makes it easier to examine patterns over time.

# 1. Reshape the Data: Wide to Long
# 2. Harmonize Employment Status
# 3. Parse and Clean Date Information
# 4. Create a Standardized Date Variable

shp_ca_long  = shp_ca %>% 
  # 1. Reshape the Data: Wide to Long
  pivot_longer(cols=names(shp_ca)[-1],
               names_to='date', # stores the original column names (e.g., sep99, oct99) as a new variable.
               values_to='emp_activity' # tores the employment status for that month
               ) %>% 
  mutate(
    # 2. Harmonize Employment Status
    emp_activity = sjlabelled::as_character(emp_activity),
    employment_status = case_when(
      # [1] Working 
      emp_activity %in% c("full-time paid job (37 hours or more per week)", "part-time paid job (1-36 hours per week)") ~ "Working",
      # [2] Unemployed
      emp_activity %in% c("unemployment") ~ "Unemployed",
      # [3] Not in the Labor Force
      emp_activity %in% c("inactive","unemployed or inactive") ~ "Not in the Labor Force",
      TRUE ~ NA
    )
  ) %>% 
  mutate(
    # 3. Parse and Clean Date Information
    month_part = substr(date, 1, 3),
    year_part = as.numeric(substr(date, 4, 5)),
    
    # Convert two-digit year to four-digit year
    year_full = ifelse(year_part >= 90, 1900 + year_part, 2000 + year_part),
    
    # 4. Create a Standardized Date Variable
    date_activity = as.Date(paste0("01-", month_part, "-", year_full), format = "%d-%b-%Y")
  ) %>% filter(!is.na(employment_status))

# 5. prepare master file and the interview date nad status for each Wave in long format 
master = readRDS(paste0("output/master_shp_wide.rds"))
INTERVIEW_DATE = pivot_clean(master, "INTERVIEW_DATE", "INTERVIEW_DATE", pid = "idpers")
interview_status = pivot_clean(master, "INTERVIEW_STATUS", "INTERVIEW_STATUS", pid = "idpers")

interview = merge(INTERVIEW_DATE, interview_status, by = c("idpers","spell")) %>%
  mutate(date_activity = floor_date(as.Date(INTERVIEW_DATE), unit = "month")) %>% 
  select(idpers, date_activity, INTERVIEW_STATUS, spell) %>% rename(year_wave =spell) %>% 
  mutate(year_wave =as.numeric(year_wave))

# 6. select necessary variables form date of interview which are helpful for filling up the calendar data 
selected = SHP_append_all_waves %>% select(idpers, year_wave, 
                                           pw177, # pw177 - Active persons: Change in job: yes, no
                                           pw154, # pw154 - Non-active persons: Paid Job Last year: yes,no
                                           pw01,  # pw01  - Employment: work with payment last week:  yes, no
                                           pw06,  # pw06  - Job offered: Earliest starting date 
                                           # 1 immediately/within the next four weeks; 2 not within the next four weeks; 
                                           # 3 would not have been available; 4 In 5 weeks to 3 months; 5 Later, after 3 months or more; 6 Not available
                                           employment_code_current)

# 7. Combine calendar with interview date information
shp_ca_long_1 = merge(as.data.table(shp_ca_long), 
                      # add interview date 
                      as.data.table(interview), by = c("idpers", "date_activity"), all.x = T) %>% 
  # add select necessary variables form date of interview
  merge(as.data.table(selected), by = c("idpers","year_wave"), all.x = T)

# 8. Imputing and Harmonizing Employment Status using calendar and date oif interview information
# Voorpostel, M., Tillmann, R., Lebert, F., Kuhn, U., Lipps, O., Ryser, V.-A., Antal, E., Monsch, 
# G.-A., Dasoki, N., Klaas, H. S., & Wernli, B. (2021). Swiss Household Panel User Guide (1999–2020), Wave 22, January 2022. Lausanne: FORS.
# page 36-37
# In case the answer is “no” to pw154 or pw177 (no-->2), the activity status at the time of the interview
# is assumed to hold for every month that elapsed since the preceding interview, or for the
# last 12 months if the respondent did not respond to the individual questionnaire in the
# preceding wave. For these cases, the appropriate value is imputed for all months since
# the last wave.
# In case the answer is “yes” to one of the questions above, i.e. if the person reported any
# changes in his/her status during the period considered, the respondent is asked to report
# the employment situation for every month since the previous wave.

shp_ca_long_2 = shp_ca_long_1 %>% 
  group_by(idpers) %>% arrange(date_activity, .by_group = T) %>%
  fill(year_wave, .direction = "down") %>% 
  group_by(idpers, year_wave) %>% 
  # This code fills in missing monthly employment status values (employment_code_current) 
  # for each person within each survey year (year_wave) by carrying the last known value downward. 
  # It assumes that, within a year, the most recent employment status continues unless updated.
  fill(employment_code_current, .direction = "down") %>% as.data.frame() %>% 
  mutate(
    imputed_emp = case_when(
      pw154 == 2 & pw06 == 1 ~ "Unemployed",             # pw154 == "non-active persons: Paid Job Last year: yes,no" & pw06 == 1 immediately/within the next four weeks
      pw154 == 2 & pw06 > 1  ~ "Not in the Labor Force", # pw154 == "non-active persons: Paid Job Last year: yes,no" & pw06> 1 not interested in haveing a job 
      pw177 == 2 & pw01 == 1 ~ "Working",                # Active persons: Change in job: yes pw01 == yes Employment: work with payment last week
      TRUE ~ NA_character_
    )) %>%
  group_by(idpers, year_wave) %>% arrange(date_activity, .by_group = T) %>%  
  # If the current employment_status from calendar file is missing or not the same as at the time of interview, 
  # replace it with employment_code_current. Otherwise, leave it as is.
  mutate(employment_status = ifelse((!is.na(employment_code_current) & employment_status!=employment_code_current) | is.na(employment_status), employment_code_current,employment_status),
         
         )

################################################################################
# D.	Prepare retrospective files
################################################################################
# 1. Biographical files 2001-2002 (SHP_I)
# To obtain additional information about the respondents' life course prior to the panel
# study, a retrospective biographical questionnaire was administered in 2001 and 2002
# with questions regarding respondents’ educational, working, and family histories. This
# paper-and-pencil questionnaire was sent to the respondents by mail and was self administered.
# a retrospective biographical questionnaire was administered in 2001 and 2002
###############
# SHP0_bvwl_user.dta is is a retrospective employment history 
# Each row represents an employment or non-employment spell 
# (i.e., a continuous period in the same labour market status) for a unique individual.
###############
# We begin by importing the raw .dta  file and renaming key variables related to job spells, such as start and end years, 
# employment type, and job status. Categorical variables are converted from labelled 
# formats to plain text, and predefined nonresponse values (e.g., "inapplicable", "no answer") 
# are recoded as missing (NA). 
# The dataset then harmonize employment and non-activity variables 
# into a unified employment type, drops unused columns, and harmonizes labour force statuses 
# into standard categories: “Working,” “Unemployed,” “Not in the Labor Force,” or “Other inactive.” 
# Afterward, individuals with invalid or negative year entries are identified and excluded. 
 # Spells with illogical dates (e.g., end before start) are removed. 
# Finally, the cleaned data is expanded into a monthly format, where each row represents a 
# unique person-month observation that includes employment status and the original spell 
# start and end dates. 
# The final result, SHP0_bvwl_user_expanded, is a tidy, person-month 
# panel suitable for merging with other time-varying data such as calendar files 
# or interview dates for longitudinal analysis.

# 1: Define replacement values for NA
to_replace_na<-c("inapplicable","no answer","does not know", "other error", "filter error", "no personal income")


# Step 2: Read and rename variables
SHP0_bvwl_user <- read_dta(paste0(folder_retro_shp_1, "SHP0_bvwl_user.dta")) %>% 
  # rename variables
  rename(
    nr_emp_spell_cal = bvwl_idx,
    start = bvwl001,
    end = bvwl002,
    job_held =bvwl004,
    emp_type = bvwl005,
    full_part_time  = bvwl006,
    type_non_activity = bvwl007
  ) %>% 
  # Step 3: Convert labelled values and recode variables
  mutate(
    year_wave = start,
    job_held = sjlabelled::as_character(job_held, keep.labels = T),
    emp_type = sjlabelled::as_character(emp_type, keep.labels = T),
    full_part_time = sjlabelled::as_character(full_part_time, keep.labels = T),
    type_non_activity = sjlabelled::as_character(type_non_activity, keep.labels = T),
    year_wave = sjlabelled::as_character(year_wave, keep.labels = T),
    year_wave = as.numeric(start)
  ) %>%
  # Step 4: Replace custom missing values with NA
  replace_with_na_all(condition = ~.x %in% to_replace_na) %>% # replace with NA
  # Step 5: Merge employment and inactivity statuses
  mutate(emp_type = ifelse(is.na(emp_type), type_non_activity, emp_type)) %>%  
  # Step 6: Drop unused variables
  select(-c(bvwl003, type_non_activity)) %>% 
  # Step 7: Harmonize employment status into standard categories
  mutate(emp_activity_1 = case_when(
    # [1] Working 
    emp_type %in%  c("employed",
                     "employed and self-employed with employees",
                     "employed and self-employed with employees and self-employed without employees",
                     "employed and self-employed without employees",
                     "self-employed with employees",
                     "self-employed with employees and self-employed without employees",
                     "self-employed without employees") | 
      
      full_part_time %in% c("full time job (90-100%)",
                                                   "part time job less than 50 % and full time job (90-100%)",
                                                   "part time job less than 50 % and part time 50-89% and full time job (90-100%)",
                                                   "part time 50-89% and full time job (90-100%)",
                                                   "part time job less than 50%",
                                                   "part time job (50 - 89%)",
                                                   "part time job less than 50 % and part time job (50 - 89%)",
                                                   "employed and self-employed with employees and self-employed without employees",
                                                   "employed and self-employed with employees",
                                                   "employed and self-employed without employees",
                                                   "self-employed with employees and self-employed without employees",
                                                   "self-employed without employees",
                                                   "self-employed with employees"
                                                   ) ~ "Working",
    # [2] Unemployed
    emp_type == "unemployed" ~ "Unemployed",
    # [3] Not in the Labor Force
    emp_type %in% c("houseman/ -wife and long term disease / invalidity and unemployed",
                    "houseman/ -wife and long term disease / invalidity",
                    "houseman/ -wife",
                    "houseman/ -wife and unemployed",
                    "long term disease / invalidity and unemployed",
                    "long term disease / invalidity","other","other situation"
                    ) ~ "Not in the Labor Force",
    # [4] Retired
    # [5] Other
    TRUE ~ "Other"
  )) %>% 
  # Step 8: Final selection and renaming
  rename(employment_status = emp_activity_1) %>%
  select(idpers, year_wave, nr_emp_spell_cal, employment_status,emp_type, start,end)

# Step 9: Remove individuals with invalid dates (-1) 784
those_with_minus_one<-filter(SHP0_bvwl_user, start<0 |end <0)

# Step 10: Clean and prepare spell data only 7 people 
SHP0_bvwl_user_without_minus_1 <-SHP0_bvwl_user %>%  filter(idpers %notin% those_with_minus_one$idpers) %>% 
  group_by(idpers) %>% arrange(nr_emp_spell_cal, .by_group = T) %>% 
  mutate(check = start - lag(end),
         start = ifelse(check ==0 & !is.na(check), start+1, start)
         ) %>%  
  group_by(idpers) %>% arrange(year_wave, nr_emp_spell_cal, .by_group = T) %>%  
  mutate(max_spell = max(end)) %>% 
  mutate(start_date = as.Date(paste0(start, "-01-01")),
         end_date = as.Date(paste0(end, "-12-31"))) %>% 
  # recalculate spells 
  group_by(idpers) %>% arrange(start_date,.by_group = T) %>% 
  mutate(check = employment_status !=lag(employment_status),
         check1 = nr_emp_spell_cal!=lag(nr_emp_spell_cal),
         check2 = check1 | check,
         check3 = ifelse(is.na(check2), T, check2),
         spell = cumsum(check3 )
         ) %>% group_by(idpers, nr_emp_spell_cal) %>% 
  mutate(start_date = min(start_date)) %>% group_by(idpers) %>% 
  mutate( end_date = lead(start_date) -1,
          end_date = if_else(is.na(end_date), as.Date(paste0(end, "-12-31")),end_date)
          )

SHP0_bvwl_user_without_minus_1[SHP0_bvwl_user_without_minus_1$idpers==4101,]

# Step 11: Remove logically invalid spells
SHP0_bvwl_user_without_minus_2 = SHP0_bvwl_user_without_minus_1 %>% filter(start_date<=end_date)

# Step 12: Expand spells to monthly format
SHP0_bvwl_user_expanded = SHP0_bvwl_user_without_minus_2 %>% 
  neatRanges::expand_dates(
  start_var = "start_date",
  end_var = "end_date",
  vars_to_keep = c("idpers","nr_emp_spell_cal","employment_status", "start_date", "end_date"),
  unit = "month")

# The results is a monthly spell-level employment data-set for each person in SHP0_bvwl_user_expanded, 
# ready for merging with calendar or interview date data.

################################################################################  
# 2. SHPIII_PROF_ACT_USER in 2013 Paid work, unemployment, inactivity, social benefits
# Professional activities SHPIII_PROF_ACT_USER Paid work, unemployment, inactivity, social benefits
# For example, respondents who have held several jobs
# take up one row for every job. The index variable preserves the order of the episodes within respondents.

# # shpiii_prof_act_user.dta is a retrospective employment history file from the SHP survey,
# where each row corresponds to an employment or non-employment spell 
# (i.e., a continuous time period spent in the same labour force status) for a unique individual.
# Each row represents a single employment or non-employment spell for a unique individual, 
# along with detailed occupational classification codes and job activity labels.
###############
# 1. We begin by importing the raw .dta file and renaming the core spell-tracking variables 
# including start and end years, and the main activity status during the spell.
# 2. Labelled categorical variables are converted into plain character strings, 
# and common nonresponse codes (e.g., "inapplicable", "no answer") are recoded as missing (NA). 
# If the main activity variable is missing, it is filled using an alternate response field.
# 3. Standardizing date formats and converting time values to numeric, 
# 4. We classifies each spell into harmonized labour force categories:
# “Working,” “Unemployed,” “Not in the Labor Force,” “Retired,” and “Other.”
# 5. The cleaned dataset is then expanded into a monthly format, 
# so that each row represents one month of labour market status for an individual.
# The resulting dataset, shpiii_prof_act_expanded, provides a detailed, month-level employment panel 
# ready for integration with calendar or interview data for life-course and longitudinal analyses.

# 1. We begin by importing the raw .dta file and renaming the core spell-tracking variables 
# including start and end years, and the main activity status during the spell.
shpiii_prof_act <- read_dta(paste0(folder_retro_shp_2, "shpiii_prof_act_user.dta")) %>% 
  # rename variables 
  rename(
    nr_emp_spell_cal = ep_activity, 
    start = activity_a,                    
    end = activity_b   ,                      
    emp_activity = activity_mj
  ) %>% select(idpers, nr_emp_spell_cal, start, end, emp_activity , activity_other) %>%
  # 2. Labelled categorical variables are converted into plain character strings, 
  # and common nonresponse codes (e.g., "inapplicable", "no answer") are recoded as missing (NA). 
  # If the main activity variable is missing, it is filled using an alternate response field.
  mutate(
    emp_activity = sjlabelled::as_character(emp_activity, keep.labels = T),
    activity_other = sjlabelled::as_character(activity_other, keep.labels = T)) %>% 
  replace_with_na_all(condition = ~.x %in% to_replace_na) %>%
  # 3. Standardizing date formats and converting time values to numeric, 
  mutate(emp_activity = ifelse(is.na(emp_activity), activity_other,emp_activity),
         start = as.numeric(start),
         end = as.numeric(end),
         year_wave = as.numeric(start)
  ) %>% 
  select(-c(activity_other)) %>%
  # 4. We classifies each spell into harmonized labour force categories:
  # “Working,” “Unemployed,” “Not in the Labor Force,” “Retired,” and “Other.”
  mutate(emp_activity_2 = case_when(
    # [1] Working 
    emp_activity %in% c(
      "Fulltime working self-employed (90-100%)",
      "Fulltime working employee (90-100%)",
      "Employee, no info on activity rate",
      "Parttime working employee (50-89%)",
      "Small parttime working employee (less than 50%)",
      "Parttime working self-employed  (50-89%)",
      "Self-employed, no info on activity rate",
      "Small parttime working self-employed (less than 50%)"
    ) ~ "Working",
    # [2] Unemployed
    emp_activity %in% c("Unemployed and social help", "Unemployed") ~ "Unemployed",
    # [3] Not in the Labor Force
    emp_activity %in% c("Inactive", "Social help") ~ "Not in the Labor Force",
    # [4] Retired
    emp_activity %in% c("Retired and unemployed", "Retired") ~ "Retired",
    # [5] Other
    is.na(emp_activity)~ "Other",
    TRUE ~ NA
  ))%>% 
  # select(-emp_activity) %>% 
  rename(employment_status = emp_activity_2)

# 5. The cleaned dataset is then expanded into a monthly format, 
# so that each row represents one month of labour market status for an individual.
# The resulting dataset, shpiii_prof_act_expanded, provides a detailed, month-level employment panel 
# ready for integration with calendar or interview data for life-course and longitudinal analyses.
shpiii_prof_act_1 = shpiii_prof_act %>% 
  mutate(start_date = as.Date(paste0(start, "-01-01")),
         end_date = as.Date(paste0(end, "-01-01"))) %>% 
  filter(start_date<=end_date)

shpiii_prof_act_expanded = shpiii_prof_act_1 %>% 
  neatRanges::expand_dates(
    start_var = "start_date",
    end_var = "end_date",
    vars_to_keep = c("idpers","nr_emp_spell_cal","employment_status", "start_date", "end_date"),
    unit = "month")

################################################################################
# 3. retirement  "shp0_bvre_user.dta"
# We create a cleaned retirement spell dataset from a retrospective SHP file (shp0_bvre_user.dta). 
# Each row represents a retirement event for an individual, including the year and month they entered retirement.

retirement <-read_dta(paste0(folder_retro_shp_1, "shp0_bvre_user.dta")) %>% 
  rename( start = bvre002) %>% 
  mutate(start = expss::mis_val(start, c(-10:-1), with_labels = TRUE),
         month = expss::mis_val(bvre001, c(-10:-1), with_labels = F),
         month = as.numeric(month)) %>% 
  mutate(
    start = as.numeric(as.character(start)),
    year_wave = as.numeric(start),
    # "1","2","3","4","5","6","7","8","9"
    month = ifelse(!is.na(month) & month %in% c(1:9),paste0("0",month),month),
    start_date_activity = as.Date(ifelse(!is.na(month) & !is.na(start), paste0(start, "-", month, "-","01"), NA)),
    # end_date_activity = NA,
    emp_activity = "Retired"
  ) %>% 
  select(idpers, year_wave, start,  emp_activity, start_date_activity, 
  ) %>% 
  filter(!is.na(start)) %>% 
  mutate(if_retro_r= "retirement")

################################################################################
# E. We stack both retrospective form 2002 and 2013 and then combine it with last job

retro_both<-bind_rows(shpiii_prof_act_expanded, SHP0_bvwl_user_expanded) 

################################################################################
# F. combine all data sets into one and calculate spells
################################################################################
# Combine duration (employment activity form main survey calendar) with retrospective files 
# in order to merge safely the retrospective data sets with calendar activity, we need to add intermediate variable called  
# nr_emp_spell that only order spells from the beginning of the survey (1999)

# This code constructs a harmonized monthly employment biography (shp_biography) 
# for each individual by combining calendar-based employment data (shp_ca_long_2) 
# with retrospective employment spell data (retro_both). The process begins by 
# selecting key variables from the calendar file—namely idpers, date_activity 
# (renamed Expanded), employment_status, and job change indicator pw177. 
# It then merges this with the retrospective file using idpers and Expanded 
# as common keys, ensuring all rows from both sources are preserved.

# After the merge, employment statuses from both sources are unified using coalesce(), giving priority to the calendar 
# source (employment_status.x) but filling with retrospective data (employment_status.y) when calendar data is missing. 
# The dataset is then grouped and sorted chronologically per person.
# To identify unique labour market spells (i.e., uninterrupted periods in the same employment status), several logical checks are created:
# check flags a new spell if the spell number (nr_emp_spell_cal) changes;
# check_1 indicates job changes reported in the interview (pw177);
# check_2 detects changes in employment status compared to the previous month;
# check_3 and check_4 consolidate these checks, ensuring missing values are treated as new spells.
# Finally, a cumulative counter (spell) is created using cumsum(check_4), which increments every time a new spell is detected. 

shp_ca_long_3 = shp_ca_long_2 %>% select(idpers,date_activity, employment_status,pw177) %>% 
  mutate(Expanded = date_activity)

master_last = select(master, idpers, LASTOBS_Y)

shp_biography = 
  # shp_ca_long_3 %>% 
  merge(as.data.table(shp_ca_long_3),as.data.table(retro_both), by = c("idpers","Expanded"), all = T) %>%
  mutate(year_wave =  as.numeric(format( Expanded, "%Y"))) %>% merge(master_last, by = c("idpers")) %>% 
  filter(year_wave<=LASTOBS_Y) %>% 
  mutate(
    employment_status = coalesce(employment_status.x,employment_status.y)
  ) %>%
  group_by(idpers) %>% arrange(Expanded, .by_group = T) %>% 
  fill(employment_status, .direction = "down") %>% 
  select(idpers, Expanded, employment_status,pw177) %>% 
  group_by(idpers) %>% arrange(Expanded, employment_status, .by_group = T) %>% 
  mutate(
    check_1 = ifelse(pw177==2 | is.na(pw177) | pw177<0, F, T),
    check_2 = employment_status !=lag(employment_status),
    check_3 = check_1 | check_2,
    check_4 = ifelse(is.na(check_3), T, check_3),
    spell = cumsum(check_4)
  ) 


#################################################################################
# G. Perform the grouped summarization of spells 
# we finalize the construction of a cleaned, spell-level 
# employment biography by transforming the monthly panel data 
# into summarized employment spells for each individual.

setDT(shp_biography)

shp_biography_1 <- shp_biography[, .(
  employment_status = first(employment_status),  # Take the first employment status
  start_date = min(Expanded),                    # Earliest date in the spell
  end_date = max(Expanded)                       # Latest date in the spell
), by = .(idpers, spell)]  # Group by idpers & spell


shp_biography_1 = as.data.frame(shp_biography_1)

shp_biography_1[c("ENTRY_Y","ENTRY_M","ENTRY_D")] <- str_split_fixed(shp_biography_1$start_date, '-', 3)
shp_biography_1[c("EXIT_Y","EXIT_M","EXIT_D")] <- str_split_fixed(shp_biography_1$end_date, '-', 3)

################################################################################
# H. Transform data from long to wide format 
max = max(shp_biography_1$spell)

shp_biography_3 = shp_biography_1 %>% 
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

employment_biography_shp_wide = shp_biography_3 %>% 
  pivot_wider(
    id_cols = c(idpers),
    names_from = spell ,
    values_from = c("EMPLOYMENT_STATUS","ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M", "EXIT_D"),
    names_glue = "{.value}_{spell}"
  )

# order variables 
ordering_variables_wide_format <- function(x_c) {
  lista_order_20<-c()
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
    
    lista_order_20<- append(lista_order_20, x)
  }
  return(lista_order_20)
}

employment_biography_shp_wide<-employment_biography_shp_wide[,c("idpers",ordering_variables_wide_format(max))]

# Apply labels to employment spell variables

for (i in 1:max) {
  var_status <- paste0("EMPLOYMENT_STATUS_", i)
  var_entry_y <- paste0("ENTRY_Y_", i)
  var_entry_m <- paste0("ENTRY_M_", i)
  
  var_entry_d <- paste0("ENTRY_D_", i)
  
  var_exit_y <- paste0("EXIT_Y_", i)
  var_exit_m <- paste0("EXIT_M_", i)
  
  var_exit_d <- paste0("EXIT_D_", i)
  
  if (var_status %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_status]]) <- paste("Employment status during spell", i)
  }
  if (var_entry_y %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_entry_y]]) <- paste("Year entry for spell", i)
  }
  if (var_entry_m %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_entry_m]]) <- paste("Month entry for spell", i)
  }
  if (var_entry_d %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_entry_d]]) <- paste("Day entry for spell", i)
  }
  
  
  if (var_exit_y %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_exit_y]]) <- paste("Year exit for spell", i)
  }
  if (var_exit_m %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_exit_m]]) <- paste("Month exit for spell", i)
  }
  if (var_exit_d %in% names(employment_biography_shp_wide)) {
    var_label(employment_biography_shp_wide[[var_exit_d]]) <- paste("Day exit for spell", i)
  }
}

################################################################################
# save results

saveRDS(employment_biography_shp_wide, paste0("output/","shp_employment.rds"))

if (type_you_want==".csv") {
  write.csv(employment_biography_shp_wide, "output/shp_employment.csv")
}else if(type_you_want==".dta"){
  write_dta(employment_biography_shp_wide, "output/shp_employment.dta")
  
}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)




