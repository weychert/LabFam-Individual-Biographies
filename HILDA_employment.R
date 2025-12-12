################################# HILDA Employment Biography ###################
# Workspace Setup and Initial Variable Preparation

source("00_setting_work_space.R")

# Retrieves the names of all datasets in folder_Australia_1 that match the pattern "*Combined_", and stores them in a list.
Combined_<-list.files(folder_Australia_1,pattern="*Combined_")

# We create variable to indicate in functions for which wave we perform calculations 
# Hilda started in 2001. 
years<-c(2001:c(2000+length(Combined_)))

list_months_observed<-c(51,rep(54,7),rep(63,12))

to_remove_calenadr<-c(rep( "1", 6),"9","14","9","14",rep( "9", 3), "14",rep( "9", 3),"14", "9", "14")

# Function early_mid_late() standardizes textual date information in a dataset by converting vague time descriptions 
# like "Early", "Mid", and "Late" combined with month-year strings into a structured and consistent YYYY-MM-DD format.
# data_x (default = job_x): A data frame that includes a column named date_event, where each entry is a string 
# composed of three parts separated by underscores or spaces (depending on original formatting): a vague day description 
# ("Early", "Mid", or "Late"), a three-letter month abbreviation (e.g., "Jan"), and a year (e.g., "2005").

early_mid_late <- function(data_x=job_x) {
  
  # 1.Split the date_event column into three parts: day, month, and year.
  data_x<-data_x %>% separate(date_event, c('day', 'month', 'year'))
  
  # 2.Convert textual time descriptors into numeric days:
  data_x$day_1<-ifelse(data_x$day=="Early", "05", 
                       ifelse(data_x$day=="Mid", "15", 
                              ifelse(data_x$day=="Late", "25",NA)))
  
  # 3. Map month abbreviations to their corresponding two-digit numeric form:
  
  # 4. Construct a new date string in the format "YYYY-MM-DD" using the year, numeric month, and day values.
  data_x$month_1<-NA
  data_x$month_1[data_x$month=="Jan"]<-"01"
  data_x$month_1[data_x$month=="Feb"]<-"02"
  data_x$month_1[data_x$month=="Mar"]<-"03"
  data_x$month_1[data_x$month=="Apr"]<-"04"
  data_x$month_1[data_x$month=="May"]<-"05"
  data_x$month_1[data_x$month=="Jun"]<-"06"
  data_x$month_1[data_x$month=="Jul"]<-"07"
  data_x$month_1[data_x$month=="Aug"]<-"08"
  data_x$month_1[data_x$month=="Sep"]<-"09"
  data_x$month_1[data_x$month=="Oct"]<-"10"
  data_x$month_1[data_x$month=="Nov"]<-"11"
  data_x$month_1[data_x$month=="Dec"]<-"12"
  
  data_x$date_event_1<-paste(data_x$year,
                             data_x$month_1, 
                             data_x$day_1, sep = "-") 
  
  data_x<-select(data_x, -c(day,month, year,  day_1, month_1))
  
  # A modified version of data_x, where the original date_event has been 
  # transformed into a new column called date_event_1 containing dates in standard "YYYY-MM-DD" format.
  
  return(data_x)
}

################################################################################
# A. prepare variables and vectors to prepare calendar activity in HILDA
###############################################################################
# This section of code is designed to generate a comprehensive list of calendar activity 
# variable names for each wave of a longitudinal survey dataset. It carefully 
# handles formatting of names based on both month and period indices and stores 
# the results for each wave in separate dynamically named variables.
# 1. Initial Setup
# which_job_vector: A vector likely indicating job statuses or placeholders across 20 waves (first 9 get "0", remaining 11 get empty strings).
# list_months_observed: Number of calendar months observed per wave. First wave: 51, next 7 waves: 54 months, final 12 waves: 63 months.
# to_remove_calenadr: A list of calendar-related values to exclude per wave, perhaps for data cleaning or filtering.

which_job_vector<-c(rep("0",9),rep("",11))

list_months_observed<-c(51,rep(54,7),rep(63,12))

to_remove_calenadr<-c(rep( "1", 6),"9","14","9","14",rep( "9", 3), "14",rep( "9", 3),"14", "9", "14")

# 2. Main Loop — Create Calendar Activity Variable Names
list_calendar_activity_variables<-c()

for (which_wave in 1:length(years)) {
  
  calendar_activity<-c()
  # 3. Nested Loops — Construct Variables per Month and Observation
  for (i in 1:12) {
    
    if (i<10) {
      
      
      list_all_periods<-c()
      for (c in 1:list_months_observed[which_wave]) {
        # 4. Variable Name Construction
        if (c<10) {
          
          paricular<-
            c(paste0("caj0",i, "0", c))
          
        }else{
          
          paricular<-
            c(paste0("caj0",i, c)) 
          
        }
        
        
        list_all_periods<-append(list_all_periods,paricular)
      }
      
      
      calendar_activity<-append(calendar_activity, list_all_periods)
      
    }else{
      
      list_all_periods<-c()
      for (c in 1:list_months_observed[which_wave]) {
        
        if (c<10) {
          
          paricular<-
            c(paste0("caj",i, "0", c))
          
        }else{
          
          paricular<-
            c(paste0("caj",i, c)) 
          
        }
        
        
        list_all_periods<-append(list_all_periods,paricular)
      }
      
      
      calendar_activity<-append(calendar_activity, list_all_periods)
    }
    
  }
  
  assign(x = paste0("calendar_activity_variables_", which_wave), value = calendar_activity)
  
  list_calendar_activity_variables<-rbind(list_calendar_activity_variables,
                                          paste0("calendar_activity_variables_", which_wave))
  
}

rm(paricular, i, which_wave)

################################################################################
# B. Transforming Calendar Job Activity Data to Long Format
###############################################################################
# This script processes individual-level panel survey data that includes calendar job activity per wave (year). For each wave, the script:
# Reads and cleans data.
# Filters respondents based on eligibility.
# Extracts job activity calendar variables.
# Transforms wide-format job data into long-format, including precise date mapping.
# Combines multiple job tracks into a unified dataset for that wave.
# Each result is saved with a name like jobs_long_formt_2001, jobs_long_formt_2002, etc.
### 
# Download calendar data in wide format job1-12
# Filter data: 
### a.filter out kids and not-respondent adults 
### b.cafnj!=0 Number of jobs in last financial year cannot be zero
# transform Calendar data to long
### 
# 1. Initialize Storage
# 2. Loop Over Each Wave
# a. Select and Load Relevant Variables
# Constructs a variable list using wave-specific variable names.
# Reads in only the required variables from the Stata .dta file.
# b. Clean Variable Names - Delete first letter from variable names in a downloaded dataset
# c. Label and Filter Respondents
# Converts labelled Stata variables to readable strings for two variables:
# hgni and cafnj
# Filters out children and non-respondents.
# d. Transform Wide Calendar Job Data into Long Format
# 1. Extract Job Variables - select right columns for a given job
# 2. reshape to Long Format
# 3. Attach Variable Labels
# 4. sort data and Parse Date Strings
# 5. Remove Invalid Codes ( -1, -2, -4, -10)
# e. Merge All Jobs for the Wave

# 1. Initialize Storage
list_long_format_calendar_activity<-c()
# 2. Loop Over Each Wave
for (which_wave in 1:length(years)) {
  
  # a. Select and Load Relevant Variables
  # Constructs a variable list using wave-specific variable names.
  # Reads in only the required variables from the Stata .dta file.
  
  selected_variables<-c( "xwaveid",
                         paste0(letters[which_wave],"hgni"),
                         # esbrd Current employment status
                         paste0(letters[which_wave],"esbrd"),
                         # cafnj Number of jobs in last financial year
                         paste0(letters[which_wave],"cafnj"),
                         # jbemlha Time worked for current employer - answered years or weeks
                         paste0(letters[which_wave],"jbemlha"),
                         paste0(letters[which_wave], get(list_calendar_activity_variables[which_wave])))
  
  rperson_all <- read_dta(paste0(folder_Australia_1, Combined_[which_wave]), col_select = selected_variables)
  
  # b. Clean Variable Names - Delete first letter from variable names in a downloaded dataset
  names(rperson_all)[-1] <- substring(names(rperson_all)[-1], 2)
  
  # c. Label and Filter Respondents
  # Converts labelled Stata variables to readable strings for two variables:
  # hgni and cafnj
  # Filters out children and non-respondents.
  
  rperson_all$hgni<-sjlabelled::as_label(rperson_all$hgni, add.non.labelled = T)
  
  # Number of jobs in last financial year
  rperson_all$cafnj<-sjlabelled::as_label(rperson_all$cafnj, add.non.labelled = T)
  
  # filter out kids and not respondent 
  rperson_all<-filter(rperson_all, 
                      hgni !="[2] Not interviewed child (aged 0-14)" &
                        hgni !="[1] Not interviewed adult")
  # Keeps only respondents with at least one job.
  rperson_all<-filter(rperson_all, cafnj !=0)
  
  # d. Transform Wide Calendar Job Data into Long Format
  list_all_jobs_long_formt<-c()
  for (which_job in 1:12) {
    
    # 1. Extract Job Variables - select right columns for a given job
    
    job_x = rperson_all[,c("xwaveid",paste0("caj",which_job_vector[which_job],which_job,"0", 1:9),
                           paste0("caj",which_job_vector[which_job],
                                  which_job,10:list_months_observed[which_wave]))]
    
    # 2. reshape to Long Format
    job_x = tidyr::gather(job_x, order_job, name_variable, 
                          c(paste0("caj",which_job_vector[which_job],which_job,"0", 1:9),
                            paste0("caj",which_job_vector[which_job],
                                   which_job,10:list_months_observed[which_wave])))
    # 3. Attach Variable Labels
    names(job_x)[3]<-paste0("job_", which_job)
    
    if (which_job==1) {
      
      rperson_job_x<-select(rperson_all, paste0("caj0",which_job, "0",1:9), 
                            paste0("caj0",which_job, 10:list_months_observed[which_wave]))
      
      names_data<-as.data.frame(get_label(rperson_job_x))
      
      names_data$order_job<-row.names(names_data) 
      
      job_x<-merge(job_x, names_data, by = c("order_job"), all.x = T)
      
    }
    
    # 4. sort data and Parse Date Strings
    
    job_x$order_job<-str_remove(job_x$order_job, paste0("caj",which_job_vector[which_job],which_job))
    
    job_x$order_job<-as.numeric(job_x$order_job)
    
    if (which_wave==1 & which_job==1) {
      
      names(job_x)[4]<-"date_event"
      
      job_x$date_event<-str_remove(job_x$date_event, paste0("FG",to_remove_calenadr[which_wave],
                                                            " Calendar - Job ", which_job," - "))
      job_x <-early_mid_late(data_x = job_x)
      
    }else if(which_wave!=1 & which_job==1){
      
      names(job_x)[4]<-"date_event"
      
      job_x$date_event<-str_remove(job_x$date_event, paste0("E",to_remove_calenadr[which_wave],
                                                            " Calendar - Job ", which_job," - "))
      job_x <-early_mid_late(data_x = job_x)
    }
    
    # 5. Remove Invalid Codes ( -1, -2, -4, -10)
    
    job_x<-job_x[job_x[paste0("job_",which_job)] !=-1, ]
    job_x<-job_x[job_x[paste0("job_",which_job)] !=-2, ]
    job_x<-job_x[job_x[paste0("job_",which_job)] !=-4, ]
    job_x<-job_x[job_x[paste0("job_",which_job)] !=-10, ]
    
    
    # Save results  
    assign(x = paste0("jobs_long_formt", which_job), value = job_x)
      
    rm(job_x)
      
    list_all_jobs_long_formt<-rbind(list_all_jobs_long_formt,
                                      paste0("jobs_long_formt", which_job))
    
  }
  
  ################################################################################
  # e. Merge All Jobs for the Wave
  
  first_job<-get(list_all_jobs_long_formt[1]) %>% select(xwaveid, order_job, date_event_1, job_1)
  
  for (which_job in 2:12) {
    
    next_job<-get(list_all_jobs_long_formt[which_job]) %>% 
      select(xwaveid, order_job,  paste0("job_", which_job))
    
    first_job<-merge(first_job, next_job, by=c("xwaveid", "order_job"), all.x=T)
    
  }
  
  rm(list = list_all_jobs_long_formt)
  
  # save results for each wave 
  
  assign(x = paste0("jobs_long_formt_", years[which_wave]), value =  first_job)
  
  rm(first_job, next_job)
  
  list_long_format_calendar_activity<-rbind(list_long_format_calendar_activity,paste0("jobs_long_formt_", years[which_wave]))

}


################################################################################
# C.	Consolidation of Calendar Job Data Across Waves (jobs1-12 if employed in a given date)
################################################################################
# This code consolidates job calendar data for each wave by:
# Calculating the number of jobs held at each time point.
# Identifying the maximum number of simultaneous jobs per individual.
# Determining the total number of unique jobs held over the year.
# Merging these summaries into a final consolidated dataset for each wave.
# Each processed dataset is stored as consolidated_<year>, and tracked in list_consolidated_calendar_jobs.
### Here I calculate also:
### a.	nr of jobs in a calendar 
### b.	if individual has simultaneous jobs

# 1. choose job calendar dataset
# 2. Calculate Simultaneous Job Count - rowSum each row from job_1-job_12
# 3. Identify Max Simultaneous Jobs - examine if individual had simultaneous jobs in a given calendar
# 4. Summarizes each job column (job_1 to job_12) across all months per person.
# Produces a wide table: one row per person, one column per job.
# Reshapes to long format to count how many of the 12 jobs had at least one record.
# 5. Merge Aggregates with Original Data

list_consolidated_calendar_jobs<-c()
for (which_wave in 1:length(years)) {
  
  # 1. choose job calendar dataset
  
  data_int<-get(list_long_format_calendar_activity[which_wave])
  
  # 2. Calculate Simultaneous Job Count - rowSum each row from job_1-job_12
  
  data_int$consolidated_jobs<-rowSums(data_int[,c(paste0("job_",1:12))], na.rm = T)
  
  remove(list=list_long_format_calendar_activity[which_wave])
  
  # 3.Identify Max Simultaneous Jobs -  examine if individual had simultaneous jobs in a given calendar

  aggregate_nr_jobs<-data_int %>% group_by(xwaveid, consolidated_jobs) %>%
    arrange(consolidated_jobs, .by_group = T) %>%
    summarise(count = n())

  max_how_many_simult_jobs <- aggregate_nr_jobs %>% group_by(xwaveid) %>%
    arrange(consolidated_jobs, .by_group = T) %>%
    slice(n())

  max_how_many_simult_jobs<-rename(max_how_many_simult_jobs, max_simult_jobs_calendar=consolidated_jobs)

  # 4. Identify Number of Distinct Jobs in Total - How many jobs in general you had?
  # Summarizes each job column (job_1 to job_12) across all months per person.
  # Produces a wide table: one row per person, one column per job.
  # Reshapes to long format to count how many of the 12 jobs had at least one record.
  
  check_nr_jobs_in_cal<-data_int %>% group_by(xwaveid) %>%
    select(-xwaveid, -date_event_1, -order_job, -consolidated_jobs) %>%
    summarise_all(sum)

  check_nr_jobs_in_cal_long<-gather(check_nr_jobs_in_cal, how_many_jobs, name_variable, c(paste0("job_",1:12)))

  check_nr_jobs_in_cal_long<-filter(check_nr_jobs_in_cal_long, !is.na(name_variable))

  check_nr_jobs_in_cal_long$how_many_jobs<-as.numeric(str_remove(check_nr_jobs_in_cal_long$how_many_jobs, "job_"))

  check_nr_jobs_in_cal_long<-check_nr_jobs_in_cal_long %>% group_by(xwaveid) %>%
    arrange(how_many_jobs, .by_group = T) %>%
    slice(n()) %>% select(-name_variable)

  ##############################################################################
  # 5. Merge Aggregates with Original Data
  job_aggregate<-merge(max_how_many_simult_jobs, check_nr_jobs_in_cal_long, by = c("xwaveid"), all.x=T)

  rm("check_nr_jobs_in_cal_long", "check_nr_jobs_in_cal", aggregate_nr_jobs)
  
  ##### Add it to general data
  data_int_1<-merge(data_int, job_aggregate, 
                    by = c("xwaveid"), all.x = T)
  
  data_int_1<-select(data_int_1, -c(paste0("job_",1:12), count))
  
  ##############################################################################
  
  assign(x = paste0("consolidated_", years[which_wave]), value =  data_int_1)
  
  list_consolidated_calendar_jobs<-rbind(list_consolidated_calendar_jobs,
                                         paste0("consolidated_", years[which_wave]))
  
  rm("data_int_1", "data_int", "job_aggregate", "aggregate_nr_jobs", "max_how_many_simult_jobs", "check_nr_jobs_in_cal", "check_nr_jobs_in_cal_long")

  
}

################################################################################
# D.	For each wave download information from calendar of not in the labor force data. 
################################################################################
# The process is similar to job activity. 
# For each individual we have two questions:
# •	Not employed and not looking for work
# •	Not employed but looking for work
# We consolidate it into one column that indicates if an individual is unemployed or not in the labor force. 

list_not_lf<-c()
for (which_wave in 1:length(years)) {
  
  # canlf - Calendar - Not employed and not looking for work
  canlf<-c(paste0("canlf","0",1:9),paste0("canlf", 10:list_months_observed[which_wave]))
  
  # caune -  Not employed but looking for work 
  caune<-c(paste0("caune","0",1:9),paste0("caune", 10:list_months_observed[which_wave]))
  
  vector_chosen<-c("canlf", "caune")
  
  to_remove_name<-c(" Calendar - Not employed and not looking for work - ",
                    " Calendar - Not employed but looking for work - ")
  
  # download data
  
  modified_vars_x<-c(
    "xwaveid",
    # Any calendar activity - Not employed but looking for work
    paste0(letters[which_wave],"caune"),
    # Any calendar activity - Neither employed nor looking for work
    paste0(letters[which_wave],"canlf"),
    paste0(letters[which_wave], caune),
    paste0(letters[which_wave], canlf))
  
  
  rperson_all <- read_dta(paste0(folder_Australia_1, Combined_[which_wave]), 
                          col_select = modified_vars_x)
  
  # Delete first letter from variable names in a downloaded dataset
  names(rperson_all)[-1] <- substring(names(rperson_all)[-1], 2) 
  
  rperson_all<-filter(rperson_all, canlf==1 | caune==1)
  
  
  list_particular_wave_type<-c()
  for (each_type in 1:length(vector_chosen)) {
    
    job_x = rperson_all[,c("xwaveid",get(vector_chosen[each_type]))]
    
    # 2. from long to wide
    job_x = tidyr::gather(job_x, order_job, name_variable, 
                          get(vector_chosen[each_type]))
    # 3. rename column
    names(job_x)[3]<-vector_chosen[each_type]
    
    rperson_job_x<-select(rperson_all, get(vector_chosen[each_type]))
    
    names_data<-as.data.frame(get_label(rperson_job_x))
    
    names_data$order_job<-row.names(names_data)  
    
    # add names 
    
    job_x<-merge(job_x, names_data, by = c("order_job"), all.x = T)
    
    names(job_x)[4]<-"date_event"
    
    # create to_sort 
    
    job_x$to_sort<-str_remove(job_x$order_job, vector_chosen[each_type])
    
    job_x$to_sort<-as.numeric(job_x$to_sort)
    
    ### here date_event remove necessary
    
    if (which_wave==1) {
      
      job_x$date_event<-str_remove(job_x$date_event, paste0("FG",to_remove_calenadr[which_wave],
                                                            to_remove_name[each_type]))
      
    }else{
      
      job_x$date_event<-str_remove(job_x$date_event, paste0("E",to_remove_calenadr[which_wave],
                                                            to_remove_name[each_type]))
      
    }
    
    job_x<-select(job_x, -order_job)
    
    assign(x = paste0("not_lab_", each_type), value = job_x)
    
    list_particular_wave_type<-rbind(list_particular_wave_type,
                                     paste0("not_lab_", each_type))
  }
  
  not_lf<-get(list_particular_wave_type[1])
  
  not_lf<-merge(not_lf, get(list_particular_wave_type[2]), by = c("xwaveid", "to_sort", "date_event"))
  
  # drop -2 
  
  not_lf<-filter(not_lf, canlf !=-2)
  
  not_lf$canlf<-ifelse(not_lf$canlf<0,0,not_lf$canlf)
  
  not_lf$caune<-ifelse(not_lf$caune<0,0,not_lf$caune)
  
  # concatenate

  not_lf$not_lf<- ifelse(not_lf$canlf == 1, "[3] Not in the labour force", 
                         ifelse(not_lf$caune == 1, "[2] Unemployed", NA))
  
  
  not_lf<-filter(not_lf, !is.na(not_lf))
  
  # create date_event_1
  
  not_lf<-early_mid_late(not_lf)
  
  # rename 
  
  not_lf<-rename(not_lf, order_job=to_sort  )
  
  not_lf<-select(not_lf, -c(canlf, caune))

  assign(x = paste0("not_lf_", which_wave), value = not_lf)
  
  list_not_lf<-rbind(list_not_lf, paste0("not_lf_", which_wave))

  rm(rperson_all, rperson_job_x, names_data)
  
}

################################################################################
# E. Combining Calendar Job Data with Labor Force Status (NOT_LF)
# For each wave, combining calendar job with NOT_LF 
################################################################################
# This section consolidates two parallel data sources:
# Calendar job activity data (processed earlier).
# Labor force status (e.g., unemployed, not in labor force) per time point.
# The goal is to produce a unified employment status indicator (emp_status_calendar) for each person and time point across all waves.
# Each wave's merged dataset is saved as combined_<year> and tracked in list_combined_jobs_nfl.
# 1. Merge Calendar Job Data with NOT_LF Data
# 2. Define Employment Indicator (if_emp)
# 3. Create Unified Employment Status Column combine into one column if_emp and not_lf

list_combined_jobs_nfl<-c()
for (which_wave in 1:length(years)) {
  
  # 1. Merge Calendar Job Data with NOT_LF Data
  combined <- merge(get(list_consolidated_calendar_jobs[which_wave]),
                    get(list_not_lf[which_wave]), 
                    by = c("xwaveid", "order_job", "date_event_1"),all=T)
  
  # 2. Define Employment Indicator (if_emp)
  combined$if_emp<-ifelse(combined$consolidated_jobs>1,1,combined$consolidated_jobs)
  
  # 3. Create Unified Employment Status Column combine into one column if_emp and not_lf
  
  combined$emp_status_calendar<-NA
  
  combined$emp_status_calendar[combined$if_emp==1]<-"[1] Employed"
  
  combined$emp_status_calendar[combined$not_lf=="[2] Unemployed"]<-"[2] Unemployed"
  
  combined$emp_status_calendar[combined$not_lf=="[3] Not in the labour force"]<-"[3] Not in the labour force"
  
  combined<-select(combined, xwaveid, order_job, date_event_1,emp_status_calendar)
  
  rm(list = list_consolidated_calendar_jobs[which_wave])
  rm(list = list_not_lf[which_wave])
  
  assign(x = paste0("combined_", years[which_wave]),value = combined)
  
  list_combined_jobs_nfl<-rbind(list_combined_jobs_nfl, paste0("combined_", years[which_wave]))
  
  rm(combined)
  
}

all_objects <- ls()
filtered_objects <- all_objects[startsWith(all_objects, "calendar_activity_variables_")]

rm(list = filtered_objects, canlf, calendar_activity, all_objects, caune, each_type, c,
   which_job_vector, vector_chosen, which_job, to_remove_calenadr, to_remove_name, gen_emp_hist_variables, filtered_objects,
   modified_vars_x, selected_variables,list_months_observed, list_all_periods, which_wave,
   list_all_jobs_long_formt, list_calendar_activity_variables, list_consolidated_calendar_jobs, list_long_format_calendar_activity,
   list_not_lf, 
   list_particular_wave_type,job_x,not_lab_1, not_lab_2, not_lf
   )

################################################################################
# F. Removing Overlaps in Calendar Job Responses Across Waves
# Delete overlap in answers between waves. As individuals can be asked twice about 
################################################################################
# In panel surveys like HILDA, respondents may report on overlapping calendar periods 
# in consecutive waves. This code ensures that each calendar month is counted only once per individual, by:
# Comparing records across adjacent waves.
# Keeping only the version closer to the interview date (i.e., from the more recent wave).
# Removing duplicates to minimize recall error.
# The result is a set of non-overlapping employment records saved as 
# combined_no_overlap_<year>, tracked in list_combined_no_overlap.

# 1. Initialize List to Track Cleaned Datasets
# 2. Loop Through Waves and Compare Consecutive Pairs
# 3. Merge Consecutive Waves by Individual and Date
# 4. Identify and Filter Overlapping Records - select only those which do not overlap
# Creates a flag (to_drop) for rows that appear in both waves (i.e., duplicates).
# Keeps only the non-duplicated and new records from the later wave (assumed to be more accurate).
# 5. Optional Overlap Diagnostics - count how many individuals have overlap
# 6. Select Final Columns and Save Output
# 7. Clean Up Workspace

# This section removes duplicate monthly employment status records across adjacent survey waves to ensure that:
# Each calendar period is reported only once per respondent.
# The most recent response is retained, assuming it's closer to the interview date and therefore more accurate.
# It produces a refined panel dataset (combined_no_overlap_<year>) that is ready for longitudinal analysis free of temporal overlap

# 1. Initialize List to Track Cleaned Datasets
list_combined_no_overlap<-c()
# 2. Loop Through Waves and Compare Consecutive Pairs
for (which_wave in 1:length(years)) {
  
  if (which_wave + 1>length(years)) {
    
    break
    
  }else{
    
    if (which_wave==1) {
      
      first_wave<-get(list_combined_jobs_nfl[1])
      
      intermediate <- get(list_combined_jobs_nfl[which_wave+1])
      # 3. Merge Consecutive Waves by Individual and Date
      first_wave<-merge(first_wave, intermediate, 
                        by = c("xwaveid", "date_event_1"), 
                        all = T)
      
  
      # 4. Identify and Filter Overlapping Records - select only those which do not overlap
      # Creates a flag (to_drop) for rows that appear in both waves (i.e., duplicates).
      # Keeps only the non-duplicated and new records from the later wave (assumed to be more accurate).
      first_wave$to_drop<-ifelse(!is.na(first_wave$emp_status_calendar.x) &
                                   !is.na(first_wave$emp_status_calendar.y), 1,0)
      
      first_wave_x<-filter(first_wave, to_drop!=1 & !is.na(first_wave$emp_status_calendar.y))
      
      first_wave_x<-select(first_wave_x, xwaveid, date_event_1, order_job.y, emp_status_calendar.y)
      
      first_wave_x<-dplyr::rename(first_wave_x, order_job=order_job.y,
                                  emp_status_calendar=emp_status_calendar.y)
      
      # save results
      
      assign(x = paste0("combined_no_overlap_", years[which_wave+1]),value =  first_wave_x)
      
      list_combined_no_overlap<-rbind(list_combined_no_overlap, 
                                      paste0("combined_no_overlap_", years[which_wave+1]))
      
    } else{
      
      first_wave<-get(list_combined_no_overlap[which_wave-1])
      
      intermediate <- get(list_combined_jobs_nfl[which_wave+1])
      
      first_wave<-merge(first_wave, intermediate, 
                        by = c("xwaveid", "date_event_1"), 
                        all = T)
      
      
      # select only those which do not overlap
      
      first_wave$to_drop<-ifelse(!is.na(first_wave$emp_status_calendar.x) &
                                   !is.na(first_wave$emp_status_calendar.y), 1,0)
      first_wave_x<-filter(first_wave, to_drop!=1 & !is.na(first_wave$emp_status_calendar.y))
      
      # 5. Optional Overlap Diagnostics - count how many individuals have overlap
      
      check_nr_overlap<- first_wave %>% 
        filter(to_drop==1) %>% 
        group_by(xwaveid) %>%
        arrange(desc(emp_status_calendar.y), .by_group = T) %>% 
        slice(n())
      
      check_nr_overlap_idx<-check_nr_overlap %>% group_by(xwaveid) %>% summarise(n=n())
      
      first_wave_idx<-first_wave%>% group_by(xwaveid) %>% summarise(n=n())
      
      # print(nrow(check_nr_overlap_idx)/nrow(first_wave_idx))
      
      # 6. Select Final Columns and Save Output
      
      first_wave_x<-select(first_wave_x, xwaveid, date_event_1, order_job.y, emp_status_calendar.y)
      
      first_wave_x<-dplyr::rename(first_wave_x, order_job=order_job.y,
                                  emp_status_calendar=emp_status_calendar.y)
      
      # save results
      
      assign(x = paste0("combined_no_overlap_", years[which_wave+1]),value =  first_wave_x)
      
      list_combined_no_overlap<-rbind(list_combined_no_overlap,  
                                      paste0("combined_no_overlap_", years[which_wave+1]))
      
    }
    # 7. Clean Up Workspace
    
    rm(list = list_combined_jobs_nfl[which_wave+1])
    
    rm(first_wave_x,check_nr_overlap_idx,check_nr_overlap,first_wave, intermediate)
    
  }
}

################### check if we make mistake ###################################
all_calendar = c()
for (i in 1:length(list_combined_no_overlap)) {
  
  x = get(paste0("combined_no_overlap_", years[i+1]))
  
  x$year_wave = years[i+1]
  
  all_calendar =bind_rows(all_calendar, x)
  
}

saveRDS(all_calendar, "output/list_combined_no_overlap.rds")

all_calendar = readRDS("output/list_combined_no_overlap.rds")

all_calendar[c("year", "month", "day")] <- str_split_fixed(all_calendar $date_event_1, "-", 3)

####### at the time of interview ###############################################
interview_hilda = readRDS(paste0("output/master_","hilda","_wide.rds")) %>% 
  select(xwaveid, starts_with("INTERVIEW_DATE")) %>% 
  reshape2::melt(
    id.vars = "xwaveid", 
    variable.name = "Wave", 
    value.name = "INTERVIEW_DATE") %>% 
  mutate(Wave = as.numeric(stringr::str_remove(Wave,"INTERVIEW_DATE_"))) %>% 
  filter(!is.na(INTERVIEW_DATE)) %>% filter(!is.na(Wave)) %>% 
  mutate(
    interview_date = as.Date(INTERVIEW_DATE)) %>% 
  mutate(date_event_1 = interview_date) %>% 
  select(-c("INTERVIEW_DATE","interview_date"))

emp_status_hilda = readRDS(paste0("output/master_","hilda","_wide.rds")) %>% 
  select(xwaveid, starts_with("EMP_STATUS")) %>% 
  reshape2::melt(
    id.vars = "xwaveid", 
    variable.name = "Wave", 
    value.name = "EMP_STATUS") %>% 
  mutate(Wave = as.numeric(stringr::str_remove(Wave,"EMP_STATUS_"))) %>% 
  filter(!is.na(EMP_STATUS)) %>% filter(!is.na(Wave)) 

######## add retirement
retired =c()
for (which_wave in seq_along(years)) {
  
  # a. Select and Load Relevant Variables
  # Build the wave prefix (a, b, c, ...)
  wave_prefix <- letters[which_wave]
  
  selected_variables <- c(
    "xwaveid",
    paste0(wave_prefix, c(
      # retirement variables
      "rtcomp",     # retired completely from the workforce
      "rtcompn",    # retired completely from the workforce (numeric)
      "rtyr",       # year retired
      "rtyrn",       # year retired (numeric)
      "esbrd"
    ))
  )
  
  rperson_all <- read_dta(
    file = paste0(folder_Australia_1, Combined_[which_wave]),
    col_select = any_of(selected_variables)
  )
  
  names(rperson_all)[-1] <- substring(names(rperson_all)[-1], 2)
  
  rperson_all$year_wave = years[which_wave]
  
  retired = bind_rows(retired ,rperson_all)
  
}

retired_1 = retired[retired$rtcomp ==1, c("xwaveid","year_wave","rtyr","esbrd")] %>% 
  mutate(
    retired = "[4] retired"
  )

all_calendar_1_x = all_calendar %>% merge(retired_1, by = c("xwaveid","year_wave"), all.x = T) %>% 
  mutate(
    emp_status_calendar = ifelse(emp_status_calendar=="[3] Not in the labour force" & retired == "[4] retired" & !is.na(retired),"[4] retired", emp_status_calendar)
  )

################################################################################
# G.	 Building Employment Spells Using Job Tenure Information
################################################################################
# This section uses calendar job data, combined with tenure information (weeks/years worked for current employer), to:
# Identify likely job starts.
# Track changes in employment status across time.
# Construct and label continuous employment spells (start/end dates).
# The output is a spell-level dataset for each individual, capturing transitions between employment statuses across waves.

# •	if worked for years for current employer or weeks 
# •	Weeks worked for current employer

# 1. Loop Over Each Wave: Extract Job Tenure Information
# jbemlha: Time worked for current employer (indicator: weeks vs. years)
# jbemlwk: If weeks were specified
# jbemlyr: If years were specified
# Column names are cleaned (prefix removed).
# Negative values are set to NA.
# The type (weeks_years) is translated from numeric codes (1 = years, 2 = weeks).
# Results are merged across waves into list_jobs_add
# 2. Merge with Interview Data to Calculate Job Start Dates
# 3. Normalize Job Start Dates to 'Early/Mid/Late' Format
# 4. Flag Individuals with New Jobs
# Prepares a dataset where each person-month is marked "yes" if a new job was identified from tenure data.
# 5. Load Final Calendar Dataset and Merge with Job Start Info
# Loads the final cleaned employment calendar with no overlaps across waves.
# Merges with interview data and job start indicato
# 6. Construct Employment Spells
# check: Detects changes in employment status (e.g., employed → unemployed).
# check_1: Detects if a new job started
# check_2: Combines the two to determine if a new spell begins
# spell: A cumulative counter that increments when a new spell starts.
# 7. Extract Spell Start and End Dates
# Groups by person and spell to determine:
# Start date: first month of the spell.
# End date: last month of the spell.
# One row per employment spell per person, ready for trajectory or duration analysis.
# This section builds meaningful employment spells by combining:
# Calendar-based employment history
# Job tenure data (weeks/years)
# Detected transitions in employment status or new job starts
# The result (all_calendar_4) is a panel of spells per person, with clear start and end dates and the employment status during that period.

# 1. Loop Over Each Wave: Extract Job Tenure Information
# Column names are cleaned (prefix removed).
# Negative values are set to NA.
# The type (weeks_years) is translated from numeric
list_jobs_add<-c()
for (which_wave in 1:length(years)) {
  
  selected_variables<-c( "xwaveid",
                         # jbemlha Time worked for current employer - answered years or weeks
                         paste0(letters[which_wave],"jbemlha"),
                         # jbemlwk Weeks worked for current employer
                         paste0(letters[which_wave],"jbemlwk"),
                         # jbemlyr Years worked for current employer
                         paste0(letters[which_wave],"jbemlyr")
                         )
  
  rperson_all <- read_dta(paste0(folder_Australia_1, Combined_[which_wave]), col_select = selected_variables)
  
  # Delete first letter from variable names in a downloaded dataset
  names(rperson_all)[-1] <- substring(names(rperson_all)[-1], 2)
  
  rperson_all$Wave = years[which_wave]
  
  rperson_all = rperson_all %>% rename(
                         weeks_years = jbemlha,
                         weeks = jbemlwk,
                         years= jbemlyr
                         ) %>% 
    mutate(weeks = ifelse(weeks<0, NA,weeks),
           years = ifelse(years<0,NA,years),
           weeks_years = ifelse(weeks_years==1, "years",ifelse(weeks_years==2,"weeks", NA))
           )
                           
  list_jobs_add<-rbind(list_jobs_add,rperson_all)
  
  rm(rperson_all)
  
}

# 2. Merge with Interview Data to Calculate Job Start Date
# Uses the interview date and weeks of tenure to estimate start date of job (start_job_weeks).
# Only calculated when the response was in weeks, since they provide a more precise timing.

list_jobs_add_1 = list_jobs_add %>% 
  merge(interview_hilda, by = c("xwaveid", "Wave"), all.x = T) %>% 
  # calculate start date of a job 
  mutate(start_job_weeks = if_else(weeks_years =="weeks" & !is.na(weeks), 
                                  date_event_1 -  weeks(weeks), NA
                                  ))

# 3. Normalize Job Start Dates to 'Early/Mid/Late'
# change start_job_weeks to end 05,15,25
list_jobs_add_1[c('Year','Month', 'Day')] <- str_split_fixed(list_jobs_add_1$start_job_weeks, '-', 3)

list_jobs_add_2 = list_jobs_add_1 %>% mutate(Day = as.numeric(Day),
                           Day_1 = case_when(
                             Day %in% c(1:14) ~ "05",
                             Day %in% c(15:24) ~ "15",
                             Day %in% c(25:31) ~ "25",
                             TRUE ~ NA_character_),
                           start_job_weeks_1 = ifelse(!is.na(Year),paste0(Year,"-",Month,"-", Day_1), NA)
                           ) %>% 
  select( xwaveid, Wave,weeks_years, years,start_job_weeks_1) %>% 
  mutate(start_job_weeks_1 = as.Date(start_job_weeks_1))

# 4. Flag Individuals with New Jobs
# Prepares a dataset where each person-month is marked "yes" if a new job was identified from tenure data.
list_jobs_add_3 = select(list_jobs_add_2, xwaveid,start_job_weeks_1) %>% 
  rename(date_event_1 = start_job_weeks_1) %>% 
  mutate(new_job = ifelse(!is.na(date_event_1),"yes","no"))

# 5. Load Final Calendar Dataset and Merge with Job Start Info
# Loads the final cleaned employment calendar with no overlaps across waves.
# Merges with interview data and job start indicators.
# all_calendar_y = readRDS("output/list_combined_no_overlap.rds")

all_calendar_1 = all_calendar_1_x %>% mutate(date_event_1 = as.Date(date_event_1)) %>% 
  merge(interview_hilda, by = c("xwaveid", "date_event_1"), all.x = T)


# merge calendar and change job change job in weeks

all_calendar_2 = merge(all_calendar_1_x, list_jobs_add_3, by = c("xwaveid", "date_event_1"), all.x = T)

# 6. Construct Employment Spells
# check: Detects changes in employment status (e.g., employed → unemployed).
# check_1: Detects if a new job started.
# check_2: Combines the two to determine if a new spell begins.
# spell: A cumulative counter that increments when a new spell starts.

all_calendar_3 = all_calendar_2 %>% 
  group_by(xwaveid) %>% 
  arrange(date_event_1, .by_group = T) %>% 
  mutate(check = lag(emp_status_calendar)!=emp_status_calendar,
         check = ifelse(is.na(check), T,check),
         check_1 = ifelse(new_job == "yes",T,F),
         check_1 = ifelse(is.na(check_1), F,check_1),
         check_2 = check_1 | check,
         spell = cumsum(check_2))


# calculate start end of each spell

all_calendar_4 = all_calendar_3 %>% 
  group_by(xwaveid,spell) %>% 
  arrange(date_event_1, .by_group = T) %>% 
  mutate(
    start_date = min(date_event_1),
    end_date = max(date_event_1)
  ) %>% slice(n()) %>% select(xwaveid, spell,emp_status_calendar,start_date,end_date)


################################################################################
# H. Convert Employment Spells to Wide Format & Save
################################################################################
# This step takes the spell-level long-format data and reshapes it into wide format, 
# where each employment spell becomes a set of columns per individual. The final 
# dataset summarizes each person’s employment history in a compact, panel-ready form.

hilda_employment = all_calendar_4 %>% 
  mutate(EMPLOYMENT_STATUS = case_when(
    # [1] working 
    # [2] unemployed
    # [3] not in the labor force
    # [4] retired
    # [5] other
    emp_status_calendar == "[1] Employed" ~ 1,
    emp_status_calendar == "[3] Not in the labour force" ~ 2,
    emp_status_calendar == "[2] Unemployed" ~ 3,
    emp_status_calendar == "[4] retired" ~ 4,
    TRUE~NA
  ))

hilda_employment[c("ENTRY_Y","ENTRY_M","ENTRY_D")] <- str_split_fixed(hilda_employment$start_date, '-', 3)
hilda_employment[c("EXIT_Y","EXIT_M","EXIT_D")] <- str_split_fixed(hilda_employment$end_date, '-', 3)

#### to integer
hilda_employment = hilda_employment %>% 
  mutate(
    across(c("EMPLOYMENT_STATUS", "ENTRY_Y", "ENTRY_M", "ENTRY_D", "EXIT_Y", "EXIT_M", "EXIT_D"), as.integer),
    EMPLOYMENT_STATUS = labelled(
      EMPLOYMENT_STATUS,
      c("working" = 1,
        "unemployed" = 2,
        "not in the labor force" = 3,
        "retired" = 4,
        "other" = 5))
  )

variables = c("ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M","EXIT_D")

employment_biography_hilda_wide = hilda_employment %>% 
  pivot_wider(
    id_cols = c(xwaveid),
    names_from = spell ,
    values_from = c("EMPLOYMENT_STATUS","ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M", "EXIT_D"),
    names_glue = "{.value}_{spell}"
  )

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

employment_biography_hilda_wide<-employment_biography_hilda_wide[,c("xwaveid",ordering_variables_wide_format(max(hilda_employment$spell)))]
# add stata labels

# Apply labels to employment spell variables

for (i in 1:max(hilda_employment$spell)) {
  var_status <- paste0("EMPLOYMENT_STATUS_", i)
  var_entry_y <- paste0("ENTRY_Y_", i)
  var_entry_m <- paste0("ENTRY_M_", i)
  var_entry_d <- paste0("ENTRY_d_", i)
  var_exit_y <- paste0("EXIT_Y_", i)
  var_exit_m <- paste0("EXIT_M_", i)
  var_exit_d <- paste0("EXIT_D_", i)
  
  if (var_status %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_status]]) <- paste("Employment status during spell", i)
  }
  if (var_entry_y %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_entry_y]]) <- paste("Year entry for spell", i)
  }
  if (var_entry_m %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_entry_m]]) <- paste("Month entry for spell", i)
  }
  if (var_exit_y %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_exit_y]]) <- paste("Year exit for spell", i)
  }
  if (var_exit_m %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_exit_m]]) <- paste("Month exit for spell", i)
  }
  if (var_entry_d %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_entry_d]]) <- paste("Day entry for spell", i)
  }
  if (var_exit_d %in% names(employment_biography_hilda_wide)) {
    var_label(employment_biography_hilda_wide[[var_exit_d]]) <- paste("Day exit for spell", i)
  }
}

######### save results #########################################################

saveRDS(employment_biography_hilda_wide , "output/hilda_employment.rds")   


if (type_you_want==".csv") {
  write.csv(employment_biography_hilda_wide, "output/hilda_employment.csv")
}else if(type_you_want==".dta"){
  write_dta(employment_biography_hilda_wide, "output/hilda_employment.dta")
  
}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_soep","folder_uk_fertility_1"
)]

rm(list = x)














