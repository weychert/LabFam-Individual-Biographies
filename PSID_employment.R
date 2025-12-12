# List of required packages for this task and establish working directory
source("00_setting_work_space.R")

files <- list.files(path = folder_family_files, pattern = ".zip$")
outDir <- paste0(folder_family_files , "/new_family_folder_psid")


master_psid_constant = readRDS("output/master_psid_wide.rds") %>% select(pid, SEX, BORN_Y)

all_years = data.frame(
  year_wave = c(1968:1997, seq(1999,2021,2)),
  syear = 1:length(c(1968:1997, seq(1999,2021,2)))
  )

master_psid_wide_int_date = readRDS("output/master_psid_wide.rds") %>% 
  select(pid,starts_with("interview_date")) %>% 
  pivot_longer(
    cols = starts_with("interview_date"),
    names_to = "syear",                
    values_to = "interview_date" 
  ) %>% 
  mutate(year_wave = str_remove( syear, "INTERVIEW_DATE_")) %>% select(-syear) %>% 
  filter(!is.na(interview_date)) %>% merge(all_years, by = c("year_wave"))

master_psid_wide_int_date %>% head

psid_master = psid_master = readRDS("output/master_psid_wide.rds") %>% 
  select(pid,
         starts_with("rel_hsh"),
         starts_with("EMP_STATUS"), starts_with("INTERVIEW_NUMBER")) %>% 
  pivot_longer(
    cols = matches("^(rel_hsh_|EMP_STATUS_|INTERVIEW_NUMBER)"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(year_wave= as.integer(year)) %>% select(-year ) %>% 
  merge(master_psid_wide_int_date, by = c("pid", "year_wave"), all.x = T) %>% 
  merge(master_psid_constant, by = c("pid"), all.x = T)

psid_master %>% head


# source("psid_functions.r")
emp_stat <- function(variables) {
  variables = as.character(variables)
  variables =
    case_when(
      variables =="1"~ "Working",
      variables =="2"~ "laid off",
      variables =="3"~ "unemployed",
      variables =="4"~ "Retired",
      variables =="5"~ "disabled",
      variables =="6"~ "Keeping House",
      variables =="7"~ "Student",
      variables =="8"~ "Other",
      variables =="9"~ "refused",
      variables =="0"~ "Inap",
      variables %in% c("22","99")~ NA_character_,
      TRUE ~ variables
    )
  
}
################################################################################
# 1. prepare list of variables with psidR for period 1968-1976 and 1976-1987
# r = system.file(package="psidR")
cwf <- openxlsx::read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")

years_1968_1987 = data.frame(
  year_wave = c(seq(1968,1987,1)),
  INTERVIEW_NUMBER =  psidR::getNamesPSID("ER21002", cwf, years = seq(1968,1987,1))$variable,
  head_type =psidR::getNamesPSID("V15455", cwf, years = seq(1968,1987,1))$variable,
  hsh_composition = psidR::getNamesPSID("V114", cwf, years = seq(1968,1987,1))$variable,
  ### head 
  emp_status_HE = psidR::getNamesPSID("V196", cwf, years = seq(1968,1987,1))$variable,
  self_emp_HE = psidR::getNamesPSID("V6493", cwf, years = seq(1968,1987,1))$variable,
  more_than_two_jobs_HE  = psidR::getNamesPSID("V662", cwf, years = seq(1968,1987,1))$variable,
  # tenure  How long have you had your present position? Actual number of months
  nr_months_position_HE=psidR::getNamesPSID("V14167",cwf, years = seq(1968,1987,1))$variable,
  end_year_HE=psidR::getNamesPSID("V14247",cwf, years = seq(1968,1987,1))$variable,
  ever_worked_HE = psidR::getNamesPSID("V4552",cwf, years = seq(1968,1987,1))$variable,
  ### spouse 
  emp_status_SP = psidR::getNamesPSID("V15456", cwf, years = seq(1968,1987,1))$variable,
  more_than_two_jobs_SP =  psidR::getNamesPSID("V4903",  cwf, years = seq(1968,1987,1))$variable,
  nr_months_position_SP=psidR::getNamesPSID("V14340",cwf, years = seq(1968,1987,1))$variable,
  end_year_SP= psidR::getNamesPSID("V14411",cwf, years = seq(1968,1987,1))$variable,
  ever_worked_SP = psidR::getNamesPSID("V15620",cwf, years = seq(1968,1987,1))$variable)

################################################################################
# 2. Prepare period 1968 -1978

all_data_1968_1987<-c()
for (i in 1:length(years_1968_1987$year_wave)) {
  
  needed_variables_1968_1976_wave <- years_1968_1987 %>%
    filter(year_wave == year_wave[i]) %>% # This line may need context for `i`
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "name") %>% filter(name !="year_wave") %>% filter(!is.na(V1))
  
  x = readRDS((paste0(outDir,"/FAM",years_1968_1987$year_wave[i],".rds"))) %>% 
    select(needed_variables_1968_1976_wave$V1)
  
  x<-select(x, needed_variables_1968_1976_wave$V1)
  
  names(x)<-c(needed_variables_1968_1976_wave$name)
  
  # add year wave
  x$year_wave<-years_1968_1987$year_wave[i]
  
  # append next wave
  all_data_1968_1987 <-bind_rows(all_data_1968_1987, x)
  
  print(years_1968_1987$year_wave[i])
  
  rm(x, needed_variables_1968_1976_wave)
  
}

# in from 1968 - 1976 we have information only for the household head about employment 

self_emp <- function(variables) {
  case_when(
    variables == 0 ~ "not working",
    variables == 1 ~ "so else",
    variables == 2 ~ "both",
    variables == 3 ~ "self-emp",
    variables == 9 ~ NA_character_
    
  )
}
emp_stat_1968_1976 <- function(variables) {
  variables = as.character(variables)
  variables =
    case_when(
      variables =="1"~ "Working",
      variables =="2"~ "unemployed",
      variables =="3"~ "Retired",
      variables =="4"~ "Housework",
      variables =="5"~ "Student",
      variables =="6"~ "other",
      
      TRUE ~ variables
    )
  
}

more_than_two_jobs <- function(variables,emp_status_HE_x) {
  
  case_when(
    variables ==0 & emp_status_HE_x=="Working" ~ "one job",
    variables ==3 & emp_status_HE_x=="Working" ~ "one job",
    variables ==1 & emp_status_HE_x=="Working" ~ "two jobs",
    variables ==0 & emp_status_HE_x!="Working" ~ "not working",
    variables ==9 ~ NA_character_
  )
  
}

head_1968_1976 = psid_master %>% filter(rel_hsh == "HE") %>% filter(year_wave <1976)

emp_head_1968_1976 = all_data_1968_1987 %>%  
  select(INTERVIEW_NUMBER, year_wave,emp_status_HE, self_emp_HE,more_than_two_jobs_HE)%>% 
  mutate(
    self_emp_HE = self_emp(self_emp_HE),
    emp_status_HE = emp_stat_1968_1976(emp_status_HE),
    more_than_two_jobs_HE = more_than_two_jobs(more_than_two_jobs_HE,emp_status_HE)) %>% filter(year_wave <1976) %>% 
  merge(head_1968_1976, by = c("year_wave", "INTERVIEW_NUMBER"), all.x = T) %>% 
  group_by(pid) %>% arrange(year_wave,.by_group = T) %>% 
  mutate(change = ifelse(emp_status_HE !=lag(emp_status_HE),1,0),
         change = ifelse(is.na(change),1,change),
         nr_emp_spell = cumsum(change)
         ) %>% select(-change)


emp_head_1968_1976 = emp_head_1968_1976 %>% 
  select(pid, year_wave, INTERVIEW_NUMBER, nr_emp_spell, emp_status_HE, interview_date ) %>% 
  rename(employment_status = emp_status_HE) %>% 
  group_by(pid, nr_emp_spell) %>% 
  mutate(
    start_date = min(year_wave),
    end_date = max(year_wave)
    ) %>% 
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date)) %>% 
  select(pid, nr_emp_spell, employment_status, start_date, end_date) %>% distinct() %>% 
  mutate(start_date = paste0(start_date, "-01-01")) %>% 
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(paste0(end_date , "-12-31"))
         )

################################################################################
# 2.Prepare period 1976 - 1987 we have information for head and spouse
label_1_5 <- function(variables) {
  case_when(
    variables  ==1 ~ "yes",
    variables  ==5 ~ "no",
    variables  ==9 ~ "NA",
    variables  ==8 ~ "DK",
    variables  ==0 ~ "now working",
    is.na(variables) ~ "not present in this wave")
}
# head
years_1976_1987 = years_1968_1987 %>% filter(year_wave >=1976) %>% select(year_wave, ends_with("_HE"))

emp_head_1976_1987_HE = all_data_1968_1987 %>%  
  select("INTERVIEW_NUMBER",names(years_1976_1987))%>% 
  mutate(
    emp_status_HE = emp_stat_1968_1976(emp_status_HE),
    self_emp_HE = self_emp(self_emp_HE),
    more_than_two_jobs_HE = more_than_two_jobs(more_than_two_jobs_HE,emp_status_HE),
    # nr_months_position_HE 
    nr_months_position_HE = round(as.numeric(nr_months_position_HE), digits = 0),
    nr_months_position_HE = ifelse(emp_status_HE =="Working" | nr_months_position_HE %notin% c(0,998,999),nr_months_position_HE, NA),
    ever_worked_HE = label_1_5(ever_worked_HE),
    end_year_HE = year(end_year_HE)) %>% 
  filter(year_wave >=1976) 


DataExplorer::plot_missing(psid_master)

head_1976_1987 =  psid_master %>% filter(rel_hsh == "HE") %>% 
  select( pid, year_wave, INTERVIEW_NUMBER, interview_date) %>% 
  filter(year_wave >=1976)

emp_head_1976_1987_HE_1 = merge(emp_head_1976_1987_HE, head_1976_1987, by = c("year_wave", "INTERVIEW_NUMBER"), all.x = T) %>% 
  group_by(pid) %>% arrange(year_wave,.by_group = T) %>% 
  # once retired always retired
  mutate(
    emp_status_HE = emp_status_HE,
         if_retited = ifelse(emp_status_HE=="Retired",1,NA)) %>% 
  fill(if_retited, .direction = "down") %>% 
  mutate(if_retited = ifelse(is.na(if_retited),0,if_retited),
         emp_status_HE = ifelse(if_retited==1,"Retired",emp_status_HE)) %>% 
  mutate(start_emp = ymd(interview_date) %m-% period(months = nr_months_position_HE),
         check = ifelse(emp_status_HE==lag(emp_status_HE),0,1),
         check = ifelse(is.na(check),1,check),
         nr_spell = cumsum(check)) %>% 
  mutate(start_emp  = ifelse(is.na(start_emp ), lag(as.character(interview_date)), as.character(start_emp))) %>% 
  mutate(start_emp = ifelse(emp_status_HE!="Working",lag(as.character(interview_date)),start_emp ),
         start_emp = ifelse(is.na(start_emp ),as.character(interview_date),start_emp)
         ) %>% 
  group_by(pid, nr_spell) %>% 
  mutate(start_emp = min(as.Date(start_emp)),
         end_emp = max(as.Date(interview_date))) %>% 
  select("pid", "nr_spell",  "emp_status_HE","start_emp","end_emp") %>% distinct() %>% 
  rename(nr_emp_spell =  nr_spell,
         start_date = start_emp ,
         end_date = end_emp,
         employment_status = emp_status_HE
  ) %>% mutate(who = "HE")

################################################################################
# spouse 
years_1976_1987 = years_1968_1987 %>% filter(year_wave >=1976) %>% select(year_wave, ends_with("_SP"))

emp_spouse_1976_1987_SP = all_data_1968_1987 %>%  
  select("INTERVIEW_NUMBER",names(years_1976_1987))%>% 
  mutate(
    emp_status_SP = emp_stat_1968_1976(emp_status_SP),
    more_than_two_jobs_SP = more_than_two_jobs(more_than_two_jobs_SP,emp_status_SP),
    # nr_months_position_SP 
    nr_months_position_SP = round(as.numeric(nr_months_position_SP), digits = 0),
    nr_months_position_SP = ifelse(emp_status_SP =="Working" | nr_months_position_SP %notin% c(0,998,999),nr_months_position_SP, NA),
    ever_worked_SP = label_1_5(ever_worked_SP),
    end_year_SP = year(end_year_SP)) %>% 
  filter(year_wave >=1976) 

spouse_1976_1987 =  psid_master %>% filter(rel_hsh == "SP") %>% select( pid, year_wave, INTERVIEW_NUMBER, interview_date) %>% filter(year_wave >=1976)

emp_spouse_1976_1987_SP_1 = merge(emp_spouse_1976_1987_SP, spouse_1976_1987, by = c("year_wave", "INTERVIEW_NUMBER"), all.x = T) %>% 
  group_by(pid) %>% arrange(year_wave,.by_group = T) %>% filter(!is.na(pid) & !is.na(emp_status_SP)) %>% 
  # once retired always retired
  mutate(
    emp_status_SP = emp_status_SP,
         if_retited = ifelse(emp_status_SP=="Retired",1,NA)) %>% 
  fill(if_retited, .direction = "down") %>% 
  mutate(if_retited = ifelse(is.na(if_retited),0,if_retited),
         emp_status_SP = ifelse(if_retited==1,"Retired",emp_status_SP)) %>% 
  mutate(start_emp = ymd(interview_date) %m-% period(months = nr_months_position_SP),
         check = ifelse(emp_status_SP==lag(emp_status_SP),0,1),
         check = ifelse(is.na(check),1,check),
         nr_spell = cumsum(check)) %>% 
  mutate(start_emp  = ifelse(is.na(start_emp ), lag(as.character(interview_date)), as.character(start_emp))) %>% 
  mutate(start_emp = ifelse(emp_status_SP!="Working",lag(as.character(interview_date)),start_emp ),
         start_emp = ifelse(is.na(start_emp ),as.character(interview_date),start_emp)
         ) %>% 
  group_by(pid, nr_spell) %>% 
  mutate(start_emp = min(as.Date(start_emp)),
         end_emp = max(as.Date(interview_date))) %>% 
  select("pid", "nr_spell",  "emp_status_SP","start_emp","end_emp") %>% distinct() %>% 
  rename(nr_emp_spell =  nr_spell,
         start_date = start_emp ,
         end_date = end_emp,
         employment_status = emp_status_SP
  )  %>% mutate(who = "SP")

psid_employment_1976_1987 = bind_rows(emp_spouse_1976_1987_SP_1, emp_head_1976_1987_HE_1) 

to_delete = psid_employment_1976_1987 %>% group_by(pid) %>% summarise(n1=n_distinct(who)) %>% filter(n1==2) %>% ungroup()

psid_employment_1976_1987 = psid_employment_1976_1987 %>% filter( pid %notin% to_delete$pid)

################################################################################
# Prepare period  employment in 1988-2001
all_years = c(seq(1988,1997,1),1999,2001)
needed_variables_1988_2001 = data.frame(
  # general 
  year_wave = all_years,
  INTERVIEW_NUMBER	=  psidR::getNamesPSID("ER21002",	 cwf, years =all_years )$variable,
  head_type	=  psidR::getNamesPSID("V15455",	 cwf, years =all_years )$variable,
  # Head
  emp_status_HE	=  psidR::getNamesPSID("V15154",	 cwf, years =all_years )$variable,
  ever_worked_HE	=  psidR::getNamesPSID("V15318",	 cwf, years =all_years )$variable,
  end_year_retired_HE	=  psidR::getNamesPSID("V15155",	 cwf, years =all_years )$variable,
  # 1. head start_date_present_HE
  start_year_present_emp_HE	=  psidR::getNamesPSID("V15183",	 cwf, years =all_years )$variable, #wane 
  start_month_present_emp_HE	=  psidR::getNamesPSID("V15182",	 cwf, years =all_years )$variable, # Wazne 
  # 2. head start_date_most_recent_HE
  start_month_most_recent_HE	=  psidR::getNamesPSID("V15329",	 cwf, years =all_years )$variable,# wazne 
  start_year_most_recent_HE	=  psidR::getNamesPSID("V15330",	 cwf, years =all_years )$variable,#wazne 
  # 3. head end_date_prev_position_HE
  end_month_prev_position_HE	=  psidR::getNamesPSID("V15319",	 cwf, years =all_years )$variable,# wanze 
  end_year_prev_position_HE	=  psidR::getNamesPSID("V15320",	 cwf, years =all_years )$variable, #wazne 
  # spouse
  emp_status_SP	=  psidR::getNamesPSID("V15456",	 cwf, years =all_years )$variable,
  ever_worked_SP	=  psidR::getNamesPSID("V15620",	 cwf, years =all_years )$variable,
  end_year_retired_SP	=  psidR::getNamesPSID("V15457",	 cwf, years =all_years )$variable,
  # 1. spouse start_date_present_SP
  start_year_present_emp_SP	=  psidR::getNamesPSID("V15485",	 cwf, years =all_years )$variable,
  start_month_present_emp_SP	=  psidR::getNamesPSID("V15484",	 cwf, years =all_years )$variable,
  # 2. spouse start_date_most_recent_SP
  start_month_most_recent_SP	=  psidR::getNamesPSID("V15631",	 cwf, years =all_years )$variable,
  start_year_most_recent_SP	=  psidR::getNamesPSID("V15632",	 cwf, years =all_years )$variable,
  # 3. spouse end_date_prev_position_SP
  end_month_prev_position_SP	=  psidR::getNamesPSID("V17153",	 cwf, years =all_years )$variable,
  end_year_prev_position_SP	=  psidR::getNamesPSID("V15622",	 cwf, years =all_years )$variable,
  
  ##############################################################################
  # main job characteristics 
  # head
  total_hours_HE	=  psidR::getNamesPSID("V14835",	 cwf, years =all_years )$variable,
  avg_hours_HE	=  psidR::getNamesPSID("V15258",	 cwf, years =all_years )$variable,
  OCC_HE	=  psidR::getNamesPSID("V15162",	 cwf, years =all_years )$variable,
  IND_HE	=  psidR::getNamesPSID("V15163",	 cwf, years =all_years )$variable,
  income_HE	=  psidR::getNamesPSID("V16145",	 cwf, years =all_years )$variable,
  # spouse 
  total_hours_SP	=  psidR::getNamesPSID("V14865",	 cwf, years =all_years )$variable,
  avg_hours_SP	=  psidR::getNamesPSID("V15560",	 cwf, years =all_years )$variable,
  OCC_SP	=  psidR::getNamesPSID("V15464",	 cwf, years =all_years )$variable,
  IND_SP	=  psidR::getNamesPSID("V15465",	 cwf, years =all_years )$variable,
  income_SP_1	=  psidR::getNamesPSID("V14920",	 cwf, years =all_years )$variable,
  income_SP_2	=  psidR::getNamesPSID("ER4141",	 cwf, years =all_years )$variable,
  #### head 
  has_extra_job_HE = psidR::getNamesPSID("V15260", cwf, years = seq(1988,2001,1))$variable,
  start_month_ext_1_w_HE=psidR::getNamesPSID("V15268",cwf, years = seq(1988,2001,1))$variable,
  start_month_ext_2_w_HE=psidR::getNamesPSID("V15291",cwf, years = seq(1988,2001,1))$variable,
  start_year_ext_1_w_HE=psidR::getNamesPSID("V15269",cwf, years = seq(1988,2001,1))$variable,
  start_year_ext_2_w_HE=psidR::getNamesPSID("V15292",cwf, years = seq(1988,2001,1))$variable,
  end_month_ext_1_w_HE=psidR::getNamesPSID("V15283",cwf, years = seq(1988,2001,1))$variable,
  end_month_ext_2_w_HE=psidR::getNamesPSID("V15306",cwf, years = seq(1988,2001,1))$variable,
  end_year_ext_1_w_HE=psidR::getNamesPSID("V15284",cwf, years = seq(1988,2001,1))$variable,
  end_year_ext_2_w_HE=psidR::getNamesPSID("V15307",cwf, years = seq(1988,2001,1))$variable,
  # head extra characteristics  
  occup_ext_job_1_HE=	psidR::getNamesPSID("V15263",cwf, years = seq(1988,2001,1))$variable,
  occup_ext_job_2_HE=	psidR::getNamesPSID("V15286",cwf, years = seq(1988,2001,1))$variable,
  govt_ext_job_1_HE=	psidR::getNamesPSID("V15262",cwf, years = seq(1988,2001,1))$variable,
  govt_ext_job_2_HE=	psidR::getNamesPSID("V15285",cwf, years = seq(1988,2001,1))$variable,
  #### spouse 
  has_extra_job_SP = psidR::getNamesPSID("V15562", cwf, years = seq(1988,2001,1))$variable,
  start_month_ext_1_w_SP=psidR::getNamesPSID("V15570",cwf, years = seq(1988,2001,1))$variable,
  start_month_ext_2_w_SP=psidR::getNamesPSID("V15593",cwf, years = seq(1988,2001,1))$variable,
  start_year_ext_1_w_SP=psidR::getNamesPSID("V15571",cwf, years = seq(1988,2001,1))$variable,
  start_year_ext_2_w_SP=psidR::getNamesPSID("V15594",cwf, years = seq(1988,2001,1))$variable,
  end_month_ext_1_w_SP=psidR::getNamesPSID("V15585",cwf, years = seq(1988,2001,1))$variable,
  end_month_ext_2_w_SP=psidR::getNamesPSID("V15608",cwf, years = seq(1988,2001,1))$variable,
  end_year_ext_1_w_SP=psidR::getNamesPSID("V15586",cwf, years = seq(1988,2001,1))$variable,
  end_year_ext_2_w_SP=psidR::getNamesPSID("V15609",cwf, years = seq(1988,2001,1))$variable,
  # spouse extra characteristics
  occup_ext_job_1_SP=psidR::getNamesPSID("V15565",cwf, years = seq(1988,2001,1))$variable,
  occup_ext_job_2_SP=psidR::getNamesPSID("V15588",cwf, years = seq(1988,2001,1))$variable)

################################################################################
# download data
all_data_1988_2001<-c()
for (i in 1:length(needed_variables_1988_2001$year_wave)) {
  
  needed_variables_1988_2001_wave <- needed_variables_1988_2001 %>%
    filter(year_wave == year_wave[i]) %>% # This line may need context for `i`
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "name") %>% filter(name !="year_wave") %>% filter(!is.na(V1))
  
  x = readRDS((paste0(outDir,"/FAM",needed_variables_1988_2001$year_wave[i],".rds"))) %>% 
    select(needed_variables_1988_2001_wave$V1)
  
  x<-select(x, needed_variables_1988_2001_wave$V1)
  
  names(x)<-c(needed_variables_1988_2001_wave$name)
  
  # add year wave
  x$year_wave<-needed_variables_1988_2001$year_wave[i]
  
  # append next wave
  all_data_1988_2001 <-bind_rows(all_data_1988_2001, x)
  
  print(needed_variables_1988_2001$year_wave[i])
  
  rm(x, needed_variables_1968_1976_wave)
  
}

############## create dates ####################################################
to_iterate_year = names(all_data_1988_2001 %>% select(matches("_year_")))
to_iterate_month = names(all_data_1988_2001 %>% select(matches("_month_")))

create_date <- function(month, year) {
  month = ifelse(month<=9,"09",as.character(month))
  ifelse(!is.na(month) & !is.na(year),  paste(year,month, "01", sep = "-"),NA)
}

change_to_year  <- function(variables) {
  variables = as.integer(round(variables, digits = 0))
  ifelse(
    variables %in% c(10:99) & !is.na(variables) , as.integer(paste0(19, variables)),
    ifelse(variables %in% c(1:9) & !is.na(variables), as.integer(paste0(190, variables)), variables)
  )
  
}

has_extra_job  <- function(variables,  emp_status_x) {
  variables  = round(variables, digits = 0)
  case_when(
    variables==0 & emp_status_x !="Working" ~ "not working",
    variables==1 ~ "has extra job",
    variables==5 ~ "no extra job",
    variables %in% c(8,9) ~ NA_character_  
  )
}


to_iterate_year  <- names(all_data_1988_2001 %>% select(matches("_year_")))
to_iterate_month <- names(all_data_1988_2001 %>% select(matches("_month_")))

all_data_1988_2001_1 <- all_data_1988_2001 %>% 
  mutate(
    ever_worked_HE = label_1_5(ever_worked_HE),
    ever_worked_SP = label_1_5(ever_worked_SP)
  ) %>%
  # flag zeros BEFORE we clean
  mutate(across(all_of(to_iterate_month), ~ if_else(. == 0, 1L, 0L), .names = "flag_{.col}")) %>%
  mutate(across(all_of(to_iterate_year),  ~ if_else(. == 0, 1L, 0L), .names = "flag_{.col}")) %>%
  # clean months/years for date building (keep them as integers)
  mutate(across(all_of(to_iterate_month),
                ~ dplyr::na_if(as.integer(.), 0))) %>%                 # turn 0 -> NA
  mutate(across(all_of(to_iterate_month),
                ~ dplyr::if_else(. >= 1L & . <= 12L, ., NA_integer_))) %>%
  mutate(across(all_of(to_iterate_year),
                ~ dplyr::na_if(as.integer(.), 0))) %>%                 # turn 0 -> NA
  mutate(across(all_of(to_iterate_year),
                ~ change_to_year(.))) %>%                              # your custom cleaner (keeps numeric)
  mutate(
    head_type = dplyr::case_when(
      head_type == 1 ~ "male head with wife",
      head_type == 2 ~ "male head single",
      head_type == 3 ~ "female head"
    ),
    # prepare employment status
    emp_status_SP = emp_stat(emp_status_SP),
    emp_status_HE = emp_stat(emp_status_HE),
    # create dates (months/years are now plain integers)
    start_date_present_HE     = create_date(year = start_year_present_emp_HE,    month = start_month_present_emp_HE),
    end_date_prev_position_HE = create_date(year = end_year_prev_position_HE,    month = end_month_prev_position_HE),
    start_date_most_recent_HE = create_date(year = start_year_most_recent_HE,    month = start_month_most_recent_HE),
    
    start_date_present_SP     = create_date(year = start_year_present_emp_SP,    month = start_month_present_emp_SP),
    end_date_prev_position_SP = create_date(year = end_year_prev_position_SP,    month = end_month_prev_position_SP),
    start_date_most_recent_SP = create_date(year = start_year_most_recent_SP,    month = start_month_most_recent_SP),
    
    # extra jobs
    has_extra_job_HE = has_extra_job(has_extra_job_HE, emp_status_x = emp_status_HE),
    
    emp_status_SP = emp_stat(variables = emp_status_SP),
    has_extra_job_SP = has_extra_job(variables = has_extra_job_SP, emp_status_x = emp_status_SP),
    has_extra_job_SP = ifelse(has_extra_job_SP == "not working" & head_type != "male head with wife",
                              NA, has_extra_job_SP),
    
    start_date_ext_job_1_HE = create_date(year = start_year_ext_1_w_HE, month = start_month_ext_1_w_HE),
    start_date_ext_job_2_HE = create_date(year = start_year_ext_2_w_HE, month = start_month_ext_2_w_HE),
    end_date_ext_job_1_HE   = create_date(year = end_year_ext_1_w_HE,   month = end_month_ext_1_w_HE),
    end_date_ext_job_2_HE   = create_date(year = end_year_ext_2_w_HE,   month = end_month_ext_2_w_HE),
    
    start_date_ext_job_1_SP = create_date(year = start_year_ext_1_w_SP, month = start_month_ext_1_w_SP),
    start_date_ext_job_2_SP = create_date(year = start_year_ext_2_w_SP, month = start_month_ext_2_w_SP),
    end_date_ext_job_1_SP   = create_date(year = end_year_ext_1_w_SP,   month = end_month_ext_1_w_SP),
    end_date_ext_job_2_SP   = create_date(year = end_year_ext_2_w_SP,   month = end_month_ext_2_w_SP)
  )

################################################################################
# Select variables that are the best 
chosen_variables = all_data_1988_2001_1 %>% select(INTERVIEW_NUMBER, year_wave, head_type,
                                                   # head
                                                   emp_status_HE, 
                                                   ever_worked_HE,
                                                   start_year_present_emp_HE, 
                                                   end_year_retired_HE,
                                                   start_date_present_HE, 
                                                   start_date_most_recent_HE,
                                                   end_date_prev_position_HE,
                                                   # Spouse
                                                   emp_status_SP, ever_worked_SP,
                                                   start_year_present_emp_SP, end_year_retired_SP,
                                                   start_date_present_SP, 
                                                   start_date_most_recent_SP,end_date_prev_position_SP
)

# from master file prepared download technical and demographic variables 

interview_date = select(readRDS("output/master_psid_wide.rds"), pid, starts_with("interview_date")) %>% 
  pivot_longer(
    cols = starts_with("interview_date"),      # Columns to reshape
    names_to = "year_wave",  # New column for months
    values_to = "interview_date"  # New column for values
  ) %>% mutate(year_wave = as.numeric(stringr::str_remove(year_wave, "INTERVIEW_DATE_")))


################################################################################
# prepare employment status
# years you care about
all_years <- c(1968:1997, seq(1999, 2021, 2))

# map PSID variable names to each year
years_add <- tibble(
  year_wave      = all_years,
  INTERVIEW_VAR  = psidR::getNamesPSID("ER30001", cwf, years = all_years)$variable,
  REL_HSH_VAR    = psidR::getNamesPSID("ER33703", cwf, years = all_years)$variable,
  EMP_STATUS_VAR = psidR::getNamesPSID("ER33712", cwf, years = all_years)$variable
)

# keep only years where all three vars exist
years_keep <- years_add %>%
  filter(!is.na(INTERVIEW_VAR), !is.na(REL_HSH_VAR), !is.na(EMP_STATUS_VAR))

int_vars <- years_keep$INTERVIEW_VAR
rel_vars <- years_keep$REL_HSH_VAR
emp_vars <- years_keep$EMP_STATUS_VAR

# read data and build pid
x <- readRDS(file.path(outDir, "ind2021.rds")) %>%
  mutate(pid = ER30001 * 1000 + ER30002)

# INTERVIEW_NUMBER long
int_long <- x %>%
  select(pid, dplyr::all_of(int_vars)) %>%
  pivot_longer(
    cols      = -pid,
    names_to  = "INTERVIEW_VAR",
    values_to = "INTERVIEW_NUMBER"
  ) %>%
  left_join(years_keep %>% select(year_wave, INTERVIEW_VAR), by = "INTERVIEW_VAR")

# rel_hsh long (raw -> recode)
rel_long <- x %>%
  select(pid, dplyr::all_of(rel_vars)) %>%
  pivot_longer(
    cols      = -pid,
    names_to  = "REL_HSH_VAR",
    values_to = "rel_hsh_raw"
  ) %>%
  left_join(years_keep %>% select(year_wave, REL_HSH_VAR), by = "REL_HSH_VAR") %>%
  mutate(
    rel_hsh = dplyr::case_when(
      rel_hsh_raw %in% c(1, 10) ~ "HE",
      rel_hsh_raw %in% c(2, 20) ~ "SP",
      TRUE ~ "other"
    )
  )

# emp_status long (then apply your recoder)
emp_long <- x %>%
  select(pid, dplyr::all_of(emp_vars)) %>%
  pivot_longer(
    cols      = -pid,
    names_to  = "EMP_STATUS_VAR",
    values_to = "emp_status"
  ) %>%
  left_join(years_keep %>% select(year_wave, EMP_STATUS_VAR), by = "EMP_STATUS_VAR") %>%
  mutate(emp_status = emp_stat(emp_status))

# combine to final wide with requested columns
out <- int_long %>%
  inner_join(rel_long %>% select(pid, year_wave, rel_hsh), by = c("pid", "year_wave")) %>%
  inner_join(emp_long %>% select(pid, year_wave, emp_status), by = c("pid", "year_wave")) %>%
  select(year_wave, INTERVIEW_NUMBER, pid, rel_hsh, emp_status) %>%
  arrange(pid, year_wave)

head_1988_2001 = out %>% 
  filter(year_wave %in% c(1988:2001)) %>%
  select(year_wave, INTERVIEW_NUMBER,pid,rel_hsh, emp_status) %>% 
  filter( rel_hsh == "HE") %>% merge(select(readRDS("output/master_psid_wide.rds"), pid, SEX, BORN_Y), by = c("pid")) %>% 
  merge(interview_date, by = c("pid", "year_wave"))

spouse_1988_2001 = out %>% 
  filter(year_wave %in% c(1988:2001)) %>%
  select(year_wave, INTERVIEW_NUMBER,pid,rel_hsh, emp_status) %>% 
  filter( rel_hsh == "SP") %>% 
  merge(select(readRDS("output/master_psid_wide.rds"), pid, SEX, BORN_Y), by = c("pid")) %>% 
  merge(interview_date, by = c("pid", "year_wave"))

#### prepare head
emp_head_1988_2001 =  head_1988_2001 %>% 
  merge(select(chosen_variables, INTERVIEW_NUMBER, year_wave,head_type, ends_with("_HE")),  
        c("year_wave", "INTERVIEW_NUMBER"), all.x = T) %>% 
  mutate(
    date_retired = ifelse(is.na(end_year_retired_HE),NA,paste0(end_year_retired_HE, "-01-01")),
    # start of event
    start_event = case_when(
      emp_status == "Working" ~  start_date_present_HE,
      emp_status %notin%  c("Working","Retired") ~ end_date_prev_position_HE,
      emp_status == "Retired" ~ date_retired),
    start_event_type = case_when(
      emp_status == "Working"  & !is.na(start_event)~  "start_date_present_HE",
      emp_status %notin%  c("Working","Retired") & !is.na(start_event) ~ "end_date_prev_position_HE",
      emp_status == "Retired" & !is.na(start_event) ~ "date_retired",
      ever_worked_HE =="no" ~ "never worked in life",
      ever_worked_HE !="no" & is.na(start_date_present_HE) & is.na(end_date_prev_position_HE) & is.na(date_retired) ~ "missing date"),
    wave_end = interview_date) %>% 
  # build spells 
  group_by(pid) %>% arrange(year_wave,.by_group = T) %>% 
  # if retired after he or she retried no possible work it is simplification of course 
  mutate(
    start_event = as.character(start_event),
    start_retired = ifelse(emp_status =="Retired", as.character(start_event),NA)) %>% 
  fill(start_retired, .direction = "down") %>% mutate(start_event = ifelse(is.na(start_event), start_retired,start_event)) %>% 
  mutate(emp_status = ifelse(emp_status!="Retired" & !is.na(start_retired), "Retired" , emp_status),
         start_event = ifelse(!is.na(start_retired) & start_event!=start_retired, start_retired, start_event)) %>% 
  mutate(
    start_event =as.Date(start_event),
    diff_unusual = start_event - lead(start_event),
    start_event = as.character(start_event),
    start_event_org = start_event,
    start_event =  ifelse(start_event >lead(start_event) & diff_unusual %in% c(365,366, 730, 731,30,61,91, 1096),lead(start_event),start_event),
    mark_unusual = ifelse(start_event >lead(start_event),1,0),
    mark_unusual = ifelse(is.na(mark_unusual),0,mark_unusual),
    start_event = as.Date(start_event),
    new_spell = ifelse(emp_status !=lag(emp_status), 1,0),
    new_spell =  ifelse(is.na(new_spell),1,new_spell),
    nr_emp_spell = cumsum(new_spell)) %>%
  select( pid, nr_emp_spell, interview_date,emp_status, start_event) %>% 
  mutate(end_event = interview_date) %>% 
  group_by(pid) %>% 
  mutate(start_event = ifelse(is.na(start_event), as.character(end_event),as.character(start_event)),
         start_event = as.Date(start_event)) %>% group_by(pid,nr_emp_spell) %>% 
  mutate( start_event = min(start_event),
          end_event = max(end_event)) %>% rename(employment_status = emp_status) %>% select(-interview_date) %>% distinct() %>% 
  mutate(who = "HE") %>% rename(start_date = start_event, end_date = end_event)

# spouse
emp_spouse_1988_2001 = spouse_1988_2001 %>% 
  merge(select(chosen_variables, INTERVIEW_NUMBER, year_wave,head_type, ends_with("_SP")),  
        c("year_wave", "INTERVIEW_NUMBER"), all.x = T) %>% 
  mutate(
    date_retired = ifelse(is.na(end_year_retired_SP),NA,paste0(end_year_retired_SP, "-01-01")),
    # start of event
    start_event = case_when(
      emp_status == "Working" ~  start_date_present_SP,
      emp_status %notin%  c("Working","Retired") ~ end_date_prev_position_SP,
      emp_status == "Retired" ~ date_retired),
    start_event_type = case_when(
      emp_status == "Working"  & !is.na(start_event)~  "start_date_present_SP",
      emp_status %notin%  c("Working","Retired") & !is.na(start_event) ~ "end_date_prev_position_SP",
      emp_status == "Retired" & !is.na(start_event) ~ "date_retired",
      ever_worked_SP =="no" ~ "never worked in life",
      ever_worked_SP !="no" & is.na(start_date_present_SP) & is.na(end_date_prev_position_SP) & is.na(date_retired) ~ "missing date"),
    wave_end = interview_date) %>% 
  # build spells 
  group_by(pid) %>% arrange(year_wave,.by_group = T) %>% 
  # if retired after he or she retried no possible work it is simplification of course 
  mutate(
    start_event = as.character(start_event),
    start_retired = ifelse(emp_status =="Retired", as.character(start_event),NA)) %>% 
  fill(start_retired, .direction = "down") %>% mutate(start_event = ifelse(is.na(start_event), start_retired,start_event)) %>% 
  mutate(emp_status = ifelse(emp_status!="Retired" & !is.na(start_retired), "Retired" , emp_status),
         start_event = ifelse(!is.na(start_retired) & start_event!=start_retired, start_retired, start_event)) %>% 
  mutate(
    start_event =as.Date(start_event),
    diff_unusual = start_event - lead(start_event),
    start_event = as.character(start_event),
    start_event_org = start_event,
    start_event =  ifelse(start_event >lead(start_event) & diff_unusual %in% c(365,366, 730, 731,30,61,91, 1096),lead(start_event),start_event),
    mark_unusual = ifelse(start_event >lead(start_event),1,0),
    mark_unusual = ifelse(is.na(mark_unusual),0,mark_unusual),
    start_event = as.Date(start_event),
    new_spell = ifelse(emp_status !=lag(emp_status), 1,0),
    new_spell =  ifelse(is.na(new_spell),1,new_spell),
    nr_emp_spell = cumsum(new_spell)) %>%
  select( pid, nr_emp_spell, interview_date,emp_status, start_event) %>% 
  mutate(end_event = interview_date) %>% 
  group_by(pid) %>% 
  mutate(start_event = ifelse(is.na(start_event), as.character(end_event),as.character(start_event)),
         start_event = as.Date(start_event)) %>% group_by(pid,nr_emp_spell) %>% 
  mutate( start_event = min(start_event),
          end_event = max(end_event)) %>% rename(employment_status = emp_status) %>% select(-interview_date) %>% distinct() %>% 
  mutate(who = "SP") %>% rename(start_date = start_event, end_date = end_event)

# bind and save it 
psid_employment_1988_2001 = bind_rows(emp_head_1988_2001, emp_spouse_1988_2001)

to_delete = psid_employment_1988_2001 %>% group_by(pid) %>% summarise(n1=n_distinct(who)) %>% filter(n1==2) %>% ungroup()

psid_employment_1988_2001 = psid_employment_1988_2001 %>% filter( pid %notin% to_delete$pid)

################################################################################
# building psid_employment_2003_2021
# "pid", "nr_emp_spell", "employment_status", "start_date", "end_date" 

# Define the list of survey years from the 21st century used in your analysis
# Here, PSID data is collected every 2 years from 2003 to 2021
years_21st_century = c(seq(2003,2021,2))
# Read the PSID cross-year variable reference file directly from the PSID website
# This Excel file contains metadata about PSID variables (names, labels, years)
cwf <- openxlsx::read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")
# Load variable lists specific to the 21st-century PSID waves
# This script should define variables to extract from PSID data files
source("PSID_variables_needed_21_century.r")
source("00_setting_work_space.R")
# Load workspace settings (paths, library imports, options, etc.)
# This script probably sets up your environment so later scripts run smoothly
################################################################################
# Create a reference data frame of all PSID survey years ("all_years")
# PSID ran annually from 1968–1997, and biennially (every 2 years) from 1999–2021.
# The code below combines both periods into one continuous list of survey years.
all_years <- data.frame(
  # Combine all PSID survey years:
  #   - 1968 through 1997 (annual surveys)
  #   - 1999 through 2021 in 2-year intervals (biennial surveys)
  year_wave = c(1968:1997, seq(1999, 2021, 2)),
  
  # Create a simple sequential ID for each survey year (1, 2, 3, ...)
  # This can be useful for indexing or merging with wave-based datasets
  syear = 1:length(c(1968:1997, seq(1999, 2021, 2)))
)

################################################################################
# import INTERVIEW_DATE from master file
master_psid_wide_int_date = readRDS("output/master_psid_wide.rds") %>% 
  select(pid,starts_with("INTERVIEW_DATE")) %>% 
  pivot_longer(
    cols = starts_with("INTERVIEW_DATE"),
    names_to = "year_wave",                
    values_to = "INTERVIEW_DATE" 
  ) %>% 
  mutate(year_wave = str_remove(year_wave,"INTERVIEW_DATE_")) %>% 
  filter(!is.na(INTERVIEW_DATE)) %>% mutate(year_wave = as.numeric(year_wave)) %>% 
  merge(all_years, by = c("year_wave"))


# extract the last interview date to fill in last end_date in employment biography
setDT(master_psid_wide_int_date)
last_int <- master_psid_wide_int_date[
  , .(INTERVIEW_DATE = max(as.Date(INTERVIEW_DATE), na.rm = TRUE)), by = pid
][
  , .(pid, last_date = ceiling_date(INTERVIEW_DATE, "month") - days(1))
]

# --- Identify each respondent’s first interview date (2003 and later) ---
# This code:
#   • Filters interview records to include only waves from 2003 onward.
#   • Groups data by individual (pid) so each person’s records are handled separately.
#   • Selects the first available interview for each pid (earliest within that period).
#   • Keeps only pid and interview date.
#   • Rounds each interview date down to the first day of its month
#     (creating a standardized "first_date" variable for alignment with other timelines).
# The resulting dataset (first_date_2003) contains one row per person
# with the first observed interview date starting in 2003 or later.
# Convert to data.table (if not already)
setDT(master_psid_wide_int_date)

# --- Identify each respondent’s first interview date (2003 and later) ---
first_date_2003 <- master_psid_wide_int_date[
  year_wave >= 2003,                                   # keep only 2003+
  .SD[1],                                               # take the first observation per pid
  by = pid
][
  , first_date := floor_date(as.Date(INTERVIEW_DATE), "month")  # round down to first day of month
][
  , .(pid, INTERVIEW_DATE, first_date)                  # keep only needed columns
]

# --- Create a monthly-aligned interview date reference for all waves (2003 and later) ---
# This code:
#   • Filters interview records to include only survey waves from 2003 onward.
#   • For each interview date, rounds it down to the first day of that month
#     to ensure consistency when merging with other month-level variables.
#   • Creates a simplified 'date' variable representing that month-start value.
#   • Keeps only essential identifiers: respondent ID (pid), wave year (year_wave), and the new date field.
# The resulting dataset (date_1) provides a clean, standardized month-level
# timeline of interview dates for each individual and wave.

date_1 = master_psid_wide_int_date %>% 
  filter(year_wave>=2003) %>% mutate( date = floor_date(INTERVIEW_DATE, "month")) %>% 
  select(pid, year_wave, date)

# import rel_hsh (if head or spouse in a household, for both types of 
# individuals we have different types of employment variables) from master file
psid_master = readRDS("output/master_psid_wide.rds") %>% 
  select(pid,
         starts_with("rel_hsh"),
         starts_with("EMP_STATUS"), starts_with("INTERVIEW_NUMBER")) %>% 
  pivot_longer(
    cols = matches("^(rel_hsh_|EMP_STATUS_|INTERVIEW_NUMBER)"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(year_wave= as.integer(year)) %>% select(-year ) %>% 
  merge(master_psid_wide_int_date, by = c("pid", "year_wave"), all.x = T) %>% 
  filter(!is.na(rel_hsh) & !is.na(EMP_STATUS) & !is.na(INTERVIEW_DATE) & !is.na(syear))


# Across all years and individuals, most people (83%) kept the same household role (head, spouse, or other) from one wave to the next.
# Around 5% changed roles, and about 11% couldn’t be compared because they were new to the panel

# psid_master %>% group_by(pid) %>% arrange(year_wave, .by_group = T) %>% 
#   mutate(check = rel_hsh == lag(rel_hsh )) %>% group_by(check) %>% 
#   summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))

# check      n  total percent
# 1 FALSE  26169 488429    5.36
# 2 TRUE  405946 488429   83.1 
# 3 NA     56314 488429   11.5 

################# download data from family file for head and sposue ###########
# Download data on employment for both head or spouse in a household from family file 
# There are no such variables for category of variable rel_hsh ("RELATION TO HEAD ) other
################################################################################
# HEAD/SPOUSE: (variable name from 2003; SPOUSE with have ending SP)
# 1. "ever_worked_HE" # ER21357 "BC62 WTR EVER WORKED" 
# [Have you (HEAD) ever done any work for money?] Yes, No, DK, NA; refused Inap.: working now or only temporarily laid off
# 2. "WORK_SINCE_PRIOR_YEAR_HE" # ER21127 "BC3 WTR WORKED SINCE JAN 1 OF PRIOR YEAR" 
# [Have you done any work for money since January 1, 2001? Please include any type of work, no matter how small]
#  Yes, No, DK, NA; refused Inap.: working now or only temporarily laid off
# 3. "years_pres_emp_HE", # ER21171 "BC41 YRS PRES EMP (H-E)"  
# How many years' experience do you (HEAD) have altogether with your present employer?--YEARS FOR CURRENT MAIN JOB
# 1-65 Actual number of years
# 4. "months_look_for_work_HE" ER21370 "BC67 MOS LOOK WRK (H-U)" How long have you been looking for work?--MONTHS
# 1-12 Actual number of months
# 5. "YR_LAST_WORKED_HE" ER21359 "BC63 YR LAST WORKED"  In what month and year did you last work? [IF NECESSARY: What would be your best estimate?]-YEAR
# Actual year 1901 - 2022
# 6. "MO_LAST_WORKED_HE" ER21358 "BC63 MO LAST WORKED"  In what month and year did you last work? [IF NECESSARY: What would be your best estimate?]-MONTH
# 7. "year_retired_HE" ER21126 In what year did you (HEAD) retire? The values for this variable represent the actual year in which Head retired. Actual year 1901 - 2022
# 8. "emp_status_1_HE" ER21123 "BC1 EMPLOYMENT STATUS-1ST MENTION"  We would like to know about what you do--are you (HEAD) working now, looking for
# work, retired, keeping house, a student, or what?--FIRST MENTION
# "emp_status_2_HE" EMPLOYMENT STATUS-2ST MENTION 94.07 inapplicable 
# "emp_status_3_HE" EMPLOYMENT STATUS-3ST 87.56inapplicable  
# "START_M_1_HE" # ER21129 "BC6 BEGINNING MONTH--JOB 1" 
# "START_M_2_HE" "BEGINNING MONTH--JOB 2"
# "START_M_3_HE" "BEGINNING MONTH--JOB 3"
# "START_M_4_HE" "BEGINNING MONTH--JOB 4"
# "START_Y_1_HE" ER21130 "BC6 BEGINNING YEAR--JOB 1" 
# "START_Y_2_HE" "BEGINNING YEAR--JOB 2"
# "START_Y_3_HE" "BEGINNING YEAR--JOB 3"
# "START_Y_4_HE" "BEGINNING YEAR--JOB 4"
# "END_M_1_HE"   "BC6 ENDING MONTH--JOB 1" 
# "END_M_2_HE"   "BC6 ENDING MONTH--JOB 2" 
# "END_M_3_HE"   "BC6 ENDING MONTH--JOB 3" 
# "END_M_4_HE"   "BC6 ENDING MONTH--JOB 4" 
# "END_Y_1_HE"   "BC6 ENDING YEAR--JOB 1"
# "END_Y_2_HE"   "BC6 ENDING YEAR--JOB 2"
# "END_Y_3_HE"   "BC6 ENDING YEAR--JOB 3"
# "END_Y_4_HE"   "BC6 ENDING YEAR--JOB 4"
################################################################################
# List all ZIP files in the PSID family files folder
files <- list.files(path = folder_family_files, pattern = ".zip$")
# Define an output directory for the unzipped RDS family files
outDir <- paste0(folder_family_files , "/new_family_folder_psid")
# Initialize an empty object to hold data across all waves (2003–2021)
all_data_2003_2021<-c()
# Loop over each year_wave in the variable list
for (i in 1:length(needed_variables_2003_2021$year_wave)) {
  # ---- 1. Select variables needed for the current wave ----
  # Filter the master variable list for just this wave,
  # transpose it to make variable names accessible as rows,
  # and remove the wave identifier and any missing entries.
  needed_variables_2003_2021_wave <- needed_variables_2003_2021 %>%
    filter(year_wave == year_wave[i]) %>% # This line may need context for `i`
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "name") %>% filter(name !="year_wave") %>% filter(!is.na(V1))
  
  # ---- 2. Read the family RDS file for this wave ----
  # Each file is named like "FAM2003.rds", "FAM2005.rds", etc
  x = readRDS((paste0(outDir,"/FAM",needed_variables_2003_2021$year_wave[i],".rds"))) %>% 
    select(needed_variables_2003_2021_wave$V1)
  
  x<-select(x, needed_variables_2003_2021_wave$V1)
  # ---- 3. Rename columns ----
  # Replace generic column names (from V1 list) with friendly labels from the “name” column
  names(x)<-c(needed_variables_2003_2021_wave$name)
  
  # ---- 4. Add the year_wave identifier ----
  x$year_wave<-needed_variables_2003_2021$year_wave[i]
  
  # ---- 5. Append the data for this wave to the combined dataset ----
  all_data_2003_2021 <-bind_rows(all_data_2003_2021, x)
  
  # ---- 6. Clean up temporary objects ----
  rm(x, needed_variables_2003_2021_wave)
  
}

################################################################################
# --- Clean and standardize PSID month/year variables ---
# This section identifies all month- and year-type columns (even if some are missing),
# then replaces PSID special codes with real month values or NA.
# - Converts quarter codes (21/22/23/24) to January/April/July/October
# - Sets invalid or missing codes (98, 99, 9999, etc.) to NA
# - Rounds numeric fields and keeps them as integers
# - Applies these fixes to both Head (HE) and Spouse (SP) employment/retirement variables

# helper selectors (safe if some cols are missing)
mon_cols  <- tidyselect::vars_select(names(all_data_2003_2021),
                                     starts_with("START_M"), starts_with("END_M"),
                                     any_of(c("MO_LAST_WORKED_HE","year_retired_HE","MO_LAST_WORKED_SP","year_retired_SP"))
)

yr_cols <- tidyselect::vars_select(names(all_data_2003_2021),
                                   starts_with("START_Y"), starts_with("END_Y"),
                                   any_of(c("YR_LAST_WORKED_HE","YR_LAST_WORKED_SP"))
)

# invalid codes
bad_month_codes <- c(98L, 99L, 0L, 1085L, 9999L, 9998L, 9997L, 9996L, 1083L)
bad_year_codes  <- c(0L, 9996L, 9997L, 9998L, 9999L, 1083L)

all_data_2003_2021 <- all_data_2003_2021 %>%
  mutate(
    # months-like fields: round -> recode 21/22/23/24 -> set bad codes to NA
    across(all_of(mon_cols), ~{
      x <- as.integer(round(.x))
      # PSID special month codes for quarters to month-starts
      x <- dplyr::recode(x, `21` = 1L, `22` = 4L, `23` = 7L, `24` = 10L, .default = x)
      x[ x %in% bad_month_codes ] <- NA_integer_
      x
    }),
    # years-like fields: round -> set bad codes to NA
    across(all_of(yr_cols), ~{
      x <- as.integer(round(.x))
      x[ x %in% bad_year_codes ] <- NA_integer_
      x
    }),
    # HE/SP durations (collapse repetition)
    across(
      any_of(c("years_pres_emp_HE","months_look_for_work_HE",
               "years_pres_emp_SP","months_look_for_work_SP")),
      ~{
        x <- as.integer(round(.x))
        x[ x %in% c(0L, 98L, 99L) ] <- NA_integer_
        x
      }
    )
  )

# # check if encoded ok 
# examine  = all_data_2003_2021 %>%
#   summarise(
#     across(
#       c(starts_with("START_M"), starts_with("END_M"), MO_LAST_WORKED_HE, year_retired_HE,MO_LAST_WORKED_SP, year_retired_SP,
#         starts_with("START_Y"), starts_with("END_Y"), YR_LAST_WORKED_HE,YR_LAST_WORKED_SP,
#         years_pres_emp_HE, months_look_for_work_HE, years_pres_emp_SP, months_look_for_work_SP),
#       list(min = ~min(., na.rm = TRUE), max = ~max(., na.rm = TRUE))
#     )
#   ) %>%
#   pivot_longer(
#     cols = matches("_(min|max)$"),      # only the min/max columns
#     names_to = c("variable", ".value"), # .value -> create 'min' and 'max' cols
#     names_pattern = "^(.*)_(min|max)$"
#   ) %>%
#   arrange(variable)
# 
# write.csv(examine , "external/examine_psid_emp_var.csv")


################################################################################
# --- Create separate datasets for household head, spouse, and others ---

# Extract records where the respondent is the Head (HE)
# Keep only observations from 2003 onward and key identification/interview fields
head <- psid_master %>%
  filter(rel_hsh == "HE", year_wave >= 2003) %>%
  select(pid, year_wave, INTERVIEW_NUMBER, INTERVIEW_DATE)

# Extract records where the respondent is the Spouse (SP)
# Keep only relevant identification and interview variables
spouse <- psid_master %>%
  filter(rel_hsh == "SP", year_wave >= 2003) %>%
  select(pid, year_wave, INTERVIEW_NUMBER, INTERVIEW_DATE)

# Extract records for all "other" household members
# Keep employment and interview information from 2003 onward
other <- psid_master %>%
  filter(rel_hsh == "other", year_wave >= 2003) %>%
  select(pid, year_wave, EMP_STATUS, INTERVIEW_NUMBER, INTERVIEW_DATE)

######################### HEAD #################################################
# --- Create dataset for Heads of Household (HE) ---

data_HE <- all_data_2003_2021 %>%
  # Merge the main family-level data with the 'head' file
  # to attach the correct PID and interview date for each interview number and year
  merge(head, by = c("INTERVIEW_NUMBER", "year_wave")) %>%
  
  # Select only variables relevant for head-level employment and work history
  select(
    pid, INTERVIEW_DATE, INTERVIEW_NUMBER, year_wave,
    
    # Core head employment information
    ever_worked_HE, WORK_SINCE_PRIOR_YEAR_HE,
    
    # Duration variables (cleaned earlier; missing values encoded as NA)
    years_pres_emp_HE, months_look_for_work_HE,
    
    # Last job and retirement timing
    YR_LAST_WORKED_HE, MO_LAST_WORKED_HE, year_retired_HE,
    
    # Employment status codes across up to 3 reported jobs
    paste0("emp_status_", 1:3, "_HE"),
    
    # Job spell start and end month/year fields (up to 4 spells)
    paste0("START_M_", 1:4, "_HE"),
    paste0("START_Y_", 1:4, "_HE"),
    paste0("END_M_",   1:4, "_HE"),
    paste0("END_Y_",   1:4, "_HE")
  )

# --- Reshape Head data (data_HE) from wide to long format ---
data_HE_long <- data_HE %>%
  # Convert multiple job-related columns (emp_status, START/END month/year)
  # from wide format (e.g., emp_status_1_HE, emp_status_2_HE, etc.)
  # into long format — one row per person per job spell.
  pivot_longer(
    cols = matches("emp_status_\\d+_HE|START_Y_\\d+_HE|START_M_\\d+_HE|END_Y_\\d+_HE|END_M_\\d+_HE"),
    names_to = c(".value", "job_num"),       # .value keeps variable base names (emp_status, START_Y, etc.)
    names_pattern = "(.*)_(\\d+)_HE"         # extract variable type and job number from column names
  ) %>%
  
  # Recode numeric employment status codes into descriptive categories
  # so that job status is easier to interpret in analysis
  mutate(
    emp_status_1 = case_when(
      emp_status == 1  ~ "Working",
      emp_status == 2  ~ "Temp. laid off / leave",
      emp_status == 3  ~ "Looking for work (unemployed)",
      emp_status == 4  ~ "Retired",
      emp_status == 5  ~ "Disabled (perm./temp.)",
      emp_status == 6  ~ "Keeping house",
      emp_status == 7  ~ "Student",
      emp_status == 8  ~ "Other / workfare / in prison or jail",
      emp_status %in% c(0, 22, 99) ~ NA_character_,  # handle inapplicable or missing codes
      TRUE ~ NA_character_))
# --- Clean, impute, and standardize Head-of-Household job spell information ---
# This section repairs incomplete or inconsistent employment spell data for household heads.
# It:
#   • Converts variable types to numeric/date formats suitable for computation.
#   • Fills missing START_Y and START_M values using related variables such as
#       - year_retired_HE, YR_LAST_WORKED_HE, MO_LAST_WORKED_HE, and years_pres_emp_HE.
#   • Uses months_look_for_work_HE to backdate unemployment spells when START dates are missing.
#   • Builds valid start_date and end_date fields (first day of each month) from START_/END_ info.
#   • Cleans implausible values (e.g., years outside 1900–2100, months outside 1–12).
#   • Propagates retirement dates up and down within each pid’s timeline to ensure consistency,
#     removes future-dated retirement records, and forces emp_status = "Retired" when applicable.
#   • Fills remaining missing START dates for first observations with the interview date as a fallback.
#   • Finally, collapses detailed employment statuses into five main categories:
#         1 = Working / Temp. Laid Off
#         2 = Unemployed / Looking for Work
#         3 = Not in Labor Force (Disabled, Student, Housework)
#         4 = Retired
#         5 = Other / Inapplicable
# The result is a cleaned long-format dataset (data_HE_long_1) where each head-year record
# has consistent, interpretable start and end dates for employment or non-employment spells.

data_HE_long_1 = data_HE_long %>% 
  mutate(emp_status = as.character(emp_status),
         months_look_for_work_HE = as.numeric(as.character(months_look_for_work_HE))
  ) %>% 
  filter(emp_status!="0") %>% 
  mutate(
    START_Y = ifelse(is.na(START_Y), year_retired_HE,START_Y),
    START_Y = ifelse(is.na(START_Y), YR_LAST_WORKED_HE,START_Y),
    START_M = ifelse(
      is.na(START_Y) & is.na(START_M) & (!is.na(YR_LAST_WORKED_HE) | !is.na(START_Y)),
      MO_LAST_WORKED_HE,
      START_M
    ),
    years_pres_emp = year_wave - years_pres_emp_HE,
    START_Y = ifelse(is.na(START_Y), years_pres_emp_HE,START_Y),
    START_Y = ifelse(is.na(START_Y) & WORK_SINCE_PRIOR_YEAR_HE ==5, year_wave,START_Y),
    START_M = ifelse(is.na(START_M) & WORK_SINCE_PRIOR_YEAR_HE ==5, 1,START_M)
    
  ) %>% select(pid, year_wave, INTERVIEW_DATE, months_look_for_work_HE, 
               emp_status, emp_status_1,START_Y, START_M, END_Y, END_M, job_num, year_retired_HE) %>% 
  mutate(
    INTERVIEW_DATE = as.Date(INTERVIEW_DATE),
    check_1 = INTERVIEW_DATE - period(months = months_look_for_work_HE),
    START_M = ifelse(emp_status =="3" & is.na(START_M), as.integer(format(check_1, "%m")),START_M),
    START_Y = ifelse(emp_status =="3" & is.na(START_Y),  as.integer(format(check_1, "%y")),START_Y),
    START_M = ifelse(is.na(START_M) & !is.na(START_Y) & START_Y<=year_wave-2, 1,START_M)
  ) %>% 
  mutate(
    start_date = if_else(
      !is.na(START_Y) & !is.na(START_M),
      as.Date(sprintf("%04d-%02d-01", START_Y, START_M)),
      as.Date(NA)
    ),
    END_Y = as.integer(round(END_Y)),
    END_M = as.integer(round(END_M)),
    # coerce safely (handles character/labelled)
    END_Y = suppressWarnings(as.integer(END_Y)),
    END_M = suppressWarnings(as.integer(END_M)),
    
    # keep only plausible values
    END_Y = if_else(END_Y >= 1900 & END_Y <= 2100, END_Y, NA_integer_),
    END_M = if_else(END_M >= 1 & END_M <= 12,      END_M, NA_integer_),
    
  ) %>%
  mutate(
    END_M = ifelse(END_M<=9,paste0(0,END_M),as.character(END_M)),
    end_date = case_when(
      !is.na(END_Y) & !is.na(END_M) ~ paste0(END_Y, "-", END_M, "-01"),
      TRUE                          ~ NA
    ) ,
    end_date = as.Date(end_date),
    month_retired_HE = ifelse(!is.na(year_retired_HE), START_M, NA)
    
  ) %>% group_by(pid) %>% arrange(year_wave, START_Y, .by_group = T) %>% 
  fill(year_retired_HE, .direction = "updown") %>% 
  fill(month_retired_HE, .direction = "updown") %>% 
  mutate(
    month_retired_HE = ifelse(year_retired_HE>year_wave, NA, month_retired_HE),
    year_retired_HE = ifelse(year_retired_HE>year_wave, NA, year_retired_HE),
    emp_status = ifelse(!is.na(year_retired_HE),"4",emp_status),
    START_Y = ifelse(!is.na(year_retired_HE),year_retired_HE,START_Y),
    START_M = ifelse(!is.na(month_retired_HE),month_retired_HE,START_M),
    year  = as.numeric(format(as.Date(INTERVIEW_DATE), "%Y")),
    month = as.numeric(format(as.Date(INTERVIEW_DATE), "%m")),
    nr = 1:n(),
    first = nr==1,
    examine  = first==T & is.na(START_Y) & is.na(START_M),
    START_Y =  ifelse(examine==T, year, START_Y),
    START_M =  ifelse(examine==T,month,START_M),
    start_date = case_when(
      is.na(START_Y) | is.na(START_M) ~ NA_Date_,
      !(START_M %in% 1:12)            ~ NA_Date_,  # guard invalid months
      TRUE ~ make_date(year = START_Y, month = START_M, day = 1)
    ),
  ) %>% 
  mutate(
    # 1 working
    # 2 unemployed
    # 3 not in the labor force
    # 4 retired
    # 5 other
    emp_status_1 = case_when(
      emp_status %in% c(1, 2) ~ 1,         # working or temp. laid off
      emp_status == 3 ~ 2,                 # looking for work
      emp_status %in% c(5, 6, 7) ~ 3,      # disabled, keeping house, student
      emp_status == 4 ~ 4,                 # retired
      emp_status %in% c(0, 8, 22, 99) ~ 5, # wild codes or other
      TRUE ~ NA))

################################################################################
################### SPOUSE #####################################################
# --- Create dataset for Spouses/Partners (SP) ---
data_SP <- all_data_2003_2021 %>%
  # Merge the main family-level dataset with the spouse file
  # using interview number and year to attach each spouse’s PID and interview date
  merge(spouse, by = c("INTERVIEW_NUMBER", "year_wave")) %>%
  
  # Select only variables relevant to spouse-level employment, work history, and timing
  select(
    pid, INTERVIEW_DATE, INTERVIEW_NUMBER, year_wave,
    
    # Core work indicators for spouse
    ever_worked_SP, WORK_SINCE_PRIOR_YEAR_SP,
    
    # Duration and job-search information (with missing codes cleaned earlier)
    years_pres_emp_SP, months_look_for_work_SP,
    
    # Last work and retirement details
    YR_LAST_WORKED_SP, MO_LAST_WORKED_SP, year_retired_SP,
    
    # Employment status (up to one primary job reported per wave)
    paste0("emp_status_", 1, "_SP"),
    
    # Job spell start and end month/year information (up to 4 spells)
    paste0("START_M_", 1:4, "_SP"),
    paste0("START_Y_", 1:4, "_SP"),
    paste0("END_M_",   1:4, "_SP"),
    paste0("END_Y_",   1:4, "_SP")
  )

# --- Reshape Spouse data (data_SP) from wide to long format ---
# Converts multiple job-related columns (employment status, start/end year and month)
# from wide format (e.g., emp_status_1_SP, emp_status_2_SP, etc.)
# into long format, producing one observation per spouse per job spell.
# The 'names_pattern' extracts the base variable name (emp_status, START_Y, etc.)
# and the job number (1–4) from the original column names.
# Then, numeric employment status codes are recoded into descriptive text categories:
#   1 = Working
#   2 = Temporarily laid off / on leave
#   3 = Looking for work (unemployed)
#   4 = Retired
#   5 = Disabled (permanent/temporary)
#   6 = Keeping house
#   7 = Student
#   8 = Other / workfare / in prison or jail
#   0, 22, 99 = Missing or inapplicable (set to NA)
# The resulting dataset (data_SP_long) is ready for further cleaning and date construction.

# to long the variable: 
data_SP_long = data_SP %>% 
  pivot_longer(
    cols = matches("emp_status_\\d+_SP|START_Y_\\d+_SP|START_M_\\d+_SP|END_Y_\\d+_SP|END_M_\\d+_SP"),
    names_to = c(".value", "job_num"),
    names_pattern = "(.*)_(\\d+)_SP"
  ) %>%
  mutate(emp_status_1 = case_when(
    emp_status == 1  ~ "Working",
    emp_status == 2  ~ "Temp. laid off / leave",
    emp_status == 3  ~ "Looking for work (unemployed)",
    emp_status == 4  ~ "Retired",
    emp_status == 5  ~ "Disabled (perm./temp.)",
    emp_status == 6  ~ "Keeping house",
    emp_status == 7  ~ "Student",
    emp_status == 8  ~ "Other / workfare / in prison or jail",
    emp_status %in% c(0, 22, 99) ~ NA_character_,
    TRUE ~ NA_character_
  )
  )

# --- Clean, impute, and standardize Spouse/Partner job spell information ---
# This block performs the same repair and harmonization steps as for heads of household,
# but applied to spouse (SP) records.  It:
#   • Converts employment and duration variables to numeric/date formats.
#   • Removes rows coded as inapplicable (emp_status == 0).
#   • Fills missing START_Y and START_M values using available information:
#         - year_retired_SP, YR_LAST_WORKED_SP, MO_LAST_WORKED_SP, years_pres_emp_SP.
#   • Uses months_look_for_work_SP to backdate unemployment spells and infer missing start dates.
#   • Constructs valid start_date and end_date fields (set to the first of each month).
#   • Removes implausible years (outside 1900–2100) and months (outside 1–12).
#   • Propagates retirement information within each pid’s timeline, removes future-dated retirements,
#     and reassigns emp_status = "Retired" when a valid retirement year/month exist.
#   • Fills missing start dates for the first record of each person with interview date as a fallback.
#   • Ensures date fields are valid Date objects and month fields have leading zeros for consistency.
#   • Collapses detailed employment statuses into five summary categories:
#         1 = Working / Temporarily Laid Off
#         2 = Unemployed / Looking for Work
#         3 = Not in Labor Force (Disabled / Student / Housekeeping)
#         4 = Retired
#         5 = Other / Inapplicable
# The result (data_SP_long_1) is a cleaned, long-format dataset for spouses,
# with standardized and interpretable start and end dates for employment or non-employment spells.

data_SP_long_1 = data_SP_long %>% 
  mutate(emp_status = as.character(emp_status),
         months_look_for_work_SP = as.numeric(as.character(months_look_for_work_SP))
  ) %>% 
  filter(emp_status!="0") %>% 
  mutate(
    START_Y = ifelse(is.na(START_Y), year_retired_SP,START_Y),
    START_Y = ifelse(is.na(START_Y), YR_LAST_WORKED_SP,START_Y),
    START_M = ifelse(
      is.na(START_Y) & is.na(START_M) & (!is.na(YR_LAST_WORKED_SP) | !is.na(START_Y)),
      MO_LAST_WORKED_SP,
      START_M
    ),
    years_pres_emp = year_wave - years_pres_emp_SP,
    START_Y = ifelse(is.na(START_Y), years_pres_emp_SP,START_Y),
    START_Y = ifelse(is.na(START_Y) & WORK_SINCE_PRIOR_YEAR_SP ==5, year_wave,START_Y),
    START_M = ifelse(is.na(START_M) & WORK_SINCE_PRIOR_YEAR_SP ==5, 1,START_M)
    
  ) %>% select(pid, year_wave, INTERVIEW_DATE, months_look_for_work_SP, 
               emp_status, emp_status_1,START_Y, START_M, END_Y, END_M, job_num, year_retired_SP) %>% 
  mutate(
    INTERVIEW_DATE = as.Date(INTERVIEW_DATE),
    check_1 = INTERVIEW_DATE - period(months = months_look_for_work_SP),
    START_M = ifelse(emp_status =="3" & is.na(START_M), as.integer(format(check_1, "%m")),START_M),
    START_Y = ifelse(emp_status =="3" & is.na(START_Y),  as.integer(format(check_1, "%y")),START_Y),
    START_M = ifelse(is.na(START_M) & !is.na(START_Y) & START_Y<=year_wave-2, 1,START_M)
  ) %>% 
  mutate(
    start_date = if_else(
      !is.na(START_Y) & !is.na(START_M),
      as.Date(sprintf("%04d-%02d-01", START_Y, START_M)),
      as.Date(NA)
    ),
    END_Y = as.integer(round(END_Y)),
    END_M = as.integer(round(END_M)),
    # coerce safely (handles character/labelled)
    END_Y = suppressWarnings(as.integer(END_Y)),
    END_M = suppressWarnings(as.integer(END_M)),
    
    # keep only plausible values
    END_Y = if_else(END_Y >= 1900 & END_Y <= 2100, END_Y, NA_integer_),
    END_M = if_else(END_M >= 1 & END_M <= 12,      END_M, NA_integer_),
    
  ) %>%
  mutate(
    END_M = ifelse(END_M<=9,paste0(0,END_M),as.character(END_M)),
    end_date = case_when(
      !is.na(END_Y) & !is.na(END_M) ~ paste0(END_Y, "-", END_M, "-01"),
      TRUE                          ~ NA
    ) ,
    end_date = as.Date(end_date),
    month_retired_SP = ifelse(!is.na(year_retired_SP), START_M, NA)
    
  ) %>% group_by(pid) %>% arrange(year_wave, START_Y, .by_group = T) %>% 
  fill(year_retired_SP, .direction = "updown") %>% 
  fill(month_retired_SP, .direction = "updown") %>% 
  mutate(
    month_retired_SP = ifelse(year_retired_SP>year_wave, NA, month_retired_SP),
    year_retired_SP = ifelse(year_retired_SP>year_wave, NA, year_retired_SP),
    emp_status = ifelse(!is.na(year_retired_SP),"4",emp_status),
    START_Y = ifelse(!is.na(year_retired_SP),year_retired_SP,START_Y),
    START_M = ifelse(!is.na(month_retired_SP),month_retired_SP,START_M),
    year  = as.numeric(format(as.Date(INTERVIEW_DATE), "%Y")),
    month = as.numeric(format(as.Date(INTERVIEW_DATE), "%m")),
    nr = 1:n(),
    first = nr==1,
    examine  = first==T & is.na(START_Y) & is.na(START_M),
    START_Y =  ifelse(examine==T, year, START_Y),
    START_M =  ifelse(examine==T,month,START_M),
    start_date = case_when(
      is.na(START_Y) | is.na(START_M) ~ NA_Date_,
      !(START_M %in% 1:12)            ~ NA_Date_,  # guard invalid months
      TRUE ~ make_date(year = START_Y, month = START_M, day = 1)
    ),
  ) %>% 
  mutate(
    # 1 working
    # 2 unemployed
    # 3 not in the labor force
    # 4 retired
    # 5 other
    emp_status_1 = case_when(
      emp_status %in% c(1, 2) ~ 1,         # working or temp. laid off
      emp_status == 3 ~ 2,                 # looking for work
      emp_status %in% c(5, 6, 7) ~ 3,      # disabled, keeping house, student
      emp_status == 4 ~ 4,                 # retired
      emp_status %in% c(0, 8, 22, 99) ~ 5, # wild codes or other
      TRUE ~ NA))

################################################################################
# Combine employemnt biographies for all individuals: head, spouse and other

data_other_long <- other %>% 
  filter(year_wave >= 2003) %>% 
  mutate(start_date = floor_date(ymd(INTERVIEW_DATE), "month")) %>% 
  select(pid, year_wave, EMP_STATUS, start_date) %>% 
  filter(!is.na(EMP_STATUS)) %>% 
  mutate(
    emp_status = ifelse(EMP_STATUS == 1, 1L, 0L)  # or == 2, etc., whatever code means "working"
  ) %>% 
  select(-EMP_STATUS)

data_HE_long_2 = data_HE_long_1 %>% select(pid, year_wave, start_date, emp_status) %>% mutate(emp_status = as.numeric(emp_status))

data_SP_long_2 =  data_SP_long_1 %>% select(pid, year_wave, start_date, emp_status) %>% mutate(emp_status = as.numeric(emp_status))

################################################################################
# --- Extract and reshape PSID employment and interview variables across all survey waves ---
# As indicated in `two_variables_psid_check.R`:
#   • For spouses, ~3.88% of employment-status values are inconsistent between the family and individual files.
#   • For heads, ~4.25% of employment-status values are inconsistent between the family and individual files.
# Therefore, we also pull EMP_STATUS from the individual file and override the family-file value
# in cases of mismatch to ensure consistency.
#
# This section builds a unified long-format dataset linking employment status and interview number
# across all PSID years (1968–2021). It:
#   • Defines survey years (annual 1968–1997; biennial 1999–2021).
#   • Uses psidR::getNamesPSID() with the crosswalk (cwf) to fetch year-specific names for:
#       - EMP_STATUS (ER30293)
#       - INTERVIEW_NUMBER (ER30001)
#   • Reads the 2021 individual file, constructs pid = ER30001*1000 + ER30002, and selects needed columns.
#   • Converts wide ERxxxx columns to long (one row per pid–year_wave).
#   • Repeats for each variable group and merges into a single longitudinal table.
#   • Filters to 2003+ and harmonizes EMP_STATUS into 1=working, 2=unemployed,
#     3=not in labor force, 4=retired, NA=missing/inapplicable.
# The resulting `total_across_waves_1` is ready to be joined with head/spouse/other spell data,
# using individual-file EMP_STATUS to resolve detected inconsistencies.

all_years = c(1968:1997, seq(1999,2021,2))
vars = list(
  EMP_STATUS = psidR::getNamesPSID("ER30293", cwf, years = all_years), # emp_status 
  INTERVIEW_NUMBER =  psidR::getNamesPSID("ER30001", cwf, years =all_years ))

vars_clean <- vars[[1]]
vars_clean_1 <- vars_clean[!is.na(vars_clean$variable), ]

int = readRDS(paste0(outDir,"/ind2021.rds")) %>% 
  mutate(pid  = ER30001*1000 + ER30002) %>% 
  select("pid",vars_clean_1$variable)

total_across_waves = int %>%  pivot_longer(
  cols = starts_with("ER"),
  names_to = "variable",
  values_to = names(vars)[1]
) %>%  merge(vars_clean, by = c("variable"), all.x = T) %>% 
  mutate(year_wave = as.numeric(stringr::str_remove(year, "Y"))) %>% select(-c("year", "variable"))


for (i in 2:length(vars)) {
  
  vars_clean <- vars[[i]]
  vars_clean_1 <- vars_clean[!is.na(vars_clean$variable), ]
  
  x = readRDS(paste0(outDir,"/ind2021.rds")) %>% 
    mutate(pid  = ER30001*1000 + ER30002) %>% 
    select("pid",vars_clean_1$variable)
  
  x_1 = x %>%  pivot_longer(
    cols = starts_with("ER"),
    names_to = "variable",
    values_to = names(vars)[i]
  ) %>%  merge(vars_clean, by = c("variable"), all.x = T) %>% 
    mutate(year_wave = as.numeric(stringr::str_remove(year, "Y"))) %>% select(-c("year", "variable"))
  
  total_across_waves = merge(total_across_waves, x_1, by = c("pid", "year_wave"), all = T)
  
}

total_across_waves_1  = total_across_waves %>% filter( year_wave>=2003) %>% 
  mutate(
    EMP_STATUS = case_when(
      EMP_STATUS %in% c(1,2) ~ 1, # 1 "Working now",2 "Only temporarily laid off",
      EMP_STATUS == 3 ~ 2, # 3 "Looking for work, unemployed",
      EMP_STATUS == 4 ~ 4, # 4 "Retired",
      EMP_STATUS %in% c(5,6,7) ~ 3, # 5 Permanently disabled, 6 HouseWife; keeping house, 7 Student
      EMP_STATUS  %in% c(0,9) ~ NA, # / mover-out nonresponse / NA / DK",
      TRUE ~ NA   # fallback for unexpected or missing codes
    )
  )

# all_types = bind_rows(data_HE_long_2, data_SP_long_2,data_other_long) %>%
#   mutate(
#     # emp_status
#     emp_status = round(emp_status),
#     emp_status = case_when(
#       emp_status %in% c(1, 2) ~ 1,         # working or temp. laid off
#       emp_status == 3 ~ 2,                 # looking for work
#       emp_status %in% c(5, 6, 7) ~ 3,      # disabled, keeping house, student
#       emp_status == 4 ~ 4,                 # retired
#       emp_status %in% c(0, 8, 22, 99) ~ NA, # wild codes or other
#       TRUE ~ NA
#     )) %>%
#   merge(total_across_waves_1, by = c("pid", "year_wave"))
# 
# all_types %>% group_by(emp_status==EMP_STATUS) %>% summarise(n=n())%>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))

# `emp_status == EMP_STATUS`      n  total percent
# 1 FALSE                       15709 178135    8.82
# 2 TRUE                       141058 178135   79.2 
# 3 NA                          21368 178135   12.0 

# The comparison between emp_status (the reconstructed or cleaned employment status) 
# and EMP_STATUS (the official PSID employment status from the individual file) shows 
# that the two measures are highly consistent overall. Approximately 79.2% of 
# all observations match exactly, indicating strong alignment between the processed 
# spell-based employment classification and the official PSID data. Around 8.8% of
# cases show discrepancies, suggesting some differences between the family- and 
# individual-level sources or potential issues in reporting or coding across waves. 
# Finally, about 12% of observations have missing values in one or both variables, 
# making them non-comparable. These findings are consistent with the results noted 
# in two_variables_psid_check.R, where employment-status inconsistencies were 
# identified for roughly 3.9% of spouses and 4.3% of heads. 
# Consequently, the decision to override family-file employment statuses 
# with individual-file data ensures greater accuracy and consistency across waves.
################################################################################
# --- Construct and clean unified employment spells across all household roles ---
#
# Combine & recode:
#   • Stacks the head, spouse, and other long-format datasets into one.
#   • Rounds `emp_status` and recodes raw codes into four broad categories:
#       1 = Working / Temporarily laid off
#       2 = Unemployed / Looking for work
#       3 = Not in labor force (disabled / student / housework)
#       4 = Retired
#       NA = Wild or inapplicable codes.
#
# Join official status & dates:
#   • Merges in official `EMP_STATUS` by pid–year_wave.
#   • Keeps only observations with non-missing official employment status.
#   • Adds the month-floored interview date (`date_1`).
#   • Fills missing `start_date` values with that interview month.
#
# Resolve key inconsistency:
#   • If constructed status = 3 (Not in Labor Force) but official status = 1 (Working),
#     or if constructed status is missing, override `emp_status` with `EMP_STATUS`.
#   • When overridden, reset `start_date` to the interview month so “working” takes precedence.
#
# Build spells within person:
#   • Within each `pid`, sort by `start_date`.
#   • Flag changes in employment status, compute a running `spell` ID.
#   • Collapse to one row per spell (using earliest `start_date` and lowest `emp_status` code).
#
# Set spell ends:
#   • Define `end_date` as the day before the next spell’s `start_date`.
#   • For the final spell per person, fill `end_date` using `last_int$last_date`
#     (the end of the most recent interview month).
#
# Boundary repairs:
#   • If a spell has `end_date - start_date == -1` (an empty or inverted interval),
#     snap `start_date` to the first day of that `end_date` month.
#   • If the next spell’s `start_date` would overlap a prior `end_date`,
#     floor `end_date` to the last day of its month to eliminate overlap.
#
# Net effect:
#   • Produces a person-level sequence of clean, non-overlapping monthly spells.
#   • Ensures consistency with official PSID employment codes, especially for working vs. NILF cases.
#   • Aligns start and end dates with month boundaries and closes each person’s final spell
#     at the last recorded interview month.

all_types = bind_rows(data_HE_long_2, data_SP_long_2,data_other_long) %>% 
  mutate(
    # emp_status
    emp_status = round(emp_status),
    emp_status = case_when(
      emp_status %in% c(1, 2) ~ 1,         # working or temp. laid off
      emp_status == 3 ~ 2,                 # looking for work
      emp_status %in% c(5, 6, 7) ~ 3,      # disabled, keeping house, student
      emp_status == 4 ~ 4,                 # retired
      emp_status %in% c(0, 8, 22, 99) ~ NA, # wild codes or other
      TRUE ~ NA
    )) %>% 
  merge(total_across_waves_1, by = c("pid", "year_wave"), all = T) %>% 
  filter(!is.na(EMP_STATUS)) %>% 
  merge(date_1, by = c("pid", "year_wave")) %>% 
  mutate(start_date  = if_else(is.na(start_date), date, start_date )) %>% 
  mutate(
    # if inconsistent employment status then from individual file 
    check = emp_status %in% c(2,3,4)  & EMP_STATUS ==1,
    emp_status = ifelse(check==T | is.na(emp_status), EMP_STATUS, emp_status),
    start_date  = if_else(check==T, date, start_date )
  ) %>% 
  group_by(pid) %>% arrange(start_date, .by_group = T) %>% 
  mutate(check = emp_status !=lag(emp_status),
         check = ifelse(is.na(check),T,check),
         spell = cumsum(check)
  ) %>% 
  group_by(pid, spell) %>% 
  summarise(start_date = min(start_date, na.rm = T), emp_status = min(emp_status, na.rm = T)) %>% 
  mutate(start_date = if_else(is.infinite(start_date), NA,start_date)) %>% 
  group_by(pid) %>% arrange(spell, .by_group = T) %>% 
  mutate(end_date = lead(start_date)-1) %>% 
  merge(last_int, by = c("pid")) %>% 
  group_by(pid) %>% 
  mutate(
    last = spell==max(spell),
    end_date  = if_else(is.na(end_date ) & last==T, last_date, end_date ))%>% arrange(spell)

all_types = all_types %>% 
  mutate(x = end_date-start_date==-1,
         start_date = if_else(x==T, floor_date(end_date, "month"), start_date),
         x = lead(start_date -lag(end_date)<0),
         x = ifelse(is.na(x),F,x),
         end_date = if_else(x==T,floor_date(end_date, "month") - days(1),end_date)
  )

################################################################################
# --- Check continuity between consecutive employment spells ---
# This code evaluates whether there are gaps or perfect continuity between spells within each person (pid).
# It:
#   • Orders all spells chronologically within each individual.
#   • Calculates the gap (`x`) as the difference between each spell’s `start_date`
#     and the previous spell’s `end_date`.
#   • Counts how many cases have a 1-day gap (meaning spells connect seamlessly)
#     versus cases with missing or undefined gaps (often for the first spell or incomplete data).
#
# Results interpretation:
#   • 48.7% of spells begin exactly 1 day after the previous spell ended — continuous, gapless timelines.
#   • 51.3% are NA, representing first spells per person or cases without a valid prior end date.
# This confirms that nearly half of all employment spell transitions align perfectly without temporal gaps.
all_types %>% group_by(pid) %>% arrange(spell, .by_group = T) %>% 
  mutate(x = start_date -lag(end_date)) %>% group_by(x)%>% summarise(n=n()) %>% 
  ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))

# x           n total percent
# <drtn>  <int> <int>   <dbl>
# 1  1 days 29699 60944    48.7
# 2 NA days 31245 60944    51.3

# --- Validate chronological consistency of spell dates ---
# This check identifies any employment spells where `end_date` occurs *before* `start_date`,
# which would indicate a data or sequencing error.
# It:
#   • Orders spells by person (pid) and sequence (`spell`).
#   • Creates a logical flag `x` equal to TRUE if `end_date - start_date < 0`
#     (i.e., the spell ends before it begins).
#   • Counts and computes the share of such cases.
#
# Results interpretation:
#   • 99.5% of spells have correctly ordered dates (end_date ≥ start_date).
#   • 0.2% (131 cases) have reversed dates — likely due to data inconsistencies or merging artifacts.
#   • 0.3% have missing values (NA), typically for incomplete spells or missing date components.
# This confirms that the vast majority of spells are chronologically valid, 
# with only a very small fraction requiring correction or review.

all_types %>% group_by(pid) %>% arrange(spell, .by_group = T) %>% 
  mutate(x = end_date-start_date<0) %>% group_by(x)%>% summarise(n=n()) %>% 
  ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))
# x         n total percent
# <lgl> <int> <int>   <dbl>
# 1 FALSE 60611 60944  99.5  
# 2 TRUE    131 60944   0.215
# 3 NA      202 60944   0.331

all_types = all_types %>%  mutate(
  employment_status = case_when(
    emp_status ==1 ~ "Working",
    emp_status ==2 ~ "Unemployed",
    emp_status ==3 ~ "Not in the Labor Force",
    emp_status ==4 ~ "Retired",
    emp_status ==5 ~ "Other",
    TRUE ~ NA
    
  )
)

# building psid_employment_2003_2021
# "pid", "nr_emp_spell", "employment_status", "start_date", "end_date" 

psid_employment_2003_2021 = all_types %>% 
  rename(nr_emp_spell = spell)

################################################################################

# two wide 
# 1. recalculate spells 

emp_head_1968_1976$period = "1968_1976"
psid_employment_1976_1987$period ="1976_1987"
psid_employment_1988_2001$period ="1988_2001"
psid_employment_2003_2021$period = "2003_2021"

#################################################
# check overlap 1988_2001 decade  

df = psid_employment_1988_2001 %>% 
  mutate(
    START_Y = year(start_date),
    END_Y = year(end_date)
  )

# to wide 
dt_1_wide = 
  df %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = nr_emp_spell,
    values_from = c("START_Y", "END_Y"),
    names_glue = "{.value}_{nr_emp_spell}"
  )


max_1 = max(df$nr_emp_spell)

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  ## 1) Start of t+1 before end of t
  print(sum(dt_1_wide[[paste0("START_Y_", j)]]<dt_1_wide[[paste0("END_Y_", i)]], na.rm = TRUE))
  
}
# to wide 
dt_1_wide = 
  psid_employment_1988_2001 %>% 
  mutate(
    START_Y = year(start_date),
    END_Y = year(end_date)) %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = nr_emp_spell,
    values_from = c("START_Y", "END_Y"),
    names_glue = "{.value}_{nr_emp_spell}"
  )

max_1 = max(psid_employment_1988_2001$nr_emp_spell)
data_check_1 = c()
data_check_2 = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  ## 1) Start of t+1 before end of t

  
  x = dt_1_wide %>% 
    summarise(partial = sum(.data[[paste0("START_Y_", j)]] < 
                        .data[[paste0("END_Y_", i)]], na.rm = TRUE)) 
  
  
  y = dt_1_wide %>% 
    summarise(
      complete = sum(
        .data[[paste0("START_Y_", j)]] < .data[[paste0("END_Y_", i)]] &
          .data[[paste0("END_Y_",   j)]] < .data[[paste0("END_Y_", i)]],
        na.rm = TRUE
      )
    )
  y = y %>% mutate(nr_emp_spell = i) 
  
  x = x %>% mutate(nr_emp_spell = i) 
  
  data_check_1 = bind_rows(data_check_1, x)
  
  data_check_2 = bind_rows(data_check_2, y)
  
}

psid_employment_1988_2001 = data_check_1 %>% 
  merge(psid_employment_1988_2001, by = c("pid", "nr_emp_spell"))

psid_employment_1988_2001 = psid_employment_1988_2001 %>% group_by(pid) %>% 
  mutate(
    end_date = if_else(
      partial == 1,
      lead(start_date) -1,
      end_date
    )
  )

dt_2_wide = 
  psid_employment_1988_2001 %>% 
  mutate(
    START_Y = year(start_date),
    END_Y = year(end_date)) %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = nr_emp_spell,
    values_from = c("START_Y", "END_Y"),
    names_glue = "{.value}_{nr_emp_spell}"
  )


################################################################################
emp_head_1968_1976$period = "1968_1976"
psid_employment_1976_1987$period ="1976_1987"
psid_employment_1988_2001$period ="1988_2001"
psid_employment_1988_2001$period ="1988_2001"
psid_employment_2003_2021$period = "2003_2021"

psid_employment_1988_2001$end_date<-as.Date(psid_employment_1988_2001$end_date)

psid_employment = bind_rows(emp_head_1968_1976, psid_employment_1976_1987,psid_employment_1988_2001, psid_employment_2003_2021) %>% 
  group_by(pid) %>%
  # 1. recalculate spells 
  mutate(eq1 = employment_status !=lag(employment_status),
         eq2 = period !=lag(period),
         eq3 = start_date !=lag(start_date),
         check = ifelse(eq1 ==T | eq2==T | eq3==T, T,F),
         check = ifelse(is.na(check),T,check),
         nr_emp_spell_1 = cumsum(check)
         )


psid_employment = psid_employment %>% select(pid, nr_emp_spell_1, employment_status,start_date, end_date,period) %>% 
  mutate(start_date = as.character(start_date),
         end_date  = as.character(end_date))

psid_employment[c("ENTRY_Y","ENTRY_M","ENTRY_D")] <- str_split_fixed(psid_employment$start_date, '-', 3)
psid_employment[c("EXIT_Y","EXIT_M","EXIT_D")] <- str_split_fixed(psid_employment$end_date, '-', 3)

psid_employment = psid_employment %>% 
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

################################################################################
psid_employment = psid_employment[psid_employment$pid !="5055002", ]
dt_1_wide = 
  psid_employment %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = nr_emp_spell_1,
    values_from = c("ENTRY_Y", "EXIT_Y"),
    names_glue = "{.value}_{nr_emp_spell_1}"
  )


max_1 = max(psid_employment$nr_emp_spell_1)
data_check_1 = c()
data_check_2 = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  ## 1) Start of t+1 before end of t
  
  
  x = dt_1_wide %>% 
    summarise(partial = sum(.data[[paste0("ENTRY_Y_", j)]] < 
                              .data[[paste0("EXIT_Y_", i)]], na.rm = TRUE)) 
  
  
  y = dt_1_wide %>% 
    summarise(
      complete = sum(
        .data[[paste0("ENTRY_Y_", j)]] < .data[[paste0("EXIT_Y_", i)]] &
          .data[[paste0("EXIT_Y_",   j)]] > .data[[paste0("EXIT_Y_", i)]],
        na.rm = TRUE
      )
    )
  y = y %>% mutate(nr_emp_spell_1 = i) 
  
  x = x %>% mutate(nr_emp_spell_1 = i) 
  
  data_check_1 = bind_rows(data_check_1, x)
  
  data_check_2 = bind_rows(data_check_2, y)
  
}


psid_employment_1 = data_check_1 %>% merge(data_check_2, c("pid", "nr_emp_spell_1")) %>% 
  merge(psid_employment, by = c("pid", "nr_emp_spell_1")) %>% group_by(pid) %>% 
  arrange(nr_emp_spell_1, .by_group = T) %>% 
  mutate(
   check = ifelse(complete==1 & lead(employment_status)==employment_status, "drop", "keep")
  ) %>% 
  mutate(start_date = as.character(start_date),
       end_date  = as.character(end_date)) %>% 
  # first deal with complete overlap
  filter( check == "keep") %>% 
  mutate(
  start_date = as.Date(start_date),
  end_date = as.Date(end_date),
  start_date  = if_else(lag(partial)!=1 | is.na(lag(partial)),start_date,lag(end_date)+1)
  ) %>% group_by(pid) %>% arrange(nr_emp_spell_1, .by_group = T) %>% 
# Recalculate the spells 
mutate(eq1 = employment_status !=lag(employment_status),
       # eq2 = period !=lag(period),
       eq3 = start_date !=lag(start_date),
       check = ifelse(eq1 ==T | eq3==T, T,F),
       check = ifelse(is.na(check),T,check),
       nr_emp_spell_1 = cumsum(check)
)


# recalculate spells number per individual again 

psid_employment_1[c("ENTRY_Y","ENTRY_M","ENTRY_D")] <- str_split_fixed(psid_employment_1$start_date, '-', 3)
psid_employment_1[c("EXIT_Y","EXIT_M","EXIT_D")] <- str_split_fixed(psid_employment_1$end_date, '-', 3)

#### here as integer pls "ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M","EXIT_D"
#### check start date > 9991 (earlier periods)  and start_date <1800

psid_employment_1=  psid_employment_1 [ psid_employment_1 $pid!="1308001",]


# check overlap once again 

dt_1_wide = 
  psid_employment_1 %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = nr_emp_spell_1,
    values_from = c("start_date", "end_date"),
    names_glue = "{.value}_{nr_emp_spell_1}"
  )


max_1 = max(psid_employment_1$nr_emp_spell_1)
data_check_1_1 = c()
data_check_2_2 = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  ## 1) Start of t+1 before end of t
  
  
  x = dt_1_wide %>% 
    summarise(partial = sum(.data[[paste0("start_date_", j)]] < 
                              .data[[paste0("end_date_", i)]], na.rm = TRUE)) 
  
  
  y = dt_1_wide %>% 
    summarise(
      complete = sum(
        .data[[paste0("start_date_", j)]] < .data[[paste0("end_date_", i)]] &
          .data[[paste0("end_date_",   j)]] > .data[[paste0("end_date_", i)]],
        na.rm = TRUE
      )
    )
  y = y %>% mutate(nr_emp_spell_1 = i) 
  
  x = x %>% mutate(nr_emp_spell_1 = i) 
  
  data_check_1_1 = bind_rows(data_check_1_1, x)
  
  data_check_2_2 = bind_rows(data_check_2_2, y)
  
}


################################################################################
# to wide format 

psid_employment_wide = psid_employment_1 %>% 
  pivot_wider(
    id_cols = c(pid),
    names_from = nr_emp_spell_1 ,
    values_from = c("EMPLOYMENT_STATUS","ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M", "EXIT_D", "period"),
    names_glue = "{.value}_{nr_emp_spell_1}"
  )

x = length(select(psid_employment_wide, starts_with("ENTRY_Y_")) %>% names())-1

################################################################################
for (i in 1:x) {
  var_status <- paste0("EMPLOYMENT_STATUS_", i)
  var_entry_y <- paste0("ENTRY_Y_", i)
  var_entry_m <- paste0("ENTRY_M_", i)
  var_entry_d <- paste0("ENTRY_D_", i)
  var_exit_y <- paste0("EXIT_Y_", i)
  var_exit_m <- paste0("EXIT_M_", i)
  var_exit_d <- paste0("EXIT_D_", i)
  
  if (var_status %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_status]]) <- paste("Employment status during spell", i)
  }
  if (var_entry_y %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_entry_y]]) <- paste("Year entry for spell", i)
  }
  if (var_entry_m %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_entry_m]]) <- paste("Month entry for spell", i)
  }
  if (var_entry_d %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_entry_d]]) <- paste("Day entry for spell", i)
  }
  
  if (var_exit_y %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_exit_y]]) <- paste("Year exit for spell", i)
  }
  if (var_exit_m %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_exit_m]]) <- paste("Month exit for spell", i)
  }
  if (var_exit_d  %in% names(psid_employment_wide)) {
    var_label(psid_employment_wide[[var_exit_d ]]) <- paste("Day exit for spell", i)
  }
}

# order variable
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
      paste0("EXIT_D_",i),
      paste0("period_",i)
    )
    
    lista_order_20<- append(lista_order_20, x)
  }
  return(lista_order_20)
}

employment_biography_psid_wide<-psid_employment_wide[,c("pid",ordering_variables_wide_format(x))]

saveRDS(employment_biography_psid_wide , "output/psid_employment.rds")


if (type_you_want==".csv") {
  write.csv(employment_biography_psid_wide, "output/psid_employment.csv")
}else if(type_you_want==".dta"){
  write_dta(employment_biography_psid_wide, "output/psid_employment.dta")
  
}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_psid_1", "folder_retro_psid_3",
                "folder_psid_1","folder_psid_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)











