# 1. Download necessary packages and Establish necessary directories

source("00_setting_work_space.R")
`%notin%` <- Negate(`%in%`)

z_score <- 1.96
fontname<-"Calibri"
windowsFonts(A = windowsFont(fontname))

################################################################################
# Source of External data on employment taken from:
# OECD. (2025). Labour force statistics by sex and age – employment rates. OECD.Stat. Retrieved February 18, 2025, from https://stats.oecd.org/

external_emp_f  = readxl::read_excel("external/OECD.SDD.TPS,DSD_LFS@DF_IALFS_EMP_WAP_Q,1.0,filtered,2025-02-18 12-28-37.xlsx", sheet = "female") %>% 
  pivot_longer(
    cols = starts_with("20"),  # Select all year columns
    names_to = "time",         # New column for years
    values_to = "obs_value"        # New column for values
  ) %>% 
  filter(time %in% c(2005:2019)) %>%
  mutate(
    time = as.numeric(time),
    Wave = cut(time, 4)) %>%
  group_by(Country,Wave) %>%
  summarise(external = mean(obs_value)) %>%
  mutate(country = case_when(
    Country == "Australia" ~ "hilda",
    Country == "United Kingdom" ~ "bhps_ukhls",
    Country == "Germany" ~ "soep",
    Country ==  "United States" ~"psid",
    Country == "Switzerland" ~ "shp"
  )) %>% mutate(
    sex.label ="Sex: Female",
    SEX = ifelse(sex.label =="Sex: Female",2,1)
  )
external_emp_m  = readxl::read_excel("external/OECD.SDD.TPS,DSD_LFS@DF_IALFS_EMP_WAP_Q,1.0,filtered,2025-02-18 12-28-37.xlsx", sheet = "male") %>% 
  pivot_longer(
    cols = starts_with("20"),  # Select all year columns
    names_to = "time",         # New column for years
    values_to = "obs_value"        # New column for values
  ) %>% 
  filter(time %in% c(2005:2019)) %>%
  mutate(
    time = as.numeric(time),
    Wave = cut(time, 4)) %>%
  group_by(Country,Wave) %>%
  summarise(external = mean(obs_value)) %>%
  mutate(country = case_when(
    Country == "Australia" ~ "hilda",
    Country == "United Kingdom" ~ "bhps_ukhls",
    Country == "Germany" ~ "soep",
    Country ==  "United States" ~"psid",
    Country == "Switzerland" ~ "shp"
  )) %>% mutate(
    sex.label ="Sex: Male",
    SEX = ifelse(sex.label =="Sex: Male",1,2)
  )

external_emp = bind_rows(external_emp_m, external_emp_f) %>% select(-sex.label)

################################################################################
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

# Prepare object to iterate over 
# Available countries: "hilda", "soep", "bhps_ukhls", "shp", "psid"
country_set = c("psid", "bhps_ukhls","hilda", "shp","soep")

to_download = data.frame(Var1 = country_set) %>% 
  mutate(
    id = case_when(
      Var1 == "psid"  ~ "pid",
      Var1 == "shp"   ~ "idpers",
      Var1 == "hilda" ~ "xwaveid",
      Var1 == "soep" ~ "pid",
      Var1 == "bhps_ukhls" ~ "pidp")
  )

################################################################################
# Transform each dataset_name_employment.rds data from wide format to long format

for (i in 1:nrow(to_download)) {
  
  # A. Prepare data for calcualtion of LIB employemnt rate 
  # 0. Download emplyemnt biography in wide format 
  data_employment = readRDS(paste0("output/",to_download[i,"Var1"],"_employment.rds"))
  
  if (to_download[i,"Var1"] == "hilda") {
    # Step 1: Create long data frames for each variable
    data_employment_long <- pivot_clean(data_employment, "EMPLOYMENT_STATUS", "EMPLOYMENT_STATUS", pid = to_download[i,"id"])
    data_start_year_long <- pivot_clean(data_employment, "ENTRY_Y", "ENTRY_Y", pid = to_download[i,"id"])
    data_start_month_long <- pivot_clean(data_employment, "ENTRY_M", "ENTRY_M",pid = to_download[i,"id"])
    data_start_day_long <- pivot_clean(data_employment, "ENTRY_D", "ENTRY_D",pid = to_download[i,"id"])
    data_end_year_long <- pivot_clean(data_employment, "EXIT_Y", "EXIT_Y",pid = to_download[i,"id"])
    data_end_month_long <- pivot_clean(data_employment, "EXIT_M", "EXIT_M",pid = to_download[i,"id"])
    data_end_day_long <- pivot_clean(data_employment, "EXIT_D", "EXIT_D",pid = to_download[i,"id"])
    
    # Step 2: Merge the long data frames
    data_long <- data_employment_long %>%
      inner_join(data_start_year_long, by = c(to_download[i,"id"], "spell")) %>%
      inner_join(data_start_month_long, by = c(to_download[i,"id"], "spell"))%>%
      inner_join(data_end_year_long, by = c(to_download[i,"id"], "spell"))%>%
      inner_join(data_end_month_long, by = c(to_download[i,"id"], "spell")) %>% 
      inner_join(data_start_day_long, by = c(to_download[i,"id"], "spell")) %>% 
      inner_join(data_end_day_long, by = c(to_download[i,"id"], "spell")) %>% 
      mutate(
        start_date =  as.Date(paste(ENTRY_Y, ENTRY_M, ENTRY_D, sep = "-")),
        end_date =  as.Date(paste(EXIT_Y, EXIT_M, EXIT_D, sep = "-"))
      ) %>% 
      select(to_download[i,"id"], spell, EMPLOYMENT_STATUS, start_date,end_date)
    
    rm(data_employment, data_employment_long, data_start_year_long,data_start_month_long,data_end_year_long, data_end_month_long,
       data_start_day_long, data_end_day_long
       )
  } else if (to_download[i,"Var1"] %in%  c("soep", "bhps_ukhls", "psid", "shp")){
    # Step 1: Create long data frames for each variable
    data_employment_long <- pivot_clean(data_employment, "EMPLOYMENT_STATUS", "EMPLOYMENT_STATUS", pid = to_download[i,"id"])
    data_start_year_long <- pivot_clean(data_employment, "ENTRY_Y", "ENTRY_Y", pid = to_download[i,"id"])
    data_start_month_long <- pivot_clean(data_employment, "ENTRY_M", "ENTRY_M",pid = to_download[i,"id"])
    data_end_year_long <- pivot_clean(data_employment, "EXIT_Y", "EXIT_Y",pid = to_download[i,"id"])
    data_end_month_long <- pivot_clean(data_employment, "EXIT_M", "EXIT_M",pid = to_download[i,"id"])
    
    # Step 2: Merge the long data frames
    data_long <- data_employment_long %>%
      inner_join(data_start_year_long, by = c(to_download[i,"id"], "spell")) %>%
      inner_join(data_start_month_long, by = c(to_download[i,"id"], "spell"))%>%
      inner_join(data_end_year_long, by = c(to_download[i,"id"], "spell"))%>%
      inner_join(data_end_month_long, by = c(to_download[i,"id"], "spell")) %>% 
      filter(ENTRY_Y>0, EXIT_Y >0) %>% 
      mutate(
        ENTRY_Y = as.integer(ENTRY_Y),
        ENTRY_M = as.integer(ENTRY_M),
        EXIT_Y  = as.integer(EXIT_Y),
        EXIT_M  = as.integer(EXIT_M),
        start_date =  as.Date(paste(ENTRY_Y, ENTRY_M, "01", sep = "-")),
        end_date =  as.Date(paste(EXIT_Y, EXIT_M, "01", sep = "-"))
      ) %>% 
      select(to_download[i,"id"], spell, EMPLOYMENT_STATUS, start_date,end_date)

    rm(data_employment, data_employment_long, data_start_year_long,data_start_month_long,data_end_year_long, data_end_month_long)
    
  }
  # 3. expand spells so from one row one spell to one row one month
  data_long_1 <- data_long %>%
    filter(start_date <= end_date)

  data_long_expanded <- neatRanges::expand_dates(
      data_long_1,
      start_var = "start_date",
      end_var = "end_date",
      vars_to_keep = c(to_download[i,"id"],"EMPLOYMENT_STATUS", "start_date", "end_date"),
      unit = "month")
  

  # 4. Add interview date, interview status, SEX and BORN_Y to long format employment
  # We do it to select one month per wave  - month of interview 
  
  # transform from wide to long interview_date and interview_status
  master = readRDS(paste0("output/master_",to_download[i,"Var1"],"_wide.rds"))
  # add interview date and interview status to measure employment rate in LIB at the time of interview only
  
  interview_date = pivot_clean(master,  pid = to_download[i,"id"], "INTERVIEW_DATE", "interview_date")
    
  if (to_download[i,"Var1"] == "hilda") {
    interview = interview_date %>% 
      mutate(Expanded = as.Date(interview_date),Wave = spell) %>% select(-spell)
  }else{
    interview = interview_date %>% 
      mutate(Expanded = floor_date(as.Date(interview_date), unit = "month"), 
             Wave = spell) %>% select(-spell)
  }
  
  data_long_expanded$Expanded = as.Date(data_long_expanded$Expanded)

  data_long_lib = 
    # merge interview_status and interview_date with long employment biography 
    merge(as.data.table(data_long_expanded), 
                        as.data.table(interview), by = c(to_download[i,"id"], "Expanded")) %>% 
    # merge SEX and BORN_Y with long employment biography 
    merge(readRDS(paste0("output/master_",to_download[i,"Var1"],"_wide.rds")) %>% select(to_download[i,"id"], SEX, BORN_Y), 
          by = c(to_download[i,"id"])) 
  
 
  data_long_lib =  data_long_lib %>% 
      select(to_download[i,"id"], Expanded,  Wave, SEX, BORN_Y, EMPLOYMENT_STATUS)

    
  # save results 
  assign(x = paste0("data_long_lib_",to_download[i,"Var1"]), value = data_long_lib)
  
  ##############################################################################
  # B. prepare employment status from date of interview for internal validation 
  # this is our dataset to calucalte internal validation 
  
  interview_emp_status = pivot_clean(master, pid = to_download[i,"id"],"EMP_STATUS", "emp_status") %>% 
      rename(Wave = spell, employment_status = emp_status)

  interview_emp_status = merge(interview_emp_status, readRDS(paste0("output/master_",to_download[i,"Var1"],"_wide.rds")) %>% 
                                 select(to_download[i,"id"], SEX, BORN_Y), 
                               by = c(to_download[i,"id"]))
  
  assign(x = paste0("interview_emp_status_",to_download[i,"Var1"]), value = interview_emp_status)
  
  # remove intermediary objects 
  rm(interview_emp_status, master, data_long_expanded,data_long_1, data_long,data_long_lib, interview_date, interview)
  
  print(to_download[i,"Var1"])
  
}

################################################################################
# in some databases we need to exclude individuals who are over-represented 
# In SOEP these are mainly migrants 
# In bhps_ukhls these are migrants but also people from Ireland and Scotland 

interview_emp_status_soep = interview_emp_status_soep %>% 
  merge(select(readRDS("output/master_soep_wide.rds"), pid, psample), by = c("pid"), all.x = T) %>%  
  filter(psample %in% c(
                      1,  # [1] A 1984 Initial Sample (West)	
                      2,	# [2] B 1984 Migration (until 1983, West)	
                      3,	# [3] C 1990 Initial Sample (East)	
                      4,	# [4] D 1994/5 Migration (1984-1994, West)	
                      5,	# [5] E 1998 Refreshment	
                      6,	# [6] F 2000 Refreshment	
                      8,	# [8] H 2006 Refreshment	
                      10,	# [10] J 2011 Refreshment	
                      11,	# [11] K 2012 Refreshment
                      27	# [27] R 2022	
                      ))

interview_emp_status_bhps_ukhls = interview_emp_status_bhps_ukhls %>% 
  merge(select(readRDS("output/master_bhps_ukhls_wide.rds"), pidp, memorig), by = c("pidp"), all.x = T) %>%
  # We excluded ethnic minority boost
  # filter(memorig %notin% c(7,8))
  filter(memorig %in% c(1, # ukhls gb 2009-10
                        2, # ukhls gb 2009-10
                        3, # bhps gb 1991
                        4, # bhps sco 1999
                        5, # bhps wal 1999
                        6  # bhps ni 2001
                        ))

data_long_lib_soep = data_long_lib_soep %>% 
  merge(select(readRDS("output/master_soep_wide.rds"), pid, psample), by = c("pid"), all.x = T) %>%  
  filter(psample %in% c(1,  # [1] A 1984 Initial Sample (West)	
                        2,	# [2] B 1984 Migration (until 1983, West)	
                        3,	# [3] C 1990 Initial Sample (East)	
                        4,	# [4] D 1994/5 Migration (1984-1994, West)	
                        5,	# [5] E 1998 Refreshment	
                        6,	# [6] F 2000 Refreshment	
                        8,	# [8] H 2006 Refreshment	
                        10,	# [10] J 2011 Refreshment	
                        11,	# [11] K 2012 Refreshment
                        27	# [27] R 2022	
  ))

data_long_lib_bhps_ukhls = data_long_lib_bhps_ukhls %>% 
  merge(select(readRDS("output/master_bhps_ukhls_wide.rds"), pidp, memorig), by = c("pidp"), all.x = T) %>%
  # We excluded ethnic minority boost
  filter(memorig %in% c(1,2,3,4,5,6))
  # filter(memorig %notin% c(7,8))

################################################################################
# prepare date of interview data set to measure employment rate only at the time of interview 

for (i in 1:nrow(to_download)) {
  
 x =  readRDS(paste0("output/master_",to_download$Var1[i],"_wide.rds")) %>%
    select(to_download$id[i], starts_with("interview_date")) %>% 
    pivot_longer(
      cols = starts_with("interview_date"),
      names_to = "Wave",
      values_to = "interview_date"
    ) %>% mutate(int_year = substr(interview_date, 1, 4)) %>% 
    mutate(Wave = str_remove(Wave, "interview_date_")) %>% 
    select(to_download$id[i], Wave, int_year)
 
 assign(x = paste0(to_download$Var1[i], "_interview_date"), value = x)
 
}

################################################################################
# Calculate employment rates for LIB and for internal variable in each dataset

for (i in 1:nrow(to_download)) {
  
  ################ A. internal employment rate ##################################
  # extract external employment for a given country form ealier prepared OECD dataset
  external_emp_x = external_emp %>% filter(country == to_download[i,"Var1"])
  
  ################ B. internal employment rate ##################################
  
  emp_pop_ratio_internal <- get(paste0("interview_emp_status_",to_download[i,"Var1"]))
  

  if (to_download[i,"Var1"] %in%  c("soep")) {
  emp_pop_ratio_internal = emp_pop_ratio_internal %>%
    filter(psample %in% c(
           1, # [1] A 1984 Initial Sample (West)
           2,	# [2] B 1984 Migration (until 1983, West)
           3,	# [3] C 1990 Initial Sample (East)
           4,	# [4] D 1994/5 Migration (1984-1994, West)
           5,	# [5] E 1998 Refreshment
           6,	# [6] F 2000 Refreshment
           8,	# [8] H 2006 Refreshment
           10,# [10] J 2011 Refreshment
           11,# [11] K 2012 Refreshment
           27	# [27] R 2022
  ))

  }

  if (to_download[i,"Var1"] %in%  c("bhps_ukhls")) {
    emp_pop_ratio_internal = emp_pop_ratio_internal %>%
      # filter(memorig %notin% c(7,8))
      filter(memorig %in% c(1,2,3,4,5,6))
  }
  
  int_date = get(paste0(to_download$Var1[i], "_interview_date")) %>% mutate(Wave = stringr::str_remove( Wave, "INTERVIEW_DATE_"))
  
  if (to_download[i,"Var1"] %in% c("shp", "soep")) {
    emp_pop_ratio_internal = emp_pop_ratio_internal %>% mutate(Wave = stringr::str_remove( Wave, "emp_status_") )
    
  }
  
  emp_pop_ratio_internal %>% head()
  
  emp_pop_ratio_internal <- emp_pop_ratio_internal %>%
    merge(int_date, by = c(to_download[i,"id"], "Wave")) %>% select(-Wave) %>% 
    mutate(Wave = as.numeric(int_year),
           age = Wave - BORN_Y) %>%
    # Age: From 15 to 64 years
    filter(age>15,age<64) %>%
    # only from 2005 to 2019
    filter(Wave %in% c(2005:2019)) %>%
    mutate(Wave = cut(Wave, 4)) %>%
    # if no employment status (non responding individual) we filter them out
    # People who did not complete the individual interview
    # People who were not contactable, refused, or dropped out
    # Children under 15, who are not eligible for the individual interview
    # Proxy respondents (someone answered on their behalf)
    filter(!is.na(employment_status)) %>% 
    filter(employment_status!=-1) %>% 
    # processes employment data by Wave, SEX, and employment status to:
    # 1. Count observations
    # 2. Compute percentages
    # 3. Estimate confidence intervals for those percentages
    group_by(Wave, SEX, employment_status) %>%
    summarise(n_internal = n()) %>%
    ungroup() %>%
    group_by(Wave,SEX) %>%
    mutate(
      total_internal = sum(n_internal),
      internal = 100 * (n_internal / total_internal),
      # Calculate the confidence interval for the percentage
      internal_lower = 100 * ((n_internal / total_internal) - z_score * sqrt((n_internal / total_internal) * (1 - (n_internal / total_internal)) / total_internal)),
      internal_upper = 100 * ((n_internal / total_internal) + z_score * sqrt((n_internal / total_internal) * (1 - (n_internal / total_internal)) / total_internal))
    )
  
  emp_pop_ratio_internal
  
  emp_pop_ratio_internal = emp_pop_ratio_internal %>% filter(employment_status == 1)
  


  ################# C. LIB employment rate #####################################
  emp_pop_ratio_lib <- get(paste0("data_long_lib_",to_download[i,"Var1"]))

  emp_pop_ratio_lib = emp_pop_ratio_lib %>% mutate(Wave = stringr::str_remove(Wave,"interview_date_"))
  
  emp_pop_ratio_lib <- emp_pop_ratio_lib %>%
        mutate(employment_status = EMPLOYMENT_STATUS)
   
  emp_pop_ratio_lib <- emp_pop_ratio_lib %>% 
    merge(int_date, by = c(to_download[i,"id"], "Wave")) %>% select(-Wave) %>% 
    mutate(Wave = as.numeric(int_year),
           age = Wave - BORN_Y) %>%
    filter(age>15,age<64) %>%
    filter(Wave %in% c(2005:2019)) %>%
    mutate(Wave = cut(Wave, 4)) %>%
    # if no employment status (non responding individual) we filter them out
    # People who did not complete the individual interview
    # People who were not contactable, refused, or dropped out
    # Children under 15, who are not eligible for the individual interview
    # Proxy respondents (someone answered on their behalf)
    filter(!is.na(employment_status)) %>% 
    # processes employment data by Wave, SEX, and employment status to:
    # 1. Count observations
    # 2. Compute percentages
    # 3. Estimate confidence intervals for those percentages
    group_by(Wave, SEX,employment_status) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(Wave,SEX) %>%
    mutate(
      total = sum(n),
      UNWEIHGTED_lib = 100 * (n / total),
      # Calculate the confidence interval for the percentage
      UNWEIHGTED_lower = 100 * ((n / total) - z_score * sqrt((n / total) * (1 - (n / total)) / total)),
      UNWEIHGTED_upper = 100 * ((n / total) + z_score * sqrt((n / total) * (1 - (n / total)) / total))
    ) 
  
    emp_pop_ratio_lib <- emp_pop_ratio_lib %>% filter(employment_status == 1)

  # combine all three necessary information for the graph: internal, lib and external
  for_graph = merge(emp_pop_ratio_lib,emp_pop_ratio_internal, by = c("Wave", "SEX")) %>% merge(external_emp_x, by = c("Wave", "SEX"))
  
  assign(x = paste0("for_graph_",to_download[i,"Var1"]), value = for_graph)
  
  print(to_download[i,"Var1"])
  
}

################################################################################
##### plot graphs for employment for men and women 
source("validation_graph_functions.R")
# men
# establish limits of the graph
min = 65
max = 90

psid_men = generate_cohort_percet_plot(filter(for_graph_psid, SEX==1) , 
                                       letter_x = "a.PSID",
                                       cohort = "Wave", 
                                       country_x="",true_weights =F,
                                       by_grid = 5,
                                       min_limit_y = min,
                                       limiy_y = max, what_type = "", decide_legend ="none", title_y = "%")


soep_men = generate_cohort_percet_plot(filter(for_graph_soep, SEX==1) , 
                                             letter_x = "b.SOEP",
                                             cohort = "Wave", 
                                             country_x="",true_weights =F,
                                             by_grid = 5,
                                             min_limit_y = min,
                                             limiy_y = max, what_type = "", decide_legend ="none", title_y = "%")

bhps_ukhls_men = generate_cohort_percet_plot(filter(for_graph_bhps_ukhls, SEX==1) , 
                            letter_x = "c.BHPS&UKHLS",
                            cohort = "Wave", 
                            country_x="",true_weights =F,
                            min_limit_y = min,
                            by_grid = 5,
                            limiy_y = max, what_type = "", decide_legend ="none", title_y = "%")

shp_men = generate_cohort_percet_plot(filter(for_graph_shp, SEX==1) ,
                                      letter_x = "d.SHP",
                                      cohort = "Wave",
                                      country_x="",
                                      by_grid = 5,
                                      min_limit_y = min,true_weights =F,
                                      limiy_y = max, what_type = "",decide_legend ="none",title_y = "%")

hilda_men = generate_cohort_percet_plot(filter(for_graph_hilda, SEX==1) ,
                                             letter_x = "e.HILDA",
                                             cohort = "Wave",
                                             by_grid = 5,
                                             country_x="",
                                             min_limit_y = min,true_weights =F,
                                             limiy_y = max, what_type = "",decide_legend ="none", title_y = "%")

################################################################################ 
legend = ggpubr::get_legend(generate_cohort_percet_plot(filter(for_graph_bhps_ukhls, SEX==1) , 
                                                        letter_x = "e.HILDA",
                                                        by_grid = 5,
                                                        cohort = "Wave", 
                                                        country_x="",
                                                        min_limit_y = min,true_weights =F,
                                                        limiy_y = max, what_type = "", decide_legend = "right"))


empty <- plot_spacer()


s = (psid_men  + soep_men  )/
  (bhps_ukhls_men  + shp_men )/  
  (hilda_men  + legend)

s
ggsave(file=paste("graphs/Figure_4_6_oecd.jpg",sep=""), s,width=10.82, height=15.06, dpi=300)

################################################################################
# women
# establish limits of the graph
min_y = 60
max_y = 80

psid_WOmen = generate_cohort_percet_plot(filter(for_graph_psid, SEX==2) , 
                                         letter_x = "a.PSID",
                                         cohort = "Wave", 
                                         country_x="",true_weights =F,
                                         min_limit_y = min_y,
                                         by_grid = 5,
                                         limiy_y = max_y, what_type = "", decide_legend ="none", title_y = "%")

soep_WOmen = generate_cohort_percet_plot(filter(for_graph_soep, SEX==2) , 
                                       letter_x = "b.SOEP",
                                       cohort = "Wave", 
                                       country_x="",true_weights =F,
                                       min_limit_y = min_y,
                                       by_grid = 5,
                                       limiy_y = max_y, what_type = "", decide_legend ="none", title_y = "%")

bhps_ukhls_WOmen = generate_cohort_percet_plot(filter(for_graph_bhps_ukhls, SEX==2) , 
                                             letter_x = "c.BHPS&UKHLS",
                                             cohort = "Wave", 
                                             by_grid = 5,
                                             country_x="",
                                             min_limit_y = min_y,true_weights =F,
                                             limiy_y = max_y, what_type = "",decide_legend ="none",title_y = "%")

shp_WOmen = generate_cohort_percet_plot(filter(for_graph_shp, SEX==2) ,
                                      letter_x = "d.SHP",
                                      by_grid = 5,
                                      cohort = "Wave",
                                      country_x="",
                                      min_limit_y = min_y,true_weights =F,
                                      limiy_y = max_y, what_type = "",decide_legend ="none", title_y = "%")
  
# 
hilda_WOmen = generate_cohort_percet_plot(filter(for_graph_hilda, SEX==2) ,
                                          letter_x = "e.HILDA",
                                          by_grid = 5,
                                          cohort = "Wave",
                                          country_x="",
                                          min_limit_y = min_y,true_weights =F,
                                          limiy_y = max_y, what_type = "",decide_legend ="none",title_y = "%")

################################################################################
legend = ggpubr::get_legend(generate_cohort_percet_plot(filter(for_graph_bhps_ukhls, SEX==2) , 
                                                        letter_x = "e.HILDA",
                                                        cohort = "Wave", 
                                                        country_x="",true_weights =F,
                                                        min_limit_y = 50,
                                                        limiy_y = 90, what_type = "", decide_legend = "right", title_y = "%"))

empty <- plot_spacer()

s1 = (psid_WOmen  +soep_WOmen )/
(bhps_ukhls_WOmen  + shp_WOmen)/  
(hilda_WOmen + legend)

s1

ggsave(file=paste("graphs/Figure_4_7_oecd.jpg",sep=""), s1,width=10.82, height=15.06, dpi=300)

############################## save in a table #################################
total = bind_rows(for_graph_hilda, for_graph_shp, for_graph_bhps_ukhls, for_graph_psid, for_graph_soep)

write.csv(filter(total, SEX==1), "graphs/Fig_6_employment_ratio_male_validation_oecd.csv")
write.csv(filter(total, SEX==2), "graphs/Fig_7_employment_ratio_female_validation_oecd.csv")



