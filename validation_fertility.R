# 1. Download necessary packages and Establish necessary directories

source("00_setting_work_space.R")
`%notin%` <- Negate(`%in%`)

# Set the desired font name and register it as font family 'A' for use in plots on Windows
fontname <- "Calibri"
windowsFonts(A = windowsFont(fontname))

# Set the confidence level (e.g., 95%) and calculate the corresponding z-value 
# for a two-tailed normal distribution. This z-value is used in confidence interval calculations.
confidence_level <- 0.95
z_value <- qnorm((1 + confidence_level) / 2)

################################################################################
# Figure 4.1. Validation of Fertility Biographies: Percent Childless Women by Cohort
# External source of percent of childless 
data_childless_external <- data.frame(
  cohort = c("1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977"),
  external = c(10.49, 16.00, 14.42, 11.97,
               16.90, 16.00, 17.20, 22.20,
               14.0, 16.0, 19.3,  20.3,
               11.36,16.50,19.20,17.71,
               10.00, 13.00, 15.00, 16.00),
  dataset = c("PSID", "PSID", "PSID", "PSID",
              "SHP", "SHP", "SHP", "SHP",
              "soep", "soep", "soep", "soep",
              "BHPS_UKHLS", "BHPS_UKHLS", "BHPS_UKHLS", "BHPS_UKHLS",
              "HILDA", "HILDA", "HILDA", "HILDA"))
 


# prepare data master 

# psid
lastobs <- readRDS(paste0(folder_fertility, "/CAH85.rds")) %>% 
  rename(ER30001 = CAH3,         # "1968 INTERVIEW NUMBER OF PARENT"  
         ER30002 = CAH4) %>%     # "PERSON NUMBER OF PARENT"  
  mutate(pid     = ER30001*1000 + ER30002) %>% 
  select(pid, CAH104) %>% distinct() %>% group_by(pid) %>% 
  mutate(LASTOBS_Y = max(CAH104))

master_psid_wide = readRDS("output/master_psid_wide.rds") %>% 
  mutate(cohort = cut(BORN_Y, c(1883,1939,1950,1960,1970,1977,2021),dig.lab=4,
                                       labels = c("1883-1940","1941-1950",
                                                  "1951-1960","1961-1970","1971-1977", "1978-2021"))) %>% 
  # i) We included individuals from the original sample or born-in sample 
  # as they may not have full information about their fertility history.  
  filter(followable %in% c("This individual is original sample","This individual is born-in sample")) %>% 
  # ii) We included those whose households were not dropped in 1997 and as they may not have full information about their fertility history.  
  filter(drop =="not drop in 1997") %>%
  # iii) We included individuals who belong to either the SEO (Survey of Economic Opportunity) or 
  # SRC (Survey Research Center) (exclude Immigrants and Latino sample) 
  # as they may not have full information about their fertility history. 
  filter(sample %in% c("SEO Sample","SRC Sample")) %>%
  # iv) We included individuals who were at least 39 at the year of the most recent report of the number of children
  select(-LASTOBS_Y) %>% 
  merge(lastobs, by = c("pid")) %>% 
  filter(LASTOBS_Y>0)

# soep
master_soep_wide = readRDS("output/master_soep_wide.rds") %>% 
  mutate(cohort = cut(BORN_Y, c(1883, 1939, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
                      labels = c("1883-1940", "1941-1950", "1951-1960", "1961-1970", "1971-1977", "1978-2021"))) %>% 
  # i) We included individuals that are in: 1,2,3,4,5,6,8,10,11,27
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
  )) %>%
  # ii) We included only individuals who were at least 39 at the last available wave.
  filter(LASTOBS_Y>0)

# bhps_ukhls 
master_bhps_ukhls_wide = readRDS("output/master_bhps_ukhls_wide.rds") %>% 
  select(pidp, BORN_Y,SEX,LASTOBS_Y,memorig, ANYCHILD) %>%
  mutate(cohort = cut(BORN_Y, c(1883, 1939, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
                      labels = c("1883-1940", "1941-1950", "1951-1960", "1961-1970", "1971-1977", "1978-2021"))) %>% 
  # i) We excluded ethnic minority boost
  filter(memorig %notin% c(7,8)) %>%
  # 7 ukhls emboost 2009-10
  # 8 ukhls iemb 2014-15
  # iii) We included only individuals who were at least 39 at the last available wave.
  filter(LASTOBS_Y>0) %>% filter(!is.na(ANYCHILD))

master_shp_wide =  readRDS("output/master_shp_wide.rds") %>%
    mutate(cohort = cut(BORN_Y, c(1883, 1939, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
                        labels = c("1883-1940", "1941-1950", "1951-1960", "1961-1970", "1971-1977", "1978-2021"))) %>% 
  # i) We included individuals that are in the retrospective file (SHPIII_FA_USER family events) 
  # as they may not have full information about their fertility history. 
  filter(flag_retro_fertility =="yes") %>% 
  # ii) We included only individuals who were at least 39 at the last available wave.
  filter(LASTOBS_Y>0)

master_hilda_wide = readRDS("output/master_hilda_wide.rds") %>%
  mutate(cohort = cut(BORN_Y, c(1883, 1939, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
                      labels = c("1883-1940", "1941-1950", "1951-1960", "1961-1970", "1971-1977", "1978-2021"))) %>% 
  # i) We included only individuals who were at least 39 at the last available wave.
  filter(LASTOBS_Y>0)

# prepare data fertility
# psid
fertility_psid= readRDS("output/psid_fertility.rds") %>% 
  merge(select(master_psid_wide, -LASTOBS_Y), by = c("pid"), all.x = T) %>%
  # i) We included individuals from the original sample or born-in sample 
  # as they may not have full information about their fertility history.  
  filter(followable %in% c("This individual is original sample","This individual is born-in sample")) %>% 
  # ii) We included those whose households were not dropped in 1997 and as they may not have full information about their fertility history.  
  filter(drop =="not drop in 1997") %>%
  # iii) We included individuals who belong to either the SEO (Survey of Economic Opportunity) or 
  # SRC (Survey Research Center) (exclude Immigrants and Latino sample) 
  # as they may not have full information about their fertility history. 
  filter(sample %in% c("SEO Sample","SRC Sample")) %>%
  # iv) We included individuals who were at least 39 at the year of the most recent report of the number of children
  merge(lastobs, by = c("pid")) %>% 
  filter(LASTOBS_Y>0) 

# soep
fertility_soep = readRDS("output/soep_fertility.rds") %>% 
  # i) We included individuals that are in: 1,2,3,4,5,6,8,10,11,27
  merge(master_soep_wide, by = c("pid"), all.x = T) %>%
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
# bhps_ukhls 
fertility_bhps_ukhls = readRDS("output/bhps_ukhls_fertility.rds") %>% 
  # select(pidp, KID_1, KID_Y1) %>% 
  merge(master_bhps_ukhls_wide, by = c("pidp"), all.x = T) %>% 
  # i) We excluded ethnic minority boost
  filter(memorig %notin% c(7,8)) %>%
  # 7 ukhls emboost 2009-10
  # 8 ukhls iemb 2014-15
  # iii) We included only individuals who were at least 39 at the last available wave.
  filter(LASTOBS_Y>0)

# shp
fertility_shp = readRDS("output/shp_fertility.rds") %>%
  merge(master_shp_wide, by = c("idpers"), all.x = TRUE) %>% 
  # i) We included individuals that are in the retrospective file (SHPIII_FA_USER family events) 
  # as they may not have full information about their fertility history. 
  filter(flag_retro_fertility =="yes")

# hilda
fertility_hilda = readRDS("output/hilda_fertility.rds") %>%
  merge(master_hilda_wide, by = c("xwaveid"), all.x = TRUE) 

# process percent childless
process_childless_data <- function(master_path = master_soep_wide, 
                                   fertility_path = fertility_soep,
                                   id = "pid",
                                   dataset_1 = "soep"
                                   ) {
  
  # LIB statistics 
  lib_check = 
    # whoich dataset
    fertility_path %>% 
    # only those without missing LASTOBS_Y - Year last interview
    filter(LASTOBS_Y > 0) %>%
    # age_when_left_sample = Year last interview - Year of birth
    mutate(age_when_left_sample = LASTOBS_Y - BORN_Y) %>%
    # only women who were 40 when they left the sample
    filter(age_when_left_sample > 40, SEX == 2) %>%
    # Group the data by 'cohort' and first-born child identifier ('KID_1')
    group_by(cohort, KID_1) %>% 
    # remove potential duplicate rows within each group
    distinct() %>% 
    # Summarise the data: count the number of first-born child identifier ('KID_1') per cohort
    summarise(n = n(), .groups = "drop") %>%
    # # Regroup the summarised data by 'cohort' for further analysis or plotting
    group_by(cohort) %>%
    # Calculate summary statistics:
    # - total: total number of observations
    # - UNWEIHGTED_lib: unweighted percentage
    # - proportion: share of each group
    # - UNWEIHGTED_lower and UNWEIHGTED_upper: 95% confidence interval bounds for the unweighted percentage
    mutate(
      total = sum(n),
      UNWEIHGTED_lib = round(100 * (n / total), 2),
      proportion = n / total,
      UNWEIHGTED_lower = round(100 * (proportion - z_value * sqrt((proportion * (1 - proportion)) / total)), 2),
      UNWEIHGTED_upper = round(100 * (proportion + z_value * sqrt((proportion * (1 - proportion)) / total)), 2)
    ) %>%
    # Filter rows where KID_1 equals 0 (e.g., childless individuals) and cohort is not missing
    filter(KID_1 == 0 & !is.na(cohort)) %>%
    # Add a new column to label the dataset source
    mutate(dataset = dataset_1) %>% 
    # Merge with external childlessness data by cohort and dataset
    merge(data_childless_external, by = c("cohort","dataset")) %>%
    # Calculate the difference between external estimates and internal unweighted estimates
    mutate(diff = external - UNWEIHGTED_lib) %>%
    # Select relevant columns for comparison and output
    select(dataset, cohort, n, total, external, UNWEIHGTED_lib, diff, UNWEIHGTED_lower, UNWEIHGTED_upper)

  # Internal statistics 
  ANYCHILD <-  master_path %>% 
    # only those without missing LASTOBS_Y - Year last interview
    filter(LASTOBS_Y > 0) %>% 
    # age_when_left_sample = Year last interview - Year of birth
    mutate(age_when_left_sample = LASTOBS_Y - BORN_Y) %>%
    # only women who were 40 when they left the sample
    filter(age_when_left_sample > 40, SEX == 2) %>%
    # create variable cohort 
    mutate(cohort = cut(BORN_Y, c(1883, 1939, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
                        labels = c("1883-1940", "1941-1950", "1951-1960", "1961-1970", "1971-1977", "1978-2021"))) %>%
    # Group the data by 'cohort' and 'ANYCHILD' any child at the last possible interview?
    group_by(cohort, ANYCHILD) %>%
    summarise(n_any_child = n(), .groups = "drop") %>%
    group_by(cohort) %>%
    # Calculate:
    # - total_internal: total number of individuals per cohort
    # - internal: percentage of each 'ANYCHILD' group
    # - proportion: share of each group within the cohort
    # - internal_lower and internal_upper: 95% confidence interval bounds for the internal percentage
    mutate(
      total_internal = sum(n_any_child),
      internal = round(100 * (n_any_child / total_internal), 2),
      proportion = n_any_child / total_internal,
      internal_lower = round(100 * (proportion - z_value * sqrt((proportion * (1 - proportion)) /total_internal)), 2),
      internal_upper = round(100 * (proportion + z_value * sqrt((proportion * (1 - proportion)) / total_internal)), 2)
    ) %>%
    # [2] no children at the last possible interview
    filter(ANYCHILD == 2 & !is.na(cohort)) %>%
    # Select final relevant columns
    select(cohort, n_any_child, total_internal, internal, internal_lower, internal_upper) 
  
  # Merge processed data for LIB and Internal statistics 
  data <- merge(ANYCHILD, lib_check, by = "cohort") %>% 
    # Select final relevant columns
    select(dataset, cohort, external, internal, UNWEIHGTED_lib, UNWEIHGTED_lower, UNWEIHGTED_upper, 
      internal_lower, internal_upper, n_any_child, total_internal, n, total)

  return(data)
}

# for each data set prepare percent childless 
# psid
psid = process_childless_data(
  master_path = master_psid_wide, 
  fertility_path = fertility_psid,
  id = "pid",
  dataset = "PSID")
# soep
soep = process_childless_data(
        master_path = master_soep_wide, 
        fertility_path = fertility_soep,
        id = "pid",
        dataset_1 = "soep")
# bhps_ukhls
bhps_ukhls = process_childless_data(
  master_path = master_bhps_ukhls_wide, 
  fertility_path = fertility_bhps_ukhls,
  id = "pidp",
  dataset_1 = "BHPS_UKHLS")

# we have external statistics only for England and Wales not whole UK - on the graph it will be separate color
bhps_ukhls$external_EW = bhps_ukhls$external
bhps_ukhls$external = 100

# shp
shp = process_childless_data(
  master_path = master_shp_wide, 
  fertility_path = fertility_shp,
  id = "idpers",
  dataset_1 = "SHP")
# for cohort=="1971-1977", we have not enough observations to draw conclusions 
shp[shp$cohort=="1971-1977", c("UNWEIHGTED_lib")]<-100
shp[shp$cohort=="1971-1977", "UNWEIHGTED_lower"]<-100
shp[shp$cohort=="1971-1977", "UNWEIHGTED_upper"]<-100

shp[shp$cohort=="1971-1977", "internal"]<-100
shp[shp$cohort=="1971-1977", "internal_lower"]<-100
shp[shp$cohort=="1971-1977", "internal_upper"]<-100
shp[shp$cohort=="1971-1977", "external"]<-100

# hilda 
hilda = process_childless_data(
  master_path = master_hilda_wide, 
  fertility_path = fertility_hilda,
  id = "xwaveid",
  dataset_1 = "HILDA")

# Plot results for each dataset: psid,soep,bhps_ukhls,shp,hilda
# use function generate_cohort_percet_plot() form script validation_graph_functions.R
source("validation_graph_functions.R")

psid_plot <- generate_cohort_percet_plot(psid, country_x  = "PSID",decide_legend = "none", title_y = "% Childless", letter_x = "a.", limiy_y = 25, what_type = "", true_weights = F)

soep_plot <- generate_cohort_percet_plot(soep, country_x  = "SOEP",decide_legend = "none", title_y = "", letter_x = "b.", limiy_y = 25, what_type = "",true_weights = F)

bhps_ukhls_plot <- generate_cohort_percet_plot(bhps_ukhls, country_x  = "BHPS&UKHLS",decide_legend = "none", what_type = "", title_y = "% Childless", letter_x = "c.", limiy_y = 25,true_weights = F)

shp_plot <- generate_cohort_percet_plot(shp, country_x  = "SHP",decide_legend = "none", title_y = "",what_type = "", letter_x = "d.", limiy_y = 25,true_weights = F)

hilda_plot <- generate_cohort_percet_plot(hilda, country_x  = "HILDA",decide_legend = "none", what_type = "", title_y = "% Childless", letter_x = "e.", limiy_y = 25,true_weights = F)

legend = ggpubr::get_legend(generate_cohort_percet_plot(bhps_ukhls, country_x  = "BHPS&UKHLS",decide_legend = "right", what_type = "", limiy_y = 25,true_weights = F))

s = (psid_plot +soep_plot)/(bhps_ukhls_plot + shp_plot)/
  (hilda_plot + legend)

# print results inn the onsole 
s
# save graph 
ggsave(file=paste("graphs/Figure_4_1.jpg",sep=""),
       s,width=10.82, height=15.06, dpi=300)
# save results in a table in csv file 
data_all = bind_rows(psid,soep,bhps_ukhls,shp,hilda)
write.csv(data_all, "graphs/Fig_1_percent_childless_validation.csv")

################################################################################
# Figure_4_2: Female cohort mean age at birth for all birth orders
# Source: 
# Human Fertility Database. (2024). Mean age at birth by birth order: Germany (DEUTNP). Max Planck Institute for Demographic 
# Research (Germany) and Vienna Institute of Demography (Austria). Retrieved from https://www.humanfertility.org
# Riffe, T., & Mazzuco, S. (2021). HMDHFDplus: Access to the Human Mortality and Fertility Databases. 
# R package version 1.2.0. https://CRAN.R-project.org/package=HMDHFDplus
# "mabVH": Specifies the HFD data file to download: "mab" = Mean Age at Birth; 
# "VH" = All births (regardless of parity), by year of birth (not period) "CHE", "GBR_NP", "DEUTNP", "USA"

mab_data_external = data.frame(
    dataset = c(rep("AUS",4), rep("CHE",4), rep("DEUTNP",4),rep("GBR_NP",4), rep("USA",4),rep("GBRTEN",4)),
    BORN_Y_category = c("1941-1950","1951-1960","1961-1970","1971-1977",
                        "1941-1950","1951-1960","1961-1970","1971-1977",
                        "1941-1950","1951-1960","1961-1970","1971-1977",
                        "1941-1950","1951-1960","1961-1970","1971-1977",
                        "1941-1950","1951-1960","1961-1970","1971-1977",
                        "1941-1950","1951-1960","1961-1970","1971-1977"
                        ),
    
    CMAB = c(25.76, 26.62, 28.33, 29.53, # AUS
             26.71, 27.93, 29.20, 30.26, # CHE
             25.40, 26.36, 27.96, 29.17, # DEUTNP
             100, 27.45, 27.99, 28.85,   # GBR_NP
             25.12, 26.62, 27.38, 27.51, # USA
             25.94,26.98, 28.01,28.84    # GBRTEN
             )
    )


mab_data <- function(data = fertility_hilda, 
                     country = "AUS",
                     pid = "xwaveid"
) {

  
  max_kids = length(select(data, starts_with("KID_Y")) %>% names())
  
  mab_x =  data %>% 
    # year of birth of children (1900-2024); Year of birth, SEX, Year last interview
    select(pid,paste0("KID_Y", 1:max_kids),BORN_Y, SEX, LASTOBS_Y)

  mab_x = mab_x %>% 
    # select only women 
      filter(SEX==2) %>% 
    # Keep only individuals who were observed at least once (LASTOBS_Y > 0)
      filter(LASTOBS_Y > 0) %>%
    # Calculate the age at which each woman left the panel (last observation year minus birth year)
      mutate(age_when_left_sample = LASTOBS_Y - BORN_Y) %>%
    # only women who were 38 when they left the sample
      filter(age_when_left_sample > 40, SEX == 2) %>% 
      pivot_longer(
        cols = starts_with("KID_Y"),      # Specify the columns to pivot (KID_Y1, KID_Y2, ..., KID_Y13)
        names_to = "parity",              # Name of the new column that will hold the original column names (KID_Y1, KID_Y2, ...)
        values_to = "KID_Y",              # Name of the new column that will hold the values (the birth years)
        values_drop_na = TRUE             # Remove rows where the birth year is NA
      ) %>% 
    # Remove the "KID_Y" prefix from the 'parity' column (if present), leaving just the numeric birth order
      mutate(parity = stringr::str_remove(parity, "KID_Y")) %>% 
    # Filter out rows where the child's year of birth is missing
      filter(!is.na(KID_Y)) %>% 
    # Keep only valid birth years (greater than 0)
      filter(KID_Y>0) %>% 
    # Categorize mother's year of birth into defined cohorts for analysis
      mutate(BORN_Y_category = cut(BORN_Y, c(1883,1939,1950,1960,1970,1977,2021),dig.lab=4,
                                   labels = c("1883-1940","1941-1950","1951-1960","1961-1970","1971-1977", "1978-2021"))) %>% 
    # Exclude birth cohorts that are either too early or too recent for the analysis
      filter(BORN_Y_category %notin% c("1883-1940", "1978-2021")) %>% filter(!is.na(BORN_Y_category)) %>% 
    # Calculate age of the mother at the time of child's birth
      mutate(age = KID_Y - BORN_Y) %>% 
    # Keep only biologically plausible ages (mother must be older than 0 at childbirth)
      filter(age>0) %>% 
    mutate(age = as.integer(age)) %>% 
    # Restrict to typical reproductive age range: 15–49
    filter(age %in% c(15:49))
  
  mab_x
  
  mab_x = mab_x %>%  
    # Group the data by birth cohort category
    group_by(BORN_Y_category) %>%
    # Summarise statistics for each cohort:
    summarise(
      n = n(),
      # Calculate unweighted mean age at birth
      mean_lib_no_weigths = sum(age)/n,
      # Calculate standard deviation of age at birth
      sd_value = sd(age),
      # Get the critical t-value for a 95% confidence interval
      t_critical = qt(0.975, df = n - 1),
      # Calculate the margin of error for the mean estimate
      margin_of_error = t_critical * (sd_value / sqrt(n)),
      # Compute the lower and upper bounds of the 95% confidence interva
      lower_no_weigths = mean_lib_no_weigths - margin_of_error,
      upper_no_weigths = mean_lib_no_weigths + margin_of_error) %>% 
    # Add a column to indicate the data source or country
    mutate(dataset = country)
  
  # add external statistic 
  data_x = merge(mab_x, mab_data_external, by = c("BORN_Y_category", "dataset"), all.x = T) 
  
  return(data_x)
}

usa = mab_data(data = fertility_psid, country = "USA")%>% 
  mutate(dataset = "psid", pid = "pid")

uk = mab_data(data = fertility_bhps_ukhls,country = "GBR_NP",pid = "pidp")%>% 
  mutate(dataset = "bhps_ukhls")


CMAB_E_W = mab_data_external[mab_data_external$dataset=="GBRTEN",c("BORN_Y_category", "CMAB")] %>% 
  rename(CMAB_E_W = CMAB)
uk = merge(uk, CMAB_E_W, by = c("BORN_Y_category"))

ger = mab_data(data = fertility_soep,country = "DEUTNP",pid = "pid")%>% 
  mutate(dataset = "soep")

CHE = mab_data(data = fertility_shp, country = "CHE", pid = "idpers")%>% 
  mutate(dataset = "shp")

# for cohort=="1971-1977", we have not enough observations to draw conclusions 
CHE[CHE$BORN_Y_category=="1971-1977", c("CMAB")]<-100

CHE[CHE$BORN_Y_category=="1971-1977", c("mean_lib_no_weigths")]<-100

CHE[CHE$BORN_Y_category=="1971-1977", c("lower_no_weigths")]<-100

CHE[CHE$BORN_Y_category=="1971-1977", c("upper_no_weigths")]<-100

AUS = mab_data(data = fertility_hilda,country = "AUS",pid = "xwaveid") %>% 
  mutate(dataset = "hilda")

mean_age_birth = bind_rows(usa,uk,ger,CHE,AUS)

write.csv(mean_age_birth, "graphs/Fig_2_mean_age_any_birth_validation.csv")


### parameters for the graph
limit_y_1 = 35
limit_x = 25
y=3
label_x = 4
legend_z = 15

legend = ggpubr::get_legend(mab_graph(data = uk,
                                      BORN_Y_category = "BORN_Y_category",
                                      pos_leg = "right",
                                      letter_x = "c. ",
                                      CMAB = "CMAB",  country_x = "BHPS&UKHLS",
                                      CMAB_E_W = "CMAB_E_W",
                                      external_EW = "external_EW"
))


s = (mab_graph(data = usa, x = "PSID",limit_y = limit_y_1, letter_x = "a. ", CMAB = "CMAB", country_x = "PSID")+
       mab_graph(data = ger, x = "SOEP", limit_y = limit_y_1, title_y ="", letter_x = "b. ", country_x = "SOEP"))/
  (mab_graph(data = uk, country_x = "BHPS&UKHLS",x = "BHPS&UKHLS",limit_y = limit_y_1, pos_leg = "none", letter_x = "c. ")+
     mab_graph(data = CHE, CMAB = "CMAB", x = "SHP",  title_y ="",limit_y = limit_y_1, letter_x = "d. ", country_x ="SHP"))+
  (mab_graph(data =AUS, CMAB = "CMAB", x = "HILDA",limit_y = limit_y_1,pos_leg = "none", letter_x = "e. ", country_x ="HILDA")+ legend)

s

ggsave(file=paste("graphs/Figure_4_2.jpg",sep=""),
       s,width=10.82, height=15.06, dpi=300)

################################################################################
# Figure_4_3: Completed Cohort Fertility at 40 by cohort

CCF_LIB <- readxl::read_excel("external/CCF_LIB.xlsx") %>% 
  mutate(cohort = cut(Cohort, c(1883, 1940, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
                      labels = c("1883-1940", "1941-1950", "1951-1960", "1961-1970", "1971-1977", "1978-2021"))) %>% 
  select(cohort, CCF40_US,CCF40_UK_E_W, CCF40_Germany,CCF40_Switzerland, CCF40_Australia) %>%
  
  mutate(
    across(
      c(CCF40_US, CCF40_UK_E_W, CCF40_Germany, CCF40_Switzerland, CCF40_Australia),
      as.numeric
    )
  ) %>% 
  group_by(cohort) %>%
  summarise(CCF40_psid = mean(CCF40_US,na.rm  = T),
            CCF40_bhps_ukhls = mean(CCF40_UK_E_W, na.rm = T),
            CCF40_soep = mean(CCF40_Germany,na.rm  = T),
            CCF40_shp= mean(CCF40_Switzerland,na.rm  = T),
            CCF40_hilda= mean(CCF40_Australia, na.rm = T)) %>% 
  pivot_longer(cols = starts_with("CCF40_"),
               names_to = "dataset",
               values_to = "CCF") %>% 
  mutate(dataset = stringr::str_remove(dataset,"CCF40_"))


E_W = data.frame(cohort  =  c("1941-1950", "1951-1960", "1961-1970", "1971-1977"),   
        dataset = rep("england and wales",4),    
        external_ew = c(2.14,1.97, 1.86, 1.84))

CCF_LIB = bind_rows(CCF_LIB, E_W)

for (i in c("hilda", "shp", "bhps_ukhls","psid", "soep")) {
  

  ccf_by_cohort <- get(paste0("fertility_",i)) %>%
    group_by(cohort) %>%
    # For each cohort, calculate summary statistics on number of children
    summarise(
      n = n(),  # Number of individuals in the cohort
      # Calculate the unweighted mean number of children
      mean_lib_no_weigths = sum(NR_KIDS) / n,
      # Calculate the standard deviation of number of children
      sd_value = sd(NR_KIDS),
      # Get the critical t-value for a 95% confidence interval (two-tailed)
      t_critical = qt(0.975, df = n - 1),
      # Compute the margin of error for the mean estimate
      margin_of_error = t_critical * (sd_value / sqrt(n)),
      
      # Calculate the lower and upper bounds of the 95% confidence interval
      lower_no_weigths = mean_lib_no_weigths - margin_of_error,
      upper_no_weigths = mean_lib_no_weigths + margin_of_error
    ) %>% 
    mutate(dataset = i) %>% 
    merge(CCF_LIB, by = c("cohort", "dataset"))
  
  
  print(i)
  
  assign(x = paste0("ccf_by_cohort_", i),value = ccf_by_cohort)
  
}

# for cohort=="1971-1977", we have not enough observations to draw conclusions 
ccf_by_cohort_shp[ccf_by_cohort_shp$cohort=="1971-1977", "CCF"]<-100
ccf_by_cohort_shp[ccf_by_cohort_shp$cohort=="1971-1977", "external"]<-100
ccf_by_cohort_shp[ccf_by_cohort_shp$cohort=="1971-1977", "mean_lib_no_weigths"]<-100
ccf_by_cohort_shp[ccf_by_cohort_shp$cohort=="1971-1977", "lower_no_weigths"]<-100
ccf_by_cohort_shp[ccf_by_cohort_shp$cohort=="1971-1977", "upper_no_weigths"]<-100
ccf_by_cohort_shp[ccf_by_cohort_shp$cohort=="1971-1977", "external"]<-100

EW = CCF_LIB = filter(CCF_LIB,  dataset=="england and wales")  %>% select(-dataset)


ccf_by_cohort_bhps_ukhls = ccf_by_cohort_bhps_ukhls %>% select(-external_ew) %>% 
  merge(select(EW, -CCF), by = c("cohort"))


ccf_by_cohort_csv = bind_rows(get(paste0("ccf_by_cohort_soep")), get(paste0("ccf_by_cohort_shp")),
          get(paste0("ccf_by_cohort_hilda")),get(paste0("ccf_by_cohort_psid")),
          get(paste0("ccf_by_cohort_bhps_ukhls"))
)

write.csv(ccf_by_cohort_csv, "graphs/Fig_3_completed_cohort_fertility_validation.csv")


legend = ggpubr::get_legend(mab_graph(ccf_by_cohort_bhps_ukhls, 
                                      CMAB_E_W = "external_ew",
                                      country_x = "BHPS&UKHLS", letter_x ="BHPS & UKHLS",BORN_Y_category ="cohort",CMAB = "CCF",limit_x = 0, limit_y = 3, by_int=1, pos_leg = "right"))

s = (mab_graph(ccf_by_cohort_psid, 
               letter_x ="a.PSID", BORN_Y_category ="cohort",CMAB = "CCF", limit_x = 0, limit_y = 3, by_int=1, title_y = "Completed Cohort Fertility")+
       mab_graph(ccf_by_cohort_soep, title_y ="",
                 letter_x ="b.SOEP", BORN_Y_category ="cohort",CMAB = "CCF",limit_x = 0, limit_y = 3, by_int=1))/
  (mab_graph(ccf_by_cohort_bhps_ukhls, CMAB_E_W = "external_ew",country_x = "BHPS&UKHLS", 
             letter_x ="c.BHPS & UKHLS",BORN_Y_category ="cohort",CMAB = "CCF",limit_x = 0, limit_y = 3, by_int=1, title_y = "Completed Cohort Fertility") +
     
     mab_graph(ccf_by_cohort_shp, title_y ="", 
               letter_x ="d.SHP",BORN_Y_category ="cohort",CMAB = "CCF",limit_x = 0, limit_y = 3, by_int=1))/
  (mab_graph(ccf_by_cohort_hilda,
             letter_x = "e.HILDA", BORN_Y_category ="cohort",CMAB = "CCF", limit_x = 0, limit_y = 3, by_int=1, title_y = "Completed Cohort Fertility") + legend)

s

ggsave(file=paste("graphs/Figure_4_3.jpg",sep=""),
       s,width=10.82, height=15.06, dpi=300)


