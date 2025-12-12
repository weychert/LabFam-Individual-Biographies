# 1. Download necessary packages and Establish necessary directories

source("00_setting_work_space.R")
`%notin%` <- Negate(`%in%`)

fontname<-"Calibri"
windowsFonts(A = windowsFont(fontname))

###### external data 

data_Never_Married_external <- data.frame(
  cohort = c("1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977",
             "1941-1950", "1951-1960", "1961-1970", "1971-1977"),
  external = c(7.3,  11.3,  14.4,  16.8,
               9.70, 11.72, 15.75, 18.92,
               6.40, 7.41,  16.10, 21.18,
               4.53, 8.65,  18.66, 26.07,
               6.07,	9.60,	19.97,100,
               6.25,	9.92,	17.00,	21.94
               
               ),
  dataset = c("PSID", "PSID", "PSID", "PSID",
              "SHP", "SHP", "SHP", "SHP",
              "SOEP", "SOEP", "SOEP", "SOEP",
              "EW","EW","EW","EW",
              "BHPS_UKHLS", "BHPS_UKHLS", "BHPS_UKHLS", "BHPS_UKHLS",
              "HILDA", "HILDA", "HILDA", "HILDA"))





confidence_level <- 0.95
z_value <- qnorm((1 + confidence_level) / 2)

################################################################################
# prepare data master 

lastobs <-readRDS(paste0(folder_partnership, "/MH885.rds")) %>% 
  rename(
    ER30001 = MH2,         #"1968 INTERVIEW NUMBER OF INDIVIDUAL#"  
    ER30002 = MH3) %>% 
  mutate(pid  = ER30001*1000 + ER30002)%>% 
  select(pid, MH17) %>% distinct() %>% group_by(pid) %>% 
  mutate(LASTOBS_Y = max(MH17))


master_psid_wide = readRDS("output/master_psid_wide.rds") %>% 
  mutate(cohort = cut(BORN_Y, c(1883,1940,1950,1960,1970,1977,2021),dig.lab=4,
                      labels = c("1883-1940","1941-1950","1951-1960","1961-1970","1971-1977", "1978-2021"))) %>% 
  # i) We included individuals from the original sample or born-in sample 
  # as they may not have full information about their partnership history.  
  filter(followable %in% c("This individual is original sample","This individual is born-in sample")) %>% 
  # ii) We included those whose households were not dropped in 1997 and as they may not have full information about their partnership history.  
  filter(drop =="not drop in 1997") %>%
  # iii) We included individuals who belong to either the SEO (Survey of Economic Opportunity) or 
  # SRC (Survey Research Center) (exclude Immigrants and Latino sample) 
  # as they may not have full information about their partnership history. 
  filter(sample %in% c("SEO Sample","SRC Sample")) %>%
  # iv) We included individuals who were at least 40 at the year of the most recent report of the number of children
  filter(LASTOBS_Y>0) %>% select(-LASTOBS_Y) %>% 
  merge(lastobs, by = c("pid"), all.x = T)

master_soep_wide = readRDS("output/master_soep_wide.rds") %>%
  mutate(cohort = cut(BORN_Y, c(1883, 1940, 1950, 1960, 1970, 1977, 2021), dig.lab = 4,
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


master <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>% 
  select(pidp, memorig, memorig_bh)

master_bhps_ukhls_wide = readRDS("output/master_bhps_ukhls_wide.rds") %>% 
  select(pidp, BORN_Y,SEX,LASTOBS_Y,sampst,memorig, memorig_bh, EVER_MARRIED) %>% 
  mutate(cohort = cut(BORN_Y, c(1883,1940,1950,1960,1970,1977,2021),dig.lab=4,
                      labels = c("1883-1940","1941-1950","1951-1960","1961-1970","1971-1977", "1978-2021"))) %>% 
  # i) We excluded ethnic minority boost
  # filter(memorig %notin% c(7,8)) %>%
  # filter(sampst %in% c(1,2)) %>%
  # filter(ever_proxy == "not proxy") %>% 
  filter(memorig %in% c(1,2) | memorig_bh %in% c(1)) %>%
  filter(LASTOBS_Y>0)


master_shp_wide = readRDS("output/master_shp_wide.rds") %>%
  select(idpers, BORN_Y,SEX,LASTOBS_Y, retro_marriage) %>%
  mutate(cohort = cut(BORN_Y, c(1883,1940,1950,1960,1970,1977,2021),dig.lab=4,
                      labels = c("1883-1940","1941-1950",
                                 "1951-1960","1961-1970","1971-1977", "1978-2021")))

master_hilda_wide = readRDS("output/master_hilda_wide.rds") %>%
  select(xwaveid, BORN_Y,SEX,LASTOBS_Y, EVER_MARRIED) %>%
  mutate(cohort = cut(BORN_Y, c(1883,1940,1950,1960,1970,1977,2021),dig.lab=4,
                      labels = c("1883-1940","1941-1950",
                                 "1951-1960","1961-1970","1971-1977", "1978-2021")))

###### partnership

partner_biography_psid_wide = readRDS("output/psid_partnership.rds") %>% 
  merge(master_psid_wide, by = c("pid")) %>%
  # i) We included individuals from the original sample or born-in sample 
  # as they may not have full information about their partnership history.  
  filter(followable %in% c("This individual is original sample","This individual is born-in sample")) %>% 
  # ii) We included those whose households were not dropped in 1997 and as they may not have full information about their partnership history.  
  filter(drop =="not drop in 1997") %>%
  # iii) We included individuals who belong to either the SEO (Survey of Economic Opportunity) or 
  # SRC (Survey Research Center) (exclude Immigrants and Latino sample) 
  # as they may not have full information about their partnership history. 
  filter(sample %in% c("SEO Sample","SRC Sample")) %>%
  # iv) We included individuals who were at least 40 at the year of the most recent report of the number of children
  filter(LASTOBS_Y>0)

partner_biography_soep_wide = readRDS("output/soep_partnership.rds") %>% 
  merge(master_soep_wide, by = c("pid")) %>%
  # i) We included individuals that are in: 1,2,3,4,5,6,8,10,11,27
  filter(psample %in%c(1, # [1] A 1984 Initial Sample (West)	
                       2,	# [2] B 1984 Migration (until 1983, West)	
                       3,	# [3] C 1990 Initial Sample (East)	
                       4,	# [4] D 1994/5 Migration (1984-1994, West)	
                       5,	# [5] E 1998 Refreshment	
                       6,	# [6] F 2000 Refreshment	
                       8,	# [8] H 2006 Refreshment	
                       10,# [10] J 2011 Refreshment	
                       11,# [11] K 2012 Refreshment
                       27	# [27] R 2022	
  )) %>%
  # ii) We included only individuals who were at least 40 at the last available wave.
  filter(LASTOBS_Y>0)

partner_biography_bhps_ukhls_wide = readRDS("output/bhps_ukhls_partnership.rds") %>% 
  merge(master_bhps_ukhls_wide, by = c("pidp"), all.x = T) %>% 
  # i) We excluded ethnic minority boost
  # filter(ever_proxy == "not proxy") %>%
  # filter(sampst %in% c(1,2)) %>%
  # filter(memorig %notin% c(7,8)) %>%
  filter(memorig %in% c(1,2) | memorig_bh %in% c(1)) %>%
  filter(LASTOBS_Y>0)
 
partner_biography_shp_wide = readRDS("output/shp_partnership.rds") %>% 
  merge(master_shp_wide, by = c("idpers")) %>% 
  filter(retro_marriage=="yes")

partner_biography_hilda_wide = readRDS("output/hilda_partnership.rds") %>% 
  merge(master_hilda_wide, by = c("xwaveid"))

################################################################################ 

prepare_Never_Married <- function(
    master_path = master_bhps_ukhls_wide, 
    partner_path = partner_biography_bhps_ukhls_wide ,
    id = "pidp",
    dataset_1 = "BHPS&UKHLS"
    ) {
  

  data_x <- as.data.table(partner_path)
  
  # reshape data from wide to long format for both PARTNERSHIP_STATUS and START_Y
  combined <- melt(
    data_x,
    id.vars = id,  # Columns to keep (e.g., 'pid')
    measure = patterns("^PARTNERSHIP_STATUS", "^START_Y_"),  # Match PARTNERSHIP_STATUS_ and START_Y_ columns
    variable.name = "spellnr",                               # New column for PARTNERSHIP_STATUS identifier
    value.name = c("PARTNERSHIP_STATUS", "START_Y")          # New columns for PARTNERSHIP_STATUS and START_Y values
  )

  lib_check = combined %>% 
    merge(master_path, by = c(id)) %>% 
    filter(SEX==2) %>%
    # Create a new variable 'age_when_left_sample' as the difference 
    # between the year of last observation and year of birth
    mutate(age_when_left_sample = LASTOBS_Y - BORN_Y) %>%
    # only women who were 38 when they left the sample
    filter(age_when_left_sample > 38, SEX == 2)
    # Create a new binary variable 'married' where:
    # 1 = respondent is married (PARTNERSHIP_STATUS == "marriage")
    # 0 = otherwise
    
    lib_check = lib_check %>% mutate(married = ifelse(PARTNERSHIP_STATUS ==2,1,0))  

    
    
  lib_check = lib_check %>% 
    group_by(!!sym(id)) %>% 
    # For each group, check if the respondent was ever married (at least once)
    # If the sum of 'married' is greater than 0 (ignoring NAs), set status to "married"
    # Otherwise, set to "not married"
    mutate(married = ifelse(sum(married, na.rm = T) >0,"married","not married")) %>% 
    # Remove grouping structure from the data (e.g., after summarizing or grouping by ID)
    ungroup() %>% 
    select(id, cohort,married) %>% distinct() %>% 
    group_by(cohort,married) %>% 
    summarise(n=n()) %>% ungroup() %>% group_by(cohort) %>% 
    mutate(
      # Calculate total count across all rows (e.g., total number of individuals or observations)
      total = sum(n),
      
      # Calculate unweighted percentage (labelled UNWEIHGTED_lib) of each group
      UNWEIHGTED_lib = 100 * (n / total),
      
      # Store raw proportion (0–1) for use in confidence interval calculation
      proportion = n / total,
      
      # Calculate lower bound of 95% CI for the unweighted percentage
      UNWEIHGTED_lower = 100 * (proportion - z_value * sqrt((proportion * (1 - proportion)) / total)),
      
      # Calculate upper bound of 95% CI for the unweighted percentage
      UNWEIHGTED_upper = 100 * (proportion + z_value * sqrt((proportion * (1 - proportion)) / total))
    ) %>%
    
    # Keep only rows where the group is 'not married' and the birth cohort is within the specified range
    filter(married == "not married" & cohort %in% c("1941-1950", "1951-1960", "1961-1970", "1971-1977")) %>%
    
    # Add a column to tag which dataset the results come from (e.g., for merging or comparison)
    mutate(dataset = dataset_1) %>% 
    # Merge the current dataset with 'data_Never_Married_external' 
    # using 'cohort' and 'dataset' as the matching keys (inner join assumed)
    merge(data_Never_Married_external, by = c("cohort", "dataset")) %>%
    # Select and keep only the relevant columns for analysis or reporting:
    select(dataset, cohort, external,UNWEIHGTED_lib, UNWEIHGTED_lower, UNWEIHGTED_upper, n,total)
  
  
  # for "SHP", "SOEP" we do not have ever_married variable for internal validation 
  if (dataset_1 %in% c("SHP", "SOEP")) {
    
    lib_check = lib_check %>% 
      mutate(
      total_internal = 100,
      internal = 100,
      proportion = 100,
      internal_lower = 100,
      internal_upper = 100,
      n_any_marriage = NA,
      total_internal = NA
      
    ) %>%
      select(dataset, cohort, external, internal, UNWEIHGTED_lib, UNWEIHGTED_lower, UNWEIHGTED_upper, 
             internal_lower, internal_upper, n_any_marriage, total_internal, n, total)

    return(lib_check)
  } else{
    ever_married <-  master_path %>% 
      # Filter out respondents who were never observed (LASTOBS_Y must be > 0)
      filter(LASTOBS_Y > 0) %>%
      # Calculate age when the respondent left the sample
      mutate(age_when_left_sample = LASTOBS_Y - BORN_Y) %>%
      # Keep only women (SEX == 2) who left the sample after age 38
      filter(age_when_left_sample > 38, SEX == 2) %>%
      # Group by birth cohort and marriage history
      group_by(cohort, EVER_MARRIED) %>%
      # Count the number of individuals in each group (married vs never married)
      summarise(n_any_marriage = n(), .groups = "drop") %>%
      # Re-group by cohort to compute proportions within each cohort
      group_by(cohort) %>%
      mutate(
        # Total number of women per cohort (regardless of marital status)
        total_internal = sum(n_any_marriage),
        # Internal estimate: % of women with given marriage status
        internal = round(100 * (n_any_marriage / total_internal), 2),
        # Proportion used for CI calculation
        proportion = n_any_marriage / total_internal,
        # Lower bound of 95% CI for internal estimate
        internal_lower = round(100 * (proportion - z_value * sqrt((proportion * (1 - proportion)) / total_internal)), 2),
        # Upper bound of 95% CI for internal estimate
        internal_upper = round(100 * (proportion + z_value * sqrt((proportion * (1 - proportion)) / total_internal)), 2)
      ) %>%
      # Keep only those who were never married (ever_married == 0) and have valid cohort info
      filter(EVER_MARRIED == 0 & !is.na(cohort)) %>%
      # Select the final variables for output
      select(cohort, n_any_marriage, total_internal, internal, internal_lower, internal_upper)
    
    # Merge processed data
    data <- merge(ever_married, lib_check, by = "cohort")
    
    # Select final required columns
    data <- data %>%
      select(dataset, cohort, external, internal, UNWEIHGTED_lib, UNWEIHGTED_lower, UNWEIHGTED_upper, 
             internal_lower, internal_upper, n_any_marriage, total_internal, n, total)
    
    return(data)
    
  }

}

psid = prepare_Never_Married(
    master_path = master_psid_wide, 
    partner_path = partner_biography_psid_wide ,
    id = "pid",
    dataset_1 = "PSID"
)

soep = prepare_Never_Married(
  master_path = master_soep_wide, 
  partner_path = partner_biography_soep_wide ,
  id = "pid",
  dataset_1 = "SOEP"
)

bhps_ukhls = prepare_Never_Married(
  master_path = master_bhps_ukhls_wide, 
  partner_path = partner_biography_bhps_ukhls_wide,
  id = "pidp",
  dataset_1 = "BHPS_UKHLS"
)


EW = data_Never_Married_external[data_Never_Married_external$dataset=="EW",c("cohort", "external")] %>% 
  rename(external_EW = external)

bhps_ukhls = merge(bhps_ukhls, EW,by = c("cohort"))

shp = prepare_Never_Married(
  master_path = master_shp_wide, 
  partner_path = partner_biography_shp_wide ,
  id = "idpers",
  dataset_1 = "SHP"
)

# for cohort=="1971-1977", we have not enough observations to draw conclusions
shp[shp$cohort=="1971-1977","external"]<-100
shp[shp$cohort=="1971-1977","UNWEIHGTED_lib"]<-100
shp[shp$cohort=="1971-1977","UNWEIHGTED_lower"]<-100
shp[shp$cohort=="1971-1977","UNWEIHGTED_upper"]<-100
shp[shp$cohort=="1971-1977","external"]<-100


hilda = prepare_Never_Married(
  master_path = master_hilda_wide, 
  partner_path = partner_biography_hilda_wide ,
  id = "xwaveid",
  dataset_1 = "HILDA"
)

################################################################################
# plot 

data_all = bind_rows(psid,bhps_ukhls, shp, hilda, soep)

write.csv(data_all, "graphs/Fig_4_percent_never_married_validation.csv")

source("validation_graph_functions.R")

psid_plot <- generate_cohort_percet_plot(psid, country_x  = "PSID",decide_legend = "none", letter_x = "a.", what_type = "", true_weights = F)

soep_plot <- generate_cohort_percet_plot(soep, country_x  = "SOEP",title_y = "",decide_legend = "none", letter_x = "b.",what_type = "",true_weights = F)

bhps_ukhls_plot <- generate_cohort_percet_plot(bhps_ukhls, country_x  = "BHPS&UKHLS",decide_legend = "none", letter_x = "b.", what_type = "",true_weights = F)

shp_plot <- generate_cohort_percet_plot(shp, country_x  = "SHP",decide_legend = "none", title_y = "", letter_x = "d.", what_type = "",true_weights = F)

hilda_plot <- generate_cohort_percet_plot(hilda, country_x  = "HILDA",decide_legend = "none",letter_x = "e.", what_type = "",true_weights = F)


legend = ggpubr::get_legend(generate_cohort_percet_plot(bhps_ukhls, country_x  = "BHPS&UKHLS",decide_legend = "right", what_type = "",true_weights = F))

empty <- plot_spacer()

s = (psid_plot + soep_plot )/(bhps_ukhls_plot + shp_plot )/
  (hilda_plot  + legend)

s

ggsave(file=paste("graphs/Figure_4_4.jpg",sep=""),
       s,width=10.82, height=15.06, dpi=300)


################################################################################
# period_MAM
# Period Female Mean Age at First Marriage

marriage_external <- 
  readxl::read_excel("external/sf_3_1_marriage_divorce_rates.xlsx", 
                     sheet = "MeanAgeFirstMarriage", skip = 3) %>% 
  fill(Country,.direction = "down") %>% 
  filter(Gender =="Female") %>% 
  filter(Country %in% c("Australia","Germany","United Kingdom","United States", "Switzerland" )) %>% 
  select(-c("Note","Gender")) %>% 
  mutate(across(`1990`:`2021`, ~ifelse(. == "..", NA, .))) %>%
  mutate(across(`1990`:`2021`, as.numeric)) %>% 
  pivot_longer(
    cols = `1990`:`2021`, 
    names_to = "Year", 
    values_to = "Value"
  ) %>% 
  mutate(
    Year = as.numeric(Year),
    period = cut(Year, c(1999,2004,2009,2014,2019),
               labels = c("(2000,2004]", "(2005,2009]", "(2010,2014]", "(2015,2019]")
               , dig.lab=4)) %>% 
  filter(!is.na(period)) %>% group_by(Country,period) %>% 
  summarise(MAM = mean(Value, na.rm = T)) %>% 
  mutate(data = case_when(
    Country == "Australia" ~ "hilda",
    Country == "Switzerland" ~ "shp",
    Country == "Germany" ~ "soep",
    Country == "United Kingdom" ~ "bhps_ukhls",
    Country == "United States" ~ "psid") )

period_MAM <- function(data_x =  partner_biography_bhps_ukhls_wide, 
                       master_x = master_bhps_ukhls_wide,
                       country = "bhps_ukhls",
                       marriage_external_x = marriage_external,
                       id = "pidp"
) {

  
  marriage_external_x = marriage_external_x %>% filter(data ==country)
  
  # to long to find first marriage for each individual 
  data_x <- as.data.table(data_x)
  
  
  if (country == "soep") {
    # Melt the data to reshape it from wide to long format for both UNION and START_Y
    combined <- melt(
      data_x,
      id.vars = id,  # Columns to keep (e.g., 'pid')
      measure = patterns("^PARTNERSHIP_STATUS_", "^START_Y_", "^censor"),  # Match UNION_ and START_Y_ columns
      variable.name = "union",  # New column for union identifier
      value.name = c("PARTNERSHIP_STATUS", "START_Y", "censor")  # New columns for UNION and START_Y values
    )
  } else{
    # Melt the data to reshape it from wide to long format for both UNION and START_Y
    combined <- melt(
      data_x,
      id.vars = id,  # Columns to keep (e.g., 'pid')
      measure = patterns("^PARTNERSHIP_STATUS_", "^START_Y_"),  # Match UNION_ and START_Y_ columns
      variable.name = "union",  # New column for union identifier
      value.name = c("PARTNERSHIP_STATUS", "START_Y")  # New columns for UNION and START_Y values
    )
  }

  # Clean up the 'union' column by removing prefixes
  combined[, union := str_replace(PARTNERSHIP_STATUS, "^PARTNERSHIP_STATUS_|^START_Y_", "")]
  
  combined$START_Y = as.numeric(combined$START_Y)

  
  if (country == "soep") {
    
    total_1 = combined %>% 
      # Keep only rows where the partnership status is "marriage"
      # filter(PARTNERSHIP_STATUS == "marriage") %>%
      filter(PARTNERSHIP_STATUS == 2) %>%
      # Group the data by respondent ID (dynamically using !!sym(id) for flexible programming)
      group_by(!!sym(id)) %>%
      # Within each respondent group, sort observations by START_Y (earliest marriage first)
      arrange(START_Y, .by_group = TRUE) %>%
      # Create a row number variable within each group to identify the first marriage
      mutate(nr = row_number()) %>%
      # Keep only the first marriage record per person (i.e., where row number equals 1)
      filter(nr == 1) %>%
      # Select only the ID and START_Y (year of first marriage)
      select(id, START_Y,censor) %>%
      # Remove any records where the marriage year is missing or invalid (e.g., START_Y <= 0)
      filter(START_Y > 0) %>% 
    # "[-2] Does not apply" 
    # "[0] not censored"
    # "[1] LC: missing"   
    # "[2] LC: after gap"
    # "[3] RC: missing"
    # "[4] RC: before gap"  
    # "[5] RC: last spell"   
    # "[6] RC: death"
    # "[7] LC+RC: missing & missing"      
    # "[8] LC+RC: missing & before gap"
    # "[9] LC+RC: missing & last spell"                    
    # "[10] LC+RC: missing & death"       
    # "[13] LC+RC: after gap & last spell"
    # "[11] LC+RC: after gap & missing"   
    # "[12] LC+RC: after gap & before gap"
    # filter(censor %notin% c(1,2,7:14))
    filter(censor %notin% c("[1] LC: missing", "[2] LC: after gap",
                            "[7] LC+RC: missing & missing",      
                            "[8] LC+RC: missing & before gap",
                            "[9] LC+RC: missing & last spell",                    
                            "[10] LC+RC: missing & death",       
                            "[13] LC+RC: after gap & last spell",
                            "[11] LC+RC: after gap & missing",   
                            "[12] LC+RC: after gap & before gap"
    ))
    
  
  }else{
    total_1 = combined %>% 
      # Keep only rows where the partnership status is "marriage"
      filter(PARTNERSHIP_STATUS == 2) %>%
      # Group the data by respondent ID (dynamically using !!sym(id) for flexible programming)
      group_by(!!sym(id)) %>%
      # Within each respondent group, sort observations by START_Y (earliest marriage first)
      arrange(START_Y, .by_group = TRUE) %>%
      # Create a row number variable within each group to identify the first marriage
      mutate(nr = row_number()) %>%
      # Keep only the first marriage record per person (i.e., where row number equals 1)
      filter(nr == 1) %>%
      # Select only the ID and START_Y (year of first marriage)
      select(id, START_Y) %>%
      # Remove any records where the marriage year is missing or invalid (e.g., START_Y <= 0)
      filter(START_Y > 0) 
  }
  
  total_3 = total_1 %>% 
    # Merge selected columns (id, SEX, BORN_Y, LASTOBS_Y) from master_x into the current dataset
    # Use a left join (all.x = TRUE) to retain all rows from the current dataset, matching on 'id'
    # merge(select(master_x, id, SEX, BORN_Y,LASTOBS_Y), by = c(id), all.x = T) %>% 
    merge(master_x, by = c(id), all.x = T) %>% 
    # Ensure the START_Y variable is numeric (e.g., in case it was a character/factor)
    mutate(START_Y = as.numeric(START_Y)) %>% 
    # Remove rows with invalid or missing year of birth
    filter(BORN_Y>0, START_Y>0) %>% 
    # Calculate respondent's age at START_Y
    mutate(age =  START_Y - BORN_Y) %>% 
    # Filter out cases with negative or zero age 
    filter(age>0 ) %>% 
    # Keep only female respondents (SEX == 2)
    filter(SEX ==2)  
  
  period <- total_3 %>%
    # Filter out rows with missing or invalid birth year
    filter(BORN_Y > 0) %>%
    # Keep only records where START_Y (e.g., year of first marriage) is between 2000 and 2019
    filter(START_Y %in% c(2001:2019)) %>%
    # Create variables:
    mutate(
      # Calculate age at first marriage
      age_FM = START_Y - BORN_Y,
      # Ensure START_Y is treated as numeric (in case it's a factor or character)
      START_Y = as.numeric(as.character(START_Y)),
      # Create a categorical variable 'period' that bins START_Y into 5-year ranges
      period = cut(
        START_Y,
        breaks = c(1999, 2004, 2009, 2014, 2019),  # define period boundaries
        labels = c("(2000,2004]", "(2005,2009]", "(2010,2014]", "(2015,2019]"),
        dig.lab = 4  # number of digits to display in labels
      )
    ) %>%
    # filter(age>15 &age<60) %>% 
    # Group data by the defined time period
    group_by(period) %>%
    # Summarize key statistics for each period group
    summarise(
      min(age_FM),  # Earliest year in group
      median(age_FM),
      max(age_FM),  # Latest year in group
      MAM_lib_no_weighted = sum(age_FM) / n(),  # Mean age at marriage (unweighted)
      n = n(),  # Number of observations
      sd_value = sd(age_FM),  # Standard deviation of age at marriage
      t_critical = qt(0.975, df = n - 1),  # t critical value for 95% confidence interval
      margin_of_error = t_critical * (sd_value / sqrt(n)),  # Margin of error
      lower_no_weigths = MAM_lib_no_weighted - margin_of_error,  # Lower bound of CI
      upper_no_weigths = MAM_lib_no_weighted + margin_of_error   # Upper bound of CI
    )
  
  period_1 = merge(period, marriage_external_x, by = c("period"), all.x = T)
  
  
  return(period_1)
  
}

period_psid = period_MAM (data_x = partner_biography_psid_wide, 
                          master_x = master_psid_wide, 
                          country = "psid",
                          id = "pid"
)

period_soep = period_MAM (data_x = partner_biography_soep_wide,
                        master_x = master_soep_wide,
                        id = "pid",
                        country = "soep")


period_uk = period_MAM (data_x = partner_biography_bhps_ukhls_wide, 
                        master_x = master_bhps_ukhls_wide, 
                        country = "bhps_ukhls")


period_shp = period_MAM (data_x = partner_biography_shp_wide, 
                        master_x = master_shp_wide, 
                        id = "idpers",
                        country = "shp")

# for cohort=="1971-1977", we have not enough observations to draw conclusions
period_shp[period_shp$period=="(2015,2019]","MAM"]<-100
period_shp[period_shp$period=="(2015,2019]","MAM_lib_no_weighted"]<-100
period_shp[period_shp$period=="(2015,2019]","lower_no_weigths"]<-100
period_shp[period_shp$period=="(2015,2019]","upper_no_weigths"]<-100


period_hilda = period_MAM (data_x = partner_biography_hilda_wide, 
                        master_x = master_hilda_wide, 
                        id = "xwaveid",
                        country = "hilda")


write.csv(data_all, "graphs/Fig_5_mean_first_marraige_validation.csv")

# graph 

legend = ggpubr::get_legend(mab_graph(data = period_psid , 
                                      pos_leg = "right",
                                      BORN_Y_category = "period",
                                      mean_NO_weigths_lib = "MAM_lib_no_weighted",
                                      CMAB = "MAM", 
                                      limit_x = 24,
                                      limit_y = 35,
                                      by_int = 1,
                                      country_x = "a.PSID",))


psid_plot = mab_graph(data = period_psid , 
                      BORN_Y_category = "period",
                      mean_NO_weigths_lib = "MAM_lib_no_weighted",
                      CMAB = "MAM", 
                      pos_leg = "none",
                      title_y = "Female Mean Age at First Marriage",
                      limit_x = 24,
                      limit_y = 35,
                      by_int = 1,
                      country_x = "a.PSID",
                      mam =T)+labs(x = "period")


soep_plot = mab_graph(data = period_soep ,
                      BORN_Y_category = "period",
                      mean_NO_weigths_lib = "MAM_lib_no_weighted",
                      CMAB = "MAM", 
                      pos_leg = "none",
                      limit_x = 24,
                      limit_y = 35,
                      by_int = 1,
                      country_x = "b.SOEP",
                      mam =T,
                      title_y = ""
                      )+labs(x = "period")

bhps_ukhls_plot = mab_graph(data = period_uk , 
                            BORN_Y_category = "period",
                            mean_NO_weigths_lib = "MAM_lib_no_weighted",
                            title_y = "Female Mean Age at First Marriage",
                            CMAB = "MAM", 
                            pos_leg = "none",
                            limit_x = 24,
                            limit_y = 35,
                            by_int = 1,
                            country_x = "c.BHPS&UKHLS",
                            mam =T)+labs(x = "period")

shp_plot = mab_graph(data = period_shp , 
                     BORN_Y_category = "period",
                     mean_NO_weigths_lib = "MAM_lib_no_weighted",
                     CMAB = "MAM", 
                     pos_leg = "none",
                     limit_x = 24,
                     limit_y = 35,
                     by_int = 1,
                     country_x = "d.SHP",
                     mam =T,
                     title_y = ""
                     )+labs(x = "period")


hilda_plot = mab_graph(data = period_hilda , 
                       BORN_Y_category = "period",
                       mean_NO_weigths_lib = "MAM_lib_no_weighted",
                       title_y = "Female Mean Age at First Marriage",
                       CMAB = "MAM", 
                       pos_leg = "none",
                       limit_x = 24,
                       limit_y = 35,
                       by_int = 1,
                       country_x = "e.HILDA",
                       mam =T)+labs(x = "period")


s = (psid_plot + soep_plot + bhps_ukhls_plot + shp_plot + hilda_plot +legend) + plot_layout(ncol = 2)

s


ggsave(file=paste("graphs/Figure_4_5.jpg",sep=""),
       s,width=10.82, height=15.06, dpi=300)
















