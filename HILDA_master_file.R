############################# HILDA Main File #####################################
# Load necessary packages and directories 
source("00_setting_work_space.R")

# Creating list with the name of  Combined_ data sets that we will use used for iterations

Combined_<-list.files(folder_Australia_1,pattern="*Combined_")

# We create variable to indicate in functions for which wave we perform calculations 
# Hilda started in 2001. 
years<-c(2001:c(2000+length(Combined_)))
################################################################################
# Downloading Master_t200c.dta. We select only time invariant variables 
master_time_invariant <- read_dta(paste0(folder_Australia_2, "Master_t200c.dta"), 
                     col_select = c("xwaveid","sex", "yob", "yrenter", "yrenlst")) %>% 
  # Renaming columns in master_1 (new_name = old_name) %>% 
  dplyr::rename(SEX = sex, 
                BORN_Y =yob, 
                FIRSTOBS_Y= yrenter, 
                LASTOBS_Y = yrenlst) %>% 
  mutate(SEX = as.integer(SEX),
         FIRSTOBS_Y = ifelse(FIRSTOBS_Y<0,-1,as.integer(FIRSTOBS_Y)),
         LASTOBS_Y = ifelse(LASTOBS_Y<0,-1,as.integer(LASTOBS_Y)),
         BORN_Y = ifelse(BORN_Y<0, -1, as.integer(BORN_Y))
         ) %>% 
  mutate(
    SEX = labelled(as.integer(SEX),  # must be numeric
                   labels = c(
                     "male" = 1,
                     "female" = 2,
                     "unknown" = -1
                   ))
  )

# Collect information on interview date and interview status form Combined_ files  
time_variant<-c()
for (which_wave in 1:length(years)) {
  
  gen_emp_hist_variables<-c("xwaveid", paste0(letters[which_wave],"hhhqivw"), 
                            paste0(letters[which_wave],"esbrd"),
                            paste0(letters[which_wave],"hgni"),
                            paste0(letters[which_wave],"mrn"),
                            paste0(letters[which_wave], "tcr"),   # tcr Number of own resident children
                            paste0(letters[which_wave], "tcnr"),  # tcnr Number of own non-resident children
                            paste0(letters[which_wave], "tchad")  # Total children respondent ever had
                            
                            )
  
  # 1. Import the data
  data_emp_x <- read_dta(paste0(folder_Australia_1, Combined_[which_wave]), 
                         col_select = gen_emp_hist_variables)
  
  # Delete first letter from variable names in a downloaded dataset
  names(data_emp_x)[-1] <- substring(names(data_emp_x)[-1], 2) 
  
  data_emp_x$hhhqivw_1<- as.Date(data_emp_x$hhhqivw, format="%d/%m/%Y")
  
  # Set date as in the calendar job - day of interview is rounded to 05,15,25
  data_int_1 <- data_emp_x %>% separate(hhhqivw_1, c('INT_Y', 'INT_M', 'INT_D'))
  data_int_1$INT_D[data_int_1$INT_D> 0  & data_int_1$INT_D<11]<-"05"
  data_int_1$INT_D[data_int_1$INT_D>=11 & data_int_1$INT_D<21]<-"15"
  data_int_1$INT_D[data_int_1$INT_D>=21 & data_int_1$INT_D<31]<-"25"
  data_int_1$INTERVIEW_DATE<-paste(data_int_1$INT_Y, data_int_1$INT_M, data_int_1$INT_D, sep = "-")
  data_int_1<-select(data_int_1, xwaveid, INTERVIEW_DATE)
  
  # merge 
  data_emp_x<-merge(data_int_1, data_emp_x, by = c("xwaveid"))
  data_emp_x$year_wave = years[which_wave]
  
  # save results for each wave 
  time_variant = rbind(time_variant, data_emp_x)
  rm(data_int_1, data_emp_x)
}

INTERVIEW_DATE = time_variant %>% 
  # encode wrong interview dates as missing 
  mutate(INTERVIEW_DATE =  ifelse(INTERVIEW_DATE=="NA-NA-NA", NA,INTERVIEW_DATE)) %>% 
  # encode employment status 
  mutate(
    esbrd = sjlabelled::as_character(esbrd),
    EMP_STATUS = case_when(
        esbrd %in% c("[-10] Non-responding person") ~ -1,
        esbrd %in% c("[1] Employed") ~ 1,
        esbrd %in% c("[2] Unemployed","[3] Not in the labour force") ~ 0),
    # encode INTERVIEW_STATUS
    # [1] fully responsive
    # [2] proxy which include: child respondent, proxy respondent
    INTERVIEW_STATUS = sjlabelled::as_character(hgni),
    INTERVIEW_STATUS = ifelse(INTERVIEW_STATUS=="[0] Interviewed adult",1,2)
    ) %>% select(xwaveid,year_wave,INTERVIEW_DATE,INTERVIEW_STATUS,  EMP_STATUS) %>% 
  mutate(
    INTERVIEW_STATUS = labelled(as.integer(INTERVIEW_STATUS),  # must be numeric
                                labels = c(
                                  "fully responsive" = 1,
                                  "proxy which include: child respondent, proxy respondent" = 2)),
    EMP_STATUS = labelled(as.integer(EMP_STATUS),  # must be numeric
                          labels = c(
                            "not-working" = 0,
                            "working" = 1,
                            "unknown" = -1)))
  

EVER_MARRIED = time_variant %>% group_by(xwaveid) %>% 
  mutate(EVER_MARRIED = ifelse(mrn<0,NA,mrn),
         EVER_MARRIED = case_when(
           EVER_MARRIED >0 ~ 1,
           EVER_MARRIED ==0 ~ 0,
           TRUE ~ NA),
         EVER_MARRIED = sum(EVER_MARRIED, na.rm = T),
         EVER_MARRIED = ifelse(EVER_MARRIED>0,1,0)
         ) %>% 
  select(xwaveid,EVER_MARRIED) %>% distinct() %>% mutate(
    EVER_MARRIED = labelled(as.integer(EVER_MARRIED),  # must be numeric
                            labels = c(
                              "no" = 0,
                              "yes" = 1,
                              "unknown" = -1
                            ))
  )

# ANYCHILD at the last possible interview
# [1] yes
# [2] no
 
# total nr children from the survey (in hilda tcr+tcnr)
tot_kids<-time_variant %>% mutate(
  tcr = ifelse(tcr<0, NA, tcr),
  tcnr = ifelse(tcnr<0, NA, tcnr),
  nr_kids = tcr+tcnr) %>% select(xwaveid, year_wave, nr_kids) %>% 
  group_by(xwaveid) %>% arrange(year_wave, .by_group = T) %>% 
  select(-year_wave) %>% slice(n()) %>% 
  mutate(ANYCHILD= ifelse(nr_kids>0,1, ifelse(nr_kids==0,2,-1))) %>% 
  select(xwaveid, ANYCHILD) %>% 
  mutate(ANYCHILD = labelled(as.integer(ANYCHILD),  # must be numeric
                                 labels = c(
                                   "yes" = 1,
                                   "no" = 2,
                                   "unknown" = -1)))


################################################################################
# mere into one file in long format: list_weights,time_variant
variables_to_long = c("INTERVIEW_STATUS","EMP_STATUS")    

INTERVIEW_DATE = as.data.table(INTERVIEW_DATE)

wide_time_variant = data.table::dcast(INTERVIEW_DATE, xwaveid~ year_wave, value.var="INTERVIEW_DATE")

colnames(wide_time_variant)[-1] <- paste0("INTERVIEW_DATE" ,"_", 2001:max(years))

# transition to wide format
for (i in variables_to_long) {
  
  x = data.table::dcast(INTERVIEW_DATE ,xwaveid~ year_wave, value.var=i)
  
  colnames(x)[-1] <- paste0(i ,"_", 2001:max(years))
  
  wide_time_variant =merge(wide_time_variant, x, by = c("xwaveid"))

  
  rm(x)
}

wide = merge(master_time_invariant, wide_time_variant, by = c("xwaveid")) %>% 
  merge(EVER_MARRIED,by = c("xwaveid")) %>% 
  merge(tot_kids, by = c("xwaveid"))

rm(i,proxy,time_variant,list_weigths,master_long,master_time_invariant,
   wide_time_variant,variables_to_long, tot_kids, EVER_MARRIED)


wide  = wide %>% 
  mutate(
  BORN_Y = labelled(
    BORN_Y,
    labels = c("unknown" = -1)
  ),
  SEX = labelled(
    SEX,
    labels = c("male" = 1, "female" = 2, "unknown" = -1)
  ),
  FIRSTOBS_Y = labelled(
    FIRSTOBS_Y,
    labels = c("unknown" = -1)
  ),
  LASTOBS_Y = labelled(
    LASTOBS_Y,
    labels = c("unknown" = -1)
  )
)


# Add variable labels (descriptions)
var_label(wide$BORN_Y)         <- "Year of birth (-1 = unknown)"
var_label(wide$SEX)            <- "Biological sex"
var_label(wide$FIRSTOBS_Y)     <- "Year of first interview (-1 = unknown)"
var_label(wide$LASTOBS_Y)      <- "Year of last interview (-1 = unknown)"
var_label(wide$EVER_MARRIED)   <- "Ever married?"
var_label(wide$ANYCHILD)       <- "Any child at the last possible interview?"

# --- label all INTERVIEW_DATE# variables ---
for (v in grep("^INTERVIEW_DATE", names(wide), value = TRUE)) {
  var_label(wide[[v]]) <- "Date of interview (NA = unknown)"
}

# --- label all INTERVIEW_STATUS# variables ---
for (v in grep("^INTERVIEW_STATUS", names(wide), value = TRUE)) {
  var_label(wide[[v]]) <- "Type of survey response"
}

# --- label all EMP_STATUS# variables ---
for (v in grep("^EMP_STATUS", names(wide), value = TRUE)) {
  var_label(wide[[v]]) <- "Employment status"
}

################################################################################
# save file 

saveRDS(wide, "output/master_hilda_wide.rds")

x = ls()
x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)















