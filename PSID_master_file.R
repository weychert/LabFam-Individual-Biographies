# read UMich crosswalk from installed file
r = system.file(package="psidR")
# cwf = openxlsx::read.xlsx(file.path(r,"psid-lists","psid.xlsx"))
# or download directly from the webpage but it takes time 
cwf <- openxlsx::read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")
source("00_setting_work_space.R")

files <- list.files(path = folder_family_files, pattern = ".zip$")
outDir <- paste0(folder_family_files , "/new_family_folder_psid")


all_years <- data.frame(
  # Combine all PSID survey years:
  #   - 1968 through 1997 (annual surveys)
  #   - 1999 through 2021 in 2-year intervals (biennial surveys)
  year_wave = c(1968:1997, seq(1999, 2021, 2)),
  
  # Create a simple sequential ID for each survey year (1, 2, 3, ...)
  # This can be useful for indexing or merging with wave-based datasets
  syear = 1:length(c(1968:1997, seq(1999, 2021, 2)))
)

all_years_same = readRDS(paste0(outDir,"/ind2021.rds")) %>% 
  mutate(pid  = ER30001*1000 + ER30002) %>% 
  select("pid", 
         "ER32052", 	# "YEAR THIS INDIVIDUAL'S COHORT BEGAN"
         "ER32006",	  # followable "WHETHER SAMPLE OR NONSAMPLE"
         "ER32022",   # LIVE BIRTHS TO THIS INDIVIDUAL
         "ER32000",   # sex of individual 
         "ER32010",   # (All Years) 	"PERSON # OF MOTHER
         "ER32017",   # (All Years) 	"PERSON # OF FATHER",
         "ER32016",   # 1968 ID OF FATHER
         "ER32009",   # 1968 ID OF MOTHER
         "ER30001",   # id (interview) number for sample
         "ER32034"    # EVER_MARRIED
  ) %>% 
  rename(FIRSTOBS_Y = ER32052,
         tot_kids = ER32022,
         SEX =  ER32000,
         nr_mother = "ER32010",   
         nr_father = "ER32017", 
         pid_mother = "ER32016",   
         pid_father = "ER32009") %>% 
  mutate(
    tot_kids = ifelse(tot_kids %in% c(98,99),NA, tot_kids)) %>% 
  mutate(sample_family = 
           case_when(
             ER30001 < 3000 ~ "SRC Sample",
             ER30001 > 5000 & ER30001 < 7000 ~ "SEO Sample",
             ER30001 > 3000 & ER30001 < 5000 ~ case_when(
               ER30001 >= 3001 & ER30001 <= 3441 ~ "Immigrant Sample (1997)",
               ER30001 >= 3442 & ER30001 <= 3511 ~ "Immigrant Sample (1999)",
               ER30001 >= 4001 & ER30001 <= 4700 ~ "Immigrant Sample (2017)",
               ER30001 >= 4701 & ER30001 <= 4851 ~ "Immigrant Sample (2019)"),
             ER30001 > 7000 & ER30001 < 9309 ~ case_when(
               ER30001 >= 7001 & ER30001 <= 9043 ~ "Latino Sample (1990)",
               ER30001 >= 9044 & ER30001 <= 9308 ~ "Latino Sample (1992)"),
             TRUE ~ "Unknown"),
         sample = ifelse(sample_family %in% c("Latino Sample (1992)","Immigrant Sample (1997)",
                                              "Immigrant Sample (1999) Immigrant Sample",
                                              "Immigrant Sample (2019)","Latino Sample (1990)",
                                              "Immigrant Sample (1999)","Immigrant Sample (2017)"), 
                         "Immigrant or Latino", sample_family)) %>% 
  mutate(
    ER32006 = case_when(ER32006 == 0 ~ "This individual is nonsample and not part of the elderly group",
                        ER32006 == 1 ~"This individual is original sample",
                        ER32006 == 2 ~"This individual is born-in sample",
                        ER32006 == 3 ~"This individual is moved-in sample",
                        ER32006 == 4 ~"This individual is joint inclusion sample",
                        ER32006 == 5 ~"This individual was a followable nonsample parent",
                        ER32006 == 6 ~ "This individual is nonsample elderly"),
    followable = ifelse(ER32006 %in% c("This individual is nonsample elderly",
                                       "This individual is nonsample and not part of the elderly group",
                                       "This individual is joint inclusion sample"), 
                        "non-followable",ER32006)) %>% 
  mutate(if_has_marr_hist = ifelse(ER32034 %notin% c(99,98), "has marr hist", "no marr hist"),
         marriage_nr = ifelse(ER32034 %in% c(99,98), NA, as.numeric(as.character(ER32034))),
  ) %>% select(-ER32034) %>% 
  mutate(EVER_MARRIED = ifelse(marriage_nr==0, 0, ifelse(marriage_nr>0,1,NA))) %>% 
  select(-c("ER32006", "sample_family"))

# what variables do we have in here

all_years_same = all_years_same %>% 
  select(pid, FIRSTOBS_Y, tot_kids, SEX, sample, followable,EVER_MARRIED) %>% 
  mutate(ANYCHILD = ifelse(tot_kids==0, 2, ifelse(tot_kids>0,1,NA))) %>% 
  select(pid,  SEX, FIRSTOBS_Y, sample, followable, ANYCHILD, EVER_MARRIED) %>% 
  mutate(SEX = ifelse(SEX %notin% c(1,2), -1,SEX))


################################################################################
# "BORN_M", "BORN_Y", "EMP_STATUS"

all_years = c(1968:1997, seq(1999,2021,2))

vars = list(
  year_birth = psidR::getNamesPSID("ER30404", cwf, years = all_years), # year_birth
  month_birth = psidR::getNamesPSID("ER30403", cwf, years = all_years), # month_birth
  rel_hsh = psidR::getNamesPSID("ER33703", cwf, years = all_years), # rel_hsh
  age = psidR::getNamesPSID("ER30004", cwf, years = all_years), # age
  EMP_STATUS = psidR::getNamesPSID("ER30293", cwf, years = all_years), # emp_status 
  INTERVIEW_NUMBER =  psidR::getNamesPSID("ER30001", cwf, years =all_years )
 
) 

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
 
emp_stat <- function(variables) {
  variables = as.character(variables)
  variables =
    case_when(
      variables ==1~ "Working",
      variables ==2~ "laid off",
      variables ==3~ "unemployed",
      variables ==4~ "Retired",
      variables ==5~ "disabled",
      variables ==6~ "Keeping House",
      variables ==7~ "Student",
      variables ==8~ "Other",
      variables ==9~ "refused",
      variables ==0~ "Inap",
      variables %in% c(22,99)~ NA_character_,
      TRUE ~ variables
    )
  
}


total_across_waves = total_across_waves %>%
  mutate(
    BORN_M = ifelse(round(month_birth) %in% c(0,99),NA,round(month_birth)),
    BORN_Y = ifelse(round(year_birth) %in% c(0,9999, 969),NA,round(year_birth)),
    age = ifelse(round(age) %in% c(999), NA, age),
    BORN_Y = ifelse(is.na(BORN_Y) & age>0, year_wave-age,BORN_Y)
  ) %>% 
  mutate(
    EMP_STATUS = emp_stat(EMP_STATUS),
    EMP_STATUS = ifelse(EMP_STATUS %in% c("Working"), "working",
                                ifelse(EMP_STATUS %in% c("laid off", "Retired",
                                                         "Other", "Keeping House", 
                                                         "disabled", "unemployed", 
                                                         "Student"), "not-working", NA)))



# total_across_waves %>% group_by(year_wave, !is.na(EMP_STATUS)) %>% summarise(n()) %>% filter(`!is.na(EMP_STATUS)`!=F)

born = total_across_waves %>% group_by(pid) %>% 
  summarise(BORN_M = max(BORN_M, na.rm = T), BORN_Y = max(BORN_Y, na.rm = T)) %>% 
  mutate(BORN_M = if_else(is.infinite(BORN_M), NA, BORN_M),
         BORN_Y = if_else(is.infinite(BORN_Y), NA, BORN_Y))

born %>% summarise(min(BORN_Y, na.rm = T), max(BORN_Y, na.rm = T))
born %>% summarise(min(BORN_M, na.rm = T), max(BORN_M, na.rm = T))


all_years_same = all_years_same %>% merge(born, by = c("pid"), all = T)

total_across_waves_1 = total_across_waves %>% select(pid, year_wave, rel_hsh, EMP_STATUS, INTERVIEW_NUMBER) %>% 
  mutate(rel_hsh  = ifelse(rel_hsh %in% c(1,10), "HE", ifelse(rel_hsh %in% c(2,20), "SP", "other")))



#######################head################################################################################
# from family files
# "INTERVIEW_DATE", "INTERVIEW_STATUS", "drop"

all_years = c(1968:1997, seq(1999,2021,2))
years_add = data.frame(
  year_wave = all_years,
  INTERVIEW_NUMBER = psidR::getNamesPSID("V3", cwf, years = all_years)$variable,
  interview_date = psidR::getNamesPSID("V99", cwf, years = all_years)$variable,
  drop = psidR::getNamesPSID("ER10005H", cwf, years = all_years)$variable,
  interview_day = psidR::getNamesPSID("ER10006", cwf, years = all_years)$variable,
  interview_month = psidR::getNamesPSID("ER10005", cwf, years = all_years)$variable,
  interview_year = psidR::getNamesPSID("ER10007", cwf, years = all_years)$variable
)

master_file_psid<-c()
for (i in all_years) {
  
  # Filter and transpose
  years_add_1 <- years_add[years_add$year_wave == i, ] %>%
    select(-year_wave) %>%
    t()
  
  # Convert to data frame (optional, so you can use rownames)
  years_add_1 <- as.data.frame(years_add_1)
  
  # Add row names
  rownames(years_add_1) <- colnames(years_add)[colnames(years_add) != "year_wave"]
  
  years_add_1 <- years_add_1 %>% 
    filter(complete.cases(.))

  x = readRDS((paste0(outDir,"/FAM",i,".rds"))) %>% 
    select(years_add_1[!is.na(years_add_1)])
    
  names(x)<-c(rownames(years_add_1))
  
  
  # add year wave
  x$year_wave<- i
  
  x %>% head
  
  if (i>=1968 &i<=1973) {
    
    x <-x %>% mutate(INT_D = case_when(
      interview_date == 1 ~ 7,
      interview_date == 2 ~ 14,
      interview_date == 3 ~ 7,
      interview_date == 4 ~ 21,
      interview_date == 5 ~ 7,
      interview_date == 6 ~ 21,
      interview_date == 7 ~ 15,
      interview_date == 8 ~ 1,
      interview_date == 9 ~ NA),
      
      INT_M = case_when(
        interview_date == 1 ~ 3,
        interview_date == 2 ~ 3,
        interview_date == 3 ~ 4,
        interview_date == 4 ~ 4,
        interview_date == 5 ~ 5,
        interview_date == 6 ~ 5,
        interview_date == 7 ~ 6,
        interview_date == 8 ~ 7,
        interview_date == 9 ~ NA
      ),
      INT_Y = i,
      interview_date = as.Date(paste(INT_Y, INT_M, INT_D, sep = "-"), format = "%Y-%m-%d")
    )%>% select(-c("INT_Y", "INT_M", "INT_D"))
    
  }
  
  if (i>=1974 & i<=1979) {
    
    x <-x %>% mutate(INT_D = case_when(
      interview_date == 1 ~ 7,
      interview_date == 2 ~ 21,
      interview_date == 3 ~ 11,
      interview_date == 4 ~ 23,
      interview_date == 5 ~ 9,
      interview_date == 6 ~ 23,
      interview_date == 7 ~ 16,
      interview_date == 8 ~ 1,
      interview_date == 9 ~ NA),
      
      INT_M = case_when(
        interview_date == 1 ~ 3,
        interview_date == 2 ~ 3,
        interview_date == 3 ~ 4,
        interview_date == 4 ~ 4,
        interview_date == 5 ~ 5,
        interview_date == 6 ~ 5,
        interview_date == 7 ~ 6,
        interview_date == 8 ~ 7,
        interview_date == 9 ~ NA
      ),
      INT_Y = i,
      interview_date = as.Date(paste(INT_Y, INT_M, INT_D, sep = "-"), format = "%Y-%m-%d")) %>% 
      select(-c("INT_Y", "INT_M", "INT_D"))
  }
  
  if (i>=1980 &i<=1996) {
    
    x <- x %>%
      mutate(
        interview_date = ifelse(interview_date == "9999", NA, interview_date),
        interview_date = ifelse(nchar(interview_date) == 3, paste0("0", interview_date), interview_date),
        # This four digit variable represents the month and day the interview was taken. 
        # The first two digits represent the month and the possible range is 03-10 (March-October) 
        INT_Y = i,
        INT_M = substr(interview_date, 1, 2),
        # The last two digits represent the day of the month which has a possible range of 01-31.
        INT_D = substr(interview_date, nchar(interview_date) - 1, nchar(interview_date)),
        interview_date = as.Date(paste(INT_Y,INT_M,INT_D, sep = "-"), format = "%Y-%m-%d"))%>% 
      select(-c("INT_Y", "INT_M", "INT_D"))
    
  }
  
  if (i>1996){
    x<-x %>% mutate(interview_date = as.Date(paste(interview_year, interview_month, interview_day, sep = "-"), format = "%Y-%m-%d"))%>% 
      select(-c("interview_year", "interview_month", "interview_day"))
  }
  
   
  
  # append next wave
  master_file_psid <-bind_rows(master_file_psid, x)
  
  rm(x)
  print(i)
  
}

master_file_psid = master_file_psid %>% rename(INTERVIEW_DATE = interview_date)

################################################################################
master_psid = merge(total_across_waves_1, master_file_psid, by = c("INTERVIEW_NUMBER", "year_wave")) %>% 
  merge(all_years_same, by = c("pid"), all = T) %>%  
  # LASTOBS_Y and drop
  group_by(pid) %>% 
  arrange(year_wave, .by_group = T) %>% 
  # check if this is ok maybe better from int date?
  mutate(LASTOBS_Y = max(year_wave)) %>% 
  # rename(INTERVIEW_DATE = interview_date) %>% 
  group_by(pid) %>%
  mutate(not_dropped_1997 = any(drop == 5, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(drop = if_else(not_dropped_1997, "not drop in 1997", "drop in 1997")) %>% 
  mutate(
    EMP_STATUS = case_when(
      EMP_STATUS %in% c("working") ~ 1,
      EMP_STATUS %in% c("not-working") ~ 0),
    EMP_STATUS = labelled(as.integer(EMP_STATUS),  # must be numeric
                          labels = c(
                            "not-working" = 0,
                            "working" = 1,
                            "unknown" = -1))
    )

master_psid_1 = master_psid %>% select(pid, SEX, BORN_M, BORN_Y, FIRSTOBS_Y, EVER_MARRIED,ANYCHILD, LASTOBS_Y,sample, followable, drop) %>% distinct()

length(unique(master_psid_1$pid))-nrow(master_psid_1)

# EMP_STATUS, INTERVIEW_DATE, rel_hsh
master_psid_2 = master_psid %>% select(pid, year_wave,EMP_STATUS, INTERVIEW_DATE,INTERVIEW_NUMBER, rel_hsh) %>% 
  arrange(pid, year_wave) %>%
  pivot_wider(
    id_cols = pid,
    names_from = year_wave,
    values_from = c(EMP_STATUS, INTERVIEW_DATE,INTERVIEW_NUMBER, rel_hsh),
    names_glue = "{.value}_{year_wave}",
    values_fill = NA
  )

master_psid_wide = merge(master_psid_1, master_psid_2, by = c("pid"))

master_psid_wide = master_psid_wide %>% 
  mutate(
    ANYCHILD = labelled(as.integer(ANYCHILD),  # must be numeric
                        labels = c(
                          "yes" = 1,
                          "no" = 2,
                          "unknown" = -1)),
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
      labels = c("unknown" = -1)),
    EVER_MARRIED = labelled(as.integer(EVER_MARRIED),  # must be numeric
                            labels = c(
                              "no" = 0,
                              "yes" = 1,
                              "unknown" = -1
                            ))
    
    
  )

# Add variable labels (descriptions)
var_label(master_psid_wide$BORN_Y)         <- "Year of birth (-1 = unknown)"
var_label(master_psid_wide$BORN_M)         <- "Month of birth (-1 = unknown)"
var_label(master_psid_wide$SEX)            <- "Biological sex"
var_label(master_psid_wide$FIRSTOBS_Y)     <- "Year of first interview (-1 = unknown)"
var_label(master_psid_wide$LASTOBS_Y)      <- "Year of last interview (-1 = unknown)"
var_label(master_psid_wide$ANYCHILD)       <- "Any child at the last possible interview?"
var_label(master_psid_wide$EVER_MARRIED)   <- "Ever married?"

# --- label all INTERVIEW_DATE# variables ---
for (v in grep("^INTERVIEW_DATE", names(master_psid_wide), value = TRUE)) {
  var_label(master_psid_wide[[v]]) <- "Date of interview (NA = unknown)"
}

# --- label all INTERVIEW_STATUS# variables ---
for (v in grep("^INTERVIEW_STATUS", names(master_psid_wide), value = TRUE)) {
  var_label(master_psid_wide[[v]]) <- "Type of survey response"
}

# --- label all EMP_STATUS# variables ---
for (v in grep("^EMP_STATUS", names(master_psid_wide), value = TRUE)) {
  var_label(master_psid_wide[[v]]) <- "Employment status"
}

saveRDS(master_psid_wide,"output/master_psid_wide.rds")

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)

















