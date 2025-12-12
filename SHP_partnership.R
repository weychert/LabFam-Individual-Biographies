######################## SHP partnership #######################################
# 1. Setup and Data Loading
# a. Setting Up Workspace
source("00_setting_work_space.R")

# prepare working directory for yearly survey files 
folder_waves_available<-as.data.frame(list.files(folder_shp_1)) %>% 
  rename(folder_name = `list.files(folder_shp_1)`)

folder_waves_available[c('wave_nr', 'year_wave')] <-str_split_fixed(folder_waves_available$folder_name, "_", 2)

folder_waves_available<-folder_waves_available %>% 
  mutate(year_wave = as.numeric(year_wave)) %>% 
  arrange(year_wave) %>% 
  mutate(wave_nr = ifelse(year_wave == 1999, "99", str_remove(year_wave, "20")))

################################################################################
# 1.	Data Transformation and Cleaning of three data sources: shpiii_cs_user.dta, shp0_bvcs_user.dta and main from survey 

# shpiii_cs_user Information on partner relationships and changes in civil status 
# in this file we do not have months of start and end of relationship 

# Variable definition: 
# idpers: the respondent is present in the file as many times as relational episodes.
# ep_relationship means the number of relational episodes the respondent has experienced: The max is 10 episodes.
# relationship_a means the beginning of the episode, 
# relationship_b the end of the episode.

# relationship_c is the letter that refers to a partner; the goal was to identify whether people split 
# with a partner but, years later, get married to this partner after a spell of years of separation.

# You might see several types for one spell because during an episode of a certain length, 
# a participant might have experienced a marriage, then a separation, and a divorce. 
# The civstat variables describe that.
# Civstat.1.a: the year that the participant experienced a civil status change
# Civstat.1.typ: refers to the kind of status change
# Civstat.2.a: refers to the year of a second civil status change
# Civstat.2.typ: refers to the type of the second civil status change.
# The same logic applies to the civstat.3 variables.

# 1.1 shpiii_cs_user.dta - Data Processing
# a) change the format of data to character and re code missing values
# no missing years no negative values :)
SHP0_cs_user <- read_dta(paste0(folder_retro_shp_2,"shpiii_cs_user.dta")) %>% 
  mutate(
    spell_nr = ep_relationship,
    START_Y = relationship_a,
    END_Y = relationship_b,
    type_1  = sjlabelled::as_character(civstat_1_typ),
    type_2  = sjlabelled::as_character(civstat_2_typ),
    type_3  = sjlabelled::as_character(civstat_3_typ),
    type_4  = sjlabelled::as_character(civstat_4_typ),
    type_1 = ifelse(type_1 =="inapplicable", NA, type_1),
    type_2 = ifelse(type_2 =="inapplicable", NA, type_2),
    type_3 = ifelse(type_3 =="inapplicable", NA, type_3),
    type_4 = ifelse(type_4 =="inapplicable", NA, type_4),
    change_1 = ifelse(civstat_1_a==-33,NA,civstat_1_a), # -33       inapplicable
    change_2 = ifelse(civstat_2_a==-33,NA,civstat_2_a), # -33       inapplicable
    change_3 = ifelse(civstat_3_a==-33,NA,civstat_3_a), # -33       inapplicable
    change_4 = ifelse(civstat_4_a==-33,NA,civstat_4_a), # -33       inapplicable
    change_1 = ifelse(civstat_1_a<0,-1,civstat_1_a),    # -44 missing; -22 no answer/ refusal; -11 unreadable
    change_2 = ifelse(civstat_2_a<0,-1,civstat_2_a),    # -44 missing; -22 no answer/ refusal; -11 unreadable
    change_3 = ifelse(civstat_3_a<0,-1,civstat_3_a),    # -44 missing; -22 no answer/ refusal; -11 unreadable
    change_4 = ifelse(civstat_4_a<0,-1,civstat_4_a)     # -44 missing; -22 no answer/ refusal; -11 unreadable
    ) %>% 
  select(idpers,spell_nr,START_Y,END_Y,paste0("type_",1:3),paste0("change_",1:3))

SHP0_cs_user[SHP0_cs_user$idpers==62212101,]


# b) We create Variables MARR_Y,DIV_Y,SEP_Y,WIDOW_Y, PARTNERSHIP_STATUS in this file we have cohabitation, marriage
SHP0_cs_user = SHP0_cs_user %>% 
  mutate(
    MARR_Y = case_when(type_1 == "married" ~ change_1,
                       type_2 == "married" ~ change_2,
                       type_3 == "married" ~ change_3),
    
    DIV_Y = case_when(type_1 == "divorced" ~ change_1,
                      type_2 == "divorced" ~ change_2,
                      type_3 == "divorced" ~ change_3),
    
    SEP_Y = case_when(type_1 == "separated from spouse/partner" ~ change_1,
                      type_2 == "separated from spouse/partner" ~ change_2,
                      type_3 == "separated from spouse/partner" ~ change_3),
    WIDOW_Y = case_when(type_1 == "widowed" ~ change_1,
                        type_2 == "widowed" ~ change_2,
                        type_3 == "widowed" ~ change_3)) %>% 
  mutate(PARTNERSHIP_STATUS = case_when(
    START_Y<=MARR_Y ~ "marriage",
    !is.na(START_Y) & is.na(MARR_Y)~"cohabitation",
    is.na(type_1) & is.na(type_2) & is.na(type_3)~"cohabitation",)) %>% 
  select(idpers,spell_nr,PARTNERSHIP_STATUS,START_Y,END_Y,MARR_Y,SEP_Y,DIV_Y,WIDOW_Y)

SHP0_cs_user[SHP0_cs_user$idpers==62212101,]


# c) We need to also separate rows cohabitation and marriage: shp team identified one spell as relationship with one person
# lib decided to split spells defined as being in a relationship with one person into two
# if cohabitation changed to marriage

SHP0_cs_user_coh = SHP0_cs_user %>% 
  filter(START_Y<MARR_Y & START_Y!=-1) %>% 
  mutate(
    END_Y = MARR_Y,
    MARR_Y = NA,
    PARTNERSHIP_STATUS = "cohabitation") %>% 
  # if PARTNERSHIP_STATUS started with marriage no prior cohabitation before marriage 
  mutate(START_Y = ifelse(START_Y<=MARR_Y & !is.na(MARR_Y), MARR_Y, START_Y))

SHP0_cs_user_coh[SHP0_cs_user_coh$idpers==62212101,]


SHP0_cs_user_total = bind_rows(SHP0_cs_user,SHP0_cs_user_coh) %>% 
  group_by(idpers) %>% 
  arrange(START_Y, .by_group = T) %>% 
  select(idpers, PARTNERSHIP_STATUS,START_Y, END_Y,DIV_Y,WIDOW_Y) %>% group_by(idpers) %>% 
  arrange(START_Y, END_Y, .by_group = T) %>% 
  mutate(across(c("START_Y", "END_Y"), as.integer)) %>% 
  mutate(
    check = T,
    # check = lag(PARTNERSHIP_STATUS) == "cohabitation" & PARTNERSHIP_STATUS == "marriage",
    START_Y = if_else(!is.na(lag(END_Y)) , lag(END_Y), START_Y),
    # check = lead(PARTNERSHIP_STATUS) == "marriage" & PARTNERSHIP_STATUS == "cohabitation",
    END_Y = if_else(check ==T &!is.na(check), END_Y-1, END_Y),
  ) %>% 
  # create date START_PARTNERSHIP_STATUS_date and END_UNION_date which is needed for function neatRanges::expand_dates()
  mutate(
    START_PARTNERSHIP_STATUS_date = if_else(!is.na(START_Y), as.Date(paste0(as.numeric(START_Y), "-01-01")), as.Date(NA)),
    END_UNION_date = if_else(!is.na(END_Y),
                             as.Date(paste0(as.numeric(END_Y), "-12-31")), as.Date(NA))) %>% 
  filter(END_UNION_date>=START_PARTNERSHIP_STATUS_date) # loose 19 obs

SHP0_cs_user_total[SHP0_cs_user_total$idpers== 62212101, ]

SHP0_cs_user_total %>% mutate(
  dur =  END_Y - START_Y==0
) %>% group_by(dur) %>% summarise(n=n())%>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))



SHP0_cs_user_total %>% mutate(
  dur =  END_Y - START_Y<0
) %>% filter(dur==T)



SHP0_cs_user_total[SHP0_cs_user_total$idpers==60022101,]

SHP0_cs_user[SHP0_cs_user$idpers==60022101,]



# d) We add periods of single hood from age 15 up to year 2013

SHP0_cs_user_expanded = neatRanges::expand_dates(SHP0_cs_user_total, 
                   start_var = "START_PARTNERSHIP_STATUS_date", 
                   end_var = "END_UNION_date",
                   vars_to_keep = c("idpers","PARTNERSHIP_STATUS","START_Y", 
                                    "END_Y","DIV_Y","WIDOW_Y"), unit = "year") %>%
  mutate(year = as.numeric(format(as.Date(Expanded), "%Y")))

all_years <- data.frame(
  idpers = rep(unique(SHP0_cs_user_expanded$idpers), each = length(1924:2013)),
  # the oldest individual is at age 15 in 1924 BORN_Y = 1909
  year = rep(1924:2013, times = length(unique(SHP0_cs_user_expanded$idpers))))

# Merge with expanded_data to add empty rows for periods of single hood
SHP0_cs_user_expanded <- merge(all_years,SHP0_cs_user_expanded, by = c("idpers", "year"), all.x = TRUE) %>% 
  mutate(PARTNERSHIP_STATUS = ifelse(is.na(PARTNERSHIP_STATUS), "no union",PARTNERSHIP_STATUS))

# add year of birth to start observing all individuals form age of 15
master_shp = readRDS("output/master_shp_wide.rds") %>% 
  select(idpers, BORN_Y)

SHP0_cs_user_expanded = merge(SHP0_cs_user_expanded,master_shp, by = c("idpers"), all.x = T) %>% 
  # Data set includes only observations of individuals aged 15 or older
  mutate(
    age = year - BORN_Y) %>% filter(age >=15) %>% 
  # Calculating Partnership Spells to fill missing start and end
  group_by(idpers) %>% arrange(year,.by_group = T) %>% 
  mutate(check = lag(PARTNERSHIP_STATUS)!=PARTNERSHIP_STATUS,
         check = ifelse(is.na(check),T,check),
         spellnr = cumsum(check)) %>% 
  ungroup() %>% group_by(idpers, spellnr) %>% 
  mutate(
    # Correct variable fill missing start or end years
    START_Y = ifelse(is.na(START_Y),min(year), START_Y),
    END_Y = ifelse(is.na(END_Y), max(year), END_Y),
    DIVORCE_Y = DIV_Y,
    DIVORCE = ifelse(is.na(DIVORCE_Y),0,DIVORCE_Y)) %>% 
  select("idpers","year","spellnr","PARTNERSHIP_STATUS","START_Y", "END_Y","DIVORCE_Y","WIDOW_Y")%>% 
  mutate(source_flag_2013_cs = "retro_2013_cs") 

# result we have one row one year

rm("SHP0_cs_user","SHP0_cs_user_coh", "SHP0_cs_user_total")

################################################################################
# 1.2 shp0_bvcs_user.dta - Data Processing
# shp0_bvcs_user Changes in civil status
# It is the Biographical Civil Status file for the biographic study conducted in 2002.
# The structure corresponds to the biographical structure, i.e., 
# the date of change for the civil status of the SHP participants.
# bvcs_idx                 change of civil status: index
# bvcs001         change of civil status: marriage: year
# bvcs002       change of civil status: separation: year
# bvcs003          change of civil status: divorce: year
# bvcs004        change of civil status: widowhood: year
# bvcs005                          single, never married
# in this file we have no cohabitation, but we have never married people
# one row one spell: relationship with one person 

# a) # create variables START_Y, END_Y, PARTNERSHIP_STATUS, MARR_Y,SEP_Y, DIV_Y,WIDOW_Y,never_married
SHP0_bvwl_user <- read_dta(paste0(folder_retro_shp_1, "shp0_bvcs_user.dta")) %>% 
  rename(spell_PARTNERSHIP_STATUS  = bvcs_idx, # change of civil status: index
         MARR_Y = bvcs001,        # change of civil status: marriage: year
         SEP_Y = bvcs002,         # change of civil status: separation: year
         DIV_Y = bvcs003,         # change of civil status: divorce: year
         WIDOW_Y = bvcs004,       
         never_married= bvcs005   #  single, never married
         ) %>% 
  # delete missing
  mutate(
  MARR_Y= ifelse(MARR_Y==-3, NA, MARR_Y),    # -3 inapplicable
  SEP_Y = ifelse(SEP_Y==-3, NA, SEP_Y),      # -3 inapplicable       
  DIV_Y = ifelse(DIV_Y==-3, NA, DIV_Y),      # -3 inapplicable       
  WIDOW_Y = ifelse(WIDOW_Y==-3, NA, WIDOW_Y),# -3 inapplicable
  MARR_Y= ifelse(MARR_Y<0, -1, MARR_Y),      # -8 other error; -7 filter error; -2 no answer;;# -1 does not know
  SEP_Y = ifelse(SEP_Y<0, -1, SEP_Y),        # -8 other error; -7 filter error; -2 no answer;;# -1 does not know         
  DIV_Y = ifelse(DIV_Y<0, -1, DIV_Y),        # -8 other error; -7 filter error; -2 no answer;;# -1 does not know          
  WIDOW_Y = ifelse(WIDOW_Y<0, -1, WIDOW_Y),  # -8 other error; -7 filter error; -2 no answer;;# -1 does not know 
  START_Y = MARR_Y,
  END_Y = coalesce(SEP_Y,DIV_Y,WIDOW_Y),
  PARTNERSHIP_STATUS = ifelse(never_married == 1, "no union","marriage")) %>% 
  group_by(idpers) %>% arrange(START_Y, .by_group = T) %>% 
  rename(DIVORCE_Y = DIV_Y) %>% 
  # filter those never married
  filter(never_married %notin%  c(1, -2)) %>% 
  filter(!is.na(START_Y)) %>% 
  select(idpers, spell_PARTNERSHIP_STATUS,PARTNERSHIP_STATUS,START_Y, END_Y,DIVORCE_Y,WIDOW_Y) %>% 
  mutate(END_Y = ifelse(is.na(END_Y), 2002,END_Y))
  

SHP0_bvwl_user # end year as divorce year 


SHP0_bvwl_user %>% mutate(
  dur =  END_Y==-1 & DIVORCE_Y>0
) %>% group_by(dur) %>% summarise(n=n())%>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))






# b) expand to years to add periods of single hood

SHP0_bvwl_user_non_missing = SHP0_bvwl_user %>% 
  # filter out inconsistent dates START_PARTNERSHIP_STATUS_date<=END_UNION_date
  filter(START_Y>0 & END_Y>0) %>% 
  mutate(END_Y = ifelse(END_Y<0, 2002,END_Y)) %>% 
  mutate(
    START_PARTNERSHIP_STATUS_date = if_else(!is.na(START_Y), as.Date(paste0(as.numeric(START_Y), "-01-01")), as.Date(NA)),
    END_UNION_date = if_else(!is.na(END_Y), as.Date(paste0(as.numeric(END_Y), "-01-01")), as.Date(NA))) %>% 
  filter(START_PARTNERSHIP_STATUS_date<=END_UNION_date)
  
### those with missing

SHP0_bvwl_user_missing = SHP0_bvwl_user %>% 
  # filter out inconsistent dates START_PARTNERSHIP_STATUS_date<=END_UNION_date
  filter(START_Y<0 | END_Y<0)


SHP0_bvwl_user_expanded = neatRanges::expand_dates(SHP0_bvwl_user_non_missing, 
         start_var = "START_PARTNERSHIP_STATUS_date", 
         end_var = "END_UNION_date",
         vars_to_keep = c("idpers","spell_PARTNERSHIP_STATUS", 
                          "PARTNERSHIP_STATUS","START_Y", "END_Y","DIVORCE_Y","WIDOW_Y"), 
         unit = "year") %>%
  mutate(year = as.numeric(format(as.Date(Expanded), "%Y")))

min =  1924 # for the oldest participant
max = max(SHP0_bvwl_user$END_Y, na.rm=T)   # 2002

all_years <- data.frame(
  idpers = rep(unique(SHP0_bvwl_user$idpers), each = length(min:max)),
  year = rep(min:max, times = length(unique(SHP0_bvwl_user$idpers))))

master_shp = readRDS("output/master_shp_wide.rds") %>%
  select(idpers, BORN_Y) %>% filter(idpers %in% unique(SHP0_bvwl_user$idpers)) %>%
  as.data.frame()

master_shp_1 <- read_dta(paste0(folder_retro_shp_1, "shp0_bvcs_user.dta")) %>% 
  rename(spell_PARTNERSHIP_STATUS  = bvcs_idx, # change of civil status: index
         MARR_Y = bvcs001,        # change of civil status: marriage: year
         SEP_Y = bvcs002,         # change of civil status: separation: year
         DIV_Y = bvcs003,         # change of civil status: divorce: year
         WIDOW_Y = bvcs004,       #  single, never married
         never_married= bvcs005) %>%
  # filter those never married
  filter(never_married %in%  c(1, -2)) %>% select(idpers) %>% 
  merge(master_shp, by = c("idpers"), all.x = T)


SHP0_bvwl_user_expanded  = 
  merge(all_years,SHP0_bvwl_user_expanded , by = c("idpers", "year"), all.x = TRUE) %>% 
  merge(master_shp_1, by = c("idpers"), all.x = T) %>% 
  bind_rows(SHP0_bvwl_user_missing) %>% 
  mutate(PARTNERSHIP_STATUS = ifelse(is.na(PARTNERSHIP_STATUS), "no union",PARTNERSHIP_STATUS)) %>% 
  select(-BORN_Y) %>% merge(master_shp, by = c("idpers"), all.x = T ) %>% 
  # Data set includes only observations of individuals aged 15 or older
  mutate(age = year - BORN_Y) %>% 
  filter(age >=15) %>% 
  # Calculating Partnership Spells
  group_by(idpers) %>% arrange(year,.by_group = T) %>% 
  mutate(check = lag(PARTNERSHIP_STATUS)!=PARTNERSHIP_STATUS,
         check = ifelse(is.na(check),T,check),
         spellnr = cumsum(check)) %>% 
  ungroup() %>% group_by(idpers, spellnr) %>% 
  mutate(
    # Correct variable fill missing start or end years
    START_Y = ifelse(is.na(START_Y),min(year), START_Y),
    END_Y = ifelse(is.na(END_Y), max(year), END_Y)) %>% 
  select("idpers","year","spellnr","PARTNERSHIP_STATUS","START_Y", "END_Y","DIVORCE_Y","WIDOW_Y") %>% 
  mutate(source_flag_2002_bvwl = "retro_2002_bvwl") 

rm(SHP0_bvwl_user, master_shp)

################################################################################
# 3.3 Yearly Survey Data - Data Processing
# in here we have questions that potentially allow for month of start of event
# we have also PARTNER_ID
# list of useful variables
# CIVSTA		Civil status in year of interview
# COHAST		Cohabitor Status
# YCOUPLE		Partner or spouse: Since when: Corrected year
# PD17		Civil status: Year of most recent
# PD17M		Civil status: Month of most recent
# PD200		Civil status change: Month
# PD201		Civil status change: Year
# PD29		Partner: Yes, no
# PD31		Partner or spouse: Since when: Year
# PD31C		Partner: Since when: year
# PD32		Partner or Spouse: Month, if together since less than one year
# PD32A		Partner: Since when: check
# PD32C		Partner: Since when: month
# PD32M		Partner or spouse: Since when

# a) read survey data 
partner_variable_survey = c()
for (i in 1:nrow(folder_waves_available)) {
  
  int_data<-read_dta(paste0(folder_shp_1, folder_waves_available[i,"folder_name"],  "/","shp",folder_waves_available[i,"wave_nr"],"_p_user.dta"))
  
  names(int_data) = names(int_data) %>% str_remove(folder_waves_available[i, "wave_nr"])
  
  y = names(int_data)[names(int_data) %in%  
                        c("idpers","idspou","civsta", "cohast", "ycouple", 
                          "pd17", "pd17m", "pd200", "pd201","pd29", "pd31", 
                          "pd31c", "pd32", "pd32a","pd32c", "pd32m" )]
  
  x = int_data[,y]
  
  x$year_wave  = folder_waves_available[i, "year_wave"]
  
  partner_variable_survey = bind_rows(partner_variable_survey,x)

  rm(int_data, x, y)
  
}

# b) re-code to character 
partner_variable_survey = 
  partner_variable_survey %>% 
  mutate(cohast = sjlabelled::as_character(cohast),
         civsta = sjlabelled::as_character(civsta),
         pd29 = sjlabelled::as_character(pd29),
         pd32a = sjlabelled::as_character(pd32a),
         pd32m = sjlabelled::as_character(pd32m))


partner_variable_survey[partner_variable_survey$idpers==5101,]

# c) encode missing, create: start_relation_y, start_relation_m, marr_div_death_m, marr_div_death_y
# encode civsta, create PARTNERSHIP_STATUS and "START_Y", "START_M", "END_Y","END_M", 
# "DIVORCE_Y", "WIDOW_Y"

partner_variable_survey = partner_variable_survey %>%
  mutate(
    # encode missing 
    # pd31 Partner or spouse: Since when: Year
    pd31 = ifelse(pd31==-3,NA, pd31),                # -3 inapplicable
    pd31 = ifelse(pd31 %in% c(-8,-7,-2,-1),-1, pd31),# -8 other error; -7 filter error; -2 no answer; -1 does not know
    pd31 = ifelse(pd31==1,year_wave, pd31),          # 1 less than one year
    pd31 = ifelse(pd31==2,year_wave-1, pd31),        # 2 more than one year
    # Partner or spouse: Since when: Corrected year
    ycouple = ifelse(ycouple==-3,NA,ycouple),
    ycouple = ifelse(ycouple %in% c(-8,-7,-2,-1),-1,ycouple),
    # partner or Spouse: Month, if together since less than one year
    pd32 = ifelse(pd32==-3,NA,pd32),
    pd32 = ifelse(pd32 %in% c(-8,-7,-2,-1),-1,pd32),
    
    more_less_year = pd32m,
    # Civil status: Month of most recent
    pd17m = ifelse(pd17m==-3,NA,pd17m),
    pd17m = ifelse(pd17m %in% c(-8,-7,-2,-1),-1,pd17m),
    # Civil status change: Month
    pd200 = ifelse(pd200==-3,NA,pd200),
    pd200 = ifelse(pd200 %in% c(-8,-7,-2,-1),-1,pd200),
    
    pd17 = ifelse(pd17==-3,NA,pd17),
    pd201 = ifelse(pd201 %in% c(-8,-7,-2,-1),-1,pd201),
    cohast = ifelse(cohast == "inapplicable", NA,cohast),
    PARTNER_ID = ifelse(idspou<0, NA, idspou),
    # Combine ycouple Partner or spouse: Since when: Corrected year with 
    # pd31 - Partner or spouse: Since when: Year
    start_relation_y = coalesce(ycouple, pd31),
    start_relation_m = pd32,
    marr_div_death_m = coalesce(pd17m,pd200),
    marr_div_death_y = coalesce(pd17,pd201),
    # encode civsta
    civsta = case_when(
      civsta %in% c("dissolved partnership","divorced") ~ "divorce",
      civsta %in% c("married","registered partnership") ~ "marriage",
      civsta %in% c("separated") ~ "separation",
      civsta %in% c("widower/widow") ~ "death of partner",
      civsta %in% c("single, never married") ~ "single"),
    partnered = case_when(
      pd29 == "yes, but not living together" ~ "LAT",
      pd29 == "yes, living together" ~ "yes",
      pd29 == "no" ~ "no"),
    cohast = ifelse(cohast == "not married" & civsta == "marriage" & !is.na(PARTNER_ID), "married", cohast),
    # create PARTNERSHIP_STATUS and "START_Y", "START_M", "END_Y","END_M", "DIVORCE_Y", "WIDOW_Y"
    PARTNERSHIP_STATUS = case_when(!is.na(cohast) & !is.na(PARTNER_ID) ~ cohast,
                      is.na(cohast) & is.na(PARTNER_ID) ~ "no union"),
    PARTNERSHIP_STATUS = case_when(
      PARTNERSHIP_STATUS == "married" ~ "marriage",
      PARTNERSHIP_STATUS == "not married"~"cohabitation",
      PARTNERSHIP_STATUS =="no union" ~"no union")) %>% 
  mutate(
    SEP_Y = ifelse(civsta == "separation",marr_div_death_y, NA),
    DIVORCE_Y = ifelse(civsta == "divorce",marr_div_death_y, NA),
    WIDOW_Y = ifelse(civsta == "death of partner",marr_div_death_y, NA),
    SEP_M = ifelse(civsta == "separation",marr_div_death_m, NA),
    DIVORCE_M = ifelse(civsta == "divorce",marr_div_death_m, NA),
    WIDOW_M = ifelse(civsta == "death of partner",marr_div_death_m, NA),
    END_M = coalesce(SEP_M,DIVORCE_M,WIDOW_M),
    END_Y = coalesce(SEP_Y,DIVORCE_Y,WIDOW_Y),
    MARR_Y = ifelse(civsta=="marriage",marr_div_death_y,NA),
    MARR_M = ifelse(civsta=="marriage",marr_div_death_m,NA),
    START_Y = coalesce(start_relation_y,MARR_Y),
    START_M = coalesce(start_relation_m,MARR_M)
  )  %>% 
  group_by(idpers) %>% arrange(year_wave,.by_group = T) %>% 
  mutate(DIVORCE_Y = ifelse(civsta=="divorce" & lag(civsta)!="divorce" & is.na(DIVORCE_Y),year_wave, DIVORCE_Y),
         SEP_Y = ifelse(civsta=="separation" & lag(civsta)!="separation",year_wave, NA),
         SEP_Y = lead(SEP_Y),
         DIVORCE_Y = ifelse(is.na(DIVORCE_Y) & PARTNERSHIP_STATUS =="marriage" & lead(civsta) =="separation" & lead(lead(civsta))=="divorce",	
                            lead(DIVORCE_Y, 2), DIVORCE_Y),
         SEP_Y = ifelse(is.na(SEP_Y) & PARTNERSHIP_STATUS =="marriage" & lead(civsta) =="separation",
                            lead(SEP_Y), SEP_Y),
         END_Y = ifelse(is.na(END_Y) & !is.na(SEP_Y),SEP_Y, END_Y)
         ) %>% 
  mutate(check = lag(PARTNERSHIP_STATUS)!=PARTNERSHIP_STATUS,
         check = ifelse(is.na(check), T,check),
         spellnr = cumsum(check))  %>% 
  ungroup() %>% group_by(idpers, spellnr) %>% 
  # fill missing
  fill(END_Y, .direction = "updown") %>% 
  fill(END_M, .direction = "updown") %>% 
  fill(DIVORCE_Y, .direction = "updown") %>% 
  fill(START_M, .direction = "updown") %>% 
  fill(START_Y, .direction = "updown") %>% 
  fill(WIDOW_Y, .direction = "updown") %>%
  mutate(
    # START_Y = ifelse(is.na(START_Y), -1, START_Y),
    START_Y = ifelse(is.na(START_Y), min(year_wave), START_Y),
    END_Y = ifelse(is.na(END_Y), max(year_wave), END_Y)
    ) %>% 
  # if one spell then the same start and end   
  mutate(START_Y = min(START_Y),
         END_Y = max(END_Y),
         START_M = min(START_M),
         END_M = max(END_M)) %>% 
  ungroup() %>% mutate(year = year_wave ) %>% 
  select("idpers","year","PARTNER_ID","spellnr","PARTNERSHIP_STATUS" ,
         "START_Y", "END_Y",  "END_M","START_M", 
         "DIVORCE_Y", "WIDOW_Y") %>% 
  mutate(source_flag_survey = "survey")

################################################################################
# 4. Combining data into one set:
# merge survey partner_variable_survey with SHP0_bvwl_user_expanded by "idpers", "year"
# and then merge it with SHP0_cs_user_expanded
partner_shp_all = merge(partner_variable_survey, SHP0_bvwl_user_expanded, 
                           by = c("idpers", "year"), all = T)%>% 
  mutate(PARTNERSHIP_STATUS = coalesce(PARTNERSHIP_STATUS.y,PARTNERSHIP_STATUS.x),
         START_Y =coalesce(START_Y.y, START_Y.x),
         DIVORCE_Y = coalesce(DIVORCE_Y.y,DIVORCE_Y.x),
         WIDOW_Y = coalesce(WIDOW_Y.y,WIDOW_Y.x),
         END_Y = coalesce(END_Y.y,END_Y.x)
         ) %>% 
  select(-ends_with(".y"), -ends_with(".x")) %>% 
  merge(SHP0_cs_user_expanded, by = c("idpers", "year"), all = T) %>% 
  mutate(PARTNERSHIP_STATUS = coalesce(PARTNERSHIP_STATUS.y,PARTNERSHIP_STATUS.x),
         START_Y =coalesce(START_Y.y, START_Y.x),
         DIVORCE_Y = coalesce(DIVORCE_Y.y,DIVORCE_Y.x),
         WIDOW_Y = coalesce(WIDOW_Y.y,WIDOW_Y.x),
         END_Y = coalesce(END_Y.y,END_Y.x)
  ) %>% 
  select(-ends_with(".y"), -ends_with(".x")) %>% 
  group_by(idpers) %>% arrange(year,.by_group = T) %>% 
  mutate(
    # c)	We create variable DIVORCE
    DIVORCE = ifelse(!is.na(DIVORCE_Y),1,0),
    # b)	We create END_SINGLE, END_UNION by comparing values of variable PARTNERSHIP_STATUS and lead of PARTNERSHIP_STATUS
    # -----------------------------------------------
    # END_SINGLE: How a spell of being single ended
    # -----------------------------------------------
    # [0] ongoing not ended spell
    # [1] cohabitation
    # [2] marriage
    END_SINGLE = case_when(
      # Single spell ends in cohabitation
      PARTNERSHIP_STATUS %in% c("no union") & lead(PARTNERSHIP_STATUS) == "cohabitation" ~ "cohabitation",
      # Single spell ends in marriage
      PARTNERSHIP_STATUS %in% c("no union") & lead(PARTNERSHIP_STATUS) == "marriage" ~ "marriage",
      # If currently marriage, not applicable (not a single spell)
      PARTNERSHIP_STATUS == "marriage" ~ NA_character_,
      # If current status is known and there's no next spell, the spell is ongoing
      !is.na(PARTNERSHIP_STATUS) & is.na(lead(PARTNERSHIP_STATUS)) ~ "ongoing not ended spell",
      (PARTNERSHIP_STATUS %in% c("no union","gap") & lead(PARTNERSHIP_STATUS) %in%  c("gap", "no union")) | (PARTNERSHIP_STATUS =="gap" & lead(PARTNERSHIP_STATUS) != "gap") ~ "unknown end",
      # Fallback for all other cases
      TRUE ~ NA_character_
    ),
    # -----------------------------------------------
    # END_UNION: How a union spell ended
    # -----------------------------------------------
    # [0] ongoing not ended spell
    # [1] marriage
    # [2] separation / break up
    # [3] death of partner
    END_UNION = case_when(
      # If a union is followed by a new one, but current spell is "no union", we skip
      PARTNERSHIP_STATUS == "no union" & lead(PARTNERSHIP_STATUS) %in% c("marriage", "cohabitation") ~ NA_character_,
      !is.na(WIDOW_Y) ~ "death of partner",
      # If divorced, end of union is a separation
      DIVORCE == 1 ~ "separation",
      # If separation year is recorded, it's a separation
      # If currently marriage and next spell is no union or cohabitation => separation
      PARTNERSHIP_STATUS == "marriage" & lead(PARTNERSHIP_STATUS) %in% c("cohabitation", "no union") ~ "separation",
      # If currently cohabitation and next spell is no union => separation
      PARTNERSHIP_STATUS %in% c("cohabitation") & lead(PARTNERSHIP_STATUS) == "no union" ~ "separation",
      # If partner ID changes between spells, it's a separation
      PARTNER_ID != lead(PARTNER_ID) ~ "separation",
      (is.na(PARTNER_ID) | is.na(lead(PARTNER_ID)))& PARTNERSHIP_STATUS =="cohabitation" & lead(PARTNERSHIP_STATUS)=="cohabitation" ~ "separation",
      (PARTNER_ID ==lead(PARTNER_ID)) & PARTNERSHIP_STATUS =="cohabitation" & lead(PARTNERSHIP_STATUS)=="cohabitation" ~ "separation",
      (is.na(PARTNER_ID) | is.na(lead(PARTNER_ID)))& PARTNERSHIP_STATUS =="marriage" & lead(PARTNERSHIP_STATUS)=="marriage" ~ "separation",
      # If widow year is recorded, the union ended in partner's death
      # If currently cohabitation and next spell is marriage => upgraded to marriage
      PARTNERSHIP_STATUS %in% c("cohabitation") & lead(PARTNERSHIP_STATUS) == "marriage" ~ "marriage",
      # If no next spell exists and current one is known, it's still ongoing
      !is.na(PARTNERSHIP_STATUS) & is.na(lead(PARTNERSHIP_STATUS)) ~ "ongoing not ended spell",
      PARTNERSHIP_STATUS != "no union" & lead(PARTNERSHIP_STATUS) == "gap" ~ "unknown end",
      PARTNERSHIP_STATUS =="marriage" & lead(PARTNERSHIP_STATUS)=="marriage" ~ "separation",
      # Fallback
      TRUE ~ NA_character_
    ),
    END_SINGLE = ifelse(!is.na( END_SINGLE) & PARTNERSHIP_STATUS %in% c("no union", "gap"),END_SINGLE,NA ),
    END_UNION = ifelse(!is.na( END_UNION) & PARTNERSHIP_STATUS %notin% c("no union", "gap"),END_UNION,NA ),
    # create spellnr
    check = PARTNERSHIP_STATUS!=lag(PARTNERSHIP_STATUS),
    check = ifelse(is.na(check),T,check),
    spellnr = cumsum(check),
    ) %>% select(-c("check")) %>% 
  mutate(
    # d)	We create variables: source_flag_start, source_flag_end and retro_marriage
    source_flag_start = coalesce(source_flag_survey,source_flag_2013_cs, source_flag_2002_bvwl),
    source_flag_end = coalesce(source_flag_survey,source_flag_2013_cs, source_flag_2002_bvwl),
    retro_marriage = coalesce(source_flag_2013_cs, source_flag_2002_bvwl)
  ) %>% select(-c("source_flag_survey","source_flag_2013_cs","source_flag_2002_bvwl")) %>% 
  group_by(idpers) %>% 
  fill(retro_marriage, .direction = "updown")

# e)	The combined data is transformed from a year-row format to a spell-row format for each respondent. One row represents one spell
partner_shp_all_1 = partner_shp_all %>% arrange(year, .by_group = T) %>% 
  group_by(idpers,spellnr) %>% slice(n()) %>% 
  select(idpers,spellnr,PARTNERSHIP_STATUS,PARTNER_ID, START_Y,END_Y,START_M,END_M,,DIVORCE_Y,DIVORCE,
         END_UNION,END_SINGLE,retro_marriage,source_flag_start,source_flag_end)

# f)

partner_shp_all_1 = partner_shp_all_1 %>% 
  mutate(
    PARTNERSHIP_STATUS = case_when(
      # [0] no union
      # [1] cohabitation
      # [2] marriage
      PARTNERSHIP_STATUS =="no union" ~ 0,
      PARTNERSHIP_STATUS =="cohabitation"~ 1,
      PARTNERSHIP_STATUS =="marriage"~ 2,
      is.na(PARTNERSHIP_STATUS)~ 3
    ),
    
    # How union spell ended:
    # [0] ongoing not ended spell
    # [1] marriage
    # [2] Separation
    # [3] death of partner
    END_UNION = case_when(
      # END_UNION == "-1" ~ -1,
      END_UNION == "ongoing not ended spell" ~ 0,
      END_UNION == "marriage"                ~ 1,
      END_UNION == "separation"              ~ 2,
      END_UNION == "death of partner"        ~ 3
      # END_UNION == "unknown end" ~ -1,
    ),
    # How single spell ended:
    # [0] ongoing not ended spell
    # [1] cohabitation
    # [2] marriage
    END_SINGLE = case_when(
      END_SINGLE == "ongoing not ended spell" ~ 0,
      END_SINGLE == "cohabitation" ~ 1,
      END_SINGLE == "marriage" ~ 2,
      # END_SINGLE == "unknown end" ~ -1
    ),
  )

partner_shp_all_1 = partner_shp_all_1 %>% 
  mutate(across(c("PARTNERSHIP_STATUS","END_SINGLE","END_UNION", "DIVORCE"), as.integer))

partner_shp_all_1 = partner_shp_all_1 %>% 
  mutate(START_Y = as.integer(START_Y),
         START_Y = ifelse(START_Y<0,-1,START_Y),
         END_Y = as.integer(END_Y),
         END_Y = ifelse(END_Y<0, -1,END_Y)
         )
x = partner_shp_all_1 %>% 
  merge(select(readRDS("output/master_shp_wide.rds"), idpers, BORN_Y), by = c("idpers"), all.x = T) %>% 
  mutate(age_first = ifelse(START_Y>0 & BORN_Y >0,START_Y - BORN_Y, NA),
         START_Y = ifelse(age_first==0 & spellnr==1,START_Y+15, START_Y),
         age_first = ifelse(START_Y>0 & BORN_Y >0,START_Y - BORN_Y, NA),
  ) %>% 
  filter(age_first>15, spellnr==1) %>% 
  mutate(
    START_Y = BORN_Y+15,
    END_Y = START_Y,
    spellnr = 0,
    PARTNERSHIP_STATUS = 0,
    DIVORCE = 0,
    DIVORCE_Y = NA,
    PARTNER_ID = NA) %>% 
  select(idpers, START_Y, END_Y, spellnr,PARTNERSHIP_STATUS,DIVORCE,DIVORCE_Y,PARTNER_ID)

partner_shp_all_2 = partner_shp_all_1 %>% bind_rows(x) %>% 
  mutate(START_Y = as.integer(START_Y),
         START_Y = ifelse(START_Y<0,-1,START_Y),
         END_Y = as.integer(END_Y),
         END_Y = ifelse(END_Y<0, -1,END_Y)
  ) %>% group_by(idpers) %>% arrange(spellnr, .by_group = T) %>% 
  mutate(spellnr = 1:n()) %>% 
  mutate(DIVORCE_Y = as.integer(DIVORCE_Y),
         DIVORCE_Y = ifelse(DIVORCE_Y<0,-1,DIVORCE_Y)
         ) %>%
  mutate(across(c("PARTNERSHIP_STATUS","END_SINGLE","END_UNION", "DIVORCE",
                  "START_Y", "END_Y", "START_M", "END_M", "DIVORCE_Y"
                  ), as.integer))

# check if we fill last spell end_y with lastobs_y 

last = readRDS("output/master_shp_wide.rds") %>% select(idpers, LASTOBS_Y)

partner_shp_all_2 = partner_shp_all_2 %>% 
  merge(last, by = c("idpers"), all.x = T) %>% 
  group_by(idpers) %>% 
  mutate(
    last = as.integer(max(spellnr)),
    END_Y = ifelse(last ==spellnr, LASTOBS_Y, END_Y)
  )


partner_shp_all_2 = partner_shp_all_2 %>% 
  mutate(
    PARTNERSHIP_STATUS = ifelse(!is.na(PARTNER_ID) & PARTNERSHIP_STATUS ==0,1,PARTNERSHIP_STATUS)
  ) %>% 
  mutate(
    PARTNERSHIP_STATUS =  labelled(
      x = as.integer(PARTNERSHIP_STATUS),  # must be numeric
      labels = c(
        "no union" = 0, 
        "cohabitation" = 1, 
        "marriage" = 2,
        "gap" = 3
      )),
    
    END_UNION =  labelled(
      x = as.integer(END_UNION),  # must be numeric
      labels = c(
        # "unknown end" = -1,
        "ongoing not ended spell" = 0,
        "marriage"= 1,
        "separation"= 2,
        "death of partner"= 3)),
    END_SINGLE = labelled(
      x = as.integer(END_SINGLE),
      labels = c(
        "ongoing not ended spell" = 0,
        "cohabitation" = 1,
        "marriage" = 2
        # "unknown end" = -1
      )
    ),
    DIVORCE =  labelled(
      x = as.integer(DIVORCE),  # must be numeric
      labels = c(
        "divorce did not occurred" = 0,
        "divorce occurred" = 1))
  ) 
  

partner_shp_all_2 = partner_shp_all_2 %>% 
  mutate(
    PARTNER_ID = as.character(PARTNER_ID),
    DIVORCE_M = -1
  )

partner_shp_all_2 %>% group_by(END_UNION) %>% summarise(n())

# END_UNION                    `n()`
# <int+lbl>                    <int>
# 1  0 [ongoing not ended spell] 24403
# 2  1 [marriage]                 3870
# 3  2 [separation]               9765
# 4  3 [death of partner]          768
# 5 NA                           70245

partner_shp_all_2 %>% group_by(spellnr,PARTNERSHIP_STATUS, retro_marriage) %>% 
  summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))

partner_shp_all_2 %>% group_by(PARTNERSHIP_STATUS) %>% 
  summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))



partner_shp_all_2 %>% group_by(PARTNERSHIP_STATUS) %>% 
  summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))

partner_shp_all_2 %>% 
  filter(spellnr==15) %>% 
  group_by(PARTNERSHIP_STATUS) %>% 
  summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))


################################################################################
# 5. Transformation from Long to Wide Format
variables = c("PARTNER_ID", "START_Y","START_M","END_Y","END_M","DIVORCE_Y","DIVORCE_M","DIVORCE",
         "END_UNION","END_SINGLE", "retro_marriage","source_flag_start","source_flag_end")

max_PARTNERSHIP_STATUS = max(partner_shp_all_2$spellnr)

partner_biography_shp_wide<-partner_shp_all_2 %>%
  pivot_wider(
    id_cols = c(idpers),
    names_from = spellnr,
    values_from = c("PARTNERSHIP_STATUS","PARTNER_ID", "START_Y","START_M","END_Y","END_M","DIVORCE_Y","DIVORCE_M","DIVORCE",
                    "END_UNION","END_SINGLE", "retro_marriage"),
    names_glue = "{.value}_{spellnr}"
  )


partner_biography_shp_wide

################################################################################
## Sort variables
ordering_variables_wide_format <- function(x_c) {
  
  list_order<-c()
  for (i in 1:x_c) {
    
    x<-c(paste0("PARTNERSHIP_STATUS_", i),
         paste0("PARTNER_ID_", i),
         paste0("START_Y_", i),
         paste0("START_M_", i),
         paste0("END_Y_", i),
         paste0("END_M_", i),
         paste0("DIVORCE_Y_", i),
         paste0("DIVORCE_M_", i),
         paste0("DIVORCE_", i),
         paste0("END_UNION_", i),
         paste0("END_SINGLE_", i),
         paste0("retro_marriage_", i)
    )
    
    list_order<- append(list_order, x)
    
  }
  
  return(list_order)
  
}

partner_biography_shp_wide = as.data.frame(partner_biography_shp_wide)
partner_biography_shp_wide = partner_biography_shp_wide[,c("idpers",ordering_variables_wide_format(max_PARTNERSHIP_STATUS))]

# Add stata labels

for (i in 1:max_PARTNERSHIP_STATUS) {
  
  var_status <- paste0("PARTNERSHIP_STATUS_", i)
  var_pid <- paste0("PARTNER_ID_", i)
  var_start_y <- paste0("START_Y_", i)
  var_start_m <- paste0("START_M_", i)
  var_end_y <- paste0("END_Y_", i)
  var_end_m <- paste0("END_M_", i)
  var_div_y <- paste0("DIVORCE_Y_", i)
  var_div_m <- paste0("DIVORCE_M_", i)
  var_div <- paste0("DIVORCE_", i)
  var_end_type <- paste0("END_UNION_", i)
  var_END_SINGLE <- paste0("END_SINGLE_", i)
  var_retro <- paste0("retro_marriage_", i)
  
  var_label(partner_biography_shp_wide[[var_status]]) <- paste("Partnership status in spell", i, 
                                                               "([0] No union, [1] Cohabitation, [2] Marriage)")
  
  var_label(partner_biography_shp_wide[[var_pid]]) <- paste("Partner ID in spell", i)
  
  var_label(partner_biography_shp_wide[[var_start_y]]) <- paste("Year when partnership started in spell", i)
  var_label(partner_biography_shp_wide[[var_start_m]]) <- paste("Month when partnership started in spell", i)
  
  var_label(partner_biography_shp_wide[[var_end_y]]) <- paste("Year when partnership ended in spell", i)
  var_label(partner_biography_shp_wide[[var_end_m]]) <- paste("Month when partnership ended in spell", i)
  
  var_label(partner_biography_shp_wide[[var_div_y]]) <- paste("Year of divorce in spell", i)
  # var_label(partner_biography_shp_wide[[var_div_m]]) <- paste("Month of divorce in spell", i)
  
  var_label(partner_biography_shp_wide[[var_div]]) <- paste("Divorce status in spell", i, 
                                                            "([1] Divorce occurred, [0] Divorce did not occur)")
  
  var_label(partner_biography_shp_wide[[var_end_type]]) <- paste("How the union ended in spell", i, 
                                                                 "([0] Ongoing, [1] Marriage, [2] Separation/break-up, [3] Death of partner)")
  
  var_label(partner_biography_shp_wide[[var_END_SINGLE]]) <- paste("How the single spell ended in spell", i, 
                                                                   "([0] Ongoing, [1] Cohabitation, [2] Marriage)")
  
  var_label(partner_biography_shp_wide[[var_retro]]) <- paste("Retrospective report flag for spell", i)
}


# Save results 

saveRDS(partner_biography_shp_wide, "output/shp_partnership.rds")


if (type_you_want==".csv") {
  write.csv(partner_biography_shp_wide, "output/shp_partnership.csv")
}else if(type_you_want==".dta"){
  write_dta(partner_biography_shp_wide, "output/shp_partnership.dta")

}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
                )]

rm(list = x)



