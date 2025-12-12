####################### master file SHP ##################################
# Load necessary packages and directories 
source("00_setting_work_space.R")


folder_waves_avaiable<-as.data.frame(list.files(folder_shp_1)) %>% rename(folder_name = `list.files(folder_shp_1)`)

folder_waves_avaiable[c('wave_nr', 'year_wave')] <-str_split_fixed(folder_waves_avaiable$folder_name, "_", 2)

folder_waves_avaiable<-folder_waves_avaiable %>% 
  mutate(year_wave = as.numeric(year_wave)) %>% 
  arrange(year_wave) %>% 
  mutate(wave_nr = ifelse(year_wave == 1999, "99", str_remove(year_wave, "20")))


# 1. Read file shp_mp.dta (individual master file) it is wide format
master_file <- read_dta(paste0(folder_shp_2, "shp_mp.dta"))%>% 
  rename(SEX = sex,
         BORN_Y =birthy,
         BORN_M = birthm) %>% 
  select(idpers,BORN_M, BORN_Y, SEX) %>% 
  mutate(
    BORN_M = ifelse(BORN_M<0, -1, BORN_M),
    SEX = ifelse(SEX<0 | SEX==3, -1, SEX),
    BORN_Y = ifelse(BORN_Y<0, -1, BORN_Y))

# 2. pdate, status

interview_date =
  read_dta(paste0(folder_shp_2, "shp_mp.dta"))  %>% 
  select(idpers, starts_with("pdate")) %>% 
  pivot_longer(
  cols = starts_with("pdate"),       
  names_to = "wave_nr",                    
  names_prefix = "pdate",            
  values_to = "pdate") %>% 
  mutate(wave_nr = as.numeric(stringr::str_remove(wave_nr, "_")))%>% 
  mutate(wave_nr = ifelse(wave_nr>=1 & wave_nr<=9,paste0("0",wave_nr),as.character(wave_nr))) %>% 
  merge(select(folder_waves_avaiable, wave_nr, year_wave), by = c("wave_nr")) %>% 
  select(-wave_nr) %>% 
  rename(interview_date = pdate)



INTERVIEW_STATUS =read_dta(paste0(folder_shp_2, "shp_mp.dta")) %>% 
  select(idpers, starts_with("status")) %>% 
  select(-statuscovid) %>% 
  pivot_longer(
    cols = starts_with("status"),       
    names_to = "wave_nr",                    
    names_prefix = "status",            
    values_to = "status") %>% 
  mutate(wave_nr = as.numeric(stringr::str_remove(wave_nr, "_"))) %>% 
  mutate(wave_nr = ifelse(wave_nr>=1 & wave_nr<=9,paste0("0",wave_nr),as.character(wave_nr))) %>% 
  # 0   individual questionnaire
  mutate(INTERVIEW_STATUS = ifelse(status ==0, "fully responsive","proxy")) %>% 
  select(-status) %>% merge(select(folder_waves_avaiable, wave_nr, year_wave), by = c("wave_nr")) %>% 
  select(-wave_nr) %>% 
  mutate(
    INTERVIEW_STATUS = case_when(
      INTERVIEW_STATUS == "fully responsive" ~ 1L,
      INTERVIEW_STATUS == "proxy" ~ 2L,
      TRUE ~ NA_integer_
    ),
    INTERVIEW_STATUS = labelled(
      INTERVIEW_STATUS,
      labels = c(
        "fully responsive" = 1,
        "proxy, which includes the child respondent, proxy respondent" = 2
      )
    )
  )


INTERVIEW_STATUS %>% group_by(INTERVIEW_STATUS) %>% summarise(n())

############# time variable fro main survey
# 4. EMP_STATUS       - wstat
# WSTAT
# 1 active occupied
# 2 unemployed
# 3 not in labor force

folder_waves_available<-as.data.frame(list.files(folder_shp_1)) %>% rename(folder_name = `list.files(folder_shp_1)`)

folder_waves_available[c('wave_nr', 'year_wave')] <-str_split_fixed(folder_waves_available$folder_name, "_", 2)

folder_waves_available<-folder_waves_available %>% 
  mutate(year_wave = as.numeric(year_wave)) %>% 
  arrange(year_wave) %>% 
  mutate(wave_nr = ifelse(year_wave == 1999, "99", 
                          str_remove(year_wave, "20")))

employment<-c()
for (wave in 1:length(folder_waves_available$year_wave)) {
  
  data_personal <- haven::read_dta(
    paste0(folder_shp_1, folder_waves_available[wave,"folder_name"], 
           "/","shp",
           folder_waves_available[wave,"wave_nr"],"_p_user.dta"),
    col_select = c("idpers", 
                   paste0( "sex", folder_waves_available$wave_nr[wave]),
                   paste0( "age", folder_waves_available$wave_nr[wave]),
                   
                   paste0( "wstat", folder_waves_available$wave_nr[wave]),
                   # P$$W12 – Currently not working: First reason
                   paste0( "p",folder_waves_available$wave_nr[wave],"w12"),
                   # P$$W13 – Currently not working: Second reason
                   paste0( "p",folder_waves_available$wave_nr[wave],"w13"),
                   # P$$W14 – Currently not working: Third reason
                   paste0( "p",folder_waves_available$wave_nr[wave],"w14"),
                   # P$$W01 – Worked for pay last week 
                   # P$$W02 – Worked without pay last week
                   # P$$W03 – Had a job but didn’t work last week
                   paste0("p",folder_waves_available$wave_nr[wave], "w01"),
                   paste0("p",folder_waves_available$wave_nr[wave], "w02"),
                   paste0("p",folder_waves_available$wave_nr[wave], "w03"),
                   paste0("status",folder_waves_available$wave_nr[wave])
                  
                   
                   ))
  
  names(data_personal)
  
  colnames(data_personal) <- sub(folder_waves_available$wave_nr[wave], "", colnames(data_personal))
  data_personal = data_personal %>% rename( EMP_STATUS = wstat)
  
  # data_personal$employed <- ifelse(data_personal$pw01 == 1 , "working", "not-working")
  
  data_personal$year_wave<-folder_waves_available[wave,"year_wave"]
  
  employment<-bind_rows(employment,data_personal)
  
}
employment = employment %>%  mutate(EMP_STATUS = case_when(
  pw01 == 1 ~1, 
  pw01 == 2 ~0,
  TRUE ~-1)) %>% 
  mutate(EMP_STATUS = as.integer(EMP_STATUS)) %>% 
  mutate(
    EMP_STATUS = labelled(
      EMP_STATUS,
      labels = c("not-working" = 0, "working" = 1, "unknown" = -1)
    )
  )

############# time constant 
# 6. FIRSTOBS_Y LASTOBS_Y

first<-interview_date %>% group_by(idpers) %>% 
  filter(!is.na(interview_date)) %>%
  arrange(year_wave, .by_group = T) %>% slice(1) %>% 
  rename(FIRSTOBS_Y = year_wave, 
         first_pdate =interview_date)

last<- interview_date %>% group_by(idpers) %>% 
  filter(!is.na(interview_date)) %>% 
  arrange(year_wave, .by_group = T) %>% slice(n()) %>% 
  rename(LASTOBS_Y = year_wave, 
         last_pdate =interview_date) 

first_last<-merge(first, last, by = c("idpers")) %>% 
  select("idpers","FIRSTOBS_Y","LASTOBS_Y", "last_pdate", "first_pdate")

# 7. ever_proxy
ever_proxy = INTERVIEW_STATUS %>% 
  mutate(if_proxy = ifelse(INTERVIEW_STATUS==1,0,1)) %>% 
  group_by(idpers) %>% 
  summarise(ever_proxy_1 = sum(if_proxy), 
            ever_proxy  = ifelse(ever_proxy_1, "proxy", "not proxy")) %>% select(-ever_proxy_1)

# flag retro 
# retro_marriage
those_retro_1 = read_dta(paste0(folder_retro_shp_1, "shp0_bvcs_user.dta")) %>% 
  select(idpers) %>% distinct() %>% mutate(retro_marriage ="yes")

those_retro_2 = read_dta(paste0(folder_retro_shp_2,"shpiii_cs_user.dta")) %>% 
  select(idpers) %>% distinct() %>% mutate(flag_retro_marriage ="yes")

those_retro_marriage = bind_rows(those_retro_1, those_retro_2)
# flag_retro_fertility
id_those_with_retro = unique(read_dta(paste0(folder_retro_shp_2 ,"shpiii_fa_user.dta"))$idpers)
those_retro_fertility = data.frame(idpers = id_those_with_retro,flag_retro_fertility = "yes")

flag_retro = merge(those_retro_fertility,those_retro_marriage, by = c("idpers"), all = T)


# anychild
survery_nr_kids<-c()
for (wave in 1:length(folder_waves_avaiable$year_wave)) {
  
  if (folder_waves_avaiable$wave_nr[wave] %in% c("13", "14", "15", "16", "17", "18", "19", "20")) {
    # download the data 
    data_personal <- haven::read_dta(
      paste0(folder_shp_1,folder_waves_avaiable$folder_name[wave],"/",
             "shp",folder_waves_avaiable$wave_nr[wave],"_p_user.dta"),
      col_select = c("idpers",paste0("ownkid",folder_waves_avaiable$wave_nr[wave]), paste0("p",folder_waves_avaiable$wave_nr[wave],"d110a"),
                     paste0("p",folder_waves_avaiable$wave_nr[wave],"d110b")))
    
    names(data_personal)<-c("idpers","nr_kids","D110A","D110B")
    
  }else{
    
    # download the data 
    data_personal <- haven::read_dta(
      paste0(folder_shp_1,folder_waves_avaiable$folder_name[wave],"/",
             "shp",folder_waves_avaiable$wave_nr[wave],"_p_user.dta"),
      col_select = c("idpers",paste0("ownkid",folder_waves_avaiable$wave_nr[wave])))
    
    
    names(data_personal)<-c("idpers","nr_kids")
  }
  
  
  data_personal$year_wave <-folder_waves_avaiable$year_wave[wave]
  survery_nr_kids<-bind_rows(survery_nr_kids,data_personal) 
  rm(data_personal)
  # print(wave)
  
}

survery_nr_kids$D110A = sjlabelled::as_character(survery_nr_kids$D110A)

survery_nr_kids<-survery_nr_kids %>% 
  mutate(nr_kids = ifelse(nr_kids<0,NA,nr_kids),
         ANYCHILD = ifelse(nr_kids==0,2,ifelse(nr_kids>0,1,NA))
         ) %>% group_by(idpers) %>% arrange(year_wave, .by_group = T) %>% 
  slice(n()) %>% select(idpers, ANYCHILD)

# merge time variant

interview_date$INTERVIEW_DATE = as.character(interview_date$interview_date)

time_variant = merge(employment, INTERVIEW_STATUS, by = c("idpers", "year_wave"), all = T) %>% 
  merge(interview_date,by = c("idpers", "year_wave"), all = T) 

# to wide

variables_to_long = c("INTERVIEW_STATUS","EMP_STATUS", "INTERVIEW_DATE")    

wide_time_variant = time_variant %>% 
  pivot_wider(
    id_cols = c(idpers),
    names_from = year_wave,
    values_from = c("INTERVIEW_STATUS", "INTERVIEW_DATE", "EMP_STATUS"),
    names_glue = "{.value}_{year_wave}"
  )

# merge time invariant with time variant

time_invariant  = merge(master_file,ever_proxy, by = c("idpers"), all = T) %>% 
  merge(survery_nr_kids, by = c("idpers"), all = T) %>% 
  # first and last obs
  merge(first_last,by = c("idpers"), all = T) %>% 
  # flags 
  merge(flag_retro, by = c("idpers"), all = T)


master_shp_wide = merge(time_invariant, wide_time_variant, by = c("idpers"), all = T) %>% 
  select(-c("last_pdate", "first_pdate"))


################################################################################
# add variable labels

master_shp_wide = master_shp_wide %>% 
  mutate(across(c(BORN_Y, BORN_M, SEX, FIRSTOBS_Y, LASTOBS_Y), ~ as.integer(.))) %>% 
  mutate(
    BORN_Y = labelled(
      BORN_Y,
      labels = c("unknown" = -1)
    ),
    BORN_M = labelled(
      BORN_M,
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
  )%>% 
  mutate(ANYCHILD = labelled(as.integer(ANYCHILD),  # must be numeric
                             labels = c(
                               "yes" = 1,
                               "no" = 2,
                               "unknown" = -1)))

# Add variable labels (descriptions)
var_label(master_shp_wide$BORN_Y)         <- "Year of birth (-1 = unknown)"
var_label(master_shp_wide$BORN_M)         <- "Month of birth (-1 = unknown)"
var_label(master_shp_wide$SEX)            <- "Biological sex"
var_label(master_shp_wide$FIRSTOBS_Y)     <- "Year of first interview (-1 = unknown)"
var_label(master_shp_wide$LASTOBS_Y)      <- "Year of last interview (-1 = unknown)"
var_label(master_shp_wide$ANYCHILD)       <- "Any child at the last possible interview?"
# --- label all INTERVIEW_DATE# variables ---
for (v in grep("^INTERVIEW_DATE", names(master_shp_wide), value = TRUE)) {
  var_label(master_shp_wide[[v]]) <- "Date of interview (NA = unknown)"
}

# --- label all INTERVIEW_STATUS# variables ---
for (v in grep("^INTERVIEW_STATUS", names(master_shp_wide), value = TRUE)) {
  var_label(master_shp_wide[[v]]) <- "Type of survey response"
}

# --- label all EMP_STATUS# variables ---
for (v in grep("^EMP_STATUS", names(master_shp_wide), value = TRUE)) {
  var_label(master_shp_wide[[v]]) <- "Employment status"
}

# save 

saveRDS(master_shp_wide, "output/master_shp_wide.rds")


x = ls()
x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)





