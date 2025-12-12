##################### BHPS/UKHLS master file ############################
# download required packages for this task: "dplyr", "haven", "tidyr"
source("00_setting_work_space.R")

# Information for all persons in household, incl. children and non-respondents
folder_indall<-list.files(path = paste0(folder_main_uk, "ukhls/"), pattern = "_indall_protect\\.dta$")

year_bhps = data.frame(year_wave = 1991:2008,
                       letter = letters[1:18],
                       FIRSTOBS_Y_bhps = 1:18,
                       LASTOBS_Y_bhps  = 1:18)

year_ukhls = data.frame(letter = str_remove(folder_indall, "_indall_protect.dta"),
                        year_wave = 2009:2019,
                        wave = 1:length(folder_indall),
                        FIRSTOBS_Y_ukhls = 1:11,
                        LASTOBS_Y_ukhls  = 1:11)

################################################################################
# xwavedat - Stable characteristics of individuals

time_invariant <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>%
  select(pidp, pid, birthm, birthy, sex,
         lwintvd_dv,    # Last wave interviewed (incl. proxy), (UKHLS)
         fwintvd_dv,    # First wave interviewed (inc. proxy), (UKHLS)
         lwintvd_dv_bh, # Last wave interviewed (incl. proxy), (BHPS)
         fwintvd_dv_bh, # First wave interviewed (inc. proxy), (BHPS)
         lwenum_dv,     # Last wave enumerated (UKHLS)
         fwenum_dv,     # First wave enumerated (UKHLS)
         lwenum_dv_bh,  # Last wave enumerated (BHPS)
         fwenum_dv_bh,  # First wave enumerated (BHPS)
         xwdat_dv,      # Study enumerated in: UKHLS, BHPS or both
         anychild_dv,   # Ever had a (biological) child
         memorig,       # sample origin: original ukhls
         memorig_bh,    # sample origin: original bhps
         sampst         # sample membership status
  ) %>% 
  # prepare first and last observation: 
  # FIRSTOBS_Y - Year first interview [-1] unknown
  # LASTOBS_Y - Year last interview  [-1] unknown
  mutate(
    # UKHLS
    lwintvd_dv = ifelse(lwintvd_dv==-8, lwenum_dv, lwintvd_dv),
    fwintvd_dv = ifelse(fwintvd_dv==-8, fwenum_dv, fwintvd_dv),
    # BHPS
    lwintvd_dv_bh = ifelse(lwintvd_dv_bh==-8, lwenum_dv_bh, lwintvd_dv_bh),
    fwintvd_dv_bh = ifelse(fwintvd_dv_bh==-8, fwenum_dv_bh, fwintvd_dv_bh),
    FIRSTOBS_Y_bhps  = fwintvd_dv_bh,
    LASTOBS_Y_bhps   = lwintvd_dv_bh, 
    FIRSTOBS_Y_ukhls = fwintvd_dv,
    LASTOBS_Y_ukhls  = lwintvd_dv) %>% 
  # bhps
  mutate(FIRSTOBS_Y_bhps  = as.numeric(FIRSTOBS_Y_bhps),
         LASTOBS_Y_bhps   = as.numeric(LASTOBS_Y_bhps),
         FIRSTOBS_Y_ukhls = as.numeric(FIRSTOBS_Y_ukhls),
         LASTOBS_Y_ukhls  = as.numeric(LASTOBS_Y_ukhls)) %>% 
  merge(select(year_bhps, year_wave, LASTOBS_Y_bhps), by = c("LASTOBS_Y_bhps"), all.x = T) %>% 
  select(-LASTOBS_Y_bhps) %>%
  rename(LASTOBS_Y_bhps = year_wave) %>%
  merge(select(year_bhps,year_wave,FIRSTOBS_Y_bhps), by = c("FIRSTOBS_Y_bhps"), all.x = T) %>%
  select(-FIRSTOBS_Y_bhps) %>%
  rename(FIRSTOBS_Y_bhps = year_wave) %>% 
  # ukhls 
  merge(select(year_ukhls,year_wave,LASTOBS_Y_ukhls), by = c("LASTOBS_Y_ukhls"), all.x = T) %>% 
  select(-LASTOBS_Y_ukhls) %>%
  rename(LASTOBS_Y_ukhls = year_wave) %>%
  merge(select(year_ukhls,year_wave,FIRSTOBS_Y_ukhls), by = c("FIRSTOBS_Y_ukhls"), all.x = T) %>%
  select(-FIRSTOBS_Y_ukhls) %>%
  rename(FIRSTOBS_Y_ukhls = year_wave) %>%  
  mutate(FIRSTOBS_Y = coalesce(FIRSTOBS_Y_bhps,FIRSTOBS_Y_ukhls),
         LASTOBS_Y = coalesce(LASTOBS_Y_ukhls, LASTOBS_Y_bhps)
  ) %>% 
  # prepare BORN_M, BORN_Y, SEX, ANYCHILD
  rename( BORN_M = birthm,
          BORN_Y = birthy,
          SEX = sex,
          which_study = xwdat_dv) %>% 
  mutate(
    ANYCHILD = ifelse(anychild_dv==1, 1, ifelse(anychild_dv==2,2, -1)),
    which_study = case_when(
          which_study == 1 ~ "in UKHLS",
          which_study == 2 ~ "in BHPS",
          which_study == 3 ~ "in both"))


# pidp,which_study,SEX, BORN_M, BORN_Y, FIRSTOBS_Y, LASTOBS_Y, memorig, memorig_bh, sampst
time_invariant = time_invariant %>% select(pidp, BORN_Y, BORN_M, SEX,  FIRSTOBS_Y, LASTOBS_Y, ANYCHILD, memorig, memorig_bh, sampst) %>% 
  # put in right data type and encode missing values correclty 
  mutate(
    BORN_Y = ifelse(BORN_Y<0, -1,BORN_Y),
    BORN_M = ifelse(BORN_M<0, -1,BORN_M),
    SEX = ifelse(SEX<0, -1, SEX),
    FIRSTOBS_Y = ifelse(is.na(FIRSTOBS_Y) | FIRSTOBS_Y<0, -1,FIRSTOBS_Y),
    LASTOBS_Y = ifelse(is.na(LASTOBS_Y) | LASTOBS_Y<0, -1,LASTOBS_Y),
    ANYCHILD = ifelse(is.na(ANYCHILD), -1, ANYCHILD)
  ) %>% 
  mutate(
    SEX = labelled(as.integer(SEX),  # must be numeric
                                labels = c(
                                  "male" = 1,
                                  "female" = 2,
                                  "unknown" = -1
                                  )),
    ANYCHILD = labelled(as.integer(ANYCHILD),  # must be numeric
                   labels = c(
                     "yes" = 1,
                     "no" = 2,
                     "unknown" = -1
                     
                   ))
  )


# EVER_MARRIED

################################################################################
# Time variant variables
# 1. BHPS  variables from the interview date from files indresp: INTERVIEW_DATE, EMP_STATUS
INTERVIEW_DATE_bhps<-c()
for (i in 1:18) {
  if (i==1) {
    indresp <- haven::read_dta(paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")),
                               col_select = c("pidp",
                                              paste0("b",letters[i],"_istrtdatd"),   # istrtdatd Date of interview: day
                                              paste0("b",letters[i],"_istrtdatm"),   # istrtdatm Date of interview: month
                                              paste0("b",letters[i],"_jbstat"),
                                              paste0("b",letters[i],"_mastat")
                                              
                               ))
    
    indresp[ , paste0("b", letters[i], "_istrtdaty")] <-1991

  }else{
    indresp <- haven::read_dta(paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")),
                               col_select = c("pidp",
                                              paste0("b",letters[i],"_istrtdatd"),  # istrtdatd Date of interview: day
                                              paste0("b",letters[i],"_istrtdatm"),  # istrtdatm Date of interview: month
                                              paste0("b",letters[i],"_istrtdaty"),  # istrtdaty Date of interview: 4 digit year
                                              paste0("b",letters[i],"_jbstat"),
                                              paste0("b",letters[i],"_mastat")
                               ))

  }
  
  colnames(indresp) <- sub(paste0("b",letters[i], "_"), "", colnames(indresp))
  
  indresp <- indresp %>%
    mutate(
      istrtdaty = as.numeric(istrtdaty),
      istrtdatd = as.numeric(istrtdatd),
      istrtdatm = as.numeric(istrtdatm),
      INTERVIEW_DATE = as.Date(paste(istrtdaty, istrtdatm, istrtdatd, sep = "-"), format = "%Y-%m-%d"),
      year_wave = year_bhps$year_wave[i]) %>% 
    select(pidp, year_wave, INTERVIEW_DATE,jbstat,mastat) %>% 
    mutate(jbstat = sjlabelled::as_character(jbstat),
           mastat = sjlabelled::as_character(mastat),
           )
  
  INTERVIEW_DATE_bhps <- bind_rows(INTERVIEW_DATE_bhps, indresp)
  
  rm(indresp)
  
}

# INTERVIEW_STATUS
INTERVIEW_STATUS_bhps <- haven::read_dta(paste0(folder_bhps_1,"xwaveid_bh_protect.dta"),
                                         col_select = c("pid","pidp", "ba_ivfio1_bh", ends_with("ivfio"))) %>% 
  mutate(
    across(
      .cols = c("ba_ivfio1_bh",ends_with("ivfio")),
      .fns = ~ if_else(. == 1, 1, 2)
    )
  ) %>% 
  tidyr::pivot_longer(cols = c("ba_ivfio1_bh", paste0("b", letters[2:18], "_ivfio")),
                      names_to  = 'letter',
                      values_to = 'INTERVIEW_STATUS') %>%
  mutate(letter = str_replace(letter, "_ivfio", "")) %>% 
  mutate(letter = str_replace(letter, "1_bh", "")) %>% 
  mutate(letter = str_replace(letter, "b", "")) %>% 
  select(pidp, letter, INTERVIEW_STATUS ) %>% 
  merge(year_bhps, by = c("letter"), all.x = T) %>% 
  select(pidp, year_wave, INTERVIEW_STATUS )

interview_bhps<-merge(INTERVIEW_STATUS_bhps, INTERVIEW_DATE_bhps, by = c("pidp", "year_wave"), all.x =T)

# UKHLS: interview date
INTERVIEW_DATE_ukhls <-c()
for (i in 1:length(folder_indall)) {
  
  x <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indresp_protect.dta"), 
                       col_select = c("pidp", 
                                      ends_with("intdaty_dv"),      # Interview date: Year, derived
                                      ends_with("intdatm_dv"),      # Interview date: Month, derived
                                      ends_with("intdatd_dv"),      # Interview date: Day, derived
                                      paste0(letters[i],"_jbstat"), # Current labour force status
                                      paste0(letters[i],"_mastat_dv")
                                      
                       ))
  
  names(x)[-1] <- str_remove(names(x)[-1], paste0(letters[i], "_"))
  
  x = x %>% mutate(
    year_wave = year_ukhls$year_wave[i],
    INTERVIEW_DATE = as.Date(paste(intdaty_dv, intdatm_dv, intdatd_dv, sep = "-"), format = "%Y-%m-%d"),
    jbstat = sjlabelled::as_character(jbstat),
    mastat_dv = sjlabelled::as_character(mastat_dv),
    ) %>% rename(mastat = mastat_dv)
  
  INTERVIEW_DATE_ukhls<-rbind(INTERVIEW_DATE_ukhls, x)
  rm(x)
  
}

# INTERVIEW_STATUS
INTERVIEW_STATUS_ukhls <- read_dta(paste0(folder_main_uk,"ukhls/","xwaveid_protect.dta")) %>% 
  select(pidp, paste0(letters[1:length(year_ukhls$year_wave)], "_ivfio")) %>% 
  mutate(
    across(
      .cols = c(ends_with("ivfio")),
      .fns = ~ if_else(. == 1, 1, 2)
    )
  ) %>% 
  tidyr::pivot_longer(cols=paste0(letters[1:length(year_ukhls$year_wave)], "_ivfio"),
                      names_to='letter',
                      values_to='INTERVIEW_STATUS') %>%
  mutate(letter = str_replace(letter, "_ivfio","")) %>% 
  merge(year_ukhls, by = c("letter"), all.x = T) %>% 
  select(pidp, year_wave, INTERVIEW_STATUS ) 

interview_ukhls<-merge(INTERVIEW_STATUS_ukhls, INTERVIEW_DATE_ukhls, by = c("pidp", "year_wave"), all.x =T) %>% 
  select(-c("intdatd_dv", "intdatm_dv", "intdaty_dv"))

# combine time variant variables 

# combine time variant variables across bhps and ukhls 
INTERVIEW_STATUS = bind_rows(interview_bhps, interview_ukhls) %>% 
  merge(time_invariant, by = c(("pidp")), all.y = T) %>% 
  # create EMP_STATUS variable
  # [0] - not-working
  # [1] - working
  mutate(
    jbstat_clean = tolower(trimws(jbstat)),
    EMP_STATUS = case_when(
      jbstat_clean %in% c(
        "self employed", "self-employed", "employed", "paid employment(ft/pt)",
        "maternity leave", "on maternity leave"
      ) ~ 1,
      jbstat_clean %in% c(
        "unemployed", "retired", "lt sick or disabled", "lt sick, disabld",
        "ft studt, school", "full-time student", "family care", "family care or home",
        "gvt trng scheme", "govt training scheme", "on apprenticeship",
        "unpaid, family business", "other", "doing something else"
      ) ~ 0,
      jbstat_clean %in% c("refused", "refusal", "don't know", "missing", "inapplicable", "dont know", "proxy") | is.na(jbstat)~ -1,
      is.na(jbstat_clean)~ -1
      
    )) %>% select(pidp,year_wave, INTERVIEW_STATUS, INTERVIEW_DATE,EMP_STATUS) %>% 
  mutate(
    EMP_STATUS = as.integer(EMP_STATUS),
    INTERVIEW_STATUS = as.integer(INTERVIEW_STATUS)
  ) %>% 
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

# prepare variable EVER_MARRIED
INTERVIEW_mastat = bind_rows(INTERVIEW_DATE_bhps, INTERVIEW_DATE_ukhls) %>% select(pidp, year_wave, mastat) %>% 
  mutate(
    married_status = case_when(
      mastat %in% c(
        "Married","Divorced","Widowed",
        "In a registered same-sex civil partnership", "Separated but legally married","Separated from civil partner",
        "A former civil partner", "A surviving civil partner",
        "Married", "Civil partnership", "Divorced", "Widowed", "Separated",
        "Dissolved civil part", "Sep from civil part", "Survive from civ par"
        
      ) ~ 1,
      
      mastat %in% c("Child under","Living as couple", "Single and never married/in civil partnership",
                    "Child under","Under 16 years",
                    "Sep from civil part",
                    "Survive from civ par","Dissolved civil part", 
                    "Living as couple",
                    "Never married"
      ) ~ 0,
      mastat %in% c("Don't know", "Missing", "missing", "refusal", "inapplicable", "don't know",
                    "don't know", "missing","refusal"
      ) ~ NA,
      TRUE ~ 0
    )
  ) 

INTERVIEW_mastat_1 = INTERVIEW_mastat %>% 
  filter(!is.na(married_status)) %>%
  group_by(pidp) %>%
  mutate(
    # [0] - no
    # [1] - yes
    EVER_MARRIED = ifelse(sum(married_status)>0,1, 0)) %>%
  select(pidp,EVER_MARRIED) %>% distinct() %>% 
  mutate(EVER_MARRIED = as.integer(EVER_MARRIED))

time_invariant_1 = merge(time_invariant, INTERVIEW_mastat_1, all.x = T, by = c("pidp")) %>% 
  mutate(EVER_MARRIED = ifelse(is.na(EVER_MARRIED), -1, EVER_MARRIED),
         EVER_MARRIED = labelled(as.integer(EVER_MARRIED),  # must be numeric
                                 labels = c(
                                   "no" = 0,
                                   "yes" = 1,
                                   "unknown" = -1
                                 ))) %>% 
  mutate(
    BORN_Y = as.integer(BORN_Y),
    BORN_M = as.integer(BORN_M),
    SEX = as.integer(SEX),
    FIRSTOBS_Y = as.integer(FIRSTOBS_Y),
    LASTOBS_Y = as.integer(LASTOBS_Y),
    ###
    SEX = labelled(as.integer(SEX),  # must be numeric
                   labels = c(
                     "male" = 1,
                     "female" = 2,
                     "unknown" = -1
                   )),
    
  ) 

# time variant to wide format 
variables = c("INTERVIEW_STATUS","EMP_STATUS")

INTERVIEW_STATUS$INTERVIEW_DATE =as.character(INTERVIEW_STATUS$INTERVIEW_DATE)

INTERVIEW_STATUS = as.data.table(INTERVIEW_STATUS)

master_bhps_ukhls_wide = reshape2::dcast(INTERVIEW_STATUS , pidp~ year_wave, value.var="INTERVIEW_DATE")

year_max = max(as.numeric(colnames(master_bhps_ukhls_wide)[-1]))

colnames(master_bhps_ukhls_wide)[-1] <- paste0("INTERVIEW_DATE" ,"_", 1991:year_max)

for (i in variables) {
  
  x = data.table::dcast(INTERVIEW_STATUS  ,pidp~ year_wave, value.var=i)
  
  colnames(x)[-1] <- paste0(i ,"_", 1991:year_max)
  
  master_bhps_ukhls_wide<-merge(master_bhps_ukhls_wide, x, by = c("pidp"))

  rm(x)
}


class(master_bhps_ukhls_wide$INTERVIEW_STATUS_1991)

# why if all.x = T or all =T we have more 207
master_bhps_ukhls_wide = merge(master_bhps_ukhls_wide, time_invariant_1, by= c("pidp"), all= T) 

check_if_we_have_all <- names(master_bhps_ukhls_wide) %>%
  tibble(name = .) %>%
  mutate(
    type = case_when(
      name == "pidp"                  ~ "pid",
      name %in% c("BORN_Y")           ~ "BORN_Y",
      name %in% c("BORN_M")           ~ "BORN_M",
      name %in% c("SEX")              ~ "SEX",
      name %in% c("FIRSTOBS_Y")       ~ "FIRSTOBS_Y",
      name %in% c("LASTOBS_Y")        ~ "LASTOBS_Y",
      name %in% c("ANYCHILD")         ~ "ANYCHILD",
      name %in% "EVER_MARRIED" ~ "EVER_MARRIED",
      str_detect(name, "INTERVIEW_DATE")   ~ "INTERVIEW_DATE",
      str_detect(name, "INTERVIEW_STATUS") ~ "INTERVIEW_STATUS",
      str_detect(name, "EMP_STATUS")       ~ "EMP_STATUS",
      name %in% c("which_study")      ~ "which_study",
      name %in% c("memorig")          ~ "memorig",
      name %in% c("memorig_bh")       ~ "memorig_bh",
      name %in% c("sampst")           ~ "sampst",
      TRUE                            ~ "Other"
    )
  ) %>%
  count(type)

# usefun::outersect(check_if_we_have_all$type, c("pid","BORN_Y","BORN_M","SEX","FIRSTOBS_Y","LASTOBS_Y",
#                                                "INTERVIEW_DATE","INTERVIEW_STATUS","EMP_STATUS","EVER_MARRIED","ANYCHILD"))
# 
# # "memorig"    "memorig_bh" "sampst"   

##### integer

# Apply both variable and value labels
master_bhps_ukhls_wide <- master_bhps_ukhls_wide %>%
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
  ),
  EVER_MARRIED = labelled(
    EVER_MARRIED,
    labels = c("no" = 0,
               "yes" = 1,
               "unknown" = -1)
  ),
  ANYCHILD = labelled(
    ANYCHILD,
    labels = c("yes" = 1, "no" = 2)
  )
)

# Add variable labels (descriptions)
var_label(master_bhps_ukhls_wide$BORN_Y)         <- "Year of birth (-1 = unknown)"
var_label(master_bhps_ukhls_wide$BORN_M)         <- "Month of birth (1–12; -1 = unknown)"
var_label(master_bhps_ukhls_wide$SEX)            <- "Biological sex"
var_label(master_bhps_ukhls_wide$FIRSTOBS_Y)     <- "Year of first interview (-1 = unknown)"
var_label(master_bhps_ukhls_wide$LASTOBS_Y)      <- "Year of last interview (-1 = unknown)"
var_label(master_bhps_ukhls_wide$EVER_MARRIED)   <- "Ever married?"
var_label(master_bhps_ukhls_wide$ANYCHILD)       <- "Any child at the last possible interview?"


# "memorig"     
var_label(master_bhps_ukhls_wide$memorig)       <- "Sample origin: GPS, EMBS, IEMBS, BHPS, or BHPS regional booster"
# "memorig_bh" 
var_label(master_bhps_ukhls_wide$memorig_bh)       <- "Sample origin at individual level (BHPS, non-harmonised version)"
# "sampst" 
var_label(master_bhps_ukhls_wide$sampst)       <- "Sample status: OSM, born to OSM, TSM, or PSM respondent type"


# --- label all INTERVIEW_DATE# variables ---
for (v in grep("^INTERVIEW_DATE", names(master_bhps_ukhls_wide), value = TRUE)) {
  var_label(master_bhps_ukhls_wide[[v]]) <- "Date of interview (NA = unknown)"
}

# --- label all INTERVIEW_STATUS# variables ---
for (v in grep("^INTERVIEW_STATUS", names(master_bhps_ukhls_wide), value = TRUE)) {
  var_label(master_bhps_ukhls_wide[[v]]) <- "Type of survey response"
}

# --- label all EMP_STATUS# variables ---
for (v in grep("^EMP_STATUS", names(master_bhps_ukhls_wide), value = TRUE)) {
  var_label(master_bhps_ukhls_wide[[v]]) <- "Employment status"
}


################################################################################
saveRDS(master_bhps_ukhls_wide, "output/master_bhps_ukhls_wide.rds")

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)


