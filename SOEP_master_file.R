############################# soep Main file ###################################
# The file creates a database with individual identifiers and personal characteristics
# It is an input for all the other biographies

# List of required packages for this task: "dplyr", "haven", "tidyr"
source("00_setting_work_space.R")

# read ppathl data 
by_wave <- haven::read_dta(paste0(folder_soep,"ppathl.dta"), 
                            col_select = c("pid","syear",
                                           "piyear",   # Year of Interview
                                           "nett1",    # Current survey status (old 1 digit)
                                           "sex",      # SEX
                                           "gebjahr",  # year of birth
                                           "gebmonat", # Month Of Birth
                                           "erstbefr", # Year First Surveyed, Netto=10-99
                                           "letztbef", # Year Of Last Survey, Netto=10-99
                                           "psample",  # Subsample Identifier
                            )) %>% 
  rename(
    SEX = sex,
    BORN_Y = gebjahr,
    BORN_M = gebmonat,
    FIRSTOBS_Y = erstbefr,
    LASTOBS_Y  = letztbef,
    INTERVIEW_STATUS = nett1
  ) %>% 
  mutate(
    BORN_M = ifelse(BORN_M<0, -1, BORN_M),
    SEX = ifelse(SEX<0 | SEX==3, -1, SEX),
    BORN_Y = ifelse(BORN_Y<0, -1, BORN_Y))



# modify LASTOBS_Y when it equals [-2] Does not apply

first_last = by_wave %>% group_by(pid) %>% 
  arrange(syear,.by_group = T) %>% 
  summarise(FIRSTOBS_Y = min(syear),
            LASTOBS_Y  = max(syear))

by_wave = merge(by_wave,first_last, by = c("pid"), all.x = T) %>% 
  mutate( FIRSTOBS_Y = ifelse(FIRSTOBS_Y.x <0,FIRSTOBS_Y.y,FIRSTOBS_Y.x),
          LASTOBS_Y  = ifelse(LASTOBS_Y.x <0,LASTOBS_Y.y,LASTOBS_Y.x)) %>% 
  select(-c("LASTOBS_Y.x","LASTOBS_Y.y","FIRSTOBS_Y.x","FIRSTOBS_Y.y"))


# interview date 
int_date <- haven::read_dta(paste0(folder_soep, "/pgen.dta"),
                            col_select = c("pid", "syear", "pgmonth")) %>% 
  merge(select(by_wave, pid, syear,piyear), by = c("pid","syear"), all.x=T) %>% 
  mutate(
    pgmonth = ifelse(pgmonth < 0, 1, as.numeric(pgmonth)),
    INTERVIEW_DATE = paste0(piyear, "-",ifelse(pgmonth <= 9 & pgmonth > 0, paste0("0", pgmonth), pgmonth), "-01")) %>% 
  select(pid, syear, INTERVIEW_DATE) 


master_soep = merge(by_wave,int_date, by = c("pid", "syear"), all.x = T) %>% 
  select(pid, syear, BORN_Y, BORN_M,SEX, FIRSTOBS_Y, LASTOBS_Y, INTERVIEW_DATE, 
         INTERVIEW_STATUS,psample) %>% 
  mutate(INTERVIEW_STATUS = sjlabelled::as_character(INTERVIEW_STATUS)) %>% 
  mutate(across(c(BORN_Y, BORN_M, SEX, FIRSTOBS_Y, LASTOBS_Y), ~ as.integer(.))) %>% 
  mutate(
    INTERVIEW_STATUS = case_when(
      INTERVIEW_STATUS %in% c("[1] Successful Interview _P, _JUGEND") ~ 1L,
      INTERVIEW_STATUS %in% c("[2] Below Survey Age _KIND",
                              "[5] Interviewee Without Household Interview") ~ 2L,
      TRUE ~ -1L
    ),
    INTERVIEW_STATUS = labelled(
      INTERVIEW_STATUS,
      labels = c("fully responsive" = 1,
                 "proxy respondent" = 2,
                 "unknown" = -1)
    )
  )

master_soep_nonchange = master_soep %>% 
  select(pid,BORN_Y,BORN_M,SEX,FIRSTOBS_Y,LASTOBS_Y,psample) %>% distinct()

pequiv = haven::read_dta(paste0(folder_soep, "/pequiv.dta"),col_select = c("pid", "syear", "i11110")) %>% 
  mutate(i11110 = ifelse(i11110<0,NA,i11110))

emp_status <- haven::read_dta(paste0(folder_soep, "/pgen.dta"),
                              col_select = c("pid", "syear", "pglfs","pgstib", "pgemplst")) %>%
  mutate(
    work_status = case_when(
      # Clear working categories
      pgstib %in% c(
        110, 120, 130, 210, 220, 230, 240, 250,
        310, 320, 330, 340, 410:413, 420:423, 430:433, 440,
        510, 520, 521, 522, 530, 540, 550, 560,
        610, 620, 630, 640
      ) ~ "Working",
      
      # Clear not working categories
      pgstib %in% c(10, 12, 150) ~ "Not Working",
      
      # Clear non-labor-force categories
      pgstib %in% c(11, 140) ~ "Not in the Labor Force",
      
      # Retired
      pgstib == 13 ~ "Retired",
      
      # Missing or invalid
      pgstib %in% c(-1,-2, -3, -4, -5, -6, -7) ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    emp_status = case_when(
      # 1 = Working
      work_status == "Working" & pglfs %in% c(11) ~ "Working",
      
      # 2 = Unemployed
      pglfs == 6 &  work_status == "Unemployed" ~ "Unemployed",
      
      # 4 = Retired
      work_status == "Retired" | pglfs == 2 ~ "Retired",
      
      # 3 = Not in the Labor Force
      work_status == "Not in the Labor Force" |
        pglfs %in% c(1, 3, 4, 5, 8, 9, 10, 13) ~ "Not in the Labor Force",
      
      # Special rules
      pgemplst == 4 & pglfs == 12 ~ "Not in the Labor Force",  # Marginal + inactive
      pgemplst == 6 ~ "Not in the Labor Force",               # Sheltered workshop
      pgemplst == 3 & pglfs == 12 ~ "Not in the Labor Force", # Vocational + inactive
      
      # 5 = Other
      TRUE ~ work_status
    )
  )%>% 
  merge(pequiv, by = c("pid", "syear"), all.x = T) %>% 
  mutate( EMP_STATUS = ifelse( emp_status =="Working" & i11110 ==0, "Not Working", emp_status )) %>% 
  mutate(
    EMP_STATUS = case_when(
      EMP_STATUS %in% c("Not Working", "Not in the Labor Force", "Retired") ~ 0L,
      EMP_STATUS == "Working" ~ 1L,
      TRUE ~ -1L
    ),
    EMP_STATUS = labelled(
      EMP_STATUS,
      labels = c("not-working" = 0, "working" = 1, "unknown" = -1)
    )
  )

# merge all in long file
# emp_status,long_weights,

master_soep_long = merge(as.data.table(master_soep),as.data.table(emp_status), by = c("pid", "syear"), all.x = T) %>% 
  select(pid, syear,INTERVIEW_STATUS,INTERVIEW_DATE,EMP_STATUS)

# to wide format 
variables = c("INTERVIEW_STATUS","EMP_STATUS")
master_soep_long$INTERVIEW_DATE = as.character(master_soep_long $INTERVIEW_DATE)


master_soep_wide = master_soep_long %>% 
  pivot_wider(
  id_cols = c(pid),
  names_from = syear,
  values_from = c("INTERVIEW_STATUS", "INTERVIEW_DATE", "EMP_STATUS"),
  names_glue = "{.value}_{syear}"
)


master_soep_nonchange = master_soep_nonchange %>% 
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
  )
  
master_soep_wide = merge(master_soep_nonchange, master_soep_wide, by = c("pid"), all.x = T)

biobirth <- haven::read_dta(paste0(folder_soep,"biobirth.dta")) %>% 
  select("pid", "biovalid", "bioinfo","sumkids") %>% 
  mutate(ANYCHILD = ifelse(sumkids == 0, 2, ifelse(sumkids>0, 1,NA)))

master_soep_wide = merge(master_soep_wide, biobirth, by = c("pid"), all.x = T) %>% 
  select(-c("biovalid", "bioinfo", "sumkids")) %>% 
  mutate(ANYCHILD = labelled(as.integer(ANYCHILD),  # must be numeric
                             labels = c(
                               "yes" = 1,
                               "no" = 2,
                               "unknown" = -1)))


# Add variable labels (descriptions)
var_label(master_soep_wide$BORN_Y)         <- "Year of birth (-1 = unknown)"
var_label(master_soep_wide$SEX)            <- "Biological sex"
var_label(master_soep_wide$FIRSTOBS_Y)     <- "Year of first interview (-1 = unknown)"
var_label(master_soep_wide$LASTOBS_Y)      <- "Year of last interview (-1 = unknown)"
var_label(master_soep_wide$ANYCHILD)       <- "Any child at the last possible interview?"

# --- label all INTERVIEW_DATE# variables ---
for (v in grep("^INTERVIEW_DATE", names(master_soep_wide), value = TRUE)) {
  var_label(master_soep_wide[[v]]) <- "Date of interview (NA = unknown)"
}

# --- label all INTERVIEW_STATUS# variables ---
for (v in grep("^INTERVIEW_STATUS", names(master_soep_wide), value = TRUE)) {
  var_label(master_soep_wide[[v]]) <- "Type of survey response"
}

# --- label all EMP_STATUS# variables ---
for (v in grep("^EMP_STATUS", names(master_soep_wide), value = TRUE)) {
  var_label(master_soep_wide[[v]]) <- "Employment status"
}

saveRDS(master_soep_wide, "output/master_soep_wide.rds")


x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)



