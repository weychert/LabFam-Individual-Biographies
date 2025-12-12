######################## Partnership Biography PSID  ###########################
# List of required packages for this task and establish working directory
source("00_setting_work_space.R")


# Survey Research Center, Institute for Social Research. (2023). 
# A Panel Study of Income Dynamics: 1985–2021 marriage history file documentation (Release 1). The University of Michigan.

# list of steps:
# 1. Load Marriage History file
# 2. Rename variables from PSID Marriage History file
# 3. Generate unique IDs for individuals and their partners
# 4. Handle missing or special values in marriage order if spellnr==99 (MH9 ORDER OF THIS MARRIAGE  == 99 Inap) --> nr_marriages == 0 (never married (MH18=0))
# 5. Assign basic partnership status (note: PSID does not capture cohabitation)
# 6. Convert date variables to numeric (handling character strings and missing codes)
# 7. Create start year and month for each union
# 8. Categorize type of union end event
# 9. Create divorce-specific variables
# 10. Fix any placeholder month values used in PSID coding
# 11. Impute default dates for never-married individuals
# 12. Adding periods of being single (not married but it might be cohabitation)
# 13. Recalculate spells with periods of being single (not marired but it might be cohabitation)
# 14. Transform data from  wide to long format 


if (!file.exists(paste0(folder_partnership,"MH885.rds"))){
easyPSID::convert_to_rds(
  in_direc  = folder_partnership, # Directory containing unzipped PSID .txt and .do files
  out_direc = folder_partnership  # Directory to place PSID .rds files into
)} else{
  print("ok")
}

partner_biography_psid <-
  # 1. Load Marriage History file
  readRDS(paste0(folder_partnership, "/MH885.rds")) %>% 
  # 2. Rename variables from PSID Marriage History file
  rename(
    ER30001 = MH2,         #"1968 INTERVIEW NUMBER OF INDIVIDUAL#"  
    ER30002 = MH3,         #"PERSON NUMBER OF INDIVIDUAL#" 
    SEX = MH4,             #"SEX OF INDIVIDUAL#"
    BORN_M = MH5,          #"MONTH INDIVIDUAL BORN#"
    BORN_Y = MH6,          #"YEAR INDIVIDUAL BORN#"
    ER30001_spouse = MH7,  #"1968 INTERVIEW NUMBER OF SPOUSE#"                 
    ER30002_spouse = MH8,  #"PERSON NUMBER OF SPOUSE#"                         
    spellnr = MH9,         #"ORDER OF THIS MARRIAGE#"                          
    MARR_M = MH10,         #"MONTH MARRIED#"                                   
    MARR_Y = MH11,         #"YEAR MARRIED#"                                    
    MARR_STATUS = MH12,    #"STATUS OF THIS MARRIAGE#"   
    # 1	Marriage intact
    # 3	Marriage ended in widowhood
    # 4	Marriage ended in divorce or annulment
    # 5	Spouses separated
    # 7	Other: more recent marriage also reported, but no evidence of divorce or widowhood.
    # 8	NA; DK
    # 9	Inap.: never married (MH18=0)
    WIDOW_M = MH13,        #"MONTH WIDOWED OR DIVORCED#"                      
    WIDOW_Y = MH14,        #"YEAR WIDOWED OR DIVORCED#"                        
    SEP_M = MH15,          #"MONTH SEPARATED#"                               
    SEP_Y = MH16,          #"YEAR SEPARATED#" 
    nr_marriages = MH18,   #"Number of Marriages of This Individual" - never married (MH18=0)
    year_last = MH17
  ) %>% 
  # 3. Generate unique IDs for individuals and their partners
  mutate(pid  = ER30001*1000 + ER30002,
         partner_id  = ER30001_spouse*1000 + ER30002_spouse) %>%
  # 4. Handle missing or special values in marriage order if spellnr==99 (MH9 ORDER OF THIS MARRIAGE  == 99 Inap) --> nr_marriages == 0 (never married (MH18=0))
  mutate(spellnr = ifelse(spellnr==99,1,spellnr)) %>% 
  mutate(spellnr = ifelse(spellnr==98,0,spellnr)) %>% 
  # Sort and index marriages for each person
  group_by(pid) %>% 
  arrange(spellnr, .by_group = T) %>% 
  mutate(spell_nr = 1:n()) %>% 
  # 5. Assign basic partnership status (note: PSID does not capture cohabitation)
  mutate(PARTNERSHIP_STATUS = ifelse(nr_marriages==0,"no union", "marriage")) %>% 
  # 6. Convert date variables to numeric (handling character strings and missing codes)
  mutate(across(c("MARR_Y","MARR_M","SEP_Y","SEP_M", "WIDOW_M", "WIDOW_Y"), as.character)) %>% 
  mutate(across(c("MARR_Y","MARR_M","SEP_Y","SEP_M", "WIDOW_M", "WIDOW_Y"), as.numeric)) %>% 
  mutate(across(c("MARR_Y", "MARR_M", "SEP_Y", "SEP_M", "WIDOW_M", "WIDOW_Y"),~ 
                  ifelse(. %in%  c(99, 9999) & PARTNERSHIP_STATUS == "no union", NA, .)))%>% 
  mutate(across(c("MARR_Y", "MARR_M", "SEP_Y", "SEP_M", "WIDOW_M", "WIDOW_Y"), 
                ~ ifelse(. %in%  c(98, 9998,9999,99), -1, .))) %>% 
  # 7. Create start year and month for each union
  mutate(START_Y = MARR_Y,
         START_M = MARR_M ) %>%
  # 8. Categorize type of union end event
  mutate(
    END_UNION = case_when(
      MARR_STATUS == 1  ~ "ongoing not ended spell", # 1	Marriage intact
      MARR_STATUS == 3  ~"death of partner",# 3	Marriage ended in widowhood
      MARR_STATUS == 4  ~"divorce occurred",# 4	Marriage ended in divorce or annulment
      MARR_STATUS == 5  ~"Separation / break up",# 5	Spouses separated
      MARR_STATUS %in% c(7,8) ~ "other",# 7	Other: more recent marriage also reported, but no evidence of divorce or widowhood. # 8 NA	DK
      MARR_STATUS == 9  ~ NA,# 9	Inap.: never married (MH18=0)
      )) %>% 
  # 9. Create divorce-specific variables
  mutate(DIVORCE_Y = ifelse(END_UNION == "divorce occurred", WIDOW_Y,NA),
         DIVORCE_M = ifelse(END_UNION == "divorce occurred", WIDOW_M,NA),
         DIVORCE = ifelse(END_UNION == "divorce occurred","divorce occurred","divorce did not occurred"),
         END_UNION = ifelse(END_UNION == "divorce occurred","Separation / break up",END_UNION),
         END_Y = case_when(
           END_UNION == "divorce occurred" ~ SEP_Y,
           END_UNION == "death of partner" ~ WIDOW_Y,
           END_UNION == "Separation / break up"~ SEP_Y,
           TRUE ~ NA
           ),
         END_M = case_when(
           END_UNION == "divorce occurred" ~ SEP_M,
           END_UNION == "death of partner" ~ WIDOW_M,
           END_UNION == "Separation / break up"~ SEP_M,
           TRUE ~ NA
         ),
         ) %>% 
  # Select final variables for analysis
  select("pid", "BORN_Y","partner_id", "PARTNERSHIP_STATUS", "spellnr","spell_nr", 
         "START_Y","START_M", "END_Y", "END_M", "DIVORCE_Y","DIVORCE_M","DIVORCE",
         "END_UNION","nr_marriages", "year_last") %>% 
  # 10. Fix any placeholder month values used in PSID coding
  mutate(END_M = 
           case_when(
             END_M == 21 ~ 1,
             END_M == 22 ~ 5,
             END_M == 23 ~ 8,
             END_M == 24 ~ 10,
             TRUE ~ END_M # If no conditions are met, the TRUE ~ END_M line ensures that END_M remains unchanged.
           )) %>% 
  mutate(START_M = 
           case_when(
             START_M == 21 ~ 1,
             START_M == 22 ~ 5,
             START_M == 23 ~ 8,
             START_M == 24 ~ 10,
             TRUE ~ START_M # If no conditions are met, the TRUE ~ START_M line ensures that START_M remains unchanged.
           ),
         DIVORCE_M = 
           case_when(
             DIVORCE_M == 21 ~ 1,
             DIVORCE_M == 22 ~ 5,
             DIVORCE_M == 23 ~ 8,
             DIVORCE_M == 24 ~ 10,
             TRUE ~ DIVORCE_M # If no conditions are met, the TRUE ~ DIVORCE_M line ensures that START_M remains unchanged.
           )
         ) %>% 
  # 11. Impute default dates for never-married individuals
  mutate(START_Y = ifelse(PARTNERSHIP_STATUS =="no union", BORN_Y+15,START_Y),
         END_Y = ifelse(PARTNERSHIP_STATUS =="no union",year_last, END_Y),
         END_Y = ifelse(PARTNERSHIP_STATUS =="marriage" & END_UNION=="ongoing not ended spell" & END_Y ==-1,year_last, END_Y)
         )

# 12. Adding periods of being single (not married but it might be cohabitation)
partner_biography_psid_1 <- partner_biography_psid %>%
  # Ensure BORN_Y is numeric (sometimes read as character from RDS)
  # Create a 'year' column set to START_Y (i.e., year when marriage started)
  mutate(
    BORN_Y = as.numeric(as.character(BORN_Y)),
    year = START_Y
  ) %>%
  # Filter out records where birth year is 9998 (PSID code for missing)
  filter(BORN_Y != 9998)

# add at first age 
partner_biography_psid_2 = partner_biography_psid_1 %>% group_by(pid) %>% 
  mutate( 
    END_Y = ifelse(is.na(END_Y)& spell_nr == max(spell_nr),year_last, END_Y),
    age_first = if_else(spell_nr==1 & START_Y>0, START_Y - BORN_Y, NA)
    )



df_with_gaps <- partner_biography_psid_2 %>%
  arrange(pid, START_Y, END_Y) %>%
  group_by(pid) %>%
  mutate(
    next_start = lead(START_Y),
    next_spell = lead(spell_nr)
  ) %>%
  # keep rows where there is a gap of at least 1 full year
  filter(!is.na(next_start), next_start - END_Y > 1) %>%
  transmute(
    pid,
    PARTNERSHIP_STATUS = "no union",
    START_Y = END_Y + 1,
    END_Y   = next_start - 1
  )


df_with_gaps <- partner_biography_psid_2 %>%
mutate(
# start = first day of start month
START_DATE = make_date(START_Y, START_M, 1),
# end = last day of end month (if END_M is missing, treat as ongoing)
END_DATE = if_else(
  is.na(END_M),
  as.Date(NA),
  ceiling_date(make_date(END_Y, END_M, 1), "month") - days(1)
)) %>% 
  arrange(pid, START_DATE, END_DATE) %>%
  group_by(pid) %>%
  mutate(
    next_start = lead(START_DATE),
    next_end   = lead(END_DATE)
  ) %>%
  
  # gap exists if the next spell starts AFTER the day following the current spell's end
  filter(!is.na(next_start), next_start > END_DATE + days(1)) %>%
  
  transmute(
    pid,
    PARTNERSHIP_STATUS = "no union",
    START_DATE = END_DATE + days(1),
    END_DATE   = next_start - days(1)
  ) %>%
  ungroup()


df_with_gaps[df_with_gaps$pid=="2002",]

partner_biography_psid_4 = bind_rows(partner_biography_psid_2, df_with_gaps)

partner_biography_psid_4[partner_biography_psid_4$pid=="2002",]


partner_biography_psid_5 = partner_biography_psid_4 %>% 
  group_by(pid) %>% 
  arrange(START_Y, .by_group = T) %>% 
  fill(nr_marriages, BORN_Y,year_last, .direction = "down") %>% 
  mutate(
    check = ifelse(is.na(spell_nr), "x", as.character(spell_nr)),      # Label NAs as "x"
    check_1 = check != lag(check),                                     # Flag change in spell_nr
    check_1 = ifelse(is.na(check_1), TRUE, check_1),                   # Set TRUE for first row
    spell_nr= cumsum(check_1),                                      # Unique spell ID per person
  )
  

age_first = partner_biography_psid_5 %>% filter(spell_nr==1 & age_first>15) %>% 
  mutate(
    spell_nr = 0,
    PARTNERSHIP_STATUS = "no union",
    START_Y = BORN_Y+15,
    END_Y = START_Y-1,
    START_M = -1,
    END_M = -1
  ) %>% select(pid,spell_nr,PARTNERSHIP_STATUS,START_Y, START_M,END_Y,END_M)

partner_biography_psid_6 = bind_rows(partner_biography_psid_5,age_first) %>% 
  mutate(
    START_M = ifelse(is.na(START_M),  month(START_DATE),START_M),
    START_Y = ifelse(is.na(START_Y),  year(START_DATE),START_Y),
    END_M = ifelse(is.na(END_M),  month(END_DATE),END_M),
    END_Y = ifelse(is.na(END_Y),  year(END_DATE),END_Y)
  ) %>% 
  group_by(pid) %>% 
  fill(nr_marriages, BORN_Y,year_last, .direction = "updown") %>% 
  mutate(START_Y = as.integer(START_Y)) %>% 
  arrange(START_Y, .by_group = T) %>% 
  mutate(
    check = ifelse(is.na(spell_nr), "x", as.character(spell_nr)),      # Label NAs as "x"
    check_1 = check != lag(check),                                     # Flag change in spell_nr
    check_2 = PARTNERSHIP_STATUS !=lag(PARTNERSHIP_STATUS),
    check_1 = ifelse(is.na(check_1), TRUE, check_1),  
    check_2 = ifelse(is.na(check_2), TRUE, check_2),                   # Set TRUE for first row
    check_3 = check_1 | check_2,
    spell_nr= cumsum(check_3),
  ) %>% 
  mutate(START_M = ifelse(is.na(START_M), -1, START_M),
         END_M = ifelse(is.na(END_M), -1, END_M)
  ) %>% 
  mutate(
    END_SINGLE = case_when(
      PARTNERSHIP_STATUS == "no union" & lead(PARTNERSHIP_STATUS) == "marriage" ~ "marriage",
      PARTNERSHIP_STATUS == "no union" & is.na(lead(PARTNERSHIP_STATUS)) ~ "ongoing not ended spell",
      PARTNERSHIP_STATUS == "no union" & lead(PARTNERSHIP_STATUS) == "no union" ~ "ongoing not ended spell",
      TRUE ~ NA_character_ )) %>% 
  select(
    pid, BORN_Y, partner_id, PARTNERSHIP_STATUS, spell_nr, START_Y, START_M, END_Y, END_M, DIVORCE_Y, DIVORCE_M, DIVORCE, END_SINGLE,END_UNION, nr_marriages
  )

partner_biography_psid_6 =  partner_biography_psid_6 %>% 
  group_by(pid) %>% 
  mutate(
    check_1 = PARTNERSHIP_STATUS ==lag(PARTNERSHIP_STATUS),
    check_2 = spell_nr ==lag(spell_nr),
    check_3 = check_1 & check_2
  ) %>% 
  mutate(
    use_lead = !is.na(lead(check_3)) & lead(check_3),
    END_Y = if_else(use_lead, coalesce(lead(START_Y), END_Y), END_Y),
    END_M = if_else(use_lead, coalesce(lead(START_M) - 1, END_M), END_M)
  ) %>%
  filter(!use_lead | is.na(check_3)) %>% select(-c("check_1", "check_2","check_3", "use_lead"))

################################################################################
# to integer
# labels 

partner_biography_psid_7 = partner_biography_psid_6 %>% 
  mutate(across(c("START_Y", "END_Y","START_M", "END_M", "DIVORCE_Y", "DIVORCE_M"), as.integer)) %>% 
  mutate(
    PARTNERSHIP_STATUS = case_when(
      # [0] no union
      # [1] cohabitation
      # [2] marriage
      PARTNERSHIP_STATUS =="no union" ~ 0,
      PARTNERSHIP_STATUS =="cohabitation"~ 1,
      PARTNERSHIP_STATUS =="marriage"~ 2,
      PARTNERSHIP_STATUS =="gap"~ 3
    ),
    END_UNION = case_when(
      END_UNION == "-1" ~ -1,
      END_UNION == "ongoing not ended spell" ~ 0,
      END_UNION == "marriage"                ~ 1,
      END_UNION == "Separation / break up"  ~ 2,
      END_UNION == "death of partner"        ~ 3,
      END_UNION == "unknown end" ~ -1,
    ),
    END_SINGLE = case_when(
      END_SINGLE == "ongoing not ended spell" ~ 0,
      END_SINGLE == "cohabitation" ~ 1,
      END_SINGLE == "marriage" ~ 2,
      END_SINGLE == "unknown end" ~ -1
    ),
    DIVORCE =   case_when(

      DIVORCE== "divorce did not occurred" ~ 0,
      DIVORCE== "divorce occurred"  ~ 1)
  ) %>% 
  mutate(across(c("START_Y", "END_Y","START_M", "END_M", "DIVORCE","DIVORCE_Y", "DIVORCE_M"), as.integer)) %>% 
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
        "unknown end" = -1,
        "ongoing not ended spell" = 0,
        "marriage"= 1,
        "separation"= 2,
        "death of partner"= 3)),

    END_SINGLE = labelled(
      x = as.integer(END_SINGLE),
      labels = c(
        "ongoing not ended spell" = 0,
        "cohabitation" = 1,
        "marriage" = 2,
        "unknown end" = -1
      )),
    DIVORCE =  labelled(
      x = as.integer(DIVORCE),  # must be numeric
      labels = c(
        "divorce did not occurred" = 0,
        "divorce occurred" = 1))
  ) %>% rename(PARTNER_ID = partner_id)
  

################################################################################
# 14. Transform data from  wide to long format 

max_c = max(partner_biography_psid_7$spell_nr)

partner_biography_psid_wide<-partner_biography_psid_7 %>%
  pivot_wider(
    id_cols = c(pid),
    names_from = spell_nr,
    values_from = c("PARTNERSHIP_STATUS","PARTNER_ID", "START_Y","START_M","END_Y","END_M","DIVORCE_Y","DIVORCE_M","DIVORCE",
                    "END_UNION","END_SINGLE"),
    names_glue = "{.value}_{spell_nr}"
  )


# order variables 
ordering_variables_wide_format <- function(x_c) {
  lista_order_20<-c()
  for (i in 1:x_c) {
    x<-c(
      paste0("PARTNERSHIP_STATUS_", i),
      paste0("PARTNER_ID_", i),
      paste0("START_Y_", i),
      paste0("START_M_", i),
      paste0("END_Y_", i),
      paste0("END_M_", i),
      paste0("DIVORCE_Y_", i),
      paste0("DIVORCE_M_", i),
      paste0("DIVORCE_", i),
      paste0("END_UNION_", i),
      paste0("END_SINGLE_", i)
      )
    
    lista_order_20<- append(lista_order_20, x)
  }
  return(lista_order_20)
}

partner_biography_psid_wide_1<-partner_biography_psid_wide[,c("pid",ordering_variables_wide_format(max_c))]

# add internal variable psid	ER32034 - Number of Marriages of this Individual

files <- list.files(path = folder_family_files, pattern = ".zip$")
outDir <- paste0(folder_family_files , "/new_family_folder_psid")

# 99 98 NA; DK --> No marriage history was collected for this individual in 1985-2021 (ER32033=9999)

nr_marriages = readRDS(paste0(outDir,"/ind2021.rds")) %>% 
  mutate(pid  = ER30001*1000 + ER30002) %>% select(pid,ER32034) %>% 
  mutate(if_has_marr_hist = ifelse(ER32034 %notin% c(99,98), "has marr hist", "no marr hist"),
         marriage_nr = ifelse(ER32034 %in% c(99,98), NA, as.numeric(as.character(ER32034))),
         ) %>% select(-ER32034)


partner_biography_psid_wide_2 = merge(partner_biography_psid_wide_1, nr_marriages,
                                      by = c("pid"), all.x = T
                                      )

# Add stata labels

for (i in 1:max_c) {
  
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
  
  
  var_label(partner_biography_psid_wide_2 [[var_status]]) <- paste("Partnership status in spell", i, 
                                                                "([0] No union, [1] Cohabitation, [2] Marriage)")
  
  var_label(partner_biography_psid_wide_2 [[var_pid]]) <- paste("Partner ID in spell", i)
  
  var_label(partner_biography_psid_wide_2 [[var_start_y]]) <- paste("Year when partnership started in spell", i)
  var_label(partner_biography_psid_wide_2 [[var_start_m]]) <- paste("Month when partnership started in spell", i)
  
  var_label(partner_biography_psid_wide_2 [[var_end_y]]) <- paste("Year when partnership ended in spell", i)
  var_label(partner_biography_psid_wide_2 [[var_end_m]]) <- paste("Month when partnership ended in spell", i)
  
  var_label(partner_biography_psid_wide_2 [[var_div_y]]) <- paste("Year of divorce in spell", i)
  var_label(partner_biography_psid_wide_2 [[var_div_m]]) <- paste("Month of divorce in spell", i)
  
  var_label(partner_biography_psid_wide_2 [[var_div]]) <- paste("Divorce status in spell", i, 
                                                             "([1] Divorce occurred, [0] Divorce did not occur)")
  
  var_label(partner_biography_psid_wide_2 [[var_end_type]]) <- paste("How the union ended in spell", i, 
                                                                  "([0] Ongoing, [1] Marriage, [2] Separation/break-up, [3] Death of partner)")
  
  var_label(partner_biography_psid_wide_2 [[var_END_SINGLE]]) <- paste("How the single spell ended in spell", i, 
                                                                    "([0] Ongoing, [1] Cohabitation, [2] Marriage)")
  
  
}

var_label(partner_biography_psid_wide_2$marriage_nr) <- paste("Number of marriages")
var_label(partner_biography_psid_wide_2$if_has_marr_hist) <- paste("if has marriage history:has marr hist, no marr hist")

# correction here
saveRDS(partner_biography_psid_wide_2, "output/psid_partnership.rds")



if (type_you_want==".csv") {
  write.csv(partner_biography_psid, "output/psid_partnership.csv")
}else if(type_you_want==".dta"){
  write_dta(partner_biography_psid, "output/psid_partnership.dta")
  
}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk", 
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3", 
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1" 
)]

rm(list = x)













