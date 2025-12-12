######################### Partnership biography  in SOEP ######################
# Setting up the Workspace
source("00_setting_work_space.R")
# This R script is designed to process and clean data related to union and 
# marriage spells using the German Socio-Economic Panel (SOEP) study datasets. 
# The script performs various data transformations and merges to construct a 
# detailed history of marital and union spells for individuals, incorporating retrospective and survey information.
# list of steps
# 1. we use only  files  BIOCOUPLM (for prospective data)  and  BIOMARSY (for retrospective data)
# 2. Changes to BIOMARSY
# Drop spells that are from Personnal Questionnaire (prospective)
# These data are on spell format
# 3. Changes to BIOCOUPLM
# We recode partnership status and  create end_ variables in each file
# expand to year and add partner id from file pgen
# Transform again to spell format
# 4. Combining the files
# We first stack the databases: BIOMARSY followed by BIOCOUPLM (by individual ID)
# The last spell in BIOMARSY = first spell in BIOCOUPLM, but in BIOCOUPLM the spell is 
# left-censored (start date = first observation in SOEP). So, we impute the date from the start of the same spell in BIOMARSY
# We drop the duplicated spell from BIOMARSY
# 5. We renumber  spells based on the  following conditions: (we group by pid and spellnr)
# a. PARTNERSHIP_STATUS!=lag(PARTNERSHIP_STATUS)
# b. PARTNER_ID != lag(PARTNER_ID)
# c. spellnr!=lag(spellnr)
# 6. Transform spell data to wide format
################################################################################
# 1. We use only  files  BIOCOUPLM (for prospective data)  and  BIOMARSY (for retrospective data)
# 2. Chages to BIOMARSY
# Drop spells that are from Personnal Questionnaire (prospective)
# These data are on spell format

biomarsy_retro =  read_dta(paste0(folder_soep,"biomarsy.dta")) %>% 
  filter(source %in% c(1)) %>% # [1] Biography-questionnaire
  mutate(flag_source="retro") %>% 
  mutate(
    PARTNERSHIP_STATUS = case_when(
      # [1] single
      spelltyp %in% c(1)~ "no union",
      # [7] living in reg. same-sex partnership
      # [2] married
      spelltyp %in% c(2,7) ~ "marriage",
      # [3] divorced / reg. same-sex p. annulled
      spelltyp %in% c(3) ~ "divorced",
      # [4] widowed / reg. same-sex p. deceased
      # [5] divorced or widowed / reg. same-sex p. annulled or deceased
      spelltyp %in% c(4,5) ~ "widowed",
      # [6] married, separated / reg. same-sex p., separated
      spelltyp %in% c(6) ~ "separation",
      spelltyp %in% c("[3] coupled, partner in household") ~ "cohabitation",
      # [9] gap
      spelltyp %in% c(9) ~ "gap",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(pid, spellnr, PARTNERSHIP_STATUS,  beginy, endy, censor, flag_source, spelltyp) %>% 
  group_by(pid) %>% 
  arrange(spellnr, .by_group = T) %>% 
  mutate(
    # Mark a divorce event: if currently married and next spell is 'divorced'
    DIVORCE = if_else(PARTNERSHIP_STATUS == "marriage" & lead(PARTNERSHIP_STATUS)=="divorced", 1, 0),
    DIVORCE = ifelse(is.na(DIVORCE),0,DIVORCE),
    # Record the year of divorce from the next spell's 'beginy', if divorce occurred
    DIVORCE_Y = if_else(DIVORCE==1, lead(beginy), NA),
    # Recode 'divorced' as 'no union' to standardize partnership states
    PARTNERSHIP_STATUS = if_else(PARTNERSHIP_STATUS=="divorced", "no union", PARTNERSHIP_STATUS),
    # Mark a partner's death: if currently married and next spell is 'widowed'
    pdeath = if_else(PARTNERSHIP_STATUS == "marriage" & lead(PARTNERSHIP_STATUS)=="widowed", 1, 0),
    pdeath = ifelse(is.na(pdeath),0,pdeath),
    # Recode 'widowed' as 'no union' to standardize partnership states
    PARTNERSHIP_STATUS = if_else(PARTNERSHIP_STATUS=="widowed", "no union", PARTNERSHIP_STATUS),
    END_M  = -1,
    beginy = ifelse(beginy<0,-1,beginy)
         )

################################################################################
# 3. Changes to BIOCOUPLM
# We recode partnership status and  create end_ variables in each file
# expand to year and add partner id from file pgen
# Transform again to spell format

biocouplm = read_dta(paste0(folder_soep,"biocouplm.dta")) %>% 
  mutate(begin = as.Date("1983-01-01") %m+% months(begin-1),# [1] Jan 1983 add comments to what is m+ etc 
         end = as.Date("1983-01-01") %m+% months(end-1)) %>% 
  select(pid,spelltyp, spellnr,beginy,endy,begin,end, censor,events, remark, divorce, pdeath) %>% 
  mutate(
      PARTNERSHIP_STATUS = case_when(
        # "[5] single",
        # "[8] reg. same-sex p.,partner not in household"
        spelltyp %in% c(5,8)~ "no union",
        # "[1] married, spouse in household", 
        # "[2] married, spouse not in household",
        # "[7] reg. same-sex p.,partner in household","reg. same-sex p.,partner not in household"
        spelltyp %in% c(1,2,7) ~ "marriage",
        # "[6] married / reg. same-sex p., separated" 
        spelltyp == 6~ "separation",
        # [3] coupled, partner in household]
        # [4] coupled, partner not in household
        spelltyp %in% c(3,4) ~ "cohabitation",
        # "[98] unknown", "[99] unit nonresponse"
        spelltyp %in% c(98,99) ~ "gap",
        TRUE ~ NA_character_
      )
  ) %>% 
  select(pid, spellnr,PARTNERSHIP_STATUS,beginy, endy, begin, end, divorce, pdeath, censor, spelltyp) %>% 
  group_by(pid) %>% arrange(spellnr, .by_group = T) %>% 
  mutate(
    pdeath = ifelse(pdeath<0, 0, pdeath),
    divorce  = ifelse(divorce <0, 0, divorce ),
    DIVORCE = ifelse(divorce==0 & lead(divorce)==1, 1, 0),
    DIVORCE = ifelse(is.na(DIVORCE), 0,DIVORCE),
    DIVORCE_Y = ifelse(divorce==0 & lead(divorce)==1, lead(beginy), NA),
    DIVORCE_M =  if_else(divorce==0 & lead(divorce)==1, month(lead(end)), NA),
    PARTNERSHIP_STATUS = ifelse(PARTNERSHIP_STATUS == "separation", "no union", PARTNERSHIP_STATUS)
  )


################################################################################
# delete overlap

dt_1_wide = 
  biocouplm %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = spellnr,
    values_from = c("beginy",  "endy", "censor"),
    names_glue = "{.value}_{spellnr}"
  )

df = dt_1_wide

max_1 = max(biocouplm$spellnr)
data_check = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  ## 1) Start of t+1 before end of t
  
  df[[paste0("START_Y_", j)]]<df[[paste0("END_Y_", i)]]
  
  x = df %>% 
    summarise(n = sum(.data[[paste0("beginy_", j)]] < 
                        .data[[paste0("endy_", i)]], na.rm = TRUE)) 
  
  
  # x =x %>% rename( !!paste0("n_", i) := n )
  
  x = x %>% mutate(spellnr = i) 
  
  data_check= bind_rows(data_check, x)
  
}

data_check %>% 
  merge(select(biocouplm, pid, spellnr), by = c("pid", "spellnr", "spellnr")) %>% 
  group_by(spellnr,n) %>% summarise(n()) %>% filter(n==1)

biocouplm_1 = biocouplm  %>% 
  merge(data_check, by = c("pid", "spellnr", "spellnr")) %>% 
  filter(n!=1 & spelltyp!=6)

##### examine overlap 

dt_1_wide = 
  biocouplm_1 %>% 
  pivot_wider(
    id_cols = "pid",
    names_from = spellnr,
    values_from = c("beginy",  "endy", "censor"),
    names_glue = "{.value}_{spellnr}"
  )

df = dt_1_wide

max_1 = max(biocouplm_1$spellnr)
data_check_1 = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  x = df %>% 
    summarise(n = sum(.data[[paste0("beginy_", j)]] < 
                        .data[[paste0("endy_", i)]], na.rm = TRUE)) 

  
  x = x %>% mutate(spellnr = i) 
  
  data_check_1 = bind_rows(data_check_1, x)
  
}

################################################################################
# expand dates  to add PARTNER_ID
# prepare the end dates so we may expand it 
biocouplm_2 = biocouplm_1 %>% mutate(end = end -1) %>% 
  # If the variable check equals -1, then replace end with the last day of the month that begin falls in.
  # Otherwise, keep the existing value of end.
  mutate(check  = end -begin,
         end = if_else(check==-1, ceiling_date(begin, "month") - days(1), end)
         )

# biocouplm_2 %>%group_by(pid) %>% arrange(spellnr, .by_group = T) %>%
#   mutate(check  = end -lead(begin)) %>% group_by(check) %>% summarise(n())
# check    `n()`
# <drtn>   <int>
# 1 -1 days  70708
# 2 NA days 106701

# Takes the dataset biocouplm_2 and expands each record so that every month between begin and end becomes its own row.
# This uses neatRanges::expand_dates().
# For each original spell/interval, the function generates one row per month covered by that interval.
# Keeps the listed variables from the original data (pid, spellnr, PARTNERSHIP_STATUS, beginy, endy, begin, end, censor, DIVORCE, DIVORCE_Y, DIVORCE_M, pdeath) and attaches them to every expanded month for that spell.
# Creates a new dataset called biocouplm_expanded containing these expanded rows.
# Adds a new variable year by:
# Converting the expanded monthly date (named Expanded by the function) into a proper date,
# Extracting the year,
# Converting it to numeric.
biocouplm_expanded <- neatRanges::expand_dates(
  biocouplm_2 ,
  start_var = "begin",
  end_var = "end",
  vars_to_keep = c("pid","spellnr","PARTNERSHIP_STATUS","beginy", "endy", "begin", "end",  "censor", "DIVORCE", "DIVORCE_Y", "DIVORCE_M", "pdeath"),
  unit = "month") %>% 
  mutate(year = as.numeric(format(as.Date(Expanded), "%Y")))

# add PARTNER_ID

# `pgen`: General person-level data from SOEP (needed for including of partner id).
pgen = haven::read_dta(paste0(folder_soep,"pgen.dta")) %>% 
  select(pid, syear, pgmonth, pgpartnr) %>% rename(PARTNER_ID = pgpartnr,  year = syear) %>% 
  mutate(PARTNER_ID  = ifelse(PARTNER_ID <0, NA, PARTNER_ID )) %>% 
  mutate(
    pgmonth_num = as.numeric(pgmonth),
    Expanded = make_date(year = year, month = pgmonth_num, day = 1)) %>% select(-year)

biocouplm_all_with_PARTNER_ID = biocouplm_expanded %>% 
  merge(pgen, by = c("pid", "Expanded"), all.x = T) %>% 
  mutate(spellnr = 1000+spellnr) %>% mutate(flag_source="biocouplm") %>% 
  group_by(pid,spellnr) %>% fill(PARTNER_ID,.direction = "down" )

# Transform again to spell format 
biocouplm_all_with_PARTNER_ID = biocouplm_all_with_PARTNER_ID %>% 
  as.data.frame() %>% 
  select(-c("year","Expanded", "pgmonth","pgmonth_num")) %>% distinct()


# Starts with the dataset biocouplm_all_with_PARTNER_ID.
# Creates a modified version of PARTNER_ID:
# If the person’s PARTNERSHIP_STATUS is “cohabitation” or “marriage”, keep the existing PARTNER_ID.
# For all other statuses, set PARTNER_ID to NA.
# (This ensures partner IDs only exist when the person is in a partnership.)
# Groups the data by individual (pid) and sorts each person’s spells by spellnr.
# Checks each spell against the next spell for that same person:
# If the current spell number is the same as the next spell number and PARTNER_ID is missing, then the current row is marked "delete".
# Otherwise, it is marked "keep".
# Removes (“filters out”) any rows marked "delete", keeping only the "keep" rows.

biocouplm_all_with_PARTNER_ID_1 = biocouplm_all_with_PARTNER_ID %>% 
  mutate(PARTNER_ID = ifelse(PARTNERSHIP_STATUS %in% c("cohabitation", "marriage"), PARTNER_ID, NA)) %>% 
  group_by(pid) %>% arrange(spellnr, .by_group = T) %>% 
  mutate(
    check = case_when(
      spellnr ==lead(spellnr) & is.na(PARTNER_ID) ~ "delete",
      TRUE ~ "keep")) %>% filter(check =="keep")


################################################################################
# 4. Combining the files
# We first stack the databases: BIOMARSY followed by BIOCOUPLM (by individual ID)
# The last spell in BIOMARSY = first spell in BIOCOUPLM, but in BIOCOUPLM the spell is 
# left-censored (start date = first observation in SOEP). So, we impute the date from the start of the same spell in BIOMARSY
# We drop the duplicated spell from BIOMARSY 

all_combined = bind_rows(biomarsy_retro,
                         biocouplm_all_with_PARTNER_ID_1
                         ) %>% 
  group_by(pid) %>% arrange(spellnr, .by_group = T) %>% 
  # Once we append biomarsy and biocouplm, we need to look at censoring. 
  # If biomarsy ia censor == ”RC” and source == “Biography-questionnaire” and 
  # Biocouplm censor == “not censored | not in LC (1,2,7)” then we replace end date of biomarsy spell with lead(start Biocouplm)-1
  mutate(
    endy = case_when(
      flag_source == "retro" & censor %in% c(3:14) & lead(flag_source) == "biocouplm" &  lead(censor) %notin% c(1,2,7) ~ lead(beginy)-1,
      TRUE ~ endy))

################################################################################
# 5. We renumber  spells based on the  following conditions: (we group by pid and spellnr)
# a. PARTNERSHIP_STATUS!=lag(PARTNERSHIP_STATUS)
# b. PARTNER_ID != lag(PARTNER_ID)
# c. spellnr!=lag(spellnr)
all_combined = all_combined %>% 
  group_by(pid) %>% arrange(spellnr, .by_group = T) %>% 
  mutate(
    check1 = spellnr!=lag(spellnr),
    check2 = PARTNERSHIP_STATUS!=lag(PARTNERSHIP_STATUS),
    check3 = PARTNER_ID != lag(PARTNER_ID),
    check4 = check1 | check2 | check3,
    check5 = ifelse(is.na(check4), T,check4),
    spell_nr = cumsum(check5)
  )

################################################################################
# Create "END_SINGLE","END_UNION"
master = readRDS("output/master_soep_wide.rds")

all_combined_0 = all_combined %>% 
  merge(select(master, pid, BORN_Y), by = c("pid"), all.x = T) %>% 
  mutate(age_first = ifelse(beginy>0 & BORN_Y >0,beginy - BORN_Y, NA),
         beginy_1 = ifelse(age_first==0 & spell_nr==1,beginy+15, beginy),
         age_first = ifelse(beginy>0 & BORN_Y >0,beginy - BORN_Y, NA),
  )

x = all_combined_0 %>% filter(age_first>15, spell_nr==1) %>% 
  mutate(
    spell_nr = 0,
    beginy_1 = BORN_Y+15,
    endy = beginy,
    spellnr = NA,
    PARTNERSHIP_STATUS = "gap",
    DIVORCE = 0,
    DIVORCE_Y = NA,
    PARTNER_ID = NA,
    flag_source = "added",
    censor = "censor") %>% 
  select(pid, BORN_Y, beginy, endy, spell_nr,PARTNERSHIP_STATUS,DIVORCE,DIVORCE_Y,PARTNER_ID,flag_source)

all_combined_1 = all_combined_0 %>% bind_rows(x) %>% 
  mutate(
    START_Y = beginy,
    END_Y = endy,
    END_M = month(end),
    END_M = ifelse(is.na(END_M), -1,END_M),
    WIDOW_Y = pdeath,
    START_M = ifelse(spellnr !=1001, month(begin), NA),
    START_M = ifelse(is.na(START_M), -1,START_M)
  ) %>% 
  select(
    pid, PARTNER_ID, spell_nr, spellnr, PARTNERSHIP_STATUS,START_Y, START_M, END_Y, END_M,
    DIVORCE, DIVORCE_Y,DIVORCE_M, WIDOW_Y, censor, flag_source,beginy
  ) %>%
  group_by(pid) %>% arrange(spell_nr, beginy, .by_group = T) %>% 
  mutate(
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
      WIDOW_Y==1 ~ "death of partner",
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
  ) %>% group_by(pid) %>% arrange(spell_nr, beginy, .by_group = T) %>% 
  mutate(spell_nr = 1:n())


all_combined_1 = all_combined_1 %>% 
  mutate(
    START_Y = ifelse(is.na(START_Y), -1, START_Y),
    END_Y = ifelse(END_Y<0, -1, END_Y),
    DIVORCE_M = ifelse(!is.na(DIVORCE_Y) & is.na(DIVORCE_M), -1,DIVORCE_M)
  )

# check how may overlap due to combining retro and calendar 
dt_1_wide =
  all_combined_1 %>%
  pivot_wider(
    id_cols = "pid",
    names_from = spell_nr,
    values_from = c("START_Y",  "END_Y", "censor", "flag_source"),
    names_glue = "{.value}_{spell_nr}"
  )

df = dt_1_wide
 
max_1 = max(all_combined_1$spell_nr)
data_check_2 = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1

  ## 1) Start of t+1 before end of t
  x = df %>%
    summarise(n = sum(.data[[paste0("START_Y_", j)]] <
                        .data[[paste0("END_Y_", i)]], na.rm = TRUE))


  x = x %>% mutate(spell_nr = i)

  data_check_2 = bind_rows(data_check_2, x)

}


################################################################################
all_combined_2 = all_combined_1 %>% 
  # 11. label data in so numeric labeled in STATA
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
    PARTNERSHIP_STATUS =  labelled(
      x = as.integer(PARTNERSHIP_STATUS),  # must be numeric
      labels = c(
        "no union" = 0, 
        "cohabitation" = 1, 
        "marriage" = 2,
        "gap" = 3
        )),
    # How union spell ended:
    # [0] ongoing not ended spell
    # [1] marriage
    # [2] Separation
    # [3] death of partner
    END_UNION = case_when(
      END_UNION == "-1" ~ -1,
      END_UNION == "ongoing not ended spell" ~ 0,
      END_UNION == "marriage"                ~ 1,
      END_UNION == "separation"              ~ 2,
      END_UNION == "death of partner"        ~ 3,
      END_UNION == "unknown end" ~ -1,
      ),
    END_UNION =  labelled(
      x = as.integer(END_UNION),  # must be numeric
      labels = c(
        "unknown end" = -1,
        "ongoing not ended spell" = 0,
        "marriage"= 1,
        "separation"= 2,
        "death of partner"= 3)),
    # How single spell ended:
    # [0] ongoing not ended spell
    # [1] cohabitation
    # [2] marriage
    END_SINGLE = case_when(
      END_SINGLE == "ongoing not ended spell" ~ 0,
      END_SINGLE == "cohabitation" ~ 1,
      END_SINGLE == "marriage" ~ 2,
      END_SINGLE == "unknown end" ~ -1
    ),
    END_SINGLE = labelled(
      x = as.integer(END_SINGLE),
      labels = c(
        "ongoing not ended spell" = 0,
        "cohabitation" = 1,
        "marriage" = 2,
        "unknown end" = -1
      )
    ),
    # DIVORCE
    DIVORCE =  labelled(
      x = as.integer(DIVORCE),  # must be numeric
      labels = c(
        "divorce did not occurred" = 0,
        "divorce occurred" = 1)))



################################################################################
# 6. Transform spell data to wide format
max(all_combined$spell_nr, na.rm = T)
variables = c(
  "PARTNER_ID",
  "START_Y","START_M",
  "END_Y","END_M", 
  "END_SINGLE","END_UNION",
  "DIVORCE", "DIVORCE_Y","DIVORCE_M","censor"
)


all_combined_2$censor = sjlabelled::as_character(all_combined_2$censor)

partner_history_germany_wide = all_combined_2 %>%
  pivot_wider(
    id_cols = "pid",
    names_from = spell_nr,
    values_from = c("PARTNERSHIP_STATUS","PARTNER_ID", "START_Y","START_M",
                    "END_Y", "END_M","DIVORCE_Y","DIVORCE_M","DIVORCE","END_SINGLE", "END_UNION", "censor"),
    names_glue = "{.value}_{spell_nr}"
  )

# order variables

max_PARTNERSHIP_STATUS = length(select(partner_history_germany_wide, starts_with("PARTNERSHIP_STATUS_")) %>% names())-1

PARTNERSHIP_STATUS_order_var<-c()
for (i in 1:max_PARTNERSHIP_STATUS) {
  PARTNERSHIP_STATUS_order_var<-rbind(PARTNERSHIP_STATUS_order_var,
                                      paste0("PARTNERSHIP_STATUS_",i),
                                      paste0("PARTNER_ID_",i),
                                      paste0("START_Y_",i),
                                      paste0("START_M_",i),
                                      paste0("END_Y_",i),
                                      paste0("END_M_",i),
                                      paste0("END_SINGLE_",i),
                                      paste0("END_UNION_",i),
                                      paste0("DIVORCE_", i),
                                      paste0("DIVORCE_Y_",i),
                                      paste0("DIVORCE_M_",i),
                                      paste0("censor_",i)
  )
}


all_needed_vars<-c("pid",PARTNERSHIP_STATUS_order_var)

partner_history_germany_wide_1<-partner_history_germany_wide %>% select(all_needed_vars)

# add stata labels

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
  censor <- paste0("censor_",i)
  
  var_label(partner_history_germany_wide_1[[var_status]]) <- paste("Partnership status in spell", i, 
                                                                "([0] No union, [1] Cohabitation, [2] Marriage)")
  
  var_label(partner_history_germany_wide_1[[var_pid]]) <- paste("Partner ID in spell", i)
  
  var_label(partner_history_germany_wide_1[[var_start_y]]) <- paste("Year when partnership started in spell", i)
  var_label(partner_history_germany_wide_1[[var_start_m]]) <- paste("Month when partnership started in spell", i)
  
  var_label(partner_history_germany_wide_1[[var_end_y]]) <- paste("Year when partnership ended in spell", i)
  var_label(partner_history_germany_wide_1[[var_end_m]]) <- paste("Month when partnership ended in spell", i)
  
  var_label(partner_history_germany_wide_1[[var_div_y]]) <- paste("Year of divorce in spell", i)
  var_label(partner_history_germany_wide_1[[var_div_m]]) <- paste("Month of divorce in spell", i)
  
  var_label(partner_history_germany_wide_1[[var_div]]) <- paste("Divorce status in spell", i, 
                                                             "([1] Divorce occurred, [0] Divorce did not occur)")
  
  var_label(partner_history_germany_wide_1[[var_end_type]]) <- paste("How the union ended in spell", i, 
                                                                  "([0] Ongoing, [1] Marriage, [2] Separation/break-up, [3] Death of partner)")
  
  var_label(partner_history_germany_wide_1[[var_END_SINGLE]]) <- paste("How the single spell ended in spell", i, 
                                                                    "([0] Ongoing, [1] Cohabitation, [2] Marriage)")
  
  var_label(partner_history_germany_wide_1[[censor]]) <- paste("censor")
  
}

saveRDS(partner_history_germany_wide_1, "output/soep_partnership.rds")

if (type_you_want==".csv") {
  write.csv(partner_history_germany_wide_1, "output/soep_partnership.csv")
}else if(type_you_want==".dta"){
  write_dta(partner_history_germany_wide_1, "output/soep_partnership.dta")
  
}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)









