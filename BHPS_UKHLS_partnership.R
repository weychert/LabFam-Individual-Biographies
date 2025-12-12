######################### Partnership biography in BHPS&UKHLS ##################
# Steps in the Code:
# 1. Setup and Data Loading
# 2. We use UK Partnership Biography Data: file phistory_long.dta.
# The data source is:
#   - **Title**: Understanding Society: Marital and cohabitation Histories, 1991-2022.
# - **Edition**: 5th Edition.
# - **Publisher**: University of Essex, Institute for Social and Economic Research.
# - **DOI**: http://doi.org/10.5255/UKDA-SN-8473-5
# The dataset includes various partnership details such as partnership status, 
# partner IDs, start and end dates, divorce details, and cohabitation end information.
# 3. Cleaning phistory_long.dta
# a. Selection of important variables
# b. Renaming variables
# c. Defining partnership Status
# d. Prepare variables WIDOW_Y, WIDOW_M, PARTNER_ID, DIVORCE_Y, DIVORCE_M
# 4. Identifies gaps between partnership spells for each individual by sorting records by START_Y and START_M.
# A new spell with the label "no union" is created for each period where there’s a gap between relationships.
# gap_months is computed to define these periods of single-hood, ensuring no overlap with partnership spells.
# 5. Creates a continuous data set with start and end dates for both partnerships and non-union periods.
# 6. Stacks the original partnership records with single-hood periods created in the previous step.
# 7. Recalculates the spell number (spell_nr) for each individual.
# 8. Normalizes missing or invalid values, replacing them with -1 for missing year/month values.
# 9. Adding END_SINGLE and END_UNION Spell Transition Outcomes
# END_SINGLE: Defines how a spell of being single ended (either ending in cohabitation, marriage, or remaining ongoing).
# END_UNION: Defines how a union spell ended (either ending in marriage, separation, or death of partner).
# 10. Identifying Individuals Not in Partnership History and adding Those Who Did Not Appear:
# Identifies individuals who did not appear in the partnership history data but were marked as married or cohabiting in other sources.
# These individuals are added with their relevant partnership status (either "marriage" or "no union") and a single spell that begins when they reach the age of 18.
# 11. label data in so numeric labeled in STATA
# 12. Data Reshaping (Long to Wide Format)
# Reshaping Data: Converts the data from long format (where each row is a spell) to wide format (with each spell as a separate column).
# Labels are added for each variable, including partnership status, divorce status, and union end type.
# Saving and Exporting Data
# Final Data Export:
# The reshaped wide-format data set is saved as bhps_ukhls_partnership.rds for further analysis.
# If needed, the data can also be exported to .csv or .dta formats depending on user preferences.
# Labels are checked and saved in a separate file (check_labels_bhps_ukhls_partnership.csv), 
# ensuring the variable labels are clear and meaningful.
################################################################################
# 1. Setup and Data Loading
# Setting Up Workspace
source("00_setting_work_space.R")
# 2. We use UK Partnership Biography Data: file phistory_long.dta.

# 3. Cleaning phistory_long.dta
partner_biography_uk_long <-haven::read_dta(paste0(folder_part_uk,"phistory_long.dta"), 
                                            # a. Selection of important variables
                                            col_select = c("pidp",	"pid",	
                                                           "spellno",	# spell no, earlier first
                                                           "status",	# partnership status
                                                           "partner",	# partner pidp
                                                           "starty",	# start year
                                                           "startm",	# start month
                                                           "endy",	  # end year
                                                           "endm",	  # end month
                                                           "divorcey",# divorce year
                                                           "divorcem",# divorce month
                                                           "mrgend",	# how marriage/civil partnership ended
                                                           "cohend"   # how cohabitation ended
                                                           )) %>% 
  # b. Renaming variables
  rename(PARTNER_ID = partner,
         START_Y = starty,
         START_M =  startm,
         END_Y = endy,
         END_M = endm) %>% 
  # c. Defining Partnership Status
  mutate(
    # status
    # 2 - marriage
    # 3 - civil partnership
    # 10 - cohabitation (living together as a couple
    PARTNERSHIP_STATUS = case_when(
      status %in% c(2) ~"marriage", # marriage
      status %in% c(10,3) ~"cohabitation", # cohabitation
      ),
    # d. Prepare variables WIDOW_Y, WIDOW_M, PARTNER_ID, DIVORCE_Y, DIVORCE_M
    PARTNER_ID = ifelse(PARTNER_ID<0, NA, PARTNER_ID),
    DIVORCE = ifelse(divorcey>0, 1,0),
    DIVORCE_Y = ifelse(divorcey>0, divorcey, NA),
    DIVORCE_M = ifelse(divorcem>0, divorcem, NA),
    WIDOW_Y = ifelse(mrgend ==3, END_Y, NA),
    WIDOW_M = ifelse(mrgend ==3, END_M, NA)
  ) %>% select(pidp, spellno,PARTNERSHIP_STATUS,PARTNER_ID, START_Y, START_M, END_Y, END_M,
         DIVORCE, DIVORCE_Y, DIVORCE_M,WIDOW_Y, WIDOW_M,mrgend,cohend)

################################################################################
# 4. Identifies gaps between partnership spells for each individual by sorting records by START_Y and START_M.
# A new spell with the label "no union" is created for each period where there’s a gap between relationships.
# gap_months is computed to define these periods of singlehood, ensuring no overlap with partnership spells.
# ______________________________________________________________________________
# We identify temporal gaps between partnership spells for each individual. It begins 
# by grouping the data by individual (pidp) and arranging their partnership records 
# in chronological order based on the START_Y and START_M values, ensuring that events 
# are sequenced according to actual calendar time. Within each group, it computes a 
# continuous monthly representation of the start and end of each partnership (start_month and end_month) 
# only if both the start and end years are valid (greater than zero). It then looks ahead to the next 
# spell's start month (start_month_next). We calculate the time gap between spells in months (gap_months). 
# Additionally, full date objects (START_DATE, END_DATE) are created to create a new dataset that explicitly 
# defines periods of singlehood—intervals between relationships where individuals are not in a union. 
# It builds upon the earlier calculation of gap_months, which identifies time gaps between consecutive 
# partnerships. Here, only rows with a gap_months value greater than 1 and valid date components 
# (START_Y > 0 and END_Y > 0) are retained, indicating meaningful gaps between partnerships.
# For each of these gaps, the code constructs a new spell with the PARTNERSHIP_STATUS set to "no union". 
# The START_DATE of this singlehood period is defined as one month after the end of the previous partnership 
# (END_DATE %m+% months(1)), and the END_DATE is set to one month before the start of the next relationship 
# (START_DATE_NEXT %m-% months(1)). This ensures that the singlehood spell cleanly fits between partnerships 
# without overlapping them. The resulting dataset, partner_biography_uk_long_2, captures these non-union periods 
# in a structured format, allowing them to be analysed alongside cohabitation and marriage 
# spells to produce a complete relationship timeline.

partner_biography_uk_long_1 <- partner_biography_uk_long %>% group_by(pidp) %>% 
  arrange(START_Y, START_M, .by_group = TRUE) %>%  
  mutate(
    # Calculate the start date in months (from year 0), only if current END_Y and START_Y are positive
    start_month = if_else(START_Y > 0 & END_Y > 0, START_Y * 12 + START_M - 1, NA_integer_),
    
    # Calculate the end date in months (from year 0), same condition as above
    end_month = if_else(START_Y > 0 & END_Y > 0, END_Y * 12 + END_M - 1, NA_integer_),
    
    # Look ahead to get the start_month of the next row
    start_month_next = lead(start_month),
    
    # Calculate the gap between current end_month and next start_month
    gap_months = if_else(!is.na(start_month_next) & !is.na(end_month), start_month_next - end_month, NA_integer_),
    
    # Create actual start date, only when both END_Y and START_Y are positive
    START_DATE = if_else(START_Y > 0 & END_Y > 0, make_date(START_Y, START_M, 1), as.Date(NA)),
    
    # Create actual end date under the same condition
    END_DATE = if_else(START_Y > 0 & END_Y > 0, make_date(END_Y, END_M, 1), as.Date(NA)),
    
    # Get the start date from the next row
    START_DATE_NEXT = lead(START_DATE)
  ) 

partner_biography_uk_long_2 =
  partner_biography_uk_long_1 %>%
  filter(gap_months>1 & START_Y>0 & END_Y>0) %>%
  select(pidp, END_DATE, START_DATE_NEXT) %>% mutate(PARTNERSHIP_STATUS = "no union") %>%
  mutate(
    PARTNERSHIP_STATUS = "no union",
    START_DATE = END_DATE %m+% months(1),        # Add one month
    END_DATE = START_DATE_NEXT %m-% months(1)    # Subtract one month
  ) %>% select(pidp, PARTNERSHIP_STATUS, START_DATE,END_DATE) %>%
  mutate(START_Y = year(START_DATE),
         START_M = month(START_DATE),
         END_Y = year(END_DATE),
         END_M = month(END_DATE)) %>% select(-c("START_DATE", "END_DATE"))

################################################################################
# 5. Creates a continuous dataset with start and end dates for both partnerships and non-union periods.
# - Combine original and single hood records
# - Generate START_Y_x for sorting
# - Recalculate consistent spell numbers (spell_nr)
# - Normalize missing or invalid values (-1 for missing years/months)
# - Default DIVORCE to 0 if missing

start_at_15 = partner_biography_uk_long %>%
  merge(
    select(readRDS("output/master_bhps_ukhls_wide.rds"), pidp, BORN_Y, LASTOBS_Y),
    by = "pidp",
    all.x = TRUE
  ) %>%
  mutate(
    # Strip labels / tagged missings; adjust to your needs
    START_Y = as.integer(START_Y),
    END_Y   = as.integer(END_Y),
    BORN_Y  = as.integer(BORN_Y),
    check   = (spellno == 1L) & ((START_Y - BORN_Y) == 15L)
  ) %>%
  # Keep rows where check is FALSE
  filter(!check) %>%
  select(pidp, spellno, START_Y, BORN_Y) %>%
  mutate(
    check = (spellno == 1L) & (START_Y - BORN_Y == 15L)
  ) %>%
  select(pidp, START_Y, BORN_Y) %>%
  mutate(
    spellno = 0,
    END_Y             = START_Y - 1L,
    START_Y           = BORN_Y + 15L,
    PARTNERSHIP_STATUS = "no union"
  ) %>% select(pidp, spellno, START_Y,END_Y, PARTNERSHIP_STATUS)

partner_biography_uk_long_3 = partner_biography_uk_long_1 %>% 
  # 6. Stack the original partnership records with singlehood periods created in the previous step.
  bind_rows(partner_biography_uk_long_2) %>%
  bind_rows(start_at_15 ) %>% 
  select(
    pidp, spellno,PARTNERSHIP_STATUS,PARTNER_ID, START_Y, START_M, END_Y, END_M,
    DIVORCE, DIVORCE_Y, DIVORCE_M,WIDOW_Y, WIDOW_M
  ) %>% mutate(START_Y_x = ifelse(is.na(START_Y) | START_Y < 0, END_Y, START_Y),
               START_Y_x = ifelse(START_Y_x<0, 1000000000, START_Y_x)
               ) %>% 
  # 7. Recalculates the spell number (spell_nr) for each individual.
  group_by(pidp) %>% arrange(START_Y_x,END_Y, T) %>% # change to START_Y_x
  mutate(check = PARTNERSHIP_STATUS!=lag(PARTNERSHIP_STATUS),
         check = ifelse(is.na(check), T, check),
         check2 = spellno!=lag(spellno),
         check2 = ifelse(is.na(check2), T, check2),
         check3 = PARTNER_ID!=lag(PARTNER_ID),
         check3 = ifelse(is.na(check3), T, check3),
         check4 = check | check2 | check3,
         spell_nr = cumsum(check4)
         ) %>% 
  select(pidp, spell_nr,spellno, PARTNERSHIP_STATUS, 
         PARTNER_ID, START_Y, START_M, END_Y, END_M, 
         DIVORCE, DIVORCE_Y, DIVORCE_M, WIDOW_Y, WIDOW_M) %>% 
  # 8. Normalizes missing or invalid values, replacing them with -1 for missing year/month values.
  mutate(
    # Replace missing START_Y with -1
    START_Y = ifelse(is.na(START_Y) | START_Y<0, -1, START_Y),
    # Replace missing START_M with -1
    START_M = ifelse(is.na(START_M) | START_M <0, -1, START_M),
    # Replace missing END_Y with -1
    END_Y   = ifelse(is.na(END_Y) | END_Y <0, -1, END_Y),
    # Replace missing END_M with -1
    END_M   = ifelse(is.na(END_M) | END_Y <0, -1, END_M),
    # Replace missing DIVORCE status with 0 (assume no divorce)
    DIVORCE = ifelse(is.na(DIVORCE), 0, DIVORCE),
    WIDOW_Y = ifelse(WIDOW_Y <0, -1, END_M),
    WIDOW_M = ifelse(WIDOW_Y <0, -1, END_M)
  )

################################################################################
# 9. Adding END_SINGLE and END_UNION Spell Transition Outcomes
# END_SINGLE: Defines how a spell of being single ended (either ending in cohabitation, marriage, or remaining ongoing).
# END_UNION: Defines how a union spell ended (either ending in marriage, separation, or death of partner).

check = partner_biography_uk_long <-haven::read_dta(paste0(folder_part_uk,"phistory_long.dta")) %>% 
  select(pidp, spellno, mrgend,cohend) %>%
  mutate(mrgend = sjlabelled::as_character( mrgend ),
         cohend = sjlabelled::as_character(cohend ))

partner_biography_uk_long_4 =  partner_biography_uk_long_3 %>% 
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
      (spellno !=lead(spellno) & PARTNER_ID ==lead(PARTNER_ID)) & PARTNERSHIP_STATUS =="cohabitation" & lead(PARTNERSHIP_STATUS)=="cohabitation" ~ "separation",
      (is.na(PARTNER_ID) | is.na(lead(PARTNER_ID)))& PARTNERSHIP_STATUS =="marriage" & lead(PARTNERSHIP_STATUS)=="marriage" ~ "separation",
      # If widow year is recorded, the union ended in partner's death
      !is.na(WIDOW_Y) ~ "death of partner",
      # If currently cohabitation and next spell is marriage => upgraded to marriage
      PARTNERSHIP_STATUS %in% c("cohabitation") & lead(PARTNERSHIP_STATUS) == "marriage" ~ "marriage",
      # If no next spell exists and current one is known, it's still ongoing
      !is.na(PARTNERSHIP_STATUS) & is.na(lead(PARTNERSHIP_STATUS)) ~ "ongoing not ended spell",
      # Fallback
      TRUE ~ NA_character_
    ),
    END_SINGLE = ifelse(!is.na( END_SINGLE) & PARTNERSHIP_STATUS =="no union",END_SINGLE,NA ),
    END_UNION = ifelse(!is.na( END_UNION) & PARTNERSHIP_STATUS !="no union",END_UNION,NA ),
  ) %>% merge(check, by = c("pidp", "spellno"), all.x = T) %>% arrange(spell_nr) %>% 
  select(-c("WIDOW_Y", "WIDOW_M", "mrgend", "cohend"))

################################################################################
# 10. Identifying Individuals Not in Partnership History and adding Those Who Did Not Appear:
# Identifies individuals who did not appear in the partnership history data but were marked as married or cohabiting in other sources.
# These individuals are added with their relevant partnership status (either "marriage" or "no union") and a single spell that begins when they reach the age of 18.
# ______________________________________________________________________________
# Institute for Social and Economic Research. (2025, February). Understanding Society: 
# Marital and Cohabitation Histories, 1991–2023 (Version 2) [User guide]. University of Essex. 
# UK Data Service. https://doi.org/10.5255/UKDA-SN-8473-6 
# Those who had never responded, but their marital status was married or cohabiting
# as a couple, could not be included in this partnership history file due to lack of data
# on partnership spells. Similarly, in a few cases, adult respondents reported invalid
# dates for their current marriage and no past history and again could not be included
# in this partnership history file. You can identify these cases by comparing the
# variables evermar_dv (1 if ever married or in a civil partnership, 0 otherwise) and
# evercoh_dv (1 if ever cohabited, 0 otherwise) in xwavedat with ever_married,
# ever_civil_partnership & ever_cohabit variables in these partnership history files.

ever_married_phistory = partner_biography_uk_long_4 %>% group_by(pidp) %>% 
  mutate(ever_married = ifelse(PARTNERSHIP_STATUS =="marriage",1,0),
         ever_married = sum(ever_married),
         ever_married = ifelse(ever_married>0, 1,0)
         ) %>% select(pidp, ever_married) %>% distinct()

# inconsistencies between master EVER_MARRIED and partnership ever_married
# other =
#   select(readRDS("output/master_bhps_ukhls_wide.rds"),
#          pidp, BORN_Y, LASTOBS_Y, EVER_MARRIED) %>%
#   filter(EVER_MARRIED>=0) %>%
#   merge(check, by = c("pidp"))
# other %>% group_by(EVER_MARRIED==ever_married) %>%
#   summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))
#   `EVER_MARRIED == ever_married`     n total percent
# 1 FALSE                           3042 77287    3.94
# 2 TRUE                           74245 77287   96.1

master = select(readRDS("output/master_bhps_ukhls_wide.rds"), 
               pidp, BORN_Y, LASTOBS_Y, EVER_MARRIED) %>% 
  filter(EVER_MARRIED>=0) %>% 
  merge(ever_married_phistory, by = c("pidp"), all.x = T) %>% filter(pidp %notin% partner_biography_uk_long_4$pidp) %>% 
  mutate( START_DATE = make_date(BORN_Y+15, 1, 1),
          END_DATE = make_date(LASTOBS_Y, 1, 1)) %>% 
  mutate(PARTNERSHIP_STATUS = ifelse(EVER_MARRIED == 1, "marriage", "no union"),
         PARTNERSHIP_STATUS = ifelse(is.na(PARTNERSHIP_STATUS), "no union", PARTNERSHIP_STATUS),
         spell_nr = 1
         ) %>%  # start and end -1 # for single people start = year of 18 to end = lastobs 
  mutate(
    FIRST_YEAR = ifelse(BORN_Y>0, BORN_Y+15, -1),
    LASTOBS_Y = ifelse(LASTOBS_Y>0, LASTOBS_Y, -1),
    START_Y = ifelse(PARTNERSHIP_STATUS == "marriage", -1, NA),
    START_Y = ifelse(PARTNERSHIP_STATUS != "marriage", FIRST_YEAR, START_Y),
    END_Y = ifelse(PARTNERSHIP_STATUS == "marriage", -1, NA),
    END_Y = ifelse(PARTNERSHIP_STATUS != "marriage", LASTOBS_Y, END_Y),
    
    START_M  = -1,
    END_M = -1,
    END_SINGLE = ifelse(PARTNERSHIP_STATUS != "marriage", "ongoing not ended spell", NA),
    END_UNION = ifelse(PARTNERSHIP_STATUS == "marriage", "-1", NA),
    # PARTNER_ID = ,
    DIVORCE_Y = NA,
    DIVORCE_M = NA,
    DIVORCE = NA
  ) %>% 
  select("pidp", "spell_nr", "PARTNERSHIP_STATUS", "START_Y",
         "START_M","END_Y","END_M", "DIVORCE","DIVORCE_Y","DIVORCE_M",
         "END_SINGLE", "END_UNION"
         )

# encode from characters to integers

partner_biography_uk_long_5 = bind_rows(partner_biography_uk_long_4,master) %>% 
  # 11. label data in so numeric labeled in STATA
  mutate(
    PARTNERSHIP_STATUS = case_when(
      # [0] no union
      # [1] cohabitation
      # [2] marriage
      PARTNERSHIP_STATUS =="no union" ~ 0,
      PARTNERSHIP_STATUS =="cohabitation"~ 1,
      PARTNERSHIP_STATUS =="marriage"~ 2,
      PARTNERSHIP_STATUS == "gap" ~  3
      ),
  
    END_UNION = case_when(
      END_UNION == "-1" ~ -1,
      END_UNION == "ongoing not ended spell" ~ 0,
      END_UNION == "marriage"                ~ 1,
      END_UNION == "separation"              ~ 2,
      END_UNION == "death of partner"        ~ 3),
    
    END_SINGLE = case_when(
      END_SINGLE == "ongoing not ended spell" ~ 0,
      END_SINGLE == "cohabitation" ~ 1,
      END_SINGLE == "marriage" ~ 2,),
    DIVORCE =  labelled(
      x = as.integer(DIVORCE),  # must be numeric
      labels = c(
        "divorce did not occurred" = 0,
        "divorce occurred" = 1))) %>% 
  # integers
  mutate(across(
    c("PARTNERSHIP_STATUS", "START_Y", "START_M", "END_Y", "END_M", 
             "DIVORCE_Y", "DIVORCE", "END_UNION", "END_SINGLE"),
    as.integer
  ))


last = readRDS("output/master_bhps_ukhls_wide.rds") %>% select(pidp, LASTOBS_Y)

partner_biography_uk_long_5 = partner_biography_uk_long_5 %>% 
  merge(last, by = c("pidp"), all.x = T) %>% 
  group_by(pidp) %>% 
  mutate(
    last = as.integer(max(spell_nr)),
    END_Y = ifelse(last ==spell_nr, LASTOBS_Y, END_Y)
  )


partner_biography_uk_long_5 = partner_biography_uk_long_5 %>% 
  mutate(
    # labels
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

# examine overlap 

dt_1_wide = 
  partner_biography_uk_long_5 %>% 
  pivot_wider(
    id_cols = "pidp",
    names_from = spell_nr,
    values_from = c("START_Y",  "END_Y"),
    names_glue = "{.value}_{spell_nr}"
  )

max(partner_biography_uk_long_5$spell_nr)

max_1 = 31
data_check_1 = c()

for (i in 1:(max_1 - 1)) {   # i <= n_spells - 1
  j <- i + 1
  
  ## 1) Start of t+1 before end of t
  
  x = dt_1_wide %>% group_by(pidp) %>% 
    summarise(n = sum(.data[[paste0("START_Y_", j)]] < 
                        .data[[paste0("END_Y_", i)]], na.rm = TRUE)) 
  
  
  x = x %>% mutate(spell_nr = i) 
  
  x
  data_check_1= bind_rows(data_check_1, x)
  
}


partner_biography_uk_long_6 = merge(data_check_1, partner_biography_uk_long_5, by = c("pidp", "spell_nr"))

# Remove duplicated spells
partner_biography_uk_long_6 = partner_biography_uk_long_6 %>% filter(n!=1) %>% 
  group_by(pidp) %>% arrange(spell_nr, START_Y, .by_group = T) %>% 
  mutate(spell_nr=1:n())

# Correct values for start_y there are 60 osb with 14 

partner_biography_uk_long_6 = partner_biography_uk_long_6 %>% 
  mutate(
    START_Y = ifelse(START_Y<1909, -1,START_Y)
  )


################################################################################
# 12. Data Reshaping (Long to Wide Format)
# Reshaping Data: Converts the data from long format (where each row is a spell) to wide format (with each spell as a separate column).
# Labels are added for each variable, including partnership status, divorce status, and union end type.

variables_to_wide = c("PARTNER_ID","START_Y","END_Y", "START_M", 
                      "END_M", "END_SINGLE", "END_UNION",
                      "DIVORCE", "DIVORCE_Y", "DIVORCE_M")

partner_biography_uk_long_6 = as.data.table(partner_biography_uk_long_6)
max_x = max(partner_biography_uk_long_6$spell_nr)

partner_biography_uk_wide<-partner_biography_uk_long_6 %>%
  pivot_wider(
    id_cols = c(pidp),
    names_from = spell_nr,
    values_from = c("PARTNERSHIP_STATUS","PARTNER_ID","START_Y","END_Y", "START_M", 
                    "END_M", "END_SINGLE", "END_UNION",
                    "DIVORCE", "DIVORCE_Y", "DIVORCE_M"),
    names_glue = "{.value}_{spell_nr}"
  )

# Ordering Wide-Format Variables
ordering_variables_wide_format <- function(x_c) {
  lista_order<-c()
  for (i in 1:x_c) {
    x<-c(paste0("PARTNERSHIP_STATUS_", i),
         paste0("PARTNER_ID_", i),
         paste0("START_Y_", i),
         paste0("START_M_", i),
         paste0("END_Y_", i),
         paste0("END_M_", i),
         paste0("END_SINGLE_", i), 
         paste0("END_UNION_", i),
         paste0("DIVORCE_", i), 
         paste0("DIVORCE_Y_", i), 
         paste0("DIVORCE_M_", i))
    
    lista_order<- append(lista_order, x)
  }
  return(lista_order)
}

partner_biography_uk_wide<-partner_biography_uk_wide %>% as.data.frame() %>% select(c("pidp",ordering_variables_wide_format(max_x)))

# Add STATA labels

for (i in 1:max_x) {
  
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
  var_end_single <- paste0("END_SINGLE_", i)
  
  var_label(partner_biography_uk_wide[[var_status]]) <- paste("Partnership status in spell", i, 
                                                               "([0] no union, [1] cohabitation, [2] marriage)")
  
  var_label(partner_biography_uk_wide[[var_pid]]) <- paste("Partner ID in spell", i)
  
  var_label(partner_biography_uk_wide[[var_start_y]]) <- paste("Year when partnership started in spell", i)
  var_label(partner_biography_uk_wide[[var_start_m]]) <- paste("Month when partnership started in spell", i)
  
  var_label(partner_biography_uk_wide[[var_end_y]]) <- paste("Year when partnership ended in spell", i)
  var_label(partner_biography_uk_wide[[var_end_m]]) <- paste("Month when partnership ended in spell", i)
  
  var_label(partner_biography_uk_wide[[var_div_y]]) <- paste("Year of divorce in spell", i)
  var_label(partner_biography_uk_wide[[var_div_m]]) <- paste("Month of divorce in spell", i)
  
  var_label(partner_biography_uk_wide[[var_div]]) <- paste("Divorce status in spell", i, 
                                                            "([1] divorce occurred, [0] divorce did not occur)")
  
  var_label(partner_biography_uk_wide[[var_end_type]]) <- paste("How the union ended in spell", i, 
                                                                 "([0] ongoing, [1] marriage, [2] separation, [3] death of partner)")
  
  var_label(partner_biography_uk_wide[[var_end_single]]) <- paste("How the single spell ended in spell", i, 
                                                                   "([0] ongoing, [1] cohabitation, [2] marriage)")
  
}

# Saving and Exporting Data
# Final Data Export: The reshaped wide-format dataset is saved as 
# bhps_ukhls_partnership.rds for further analysis.

saveRDS(partner_biography_uk_wide, "output/bhps_ukhls_partnership.rds")

write_dta(partner_biography_uk_wide, "output/bhps_ukhls_partnership.dta")

if (type_you_want==".csv") {
  write.csv(partner_biography_uk_wide, "output/bhps_ukhls_partnership.csv")
}else if(type_you_want==".dta"){
  write_dta(partner_biography_uk_wide, "output/bhps_ukhls_partnership.dta")

}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files", "folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership", "folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2", "folder_soep","folder_uk_fertility_1"
)]

rm(list = x)






















