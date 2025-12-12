################## SOEP employment #############################################
# List of steps:
# We will use the following
# A. prepare interview employment status
# B.	artkalen.dta - calendar data
# ARTKALEN contains spells (monthly) for events starting in January 1983.
# The information on activity status is collected on a monthly basis in the yearly individual questionnaire and stored in the file ARTKALEN.
# This is in contrast to PBIOSPE, where spells were in yearly duration, and events previous to 1983 were included.
# We processes longitudinal employment spell data from the SOEP's artkalen.dta file
# to construct a monthly employment calendar for each individual. It begins by converting spell start and
# end indices into actual dates, labeling each spell type (e.g., "Full-Time", "Unemployment"),
# and expanding spells into monthly observations. These monthly records are then reshaped
# to identify overlapping spell types and categorized into standardized labor force groups:
# "Working", "Unemployed", "Not in the Labor Force", "Retired", and "Other".
# We merge this calendar with survey-based employment status at the
# time of interview to verify consistency and assign a final status per month.
# Finally, it identifies transitions between distinct employment states,
# collapses the data back into spells, and records the duration and type of each employment period.
# Steps:
# 1: Load and Prepare Raw Spell Data
# 2: Convert Numeric Time to Date Format
# 3: Label Spell Types
# 4: Expand Spells to Monthly Format
# 5: Identify Unique Spells Per Month
# 6: Reformat Spell Types to Wide Format
# 7: Categorize Spells into Standard Employment Categories
# 8: Assign Final Labels to Employment Categories
# 9: Merge with Interview Employment Status
# 10: Merge Spell Identifiers and Detect Transitions
# 11: Collapse Monthly Data Back to Spells
# ##########
# C.	pbiospe.dta – retrospective (prior entering the survey) and Information on activity status covers
# only the period up to the time the biography is collected
# The spell file PBIOSPE is based on the information on activity status over the life course,
# which is collected as a matrix from every respondent who completes the biographical questionnaire.
# The observations start at the age of 15 and end at the current age (up to age 65).
# To update ongoing employment information in PBIOSPE, information from the yearly individual questionnaire is also used
# spells are in yearly duration
# To update the ongoing occupational career in PBIOSPE, information from the yearly
# Individual Questionnaire is also used by aggregating the recorded spells from ARTKALEN into yearly values
# The dataset pbiospe.dta provides retrospective information on individuals'
# activity statuses starting from age 15 up to the time of the biographical interview,
# capturing life course employment histories in yearly resolution.
# The data processing begins with loading and formatting spell start and
# end years into proper date objects, assuming January 1st and December 31st,
# respectively. Each spell is labeled with a human-readable activity type, such as
# "Full-Time Employment" or "School, College." Only spells marked as biographical
# (or biographical with calendar overlap) are retained. These spells are expanded
# into annual records and pivoted to a wide format, where each activity type forms a column.
# Then, spells are grouped into standardized labor market categories
# (e.g., "Working", "Unemployed", "Retired"). Final employment statuses are
# derived, and overlapping or sequential spells are analyzed to detect transitions.
# Unique spells are re-identified based on changes in status or structure, collapsed back
# into continuous periods, and final spell durations are adjusted to end on December 31 of the
# respective end year.
# 1. Load and Prepare Raw Spell Data
# 2. Encode dates - create for year of start and end of the spell a date format. I assume 1st of January for now
# 3. Encode stell typ
# 4. we select from pbiospe the if spellinf [1] Biography Only and  [3] 1 Biography + 1 Calender Spell [-2] Does not apply
# 5. Expand Spells into Yearly Records
# 6. Pivot Spell Types to Wide Format
# 7. Categorize Into Labor Market Groups (LIB Categories)
# 8. Finalize Employment Status Classification
# 9. Identify Unique Spells Per Year
# 10. Merge and Track Transitions
# 11. Collapse Back to Spells
# 12. Adjust Final Spell Dates
# D. Merging and Harmonizing Employment Spells from pbiospe and artkalen
# This step merges and harmonizes employment spell data from two sources—pbiospe
# (retrospective biographies) and calendar (monthly calendar data). First, each dataset
# is tagged with a flag_source to retain provenance, and calendar spell IDs are offset to
# prevent overlap. The two datasets are then combined into a unified spell history (total_employment),
# sorted by individual and spell order. To avoid duplication, the first calendar spell is checked for
# redundancy—if it continues an identical employment status from pbiospe, its start date is adjusted or
# the entry is flagged for removal. After filtering redundant entries, a final employment spell identifier
# is constructed by detecting transitions in employment status or spell content. The result is a clean,
# continuous dataset of non-overlapping employment spells per person, with consistent status labels,
# spell boundaries, and clear identification of their data source.
# Step 1: Add Source Flags
# Step 2: Combine Both Datasets
# Step 3: Modify Redundant First Calendar Entry
# Step 4: Final Spell Identification
# Individual Questionnaire is also used by aggregating the recorded spells from ARTKALEN into yearly values

# download necessary packages and directories 
source("00_setting_work_space.R")

################################################################################
# A. prepare interview employment status 
monhth_int = haven::read_dta(paste0(folder_soep, "/pgen.dta"), col_select = c("pid","syear","pgmonth"))

year_int <- haven::read_dta(paste0(folder_soep,"ppathl.dta"), col_select = c("pid","syear","piyear", "nett1"))

date_int = merge(monhth_int, year_int, by = c("pid", "syear"),all = T) %>% 
  mutate(
    pgmonth = ifelse(pgmonth<0, NA,pgmonth),
    piyear = ifelse(piyear<0,NA, piyear),
    Expanded = as.Date(
      format(as.Date(paste0(piyear, "-", sprintf("%02d", pgmonth), "-01")), "%Y-%m-%d"))) %>% filter(!is.na(Expanded))

pequiv = haven::read_dta(paste0(folder_soep, "/pequiv.dta"),col_select = c("pid", "syear", "i11110")) %>% 
  mutate(i11110 = ifelse(i11110<0,NA,i11110))

emp_status_gsoep <- haven::read_dta(paste0(folder_soep, "/pgen.dta"),
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
    pgemplst_1 = case_when(
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
  ) %>% 
  merge(date_int, by = c("pid", "syear"), all.x = T) %>% 
  merge(pequiv, by = c("pid", "syear"), all.x = T) %>% 
  mutate( pgemplst_1 = ifelse( pgemplst_1=="Working" & i11110 ==0, "Not Working", pgemplst_1))

################################################################################
# B. artkalen.dta - calendar data 
# ARTKALEN contains spells (monthly) for events starting in January 1983. 
# The information on activity status is collected on a monthly basis in the yearly individual questionnaire and stored in the file ARTKALEN.
# This is in contrast to PBIOSPE, where spells were in yearly duration, and events previous to 1983 were included. 
# We processes longitudinal employment spell data from the SOEP's artkalen.dta file 
# to construct a monthly employment calendar for each individual. It begins by converting spell start and 
# end indices into actual dates, labeling each spell type (e.g., "Full-Time", "Unemployment"), 
# and expanding spells into monthly observations. These monthly records are then reshaped 
# to identify overlapping spell types and categorized into standardized labor force groups: 
# "Working", "Unemployed", "Not in the Labor Force", "Retired", and "Other". 
# We merge this calendar with survey-based employment status at the 
# time of interview to verify consistency and assign a final status per month. 
# Finally, it identifies transitions between distinct employment states, 
# collapses the data back into spells, and records the duration and type of each employment period.
# Steps:
# 1: Load and Prepare Raw Spell Data
# 2: Convert Numeric Time to Date Format
# 3: Label Spell Types
# 4: Expand Spells to Monthly Format
# 5: Identify Unique Spells Per Month
# 6: Reformat Spell Types to Wide Format
# 7: Categorize Spells into Standard Employment Categories
# 8: Assign Final Labels to Employment Categories
# 9: Merge with Interview Employment Status
# 10: Merge Spell Identifiers and Detect Transitions
# 11: Collapse Monthly Data Back to Spells

calendar <- 
  # 1: Load and Prepare Raw Spell Dat
  haven::read_dta(paste0(folder_soep,"/artkalen.dta"), col_select = c("pid", "spelltyp", "spellnr", "begin", "end")) %>% 
  # 2: Convert Numeric Time to Date Format
  mutate(  
      # Calculate the starting month of the spell:
      # - `begin - 1` ensures correct month indexing (since R indexes from 1)
      # - `%% 12` extracts the month within a year (remainder when divided by 12)
      # - `1 + (...)` shifts the result to match the standard month range (1-12)
     month_begin = 1 + ((begin - 1) %% 12), 
     # Calculate the starting year of the spell:
     # - `begin - 1` ensures correct year indexing
     # - `floor((begin - 1) / 12)` computes the number of full years elapsed
     # - `+ 1983` adjusts to the reference year (assuming the dataset starts in 1983)
     year_begin = floor((begin - 1) / 12) + 1983,
     # Calculate the ending month of the spell (same logic as `month_begin`)
     month_end  = 1+ ((end - 1) %% 12),
     # Calculate the ending year of the spell (same logic as `year_begin`)
     year_end = floor((end - 1) / 12) + 1983,
    date_start = as.Date(paste0(year_begin,"-",  ifelse(month_begin<=9, paste0("0",month_begin), as.character(month_begin)), "-01")),
    date_end = as.Date(paste0(year_end,"-", ifelse(month_end<=9, paste0("0",month_end ), as.character(month_end)), "-01")),
    date_end = lubridate::ceiling_date( date_end, "month")- lubridate::days(1)
    ) %>% 
  # encode the employment 
  # 3: Label Spell Types
  mutate(
    spelltyp_text = case_when(
      spelltyp == 1 ~ "Full-Time",                            # [1] Full-Time Employment
      spelltyp == 2 ~ "Short Work Hrs",                       # [2] Short Work Hrs
      spelltyp == 3 ~ "Part-Time",                            # [3] Part-Time Employment / Marginaly Emploeyd
      spelltyp == 4 ~ "Vocational Training",                  # [4] Vocational Training
      spelltyp == 5 ~ "Unemployment",                         # [5] Registered Unemployment
      spelltyp == 6 ~ "Retired",                              # [6] Retired
      spelltyp == 7 ~ "Maternity Leave",                      # [7] Maternity Leave
      spelltyp == 8 ~ "School, College",                      # [8] School, College
      spelltyp == 9 ~ "Military",                             # [9] Military, Community Service
      spelltyp == 10 ~ "Housewife, Husband",                  # [10] Housewife, Husband
      spelltyp == 11 ~ "Second Job",                          # [11] Second Job
      spelltyp == 12 ~ "Other",                               # [12] Other
      spelltyp == 13 ~ "First Job Training, Apprent",         # [13] First Job Training, Apprenticeship
      spelltyp == 14 ~ "Continuing Education, Retraining",    # [14] Continuing Education, Retraining
      spelltyp == 15 ~ "Minijob",                             # [15] Minijob (up to 400 Euro)
      spelltyp == 99 ~ "Gap"                                  # [99] Gap 
      ))

# 4: Expand Spells to Monthly Format
# expand dates --> one row one month for each individual
calendar_expanded <- neatRanges::expand_dates(calendar,                 # The dataset containing spells with start and end dates
                                              start_var = "date_start", # Column specifying the start date of each spell
                                              end_var = "date_end",     # Column specifying the end date of each spell
                                              # List of columns to retain in the expanded dataset
                                              vars_to_keep = c("pid", 
                                                    "spelltyp_text","spellnr",
                                                    "date_start", "date_end"), 
                                   unit = "month")                       # Expands spells so that each row corresponds to one month
           # Ensures that each row corresponds to one month, effectively breaking longer spells into monthly subspells.

# 5: Identify Unique Spells Per Month
# Calculate the unique spell per month 
spell_nr_by_month <- calendar_expanded %>%
  select(pid, Expanded, spellnr) %>%
  distinct() %>%
  pivot_wider(
    # Reshape data from long to wide format:
    # - `names_from = spellnr` → Creates separate columns for each spell number
    # - `values_from = spellnr` → Fills each column with its corresponding spell number
    # - `id_cols = c(pid, Expanded)` → Keeps `pid` and `Expanded` as unique identifiers
    names_from = spellnr,
    values_from = spellnr,
    id_cols = c(pid, Expanded)) 

setDT(spell_nr_by_month)  # Convert to data.table

spell_nr_by_month[, spell_x := apply(.SD, 1, function(row) paste(na.omit(row), collapse = " ")), 
                  .SDcols = 3:ncol(spell_nr_by_month)]

spell_nr_by_month_1 = spell_nr_by_month %>% select(pid, Expanded, spell_x) 
 
# 6: Reformat Spell Types to Wide Format
calendar_expanded_x_1 <- calendar_expanded %>%
  select(pid,Expanded, spellnr, spelltyp_text) %>%
  distinct() %>%
  tidyr::pivot_wider(
    # - `names_from = spelltyp_text` → Creates separate columns for each spell type
    # - `values_from = spellnr` → Fills each column with the corresponding spell number
    # - `id_cols = c(pid, Expanded)` → Keeps `pid` and `Expanded` as unique identifiers
    names_from = spelltyp_text,
    values_from = spellnr,
    id_cols  = c(pid,Expanded))


for (i in 3:ncol(calendar_expanded_x_1)) {
  calendar_expanded_x_1[,i]<-ifelse(is.na(calendar_expanded_x_1[,i]), NA, names(calendar_expanded_x_1[,i]))
}

# 7: Categorize Spells into Standard Employment Categories
calendar_expanded_x_2 = calendar_expanded_x_1 %>% mutate(
  # [1] Working
  # We take as employment status as the first non-missing 
  # `Full-Time`,`Part-Time`,`Second Job`,`Short Work Hrs`,Minijob, # non - disputable "Working"
  # Military,
  working = coalesce(`Full-Time`,`Part-Time`,`Second Job`,`Short Work Hrs`,Minijob, # non - disputable "Working"
                      Military, 
                     `Vocational Training`,             # spelltyp in artkalen: Vocational Training 4
                     `First Job Training, Apprent`      # spelltyp in artkalen:First Job Training, Apprenticeship 13
                     # we could consider this moving to Not in the Labor Force based on freq with pgemplst
                     ),
  # [2] Unemployed
  Unemployed = Unemployment,
  # [3] Not in the Labor Force
  # We take as employment status as the first non-missing 
  # `Housewife, Husband`, `School, College`,`Maternity Leave`,`Vocational Training`,   `First Job Training, Apprent`,  `Continuing Education, Retraining`
  not_lfs = coalesce(`Housewife, Husband`, `School, College`,`Maternity Leave`,
                     # Categories that correspond to Apprenticeship/Training in PBIOSPE according to Schmelzer, Paul; Hamjediers, Maik (2020)
                     # once we look at date of int pgemplst =="Vocational Training" then we see that in variable 98% say pglfs =="Working"
                     
                     `Continuing Education, Retraining` # Continuing Education, Retraining 14
                     ),
  # [4] Retired
  Retired = Retired,
  # [5] Other
  # We take as employment status as the first non-missing `Other`,`Gap`
  Other =  coalesce(`Other`,`Gap`)
) %>% select(pid, Expanded,working,Unemployed,not_lfs,Retired,Other)

# 8: Assign Final Labels to Employment Categories
calendar_expanded_x_3 = calendar_expanded_x_2 %>% 
  mutate(working = ifelse(!is.na(working), "Working", NA),
  # [2] Unemployed
  Unemployed = ifelse(!is.na(Unemployed), "Unemployed", NA),
  # [3] Not in the Labor Force
  not_lfs = ifelse(!is.na(not_lfs), "Not in the Labor Force", NA),
  # [4] Retired
  Retired = ifelse(!is.na(Retired), "Retired", NA),
  # [5] Other
  Other =  ifelse(!is.na(Other), "Other", NA))


# 9: Merge with Interview Employment Status
# add employment at the time of interview and number of spells per month 
emp_date_int = as.data.table(select(emp_status_gsoep, pid, syear, Expanded, pgemplst_1))

calendar_expanded_x_4 = merge(as.data.table(calendar_expanded_x_3), emp_date_int, by = c("pid", "Expanded"), all.x  = T) %>% 
  mutate(
    # working not working at the time of interview
    working_not_working_dt = case_when(
      pgemplst_1 =="Working" ~ "Working",
      pgemplst_1 %in% c("Unemployed","Retired","Not in the Labor Force") ~ "not-working",
      TRUE ~ NA_character_
    ),
    artkalen_working = working,
    # here correction 
    artkalen_NOT_working = coalesce(Unemployed, not_lfs, Retired, Other),
    artkalen_NOT_working = ifelse(!is.na(artkalen_NOT_working), "not-working", NA),
    # check if ARTKALEN have same status as date of interview 
    check_1 = working_not_working_dt == artkalen_working,
    check_2 = working_not_working_dt == artkalen_NOT_working
  ) %>% 
  mutate(
    employment_status = case_when(
      check_1 == T ~ artkalen_working,
      check_2 == T ~ coalesce(Unemployed, not_lfs, Retired, Other),
      check_1 == F ~ coalesce(artkalen_working, Unemployed, not_lfs, Retired, Other),
      check_2 == F ~ coalesce(artkalen_working, Unemployed, not_lfs, Retired, Other),
      (is.na(check_1) & is.na(check_2)) | (check_1==check_2) ~ coalesce(artkalen_working, Unemployed, not_lfs, Retired, Other)
    )) %>% select( pid,Expanded, employment_status,pgemplst_1)

# 10: Merge Spell Identifiers and Detect Transitions
calendar_expanded_x_5 = calendar_expanded_x_4 %>% 
  merge(spell_nr_by_month_1, by = c("pid","Expanded"), all.x = T) %>% 
  group_by(pid) %>% arrange(Expanded) %>% 
  mutate(x = employment_status!=lag(employment_status),
         x = ifelse(is.na(x), T,x),
         y = spell_x!=lag(spell_x),
         y = ifelse(is.na(y), T,y),
         z = x|y,
         new_spell = cumsum(z))

# 11: Collapse Monthly Data Back to Spells
calendar_expanded_x_6 = calendar_expanded_x_5 %>% group_by(pid, new_spell) %>% 
  summarise(
    start_date = min(Expanded),
    end_date = max(Expanded),
    employment_status = first(employment_status),
    spell_x = first(spell_x)
  )%>% 
  mutate(end_date = ceiling_date(end_date, "month") - days(1))

################################################################################
# C.	pbiospe.dta – retrospective (prior entering the survey) and Information on activity status covers 
# only the period up to the time the biography is collected
# The spell file PBIOSPE is based on the information on activity status over the life course, 
# which is collected as a matrix from every respondent who completes the biographical questionnaire. 
# The observations start at the age of 15 and end at the current age (up to age 65). 
# To update ongoing employment information in PBIOSPE, information from the yearly individual questionnaire is also used
# spells are in yearly duration

# The dataset pbiospe.dta provides retrospective information on individuals' 
# activity statuses starting from age 15 up to the time of the biographical interview, 
# capturing life course employment histories in yearly resolution. 
# The data processing begins with loading and formatting spell start and 
# end years into proper date objects, assuming January 1st and December 31st, 
# respectively. Each spell is labeled with a human-readable activity type, such as 
# "Full-Time Employment" or "School, College." Only spells marked as biographical 
# (or biographical with calendar overlap) are retained. These spells are expanded 
# into annual records and pivoted to a wide format, where each activity type forms a column. 
# Then, spells are grouped into standardized labor market categories 
# (e.g., "Working", "Unemployed", "Retired"). Final employment statuses are 
# derived, and overlapping or sequential spells are analyzed to detect transitions. 
# Unique spells are re-identified based on changes in status or structure, collapsed back 
# into continuous periods, and final spell durations are adjusted to end on December 31 of the 
# respective end year.

# 1. Load and Prepare Raw Spell Data
# 2. Encode dates - create for year of start and end of the spell a date format. I assume 1st of January for now
# 3. Encode stell typ
# 4. we select from pbiospe the if spellinf [1] Biography Only and  [3] 1 Biography + 1 Calender Spell [-2] Does not apply
# 5. Expand Spells into Yearly Records
# 6. Pivot Spell Types to Wide Format
# 7. Categorize Into Labor Market Groups (LIB Categories)
# 8. Finalize Employment Status Classification
# 9. Identify Unique Spells Per Year
# 10. Merge and Track Transitions
# 11. Collapse Back to Spells
# 12. Adjust Final Spell Dates


pbiospe <-
  # 1. Load and Prepare Raw Spell Data
  haven::read_dta(paste0(folder_soep,"/pbiospe.dta"),
                           col_select = c("pid", "spellnr", "cid", "spelltyp", "beginy",  "endy", "zensor","spellinf")) %>% 
  mutate(
    # 2. Encode dates - create for year of start and end of the spell a date format. I assume 1st of January for now
    beginy = as.Date(paste0(beginy, "-01-01")),
    endy = as.Date(paste0(endy, "-12-31")),
    # 3. Encode stell typ
    spelltyp_text = case_when(
      spelltyp == 1 ~ "School, College",            # [1] School, College 
      spelltyp == 2 ~ "Apprenticeship, Training",   # [2] Apprenticeship, Training
      spelltyp == 3 ~ "Military, Community Service",# [3] Military, Community Service
      spelltyp == 4 ~ "Full-Time Employment",       # [4] Full-Time Employment 
      spelltyp == 5 ~ "Part-Time Employment",       # [5] Part-Time Employment
      spelltyp == 6 ~ "Unemployed",                 # [6] Unemployed
      spelltyp == 7 ~ "Housewife, Husband",         # [7] Housewife, Husband
      spelltyp == 8 ~ "Pensioner",                  # [8] Pensioner
      spelltyp == 9 ~ "Other",                      # [9] Other
      spelltyp == 99 ~ "Gap"                        # [99] Gap
      )) 

# 4. we select from pbiospe the if spellinf [1] Biography Only and  [3] 1 Biography + 1 Calender Spell [-2] Does not apply

pbiospe_selected = pbiospe[pbiospe$spellinf %in% c(-2,1,3),]

# 5. Expand Spells into Yearly Records
pbiospe_selected_0 <- neatRanges::expand_dates(pbiospe_selected, start_var = "beginy", end_var = "endy",
                                              vars_to_keep = c("pid", "spelltyp_text","spellnr","beginy", "endy", "zensor","spellinf"), 
                                              unit = "year")


# 6. Pivot Spell Types to Wide Format
pbiospe_selected_1 <-pbiospe_selected_0 %>%
  select(pid,Expanded, spellnr, spelltyp_text) %>%
  distinct() %>%
  tidyr::pivot_wider(
    names_from = spelltyp_text,
    values_from = spellnr,
    id_cols  = c(pid,Expanded))

for (i in 3:ncol(pbiospe_selected_1)) {
  pbiospe_selected_1[,i]<-ifelse(is.na(pbiospe_selected_1[,i]), NA, names(pbiospe_selected_1[,i]))
}

# 7. Categorize Into Labor Market Groups (LIB Categories)
pbiospe_selected_2 = pbiospe_selected_1 %>% mutate(
  # [1] Working
  working = coalesce(`Full-Time Employment`, `Part-Time Employment`, # non - disputable "Working"
                     `Military, Community Service`, # we could consider this moving to Not in the Labor Force based on freq with pgemplst
                     # Categories that correspond to Apprenticeship/Training in PBIOSPE according to Schmelzer, Paul; Hamjediers, Maik (2020)
                     # once we look at date of int pgemplst =="Vocational Training" then we see that in variable 98% say pglfs =="Working"
                     `Apprenticeship, Training`,
  ),
  # [2] Unemployed
  Unemployed = Unemployed,
  # [3] Not in the Labor Force
  not_lfs = coalesce(`School, College`,`Housewife, Husband`),
  # [4] Retired
  Retired = Pensioner,
  # [5] Other
  Other =  coalesce(`Other`,`Gap`)
) %>% select(pid, Expanded,working,Unemployed,not_lfs,Retired,Other)

# 8. Finalize Employment Status Classification
pbiospe_selected_3 = pbiospe_selected_2 %>%
  mutate(working = ifelse(!is.na(working), "Working", NA),
         # [2] Unemployed
         Unemployed = ifelse(!is.na(Unemployed), "Unemployed", NA),
         # [3] Not in the Labor Force
         not_lfs = ifelse(!is.na(not_lfs), "Not in the Labor Force", NA),
         # [4] Retired
         Retired = ifelse(!is.na(Retired), "Retired", NA),
         # [5] Other
         Other =  ifelse(!is.na(Other), "Other", NA)) %>% 
  mutate(NOT_working = coalesce(Unemployed, not_lfs, Retired, Other),
         NOT_working = ifelse(!is.na(NOT_working), "not working", NA),
         employment_status = coalesce(working, Unemployed, not_lfs, Retired, Other)
         )


# 9. Identify Unique Spells Per Year
spell_nr_by_year <- pbiospe_selected_0 %>%
  select(pid, Expanded, spellnr) %>%
  distinct() %>%
  pivot_wider(
    names_from = spellnr,
    values_from = spellnr,
    id_cols = c(pid, Expanded)
  ) 

spell_nr_by_year$spell_x <- purrr::pmap_chr(as.data.frame(spell_nr_by_year[,c(3:ncol(spell_nr_by_year))]), ~paste(na.omit(c(...)), collapse = " "))

spell_nr_by_year = spell_nr_by_year %>% select( pid, Expanded, spell_x)

# 10. Merge and Track Transitions

pbiospe_selected_4 = merge(as.data.table(pbiospe_selected_3), as.data.table(spell_nr_by_year), by = c("pid", "Expanded"), all.x = T)

pbiospe_selected_5 = pbiospe_selected_4 %>%  
  group_by(pid) %>% arrange(Expanded) %>% 
  mutate(x = employment_status!=lag(employment_status),
         x = ifelse(is.na(x), T,x),
         y = spell_x!=lag(spell_x),
         y = ifelse(is.na(y), T,y),
         z = x|y,
         new_spell = cumsum(z))
  
  
pbiospe_selected_6 = pbiospe_selected_5 %>% group_by(pid, new_spell) %>% 
  # 11. Collapse Back to Spells
  summarise(
    start_date = min(Expanded),
    end_date = max(Expanded),
    employment_status = first(employment_status),
    spell_x = first(spell_x)
  )

# 12. Adjust Final Spell Dates
pbiospe_selected_6 = pbiospe_selected_6 %>% 
  mutate(end_date = as.numeric(format(end_date, "%Y")),
         end_date = as.Date(paste0(end_date, "-12-31")))

################################################################################
# D. Merging and Harmonizing Employment Spells from pbiospe and artkalen 
# This step merges and harmonizes employment spell data from two sources—pbiospe 
# (retrospective biographies) and calendar (monthly calendar data). First, each dataset 
# is tagged with a flag_source to retain provenance, and calendar spell IDs are offset to 
# prevent overlap. The two datasets are then combined into a unified spell history (total_employment), 
# sorted by individual and spell order. To avoid duplication, the first calendar spell is checked for 
# redundancy—if it continues an identical employment status from pbiospe, its start date is adjusted or 
# the entry is flagged for removal. After filtering redundant entries, a final employment spell identifier 
# is constructed by detecting transitions in employment status or spell content. The result is a clean, 
# continuous dataset of non-overlapping employment spells per person, with consistent status labels, 
# spell boundaries, and clear identification of their data source.
# Step 1: Add Source Flags
# Step 2: Combine Both Datasets
# Step 3: Modify Redundant First Calendar Entry
# Step 4: Final Spell Identification

# Step 1: Add Source Flags
pbiospe_selected_6 = pbiospe_selected_6 %>% mutate(flag_source="pbiospe")

calendar_expanded_x_7 = calendar_expanded_x_6 %>% 
  mutate(new_spell = 1000+new_spell) %>% mutate(flag_source="calendar")

# Step 2: Combine Both Datasets
total_employment = bind_rows(pbiospe_selected_6,calendar_expanded_x_7) %>% 
  group_by(pid) %>% arrange(new_spell, .by_group = T) %>%
  # Step 3: Modify Redundant First Calendar Entry
  mutate(
    start_date_1 = if_else(new_spell ==1001 & employment_status==lag(employment_status), lag(as.Date(start_date)), NA),
    to_delete = if_else(lead(!is.na(start_date_1)),"to_delete", "not delete"),
    start_date = if_else(!is.na(start_date_1),start_date_1,start_date)
    
  ) %>% filter(to_delete!="to_delete" | is.na(to_delete))

# Step 4: Final Spell Identification
total_employment_1 = total_employment %>% 
  select(pid, new_spell, start_date, end_date, employment_status, spell_x, flag_source) %>% 
  group_by(pid) %>% 
  mutate(x = employment_status!=lag(employment_status),
         x = ifelse(is.na(x), T,x),
         y = new_spell!=lag(new_spell),
         y = ifelse(is.na(y), T,y),
         z = x|y,
         spell = cumsum(z)) %>% select(pid, spell, start_date, end_date, employment_status, spell_x, flag_source)

################################################################################
# 5. transform from long to wide format 
total_employment_1[c("ENTRY_Y","ENTRY_M","ENTRY_D")] <- str_split_fixed(total_employment_1$start_date, '-', 3)
total_employment_1[c("EXIT_Y","EXIT_M","EXIT_D")] <- str_split_fixed(total_employment_1$end_date, '-', 3)

total_employment_2 = total_employment_1 %>% 
  mutate(
    across(c("ENTRY_Y", "ENTRY_M", "ENTRY_D", "EXIT_Y", "EXIT_M", "EXIT_D"), as.integer),
    EMPLOYMENT_STATUS = case_when(
      employment_status == "Working" ~ 1, 
      employment_status == "Unemployed" ~ 2, 
      employment_status == "Not in the Labor Force" ~ 3, 
      employment_status == "Retired" ~ 4, 
      employment_status == "Other" ~ 5, 
      TRUE ~ NA
    ),
    EMPLOYMENT_STATUS = as.integer(EMPLOYMENT_STATUS),
    EMPLOYMENT_STATUS = labelled(
      EMPLOYMENT_STATUS,
      c("working" = 1,
        "unemployed" = 2,
        "not in the labor force" = 3,
        "retired" = 4,
        "other" = 5)))


# to wide format 

employment_biography_soep_wide = total_employment_2 %>% 
  pivot_wider(
  id_cols = c(pid),
  names_from = spell ,
  values_from = c("EMPLOYMENT_STATUS","ENTRY_Y","ENTRY_M","ENTRY_D", "EXIT_Y","EXIT_M", "EXIT_D"),
  names_glue = "{.value}_{spell}"
)

employment_biography_soep_wide %>% head()

x_c = max(total_employment_2$spell)

# order variables 
ordering_variables_wide_format <- function(x_c) {
  lista_order_20<-c()
  for (i in 1:x_c) {
    x<-c(
      paste0("EMPLOYMENT_STATUS_", i),
      paste0("ENTRY_Y_",i),
      paste0("ENTRY_M_", i),
      paste0("ENTRY_D_", i),
      paste0("EXIT_Y_",i),
      paste0("EXIT_M_",i),
      paste0("EXIT_D_",i)
      # paste0("spell_x_",i)
      # paste0("flag_source_",i)
    )
    
    lista_order_20<- append(lista_order_20, x)
  }
  return(lista_order_20)
}

employment_biography_soep_wide_1<-employment_biography_soep_wide[,c("pid",ordering_variables_wide_format(x_c))]

# Apply labels to employment spell variables

for (i in 1:x_c) {
  var_status <- paste0("EMPLOYMENT_STATUS_", i)
  var_entry_y <- paste0("ENTRY_Y_", i)
  var_entry_m <- paste0("ENTRY_M_", i)
  var_entry_d <- paste0("ENTRY_D_", i)
  
  var_exit_y <- paste0("EXIT_Y_", i)
  var_exit_m <- paste0("EXIT_M_", i)
  var_exit_d <- paste0("EXIT_D_", i)
  
  
  if (var_status %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_status]]) <- paste("Employment status during spell", i)
  }
  if (var_entry_y %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_entry_y]]) <- paste("Year entry for spell", i)
  }

  if (var_entry_m %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_entry_m]]) <- paste("Month entry for spell", i)
  }
  if (var_entry_d %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_entry_d]]) <- paste("Day entry for spell", i)
  }
  if (var_exit_y %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_exit_y]]) <- paste("Year exit for spell", i)
  }
  if (var_exit_m %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_exit_m]]) <- paste("Month exit for spell", i)
  }
  if (var_exit_d %in% names(employment_biography_soep_wide_1)) {
    var_label(employment_biography_soep_wide_1[[var_exit_d]]) <- paste("Day exit for spell", i)
  }
  

}

# save results

saveRDS(employment_biography_soep_wide_1, paste0("output/","soep_employment.rds"))



if (type_you_want==".csv") {
  write.csv(employment_biography_soep_wide_1, "output/soep_employment.csv")
}else if(type_you_want==".dta"){
  write_dta(employment_biography_soep_wide_1, "output/soep_employment.dta")
  
}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)




