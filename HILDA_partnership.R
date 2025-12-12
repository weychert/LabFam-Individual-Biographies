######################## Partnership Biography hilda  ###########################
# list of steps: 
# 1. download necessary variables for each individual in each wave
# 2. Transform to long format the variables on the last five marriages for each wave separately
# 3. Fill down "when_start_liv_y","when_start_liv_m" so it will be easier to create variable START_Y for cohabiting spells
# 4. de-conflict all marriages across waves for each individual
# We merge longitudinal marriage spell data from multiple waves (2002–2020), 
# de-conflicting overlapping or missing marital event years per individual and spell.
# 5. The purpose is to clean and finalize de-conflicted marriage records for 
# each individual by removing duplicates, empty rows, 
# and calculating standardized start and end years.
# 6. Remove repeated identical marriage spells (same event years) and renumber
# 7. Add cohabitation for marriages that started and ended before HILDA observation (if UNION_Y>FIRSTOBS_Y)
# We enrich life-course data from the HILDA survey by incorporating 
# marital and cohabitation spells that occurred before individuals entered the survey. 
# It then creates a year-by-year panel for each person, aligning their union 
# history with their observation window in HILDA.
# 8. We integrate interview-based marital status data with 
# previously constructed marital/cohabitation spells. 
# It ensures single individuals are included, fills missing fields, 
# aligns partner IDs, and reconstructs clean spells 
# based on changes in relationship status or partner.
# 9. This section adjusts and finalizes the END_Y (end year) for all spells, 
# ensuring that single and cohabiting spells have appropriate durations 
# and that the last spell aligns with the last observed year in HILDA.
# 10. End_single How single spell ended:
# 11. End_union How union spell ended:
# 12. Transform form long to wide format
# 13. order variables 
# 14. save results 
################################################################################
# download and prepare necessary objects for looping
# List of required packages for this task and establish working directory
source("00_setting_work_space.R")
# Creating list with the name of data sets that we will use form the folders provided by HILDA   
rperson_<-list.files(folder_Australia_2 ,pattern="*Rperson")
# We create variable to indicate in functions for which wave we perform calculations 
# Hilda started in 2001. 
years<-c(2001:c(2000+length(rperson_)))

################################################################################
# 1. download necessary variables for each individual in each wave
# Define the list of variables you want to download
selected_vars <- c(
  "xwaveid",
  "mrpyr", "mrpmth", "mrplvt", "mrpdiv", "mrpsep", "mrpwidw", "mrcurr", 
  "ordfyr", "hhpxid", "ordfmth", "prlcp", "orcdur", "prlcpyr", "prlcpmt", 
  "mr1yr", "mr1lvt", "mr1div", "mr1sep", "mr1widw", "mr2yr", "mr2lvt", 
  "mr2div", "mr2sep", "mr2widw", "mr3yr", "mr3lvt", "mr3div", "mr3sep", 
  "mr3widw", "mr4yr", "mr4lvt", "mr4div", "mr4sep", "mr4widw","mrn"
)

all_needed_variables = c()
for (i in 1:length(years)){
  
# Load only the selected variables
data <- read_dta(paste0(folder_Australia_2 ,rperson_[i]), col_select = ends_with(selected_vars))

names(data)[-1] <- substring(names(data)[-1], 2)

if (i==1) {
  to_subset = c("prlcp","prlcpyr","prlcpmt")
  
  selected_vars_1 = selected_vars[!grepl(paste(to_subset , collapse = "|"),selected_vars)]
  
  data = data %>% select(selected_vars_1)
}else{
  
  data = data %>% select(selected_vars)
}

# Rename variables based on your mapping
data <- data %>%
  rename(
    MARR_Y_5 = mrpyr,         # Year married - present or most recent marriage
    MARR_M_5 = mrpmth,        # Month - Present or most recent marriage 
    UNION_Y_5 = mrplvt,       # History: Years living together before present or most recent marriage
    DIV_Y_5 = mrpdiv,         # History: Year divorced - Present or most recent marriage
    SEP_Y_5 = mrpsep,         # Year separated (separated/divorced) - Present or most recent marriage
    WIDOW_Y_5 = mrpwidw,      # History: Year widowed - Present or most recent marriage
    marital_status = mrcurr,  # DV: Marital status from person questionnaire
    year_cohabit = ordfyr,    # Year began living with current partner
    partner_id = hhpxid,      # DV: Partner's xwaveid (text, 7-digit)
    month_cohabit = ordfmth,  # Month began living with current partner
    cohabit_in_years = orcdur,# History: Current defacto duration - years
    MARR_Y_4 = mr1yr,         # History: Year married - First marriage if married more than once
    UNION_Y_4 = mr1lvt,       # History: Years living together before first marriage if married more than once
    DIV_Y_4 = mr1div,         # History: Year divorced - First marriage if married more than once
    SEP_Y_4 = mr1sep,         # History: Year separated - First marriage if married more than once
    WIDOW_Y_4 = mr1widw,      # History: Year widowed - First marriage if married more than once
    MARR_Y_2 = mr2yr,         # History: Year married - Second marriage if married more than twice
    UNION_Y_2 = mr2lvt,       # History: Years living together before second marriage
    DIV_Y_2 = mr2div,         # History: Year divorced - Second marriage if married more than twice
    SEP_Y_2 = mr2sep,         # History: Year separated - Second marriage if married more than twice
    WIDOW_Y_2 = mr2widw,      # History: Year widowed - Second marriage if married more than twice
    MARR_Y_3 = mr3yr,         # History: Year married - Third marriage if married more than 3 times
    UNION_Y_3 = mr3lvt,       # History: Years living together before third marriage
    DIV_Y_3 = mr3div,         # History: Year divorced - Third marriage if married more than 3 times
    SEP_Y_3 = mr3sep,         # History: Year separated - Third marriage if married more than 3 times
    WIDOW_Y_3 = mr3widw,      # History: Year widowed - Third marriage if married more than 3 times
    MARR_Y_1 = mr4yr,         # History: Year married - Fourth marriage if married more than 4 times
    UNION_Y_1 = mr4lvt,       # History: Years living together before fourth marriage
    DIV_Y_1 = mr4div,         # History: Year divorced - Fourth marriage if married more than 4 times
    SEP_Y_1 = mr4sep,         # History: Year separated - Fourth marriage if married more than 4 times
    WIDOW_Y_1 = mr4widw,      # History: Year widowed - Fourth marriage if married more than 4 times
    nr_marriages = mrn        # History: How many times have you been legally married
    )
  
  if (i>1) {
    data <- data %>%
      rename(same_partner_last_int = prlcp,
             when_start_liv_y = prlcpyr,
             when_start_liv_m = prlcpmt
             )
    
  }
    
  data$year_wave  = years[i]
  
  remove_vars <- c("xwaveid","year_wave","marital_status", "partner_id", "nr_marriages","same_partner_last_int")
  
  # Remove specified elements from the original vector
  to_numbers = setdiff(names(data), remove_vars)
  
  data = data %>% haven::zap_labels()
  
  data = data %>% 
    mutate(across(all_of(to_numbers), ~ if_else(. %in% c(-1, -10), NA_real_, .))) %>% 
    mutate(across(all_of(to_numbers), ~ if_else(. <0, -1, .)))
  all_needed_variables = bind_rows(all_needed_variables,data)
  
  rm(data)
  
}

# 2. Transform to long format the variables on the last five marriages for each wave separately 
spell_wave =  all_needed_variables %>% 
  select(xwaveid, year_wave, paste0("UNION_Y","_",1:5)) %>% 
  pivot_longer(
    cols = c(paste0("UNION_Y","_",1:5)),
    names_to = c("spell"),
    values_to = "UNION_Y"
  ) %>% mutate(
    spell = str_remove(spell, paste0("UNION_Y","_"))
  )

for (i in c("MARR_Y","SEP_Y","DIV_Y","WIDOW_Y")) {
  
  x =  all_needed_variables %>% 
    select(xwaveid, year_wave, paste0(i,"_",1:5)) %>% 
    pivot_longer(
    cols = c(paste0(i,"_",1:5)),
    names_to = c("spell"),
    values_to = i
  ) %>% mutate(
    spell = str_remove(spell, paste0(i,"_"))
  )
  
  spell_wave = merge(spell_wave, x, by = c("xwaveid", "year_wave", "spell"))
  rm(x)
}

# Examine 
spell_wave %>% head()



################################################################################
# 3. Fill down "when_start_liv_y","when_start_liv_m" so it will be easier 
# to create variable START_Y for cohabiting spells

corrected_all = all_needed_variables %>% 
  group_by(xwaveid) %>% arrange(year_wave, .by_group = T) %>%  
  mutate(
    # we check if the same partner across waves
    same_partner = partner_id !=lag(partner_id),
    same_partner = ifelse(is.na(same_partner), T,same_partner),
    nr_same = cumsum(same_partner)
  ) %>% 
  group_by(xwaveid, nr_same) %>% arrange(year_wave, .by_group = T) %>% 
  fill(when_start_liv_y, .direction = "down") %>% 
  fill(when_start_liv_m, .direction = "down") %>% 
  select("xwaveid", "year_wave", "marital_status","partner_id", "nr_marriages",
         "when_start_liv_y","when_start_liv_m")

################################################################################
# 4. de-conflict all marriages across waves for each individual
# We merge longitudinal marriage spell data from multiple waves (2002–2020), 
# de-conflicting overlapping or missing marital 
# event years per individual and spell.

# Start with the marriage data from the 2001 wave
marriages_all = spell_wave[spell_wave$year_wave==2001,] %>% 
  # Remove duplicate rows
  distinct()%>% 
  # Round UNION_Y to 2 decimal places for consistency
  mutate(UNION_Y = round(UNION_Y, 2))

# Convert to data.table for faster merging in the loop
marriages_all = setDT(marriages_all)
for (i in 2002:2020) {
  
  # Step 1: Get distinct records for year i and round UNION_Y
  x = spell_wave[spell_wave$year_wave==i,] %>% distinct() %>% 
    mutate(UNION_Y = round(UNION_Y, 2))
  # Convert to data.table
  x = setDT(x)
  # Step 2: Merge new wave data into the cumulative marriages_all
  # - Join by individual (xwaveid), spell number, and marriage year
  # - Use full outer join (all = TRUE) to retain unmatched rows
  marriages_all = merge(marriages_all, x, 
                        by = c("xwaveid", "MARR_Y", "spell"), all = T) %>% 
  # Step 3: Coalesce conflicting or missing values:
  # - Take the first non-NA value from either wave
    mutate(
      UNION_Y  = coalesce(UNION_Y.x, UNION_Y.y),
      SEP_Y    = coalesce(SEP_Y.x, SEP_Y.y),
      DIV_Y    = coalesce(DIV_Y.x, DIV_Y.y),
      WIDOW_Y  = coalesce(WIDOW_Y.x, WIDOW_Y.y)
      ) %>% 
    # Step 4: Remove temporary .x and .y columns created during merge
    select(-ends_with(".x"),-ends_with(".y"))  
  
  rm(x)
  
  print(i)

}

################################################################################
# 5. The purpose is to clean and finalize deconflicted marriage records for 
# each individual by removing duplicates, empty rows, 
# and calculating standardized start and end years.

marriages_all = marriages_all %>% 
  # Group by individual
  group_by(xwaveid) %>% 
  # Sort marriage spells by marriage year
  arrange(MARR_Y, .by_group = T) %>% 
  # Flag duplicates: same MARR_Y and UNION_Y in consecutive rows (within person)
  mutate(check_x = ifelse(MARR_Y==lead(MARR_Y) & UNION_Y==lead(UNION_Y),1,0)) %>% 
  # Remove flagged duplicates, but keep NAs
  filter(check_x!=1 | is.na(check_x)) %>% 
  # Flag rows that have at least one non-missing marriage-related field
  mutate(check = ifelse(is.na(UNION_Y) & is.na(MARR_Y) & is.na(SEP_Y) & is.na(DIV_Y) & is.na(WIDOW_Y), 0, 1)) %>% 
  # Keep only "real" union/marriage records (at least one field filled)
  filter(check ==1) %>%
  # Drop intermediate columns
  select(-spell, -check_x) %>% 
  # Remove duplicate rows again just in case
  distinct() %>% 
  # Reorder by marriage year again and re-group by individual
  group_by(xwaveid) %>% arrange(MARR_Y, .by_group = T) %>% 
  # Assign a clean spell number (1, 2, 3, ...) per person
  mutate(
    spell_nr = 1:n()
  ) %>% 
  # Select and rename relevant columns
  select(xwaveid, spell_nr, UNION_Y,MARR_Y,SEP_Y, DIV_Y, WIDOW_Y) %>% 
  mutate(
    # Recalculate UNION_Y as duration (years before MARR_Y), if available
    UNION_Y = ifelse(UNION_Y>0, round(MARR_Y - UNION_Y), NA),
    # Set START_Y as marriage year, END_Y as separation year
    END_Y = SEP_Y,
    START_Y = MARR_Y
  )

################################################################################
# 6. Remove repeated identical marriage spells (same event years) and renumber

marriages_all = marriages_all %>% 
  # Group data by individual
  group_by(xwaveid) %>% 
  # Sort spells chronologically by marriage year
  arrange(MARR_Y, .by_group = T) %>% 
  # Check for repeated values in consecutive rows for key marital event variables
  mutate(
    check_m = MARR_Y ==lag(MARR_Y),
    check_s = SEP_Y ==lag(SEP_Y),
    check_d = DIV_Y ==lag(DIV_Y),
    check_w = WIDOW_Y ==lag(WIDOW_Y),
  # Flag row for deletion if any of the above is a repeat
    CHECK_DELETE = ifelse(check_m ==T | check_s==T | check_d==T | check_w==T,1,0)
         ) %>% 
  # Keep rows that are either not flagged or where the flag is NA
  filter(is.na(CHECK_DELETE) | CHECK_DELETE!=1) %>% 
  # Re-group and sort again (clean group after filtering)
  group_by(xwaveid) %>% arrange(MARR_Y, .by_group = T) %>%
  # Assign new sequential spell number
  mutate(
    spell_nr = 1:n()
  ) %>% 
  # Keep only relevant final columns
  select(xwaveid, spell_nr, UNION_Y,MARR_Y,SEP_Y, DIV_Y, WIDOW_Y,  START_Y, END_Y) 

################################################################################
# 7. Add cohabitation for marriages that started and ended before HILDA observation (if UNION_Y>FIRSTOBS_Y)
# We enrich life-course data from the HILDA survey by incorporating 
# marital and cohabitation spells that occurred before individuals entered the survey. 
# It then creates a year-by-year panel for each person, aligning their union 
# history with their observation window in HILDA.

# Step 1: Load master data and clean observation windows
master = readRDS("output/master_hilda_wide.rds") %>% select(xwaveid, BORN_Y,FIRSTOBS_Y, LASTOBS_Y) %>% 
  filter(BORN_Y>0) %>% 
  # Adjust for cases where FIRSTOBS_Y == LASTOBS_Y to avoid 0-length observation window
  mutate(
    FIRSTOBS_Y = ifelse(FIRSTOBS_Y==LASTOBS_Y, FIRSTOBS_Y-1,FIRSTOBS_Y),
    # Define START_Y as the year individual turns 18
    START_Y = as.Date(paste0(BORN_Y + 15, "-01-01")), 
    # Define END_Y as last observed year
    END_Y = as.Date(paste0(LASTOBS_Y, "-01-01"))) %>% 
  # Keep only valid cases (must enter HILDA after turning 18)
  filter(START_Y<=END_Y)

# Step 2: Identify unions that began before HILDA entry
marriages_all_1 = merge(marriages_all, select(master, xwaveid, FIRSTOBS_Y), by = c("xwaveid")) %>% 
  mutate(
   # Flag if union began before entering HILDA
   if_union_before_hilda_start = UNION_Y < FIRSTOBS_Y,
   # All records here are for marriages
   marital_status = "married"
  )

# Step 3: Add cohabitation episodes before HILDA for those flagged
cohabit_before_hilda_start = marriages_all_1 %>% 
  filter(if_union_before_hilda_start == T) %>% 
  # Extract relevant variables and reformat for cohabitation record
  select(xwaveid, spell_nr, UNION_Y) %>% 
  rename(START_Y = UNION_Y) %>% mutate(marital_status = "cohabit")

# Step 4: Combine marriage and cohabitation spells, update spell numbers
marriages_all_2 = bind_rows(marriages_all_1, cohabit_before_hilda_start) %>% 
  select(xwaveid,  spell_nr, marital_status, START_Y, END_Y, MARR_Y, SEP_Y, DIV_Y, WIDOW_Y) %>% 
  rename(spell = spell_nr) %>% 
  group_by(xwaveid) %>% arrange(START_Y, .by_group = T) %>% 
  # Reassign clean spell numbers and adjust END_Y to just before next START_Y
  mutate(
    spell_nr = 1:n(),
    END_Y = lead( START_Y)-1
  ) 

# Step 5: Expand master to long format by year using neatRanges::expand_dates()
master_expanded = 
  master %>%
  neatRanges::expand_dates(
    start_var =  "START_Y",
    end_var = "END_Y",
    name = "Expanded",   # name for expanded column
    fmt = "%Y-%m-%d",    # date format
    vars_to_keep = c("xwaveid", "BORN_Y","FIRSTOBS_Y", "LASTOBS_Y"),
    unit = "year"        # expand by year
  ) %>% 
  mutate(START_Y = year(Expanded)) %>%  # Extract year from date column
  select(-Expanded)

# Step 6: Merge master (per-year) with marriage/cohabitation spells
marriages_all_3  = merge(master_expanded, marriages_all_2, by = c("xwaveid","START_Y"), all.x = T)

################################################################################
# 8. We integrate interview-based marital status data with 
# previously constructed marital/cohabitation spells. 
# It ensures single individuals are included, fills missing fields, 
# aligns partner IDs, and reconstructs clean spells 
# based on changes in relationship status or partner.

# Step 1: Recode interview-based relationship status and assign start year
corr = corrected_all %>% ungroup() %>% 
  # Select only relevant columns needed for analysis
  select(xwaveid, year_wave, partner_id, marital_status, when_start_liv_y,when_start_liv_m) %>% 
  # Re-code numeric marital_status values to human-readable labels
  mutate(
    marital_status = case_when(
      marital_status == 1 ~ "married",
      marital_status == 2 ~ "cohabit",
      marital_status %in% c(3,4,5,6) ~ "no union"
    )) %>% 
  # Assign the wave year as START_Y — interpreted as the start year of the relationship state
  mutate(START_Y = year_wave)

# Step 2: Merge interview data with previous union spell dataset


marriages_all_4 = merge(marriages_all_3, corr, by = c("xwaveid", "START_Y"), all.x = T) %>% 
  # Combine marital status info from both sources (spell + interview)
  mutate(marital_status = coalesce(marital_status.x, marital_status.y)) %>% 
  # Keep relevant columns for spell reconstruction
  select(xwaveid, BORN_Y, FIRSTOBS_Y, LASTOBS_Y, marital_status, 
         partner_id, year_wave,spell, spell_nr, START_Y, END_Y, 
         MARR_Y, SEP_Y, DIV_Y, WIDOW_Y, when_start_liv_y, when_start_liv_m) %>% 
  # Sort within each individual by START_Y
  group_by(xwaveid) %>% arrange(START_Y, .by_group = T) %>% 
  # Fill down missing values for spell info and marital status
  fill(spell,spell_nr,marital_status,.direction = "down") %>% 
  mutate(
    # Fill in "no union" where marital status is still missing
    marital_status = ifelse(is.na(marital_status), "no union", marital_status),
    # Flag changes in marital status and spell number
    check1 = marital_status!=lag(marital_status),
    check1 = ifelse(is.na(check1), T,check1),
    spell_nr = ifelse(is.na(spell_nr),0,spell_nr),
    check2 = spell_nr!=lag(spell_nr),
    check2 = ifelse(is.na(check2), T,check2),
    check3 = check1 | check2,
    # Define points where new spells begin
    spell_nr_1 = cumsum(check3)) %>% 
  select(-check1,check2,check3) %>% 
  group_by(xwaveid,spell_nr_1) %>% 
  # Fill down marriage event info within spells
  fill(END_Y,MARR_Y, SEP_Y, DIV_Y, WIDOW_Y,.direction = "down") %>% 
  mutate(
    # Adjust START_Y if it falls within a marriage period
    START_Y = ifelse(marital_status == "married" & !is.na(MARR_Y) & 
                       !is.na(SEP_Y) & START_Y >= MARR_Y & START_Y <= SEP_Y,
                     MARR_Y,START_Y),
    # Clean empty partner IDs an spell
    partner_id = ifelse(marital_status == "married" & partner_id=="", NA, partner_id),
    spell = ifelse(is.na(spell), 0, spell)) %>% 
  # Fill partner ID within spells
  group_by(xwaveid,spell_nr) %>% arrange(START_Y, .by_group = T) %>% 
  fill(partner_id, .direction = "downup") %>%
  group_by(xwaveid,spell) %>% arrange(START_Y, .by_group = T) %>% 
  fill(partner_id,.direction = "downup") %>% 
  ##### modify marital_status so allign 
  mutate(
    marital_status = ifelse(marital_status=="no union" & !is.na(partner_id), "cohabit", marital_status)
  ) %>% 
  # Step 3: Recalculate clean spells by detecting changes
  group_by(xwaveid) %>% arrange(START_Y, .by_group = T) %>% 
  mutate(
    # Flag a new spell if the marital status has changed from the previous row
    check1 = lag(marital_status)!=marital_status,
    check1 = ifelse(is.na(check1), T, check1),
    # Flag a new spell if the original spell group number has changed
    check2 = lag(spell_nr_1)!=spell_nr_1,
    check2 = ifelse(is.na(check2), T, check2),
    # Standardize partner ID to ensure comparisons work (replace NA with empty string)
    partner_id = ifelse(is.na(partner_id), "",partner_id),
    # Flag a new spell if the partner ID has changed (i.e., new partner)
    check3 = lag(partner_id)!=partner_id,
    check3 = ifelse(is.na(check3), T, check3),
    # Overall spell change indicator: any change in status, spell group, or partner
    check4 = check1 | check2 | check3,
    # Create a new spell number by cumulatively summing detected changes
    spellnr = cumsum(check4)
  ) %>% 
  # Step 4: Final adjustments to spell start dates and variable cleanin
  group_by(xwaveid, spellnr) %>%
  mutate( 
    # Assign the official START_Y for each spell based on marital status logic:
    # - For married spells with a valid marriage year, use MARR_Y as start
    # - For married spells without MARR_Y, fallback to earliest observed year
    # - For cohabit or no union spells, use the earliest observed year in the group
    START_Y = case_when(
      marital_status == "married" & !is.na(MARR_Y)~ MARR_Y ,
      marital_status == "married" & is.na(MARR_Y) ~ min(START_Y),
      marital_status == "cohabit" | marital_status == "no union" ~ min(START_Y),
      )
    ) %>%
  # Keep only the final cleaned and duplicated spells
  select(xwaveid,spellnr, marital_status, partner_id, START_Y, END_Y, 
         MARR_Y, SEP_Y, DIV_Y, WIDOW_Y) %>% 
  distinct() %>% 
  # Clean up variables that do not apply to non-married statuses
  mutate(
    MARR_Y = ifelse(marital_status  !="married", NA, MARR_Y),
    SEP_Y  = ifelse(marital_status  !="married", NA, SEP_Y),
    DIV_Y  = ifelse(marital_status  !="married" ,NA, DIV_Y),
    WIDOW_Y = ifelse(marital_status !="married", NA, WIDOW_Y),
  ) 



# 9. This section adjusts and finalizes the END_Y (end year) for all spells, 
# ensuring that single and cohabiting spells have appropriate duration 
# and that the last spell aligns with the last observed year in HILDA.

marriages_all_4 = marriages_all_4 %>% 
  # Group by individual to operate within each person's timeline
  group_by(xwaveid) %>% arrange(START_Y, .by_group = T) %>% 
  # Step 1: Update END_Y for all spells based on available information
  mutate(
    # If separation year is known, use it as END_Y (overwrites earlier values)
    END_Y = ifelse(!is.na(SEP_Y) & SEP_Y>0, SEP_Y, END_Y),
    # For spells without END_Y (mostly cohabit or no union),
    # use the year before the next START_Y as an approximation
    END_Y = case_when(
      marital_status =="no union" & is.na(END_Y) ~ lead(START_Y)-1,
      marital_status =="cohabit" & is.na(END_Y) ~ lead(START_Y)-1,
      TRUE ~ END_Y),
    END_Y = ifelse(is.na(END_Y) & !is.na(WIDOW_Y), WIDOW_Y, END_Y),
    END_Y = ifelse(is.na(END_Y) & !is.na(DIV_Y), DIV_Y, END_Y),
    END_Y = ifelse(START_Y-END_Y==1, END_Y+1,END_Y),
    # Flag the last spell for each individual for later use
    last_spell = ifelse(max(spellnr)==spellnr, 1,0)
    ) %>% 
  # Step 2: Merge in LASTOBS_Y from the master file (last HILDA observation year)
  merge(select(master, xwaveid, LASTOBS_Y), by = c("xwaveid"), all.x = T) %>% 
  # Re-group and order to safely update END_Y again
  group_by(xwaveid) %>% arrange(START_Y, .by_group = T) %>% 
  mutate(END_Y = ifelse(is.na(END_Y) & last_spell==1,  LASTOBS_Y, END_Y),
         END_Y = ifelse(is.na(END_Y), lead(START_Y)-1, END_Y)
         ) %>% select(-c("last_spell", "LASTOBS_Y"))

marriages_all_4 = marriages_all_4 %>% 
  rename(PARTNERSHIP_STATUS = marital_status,
         DIVORCE_Y =  DIV_Y,
         PARTNER_ID = partner_id) %>% 
  mutate(DIVORCE =
           case_when(
             DIVORCE_Y >0 | DIVORCE_Y==-1 ~ 1,
             is.na(DIVORCE_Y) ~ 0,
             TRUE ~ NA
           ))


################################################################################
# # 10. End_single How single spell ended:
# # 11. End_union How union spell ended:
################################################################################
marriages_all_4 = marriages_all_4 %>% mutate(
  # -----------------------------------------------
  # END_SINGLE: How a spell of being single ended
  # -----------------------------------------------
  # [0] ongoing not ended spell
  # [1] cohabitation
  # [2] marriage
  END_SINGLE = case_when(
    # Single spell ends in cohabitation
    PARTNERSHIP_STATUS %in% c("no union") & lead(PARTNERSHIP_STATUS) == "cohabit" ~ "cohabitation",
    
    # Single spell ends in marriage
    PARTNERSHIP_STATUS %in% c("no union") & lead(PARTNERSHIP_STATUS) == "married" ~ "marriage",
    
    # If currently married, not applicable (not a single spell)
    PARTNERSHIP_STATUS == "married" ~ NA_character_,
    
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
    PARTNERSHIP_STATUS == "no union" & lead(PARTNERSHIP_STATUS) %in% c("married", "cohabit") ~ NA_character_,
    
    # If divorced, end of union is a separation
    DIVORCE == 1 ~ "Separation",
    
    # If separation year is recorded, it's a separation
    !is.na(SEP_Y) ~ "Separation",
    
    # If currently married and next spell is no union or cohabit => separation
    PARTNERSHIP_STATUS == "married" & lead(PARTNERSHIP_STATUS) %in% c("cohabit", "no union") ~ "Separation",
    
    # If currently cohabiting and next spell is no union => separation
    PARTNERSHIP_STATUS %in% c("cohabit") & lead(PARTNERSHIP_STATUS) == "no union" ~ "Separation",
    
    # If partner ID changes between spells, it's a separation
    PARTNER_ID != lead(PARTNER_ID) ~ "Separation",
    
    # If widow year is recorded, the union ended in partner's death
    !is.na(WIDOW_Y) ~ "death of partner",
    
    # If currently cohabiting and next spell is marriage => upgraded to marriage
    PARTNERSHIP_STATUS %in% c("cohabit") & lead(PARTNERSHIP_STATUS) == "married" ~ "marriage",
    
    # If no next spell exists and current one is known, it's still ongoing
    !is.na(PARTNERSHIP_STATUS) & is.na(lead(PARTNERSHIP_STATUS)) ~ "ongoing not ended spell",
    
    # Fallback
    TRUE ~ NA_character_
  )
)

# Data Type Conversion and Categorical Variable Recoding
# Converting columns (START_Y, END_Y, DIVORCE_Y, DIVORCE) to integers – ensures correct data types for numerical operations.
# Recoding categorical variables like:
# PARTNERSHIP_STATUS to numeric codes (0, 1, 2) and then to factors with labels.
# END_UNION and END_SINGLE to numeric codes and then to labeled factors, ensuring consistency and ease of analysis.

marriages_all_4 <- marriages_all_4 %>%
  mutate(across(
    c(START_Y,  END_Y, DIVORCE_Y, DIVORCE), 
    as.integer
  )) %>%
  mutate(
    PARTNERSHIP_STATUS = case_when(
      # [0] no union
      # [1] cohabitation
      # [2] marriage
      PARTNERSHIP_STATUS =="no union" ~ 0,
      PARTNERSHIP_STATUS =="cohabit"~ 1,
      PARTNERSHIP_STATUS =="married"~ 2,
    ),
    # PARTNERSHIP_STATUS = factor(PARTNERSHIP_STATUS, levels = c(0,1,2),labels = c("no union", "cohabitation", "marriage")),

    PARTNERSHIP_STATUS =  labelled(
      x = as.integer(PARTNERSHIP_STATUS),  # must be numeric
      labels = c(
        "no union" = 0, 
        "cohabitation" = 1, 
        "marriage" = 2
        
      )),
    # How union spell ended:
    # [0] ongoing not ended spell
    # [1] marriage
    # [2] Separation
    # [3] death of partner
    END_UNION = case_when(
      END_UNION == "ongoing not ended spell" ~ 0,
      END_UNION == "marriage"                ~ 1,
      END_UNION == "Separation"              ~ 2,
      END_UNION == "death of partner"        ~ 3
    ),
    # END_UNION = factor(END_UNION, levels = c(0,1,2,3),labels = c("ongoing not ended spell", "marriage", "separation","death of partner" )),
    END_UNION =  labelled(
      x = as.integer(END_UNION),  # must be numeric
      labels = c(
        "ongoing not ended spell" = 0,
        "marriage"= 1,
        "separation"= 2,
        "death of partner"= 3
        
      )
    ),
    # How single spell ended:
    # [0] ongoing not ended spell
    # [1] cohabitation
    # [2] marriage
    END_SINGLE = case_when(
      END_SINGLE == "ongoing not ended spell" ~ 0,
      END_SINGLE == "cohabitation" ~ 1,
      END_SINGLE == "marriage" ~ 2,
    ),
    # END_SINGLE = factor(END_SINGLE, levels = c(0,1,2),labels = c("ongoing not ended spell", "cohabitation", "marriage"))
  
    END_SINGLE =  labelled(
      x = as.integer(END_SINGLE),  # must be numeric
      labels = c(
        "ongoing not ended spell" = 0,
        "cohabitation" = 1,
        "marriage" = 2
      )
    ),
    DIVORCE =  labelled(
      x = as.integer(DIVORCE),  # must be numeric
      labels = c(
        "divorce did not occurred" = 0,
        "divorce occurred" = 1))
    )



marriages_all_4 = marriages_all_4 %>% 
  mutate(
    START_M = -1,  
    END_M = -1,
    DIVORCE_M = -1
  )

################################################################################
# 12. Transform form long to wide format 

max_x = max(marriages_all_4$spellnr)

partner_biography_hilda_wide = marriages_all_4 %>%
  pivot_wider(
    id_cols = "xwaveid",
    names_from = spellnr,
    values_from = c("PARTNERSHIP_STATUS","PARTNER_ID", "START_Y","START_M", "END_Y", "END_M","DIVORCE_Y","DIVORCE_M","DIVORCE","END_SINGLE", "END_UNION"),
    names_glue = "{.value}_{spellnr}"
  )

# 13. order variables 
ordering_variables_wide_format <- function(x_c) {
  lista_order_20<-c()
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
         paste0("END_SINGLE_", i)
    )

    lista_order_20<- append(lista_order_20, x)
  }
  return(lista_order_20)
}

partner_biography_hilda_wide_1<-partner_biography_hilda_wide %>% select("xwaveid",ordering_variables_wide_format(max_x))

# Add stata labels

for (i in 1:max_x) {
  var_status <- paste0("PARTNERSHIP_STATUS_", i)
  var_pid <- paste0("PARTNER_ID_", i)
  var_start_y <- paste0("START_Y_", i)
  # var_start_m <- paste0("START_M_", i)
  var_end_y <- paste0("END_Y_", i)
  # var_end_m <- paste0("END_M_", i)
  var_div_y <- paste0("DIVORCE_Y_", i)
  # var_div_m <- paste0("DIVORCE_M_", i)
  var_div <- paste0("DIVORCE_", i)
  var_end_type <- paste0("END_UNION_", i)
  var_end_single <- paste0("END_SINGLE_", i)

  var_label(partner_biography_hilda_wide_1[[var_status]]) <- paste("Partnership status in spell", i,
                                                                "([0] No union, [1] Cohabitation, [2] Marriage)")

  var_label(partner_biography_hilda_wide_1[[var_pid]]) <- paste("Partner ID in spell", i)

  var_label(partner_biography_hilda_wide_1[[var_start_y]]) <- paste("Year when partnership started in spell", i)
  # var_label(partner_biography_hilda_wide_1[[var_start_m]]) <- paste("Month when partnership started in spell", i)

  var_label(partner_biography_hilda_wide_1[[var_end_y]]) <- paste("Year when partnership ended in spell", i)
  # var_label(partner_biography_hilda_wide_1[[var_end_m]]) <- paste("Month when partnership ended in spell", i)

  var_label(partner_biography_hilda_wide_1[[var_div_y]]) <- paste("Year of divorce in spell", i)
  # var_label(partner_biography_hilda_wide_1[[var_div_m]]) <- paste("Month of divorce in spell", i)

  var_label(partner_biography_hilda_wide_1[[var_div]]) <- paste("Divorce status in spell", i,"([1] Divorce occurred, [0] Divorce did not occur)")

  var_label(partner_biography_hilda_wide_1[[var_end_type]]) <- paste("How the union ended in spell", i, "([0] Ongoing, [1] Marriage, [2] Separation/break-up, [3] Death of partner)")

  var_label(partner_biography_hilda_wide_1[[var_end_single]]) <- paste("How the single spell ended in spell", i,"([0] Ongoing, [1] Cohabitation, [2] Marriage)")

}

# ################################################################################
# 14. save results 
saveRDS(partner_biography_hilda_wide_1, "output/hilda_partnership.rds")


if (type_you_want==".csv") {
  write.csv(partner_biography_hilda_wide_1, "output/hilda_partnership.csv")
}else if(type_you_want==".dta"){
  write_dta(partner_biography_hilda_wide_1, "output/hilda_partnership.dta")

}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download", "script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)





