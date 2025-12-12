#-------------------------------------------------------------------------------
#      LabFam Individual Biographies compatibility with CPF    
#-------------------------------------------------------------------------------

# List of required packages for this task and establish working directory
source("00_setting_work_space.R")

# Available countries: "hilda", "gsoep", "bhps_ukhls", "shp", "psid"
country_set = c("bhps_ukhls")

# Available histories: "employment", "fertility","partnership"
history_set = c("master","employment", "fertility","partnership")

# Preferred format for the output database: "csv" , "rds" , "dta" , etc. 
type_you_want = ".rds"

# make for one country

to_download = expand.grid(country_set, history_set) %>% mutate(dataset = paste0(Var1,"_",Var2,".rds"),
                                                               script  = paste0(Var1,"_",Var2,".R")) %>% 
  mutate(
    id = case_when(
      Var1 == "psid"  ~ "pid",
      Var1 == "shp"   ~ "idpers",
      Var1 == "hilda" ~ "xwaveid",
      Var1 == "gsoep" ~ "pid",
      Var1 == "bhps_ukhls" ~ "pidp"),
    dataset = ifelse(Var2=="master", paste0("master_",Var1,"_wide.rds"), dataset) ,
    script  = ifelse(Var2=="master", paste0("master_",Var1,"_file.R"), script),
    nr =  case_when(
      Var1 == "hilda" ~ 1,
      Var1 == "psid"  ~ 3,
      Var1 == "shp"   ~ 5,
      Var1 == "gsoep" ~ 6,
      Var1 == "bhps_ukhls" ~ 7,
      TRUE ~ NA_integer_
      )
  )

for (set in to_download$dataset) {
  x = readRDS(paste0("output/",set))
  assign(x = set,value = x)
  rm(x)
}

# from wide format to monthly format 

# fertility
data_fertility = 
  get(to_download[to_download$dataset == paste0(country_set[1],"_fertility.rds"),"dataset"]) %>% 
  rename(pid = pidp)

variable_fertility = c("KID_M","KID_Y","KID_S")

max = length(data_fertility %>% select(starts_with(variable_fertility[1])) %>% names())

fertility_types <- melt(
  as.data.table(data_fertility),
  id.vars = "pid",
  measure.vars = paste0(variable_fertility[1],1:max),
  variable.name = "parity",
  value.name = variable_fertility[1])

fertility_types[, parity := sub(paste0(variable_fertility[1]), "", parity)]

not_indata = c()
for (i in 2:length(variable_fertility)) {
  
  if (!is_empty(data_fertility %>% select(starts_with(variable_fertility[i])))) {
    x =  melt(
      as.data.table(data_fertility),
      id.vars = "pid",
      measure.vars = patterns(variable_fertility[i]),
      variable.name = "parity",
      value.name = variable_fertility[i]
    )
    
    x[, parity := sub(paste0(variable_fertility[i]), "", parity)]
    
    fertility_types = merge(fertility_types, x, by = c("pid", "parity"), all.x = T)
    
  }else{
    not_indata = rbind(not_indata, variable_fertility[i])
    
  }
  
  print(variable_fertility[i])
  
}

# expand dates - mark which one are interview dates

fertility_types_parents = fertility_types %>% filter(!is.na(KID_M) & !is.na(KID_Y) & !is.na(KID_S))

fertility_types_1 = fertility_types_parents %>% 
  mutate(parity = as.numeric(parity)) %>%
  group_by(pid) %>% arrange(parity) %>% 
  mutate(
    start_date = as.Date(paste(KID_Y, KID_M, 1, sep = "-"), format = "%Y-%m-%d"),
    end_date = lead(start_date),
    end_date = if_else(!is.na(end_date), end_date %m-% months(1), NA_Date_)
  )

fertility_types_1_first_birth <- fertility_types_1 %>%
  filter(parity == 1) %>%
  select(pid, start_date) %>%
  rename(end_date = start_date) %>%
  mutate(
    end_date = as.Date(end_date) %m-% months(1),  # Subtract one month from `end_date`
    parity = 0
  )

# add paritys prior giving birth and fill end_date with last date of interview
data_master = 
  get(to_download[to_download$dataset == paste0("master_",country_set[1],"_wide",".rds"),"dataset"]) %>%
  filter(pid %notin% unique(fertility_types_1$pid)) %>% 
  mutate(pid = pidp) %>% select(pid, BORN_Y, LASTOBS_Y) %>% 
  mutate(start_date = as.Date(paste(BORN_Y+15, 1, sep = "-"), format = "%Y-%m-%d"),
         end_date = as.Date(paste(LASTOBS_Y,1, 1, sep = "-"), format = "%Y-%m-%d")
  )

# add non parents one parity from BORN_Y LASTOBS_Y ans parity of childlessness for parents

data_master_aprent = data_master %>% 
  dplyr::select(pid, BORN_Y, LASTOBS_Y) %>% 
  filter(pid %in% unique(fertility_types_1$pid)) %>%
  mutate(start_date = as.Date(paste(BORN_Y+15,1, 1, sep = "-"), format = "%Y-%m-%d"),
         parity = 0)

fertility_types_2 = bind_rows(fertility_types_1,data_master) %>% 
  select(-c("BORN_Y",    "LASTOBS_Y")) %>% 
  mutate(parity = ifelse(is.na(parity), 0,parity)) %>% 
  merge(fertility_types_1_first_birth, by = c("pid", "parity"), all.x=T) %>% 
  mutate(end_date = coalesce(end_date.x, end_date.y),
  ) %>% select(-c("end_date.x", "end_date.y")) %>% 
  # add start date 
  merge(data_master_aprent, by = c("pid", "parity"), all.x = T) %>% 
  mutate(start_date = coalesce(start_date.y,start_date.x)) %>% 
  select(-c("start_date.x", "start_date.y", "BORN_Y")) %>% group_by(pid) %>%
  arrange(parity,.by_group = T) %>% 
  fill(LASTOBS_Y, .direction = "down") %>% 
  mutate(max = max(parity),
         end_date = if_else(is.na(end_date) & max==parity, 
                            as.Date(paste(LASTOBS_Y,1, 1, sep = "-"), format = "%Y-%m-%d"),
                            as.Date(end_date)
         )
  ) %>% select(pid, parity, start_date, end_date, KID_M, KID_Y, KID_S,)

# how childless looks like?

fertility_types_2 <- fertility_types_2 %>%
  mutate(end_date = if_else(end_date < start_date, start_date, end_date)) %>%
  filter(!is.na(start_date) | !is.na(end_date)) %>% 
  filter(start_date<=end_date)

fertility_types_3  = 
  expand_dates(fertility_types_2, start_var = "start_date", end_var = "end_date",
               vars_to_keep = c("pid","parity","KID_M","KID_Y","KID_S"), unit = "month")


# partnership
data_partnership = 
  get(to_download[to_download$dataset == paste0(country_set[1],"_partnership.rds"),"dataset"]) %>% 
  select(-pid) %>% 
  rename(pid = pidp)

variable_partnership = c("PARTNERSHIP_STATUS","partner_id","START_Y","START_M","END_Y","END_M")

max = length(data_partnership %>% select(starts_with(variable_partnership[1])) %>% names())

union_types <- melt(
  as.data.table(data_partnership),
  id.vars = "pid",
  measure.vars = paste0(variable_partnership[1], "_",1:max),
  variable.name = "spell_union",
  value.name = variable_partnership[1]
)
union_types[, spell_union := sub(paste0(variable_partnership[1], "_"), "", spell_union)]

union_types = union_types %>% filter(!is.na(PARTNERSHIP_STATUS))

not_indata = c()
for (i in 2:length(variable_partnership)) {
  
  if (!is_empty(data_partnership %>% select(starts_with(variable_partnership[i])))) {
    x =  melt(
      as.data.table(data_partnership),
      id.vars = "pid",
      measure.vars = patterns(variable_partnership[i]),
      variable.name = "spell_union",
      value.name = variable_partnership[i]
    )
  
    x = as.data.frame(x)
    
    x <- x %>% filter(!is.na(!!sym(variable_partnership[i])))
    
    x = as.data.table(x)
    
    x[, spell_union := sub(paste0(variable_partnership[i], "_"), "", spell_union)]
  
    
    union_types = merge(union_types, x, by = c("pid", "spell_union"), all.x = T, allow.cartesian=TRUE)
  
  }else{
    not_indata = rbind(not_indata, variable_partnership[i])
    
  }
  
  print(variable_partnership[i])
 
}

# expand dates
# mark which one are interview dates

union_types_1 = union_types %>% 
  mutate(spell_union = as.numeric(spell_union)) %>% 
  filter(!is.na(START_Y) & !is.na(END_Y)) %>% 
  mutate(
    START_M_1 = ifelse(is.na(START_M),"01", START_M),
    END_M_1 = ifelse(is.na(END_M),"01", END_M),
    start_date = as.Date(paste(START_Y, START_M_1, 1, sep = "-"), format = "%Y-%m-%d"),
    end_date = as.Date(paste(END_Y, END_M_1, 1, sep = "-"), format = "%Y-%m-%d")
  )

union_types_2  = 
  expand_dates(union_types_1, start_var = "start_date", end_var = "end_date",
             vars_to_keep = c("pid","spell_union","PARTNERSHIP_STATUS","partner_id","START_Y","START_M",
                              "END_Y","END_M"   ), unit = "month")


# employment
data_employment = 
  get(to_download[to_download$dataset == paste0(country_set[1],"_employment.rds"),"dataset"]) %>% 
  rename(pid = pidp)

variable_employment = c("employment_status","ENTRY_Y","ENTRY_M","EXIT_Y","EXIT_M")

data_employment$employment_status

max = length(data_employment %>% select(starts_with(variable_employment[1])) %>% names())

emp_types <- melt(
  as.data.table(data_employment),
  id.vars = "pid",
  measure.vars = paste0(variable_employment[1], "_",1:max),
  variable.name = "emp_spell",
  value.name = variable_employment[1]
)

emp_types[, emp_spell := sub(paste0(variable_employment[1], "_"), "", emp_spell)]

not_indata = c()
for (i in 2:length(variable_employment)) {
  
  if (!is_empty(data_employment %>% select(starts_with(variable_employment[i])))) {
    x =  melt(
      as.data.table(data_employment),
      id.vars = "pid",
      measure.vars = patterns(variable_employment[i]),
      variable.name = "emp_spell",
      value.name = variable_employment[i]
    )
    
    x[, emp_spell := sub(paste0(variable_employment[i], "_"), "",emp_spell)]
    
    emp_types = merge(emp_types, x, by = c("pid", "emp_spell"), all.x = T)
    
  }else{
    not_indata = rbind(not_indata, variable_employment[i])
    
  }
  
  print(variable_employment[i])
  
}

# expand dates -mark which one are interview dates

emp_types_1 = emp_types %>% 
  mutate(emp_spell = as.numeric(emp_spell)) %>% 
  filter(!is.na(ENTRY_Y) & !is.na(EXIT_Y)) %>% 
  mutate(
    ENTRY_M_1 = as.numeric(ifelse(is.na(ENTRY_M),"01", ENTRY_M)),
    EXIT_M_1 = as.numeric(ifelse(is.na(EXIT_M),"01", EXIT_M)),
    start_date = as.Date(paste(ENTRY_Y, ENTRY_M_1, 1, sep = "-"), format = "%Y-%m-%d"),
    end_date = as.Date(paste(EXIT_Y, EXIT_M_1, 1, sep = "-"), format = "%Y-%m-%d")
  )

emp_types_1 = emp_types_1 %>% filter(start_date<=end_date)

emp_types_2  = 
  expand_dates(emp_types_1, start_var = "start_date", end_var = "end_date",
               vars_to_keep = c("pid","emp_spell","employment_status","ENTRY_Y","ENTRY_M","EXIT_Y","EXIT_M"   ), unit = "month")

# identify month of interview 

data_master = 
  get(to_download[to_download$dataset == paste0("master_",country_set[1],"_wide",".rds"),"dataset"]) %>% 
  mutate(pid = pidp) %>% select(pid, starts_with("interview_date")) %>% as.data.table() %>% 
  melt(
    id.vars = "pid",
    measure.vars = patterns("interview_date"),
    variable.name = "wave",
    value.name = "interview_date"
  ) %>% 
  mutate(
    Expanded = as.Date(paste0(format(ymd(interview_date), "%Y-%m"), "-01")),
    interview_indicator = 1) 

# merge fertility, partnership, employment and data_master

lib_montly_information = merge(fertility_types_3, emp_types_2,by = c("pid", "Expanded"),all = T) %>% 
  merge(union_types_2, by = c("pid", "Expanded"), all = T) %>% 
  merge(data_master, by = c("pid", "Expanded"), all.x = T)

lib_montly_information = fertility_types_3 %>% 
  merge(union_types_2, by = c("pid", "Expanded"), all = T) %>% 
  merge(data_master, by = c("pid", "Expanded"), all.x = T)

# make sure we have the same name for pid country and mark of interview_indicator

lib_montly_information = lib_montly_information %>% 
  mutate(orgpid =pid) %>% 
  mutate(
    country = "bhps_ukhls",
    country = case_when(
      country == "hilda" ~ 1,
      country == "psid"  ~ 3,
      country == "shp"   ~ 5,
      country == "gsoep" ~ 6,
      country == "bhps_ukhls" ~ 7,
      TRUE ~ as.numeric(country) 
    )
  ) 

# download CPF

col_to_be_selected_from_cpf = c("pid","country","wave", "intyear","intmonth","satlife5", "mlstat5")

cpf <- haven::read_dta("output/CPFv1.5.dta", col_select = col_to_be_selected_from_cpf) %>% 
  filter(country %in% c(unique(to_download$nr))) %>% 
  mutate(Expanded = as.Date(sprintf("%d-%02d-01", intyear, intmonth)))

# merge with CPF for selected countries by pid, country, date of interview month (Expanded)

lib_montly_information_1 = merge(lib_montly_information,cpf, by = c("pid","country","Expanded"), all.x = T)


lib_montly_information_1[lib_montly_information_1$pid==12251, ]


## Saving the database
date_run_code = Sys.Date() %>% str_replace_all("-","_")

if (type_you_want==".csv") {
  write.csv(lib_montly_information_1, paste0("output/", "LIB_CPF",date_run_code, type_you_want))
}else if(type_you_want==".dta"){
  write_dta(lib_montly_information_1, paste0("output/","LIB_CPF",date_run_code, type_you_want))
}else if(type_you_want==".rds"){
  saveRDS(lib_montly_information_1, paste0("output/", "LIB_CPF",date_run_code, ".rds"))
}






