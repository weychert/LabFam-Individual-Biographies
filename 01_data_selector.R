#----------------------------------------#
#      LabFam Individual Biographies     #
#----------------------------------------#

# This is the main editable file for the LIB. Users choose countries and histories. 
# Based on these choices, the program calls the necessary scripts from the database. 
# The default is to build all histories for all countries.

# List of required packages for this task and establish working directory
source("00_setting_work_space.R")

# Available countries: "hilda", "gsoep", "bhps_ukhls", "shp", "psid"
country_set = toupper(c("hilda","soep", "bhps_ukhls", "shp", "psid"))

# Available histories: "employment", "fertility","partnership"
history_set = c("master","fertility","partnership","employment")

# Preferred format for the output database: "csv" , "rds" , "dta" , etc. 
type_you_want = ".dta"

################################################################################

# End of customization part. Subsequent lines call all the databases and 
# export the data

################################################################################
# Create object with all desired histories to iterate 
path <- "output"

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

to_download = expand.grid(country_set, history_set) %>% mutate(dataset = paste0(Var1,"_",Var2,".rds"),
                                                               script  = paste0(Var1,"_",Var2,".R")) %>% 
  mutate(
    id = case_when(
      Var1 == "PSID"  ~ "pid",
      Var1 == "SHP"   ~ "idpers",
      Var1 == "HILDA" ~ "xwaveid",
      Var1 == "SOEP" ~ "pid",
      Var1 == "BHPS_UKHLS" ~ "pidp"),
    dataset = ifelse(Var2=="master", paste0("master_",toupper(Var1),"_wide.rds"), dataset) ,
    script  = ifelse(Var2=="master", paste0(toupper(Var1),"_master_file.R"), script)
    )
print(type_you_want)

# Run selected scripts

for (script in to_download$script) {
  source(script)
  print(script)

}


################################################################################
# 
for (set in to_download$dataset) {
  x = readRDS(paste0("output/",set))
  assign(x = set,value = x)
  }
    
# Merge biographies within countries
for (country in country_set) {
  
  to_download_x = to_download[to_download$Var1 %in% country , ]
  
  if (length(to_download_x$dataset)==1) {
    
    given_country = get(to_download_x$dataset[1])
  
  }else{
    
    given_country = get(to_download_x$dataset[1])
    
  }
  
  given_country = get(to_download_x$dataset[1])
  
  for (i in 2:length(to_download_x$dataset)) {
    
    given_country  = merge(given_country, get(to_download_x$dataset[i]), by = c(to_download_x$id[i]))
    
  }
  
  given_country$country = to_download_x$Var1[i]
  
  assign(x = paste0(country, "_history"),value =  given_country)

  rm(given_country)
  
}

# Append countries

final_history_for_select_countries = c()

for (country in country_set) {
  
  x = get(paste0(country, "_history"))
  
  x <-  x %>%
    mutate(across(everything(), as.character))
  
  final_history_for_select_countries = bind_rows(final_history_for_select_countries, x)
  
  print(country)
  
  rm(x)
  
}

# to be compatible with CPF we create one pid and country variable for easy merge
final_history_for_select_countries <- final_history_for_select_countries %>%
  mutate(orgpid = coalesce(!!!syms(unique(to_download$id)[!is.na(unique(to_download$id))]))) %>%
  mutate(
    country = case_when(
      country == "HILDA" ~ 1,
      country == "PSID"  ~ 3,
      country == "SHP"   ~ 5,
      country == "SOEP" ~ 6,
      country == "BHPS_UKHLS" ~ 7,
      TRUE ~ as.numeric(country) 
    )
  ) 


## Saving the database
date_run_code = Sys.Date() %>% str_replace_all("-","_")

if (type_you_want==".csv") {
  write.csv(final_history_for_select_countries, paste0("output/", "LIB_",date_run_code, type_you_want))
}else if(type_you_want==".dta"){
  write_dta(final_history_for_select_countries, paste0("output/","LIB_",date_run_code, type_you_want))
}else if(type_you_want==".rds"){
saveRDS(final_history_for_select_countries, paste0("output/", "LIB_",date_run_code, ".rds"))
}

## Log of created databases
fname <- paste0("ReadMe_", date_run_code, ".txt")
file.create(fname)
content = paste0("You have created ", paste0(history_set[history_set!="master"], collapse = ", "), 
                 " biographies for: "  , paste0(country_set, collapse = ","),". 
                 The database is stored in ", "../output/LIB_",date_run_code, type_you_want)
writeLines(content, fname)
