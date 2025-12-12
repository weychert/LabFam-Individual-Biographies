#----------------------------------------#
#      LabFam Individual Biographies     #
#----------------------------------------#
# This file contains all resources needed to run the LIB codes. The file
# contains two parts: a list of all paths and a list of all packages required
#  to run the LIB codes.

# Please edit the paths to those that you will be using.
# In the default version all databases are stored within a common (root) folder
# If that is not the case in your system, modify the paths accordingly.


# Setting the paths
#--------------------------------

folder_personal  = "C:/Users/ewawe/Desktop/Code_for_LIB_v1.0/LIB_data/"

# create folder containing raw data
countries = c("Australia", "Germany", "Switzerland", "United Kingdom", "United States" )
for (i in 1:length(countries)) {
  if (file.exists(paste0(folder_personal, "/",countries[i]))){
    # print("exists")
  } else {
    dir.create(file.path(paste0(folder_personal,"/" , countries[i])))
  }
}


# BHPS/UKHLS
folder_uk_fertility_1 = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/ukhls/")
folder_bhps_1         = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/bhps/")
folder_part_uk        = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/")
folder_main_uk        = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/")
folder_Pronzato_uk    = paste0(folder_personal, "United Kingdom/UKDA-5629-stata8/stata8/")

# GSOEP
folder_soep           = paste0(folder_personal, "Germany/")

# PSID
folder_family_files  = paste0(folder_personal, "United States/PSID_indiv_family_files/PSID_indiv_family_files/")
folder_fertility     = paste0(folder_personal, "United States/cah85_21/")
folder_partnership   = paste0(folder_personal, "United States/mh85_21/")

# HILDA
folder_Australia_1   = paste0(folder_personal, "Australia/2. Stata 200c (Zip file 1 of 2 - Combined Data Files)/")
folder_Australia_2   = paste0(folder_personal, "Australia/2. Stata 200c (Zip file 2 of 2 - Other Data Files)/")

# SHP
folder_shp_1         = paste0(folder_personal, "Switzerland/Data_STATA/SHP-Data-W1-W24-STATA/")
folder_shp_2         = paste0(folder_personal, "Switzerland/Data_STATA/SHP-Data-WA-STATA/")
folder_retro_shp_1   = paste0(folder_personal, "Switzerland/Data_STATA/SHP-Data-Biography-STATA/")
folder_retro_shp_2   = paste0(folder_personal, "Switzerland/Data_STATA/SHP-Data-SHP-3-W1-STATA/")

# create a folder to save results
if (file.exists("output")) {
  # print("Folder exists")
} else {
  dir.create(file.path("output"))
}

# Downloading necessary packages
#--------------------------------

requiredPackages = c("dplyr",      # 1.  dplyr - for filtering mutating etc
                     "haven",      # 2.  haven - downloading stata files (end with .dta)
                     "reshape2",   # 3.  reshape2 - for dcast function (from long to wide)
                     "stringr",    # 4.  stringr - for string operations
                     "tidyr",      # 5.  tidyr - for data cleaning 
                     "sjlabelled", # 6.  retrive stata labels
                     "neatRanges", # 6.  expand_dates() Returns a full data frame with expanded sequences in a column, e.g. by day or month
                     "lubridate",  # 8.  lubridate::ceiling_date()
                     "naniar",     # 9.  in shp replace_with_na
                     "easyPSID",   # 10. useful for PSID data
                     "psidR",      # 11. useful for PSID data
                     "openxlsx",   # 12. reading excel for PSID data
                     "sjmisc",     # 13. is_empty () in shp master 
                     "data.table", # 14. merge.date.tabel()
                     "survival",   # 15. KM curve in validation survfit()
                     "survminer",  # 16. plottting KM curve in validation ggsurvplot()
                     "ggplot2",    # 17. graphical analysis in validation 
                     "patchwork",  # 18. graphical analysis in validation ggpubr::get_legend()
                     "readr",      # 19. reading R objects
                     "rlang",      # For !! and sym
                     "collapse",
                     "purrr",
                     "labelled"   # for stata labeles 
)

for(i in requiredPackages){
  for(i in requiredPackages) {if(!require(i,character.only = TRUE)) install.packages(i)}
  for(i in requiredPackages) {if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 
}

for(pkg in requiredPackages) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

rm("requiredPackages", i)

# Object to filter "not in" using dplyr package
`%notin%` <- Negate(`%in%`)