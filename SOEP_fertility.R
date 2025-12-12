############################# soep fertility history ##########################
# This script processes fertility history data from the German Socio-Economic Panel (soep), 
# transforming raw data on respondents' fertility histories into a structured dataset suitable 
# for survival analysis.

# List of required packages for this task: "dplyr", "haven", "tidyr"
source("00_setting_work_space.R")

# 1. Import biobirth file
# https://companion.soep.de/Data%20Structure%20of%20SOEPcore/Data%20Sets.html
# biobirth “Generated biographical information” (wide): 
# The file BIOBIRTH provides information on fertility histories 
# of adult respondents in the SOEP. Up to 2014 (version 30, wave BD), 
# the data were stored in two separate files: BIOBIRTH containing female 
# fertility histories, and BIOBRTHM providing male fertility histories. 
# Fertility histories in BIOBIRTH provide information on every woman 
# (as well as every man with panel entry since 2001) who has ever completed 
# at least one SOEP interview.

# 1. Import BIOBIRTH File

biobirth <- haven::read_dta(paste0(folder_soep,"biobirth.dta")) %>% 
  select("pid", starts_with("kidgeb"),starts_with("kidmon"),starts_with("kidsex"),
         "biokids", "sumkids")

# 2. Replace Missing Values in BIOBIRTH
soep_fertility_biography<-apply( biobirth, 2, function(x) ifelse(x==-2,NA,x)) %>% 
  as.data.frame()

soep_fertility_biography<-apply( soep_fertility_biography, 2, function(x) ifelse(x<0,-1,x)) %>% 
  as.data.frame()



# Determine how many child slots exist in the wide SOEP fertility file
## (counts variables kidsex01, kidsex02, ..., kidsexXX)
nr_kids_x <- length(grep("^kidsex\\d{2}$", names(soep_fertility_biography)))

## Loop over all possible children (01, 02, ..., nr_kids_x)
for (i in 1:nr_kids_x) {
  
  ## Create zero-padded child index (e.g. 1 -> "01", 10 -> "10")
  idx <- sprintf("%02d", i)
  
  ## Construct variable names for this child:
  ## sex  = child's sex (kid_s in SOEP)
  ## geb  = child's year of birth (kidgeb)
  ## month = child's month of birth (kid_m in SOEP)
  sex   <- paste0("kidsex", idx)
  geb   <- paste0("kidgeb", idx)
  month <- paste0("kidmon", idx)
  
  ## Identify cases where some child information exists (sex or month observed)
  ## but the birth year is missing (.)
  cond <- ( !is.na(soep_fertility_biography[[sex]]) |
              !is.na(soep_fertility_biography[[month]]) ) &
    is.na(soep_fertility_biography[[geb]])
  
  ## Correct these cases by recoding missing birth year (.)
  ## to -1, following the SOEP fertility-history coding convention
  soep_fertility_biography[[geb]][cond] <- -1
}


max = as.numeric(max(ncol(select(biobirth, starts_with("kidgeb")))))

names(soep_fertility_biography)<-c("pid", paste0("KID_Y",1:max), 
                                    paste0("KID_M",1:max),
                                    paste0("KID_S",1:max),
                                    "biokids", "sumkids")

# 3. Build Child Presence Dummies (KID_1 - KID_19)

soep_fertility_biography = soep_fertility_biography %>% 
  mutate(across(.cols = paste0("KID_Y",1:max),
                .fns = ~ifelse(is.na(.), 0, 1),
                .names = "KID_{col}") ) %>% 
  rename_with(
    ~ gsub("KID_KID_Y", "KID_", .x, fixed = TRUE),
    starts_with("KID_KID_Y"))

base_variables<-c("pid", "NR_KIDS")      

soep_fertility_biography$NR_KIDS = rowSums(soep_fertility_biography [, paste0("KID_",1:max)])

kids_order_var<-c()
for (i in 1:max) {
  kids_order_var<-rbind(kids_order_var,paste0("KID_Y",i),paste0("KID_M",i),paste0("KID_S",i),paste0("KID_",i))
}


all_needed_vars<-c(base_variables, kids_order_var)

soep_fertility_biography_1<-soep_fertility_biography %>% select(all_needed_vars)

soep_fertility_biography_1[,paste0("KID_",1:max)]<-apply(soep_fertility_biography_1[,paste0("KID_",1:max)], 2, function(x)
  as.integer(x)
)

soep_fertility_biography_1[,paste0("KID_S",1:max)]<-apply(soep_fertility_biography_1[,paste0("KID_S",1:max)], 2, function(x)
  as.integer(x)
)


soep_fertility_biography_1[,paste0("KID_Y",1:max)]<-apply(soep_fertility_biography_1[,paste0("KID_Y",1:max)], 2, function(x)
  as.integer(x)
)

soep_fertility_biography_1[,paste0("KID_M",1:max)]<-apply(soep_fertility_biography_1[,paste0("KID_M",1:max)], 2, function(x)
  as.integer(x)
)



# add STATA labels for each variable 
for (i in 1:max) {
  
  var_y <- paste0("KID_Y", i)
  var_m <- paste0("KID_M", i)
  var_i <- paste0("KID_", i)
  var_s <- paste0("KID_S", i)
  
  labelled::var_label(soep_fertility_biography_1[[var_m]]) <- paste("month of birth of ", i, "child")
  labelled::var_label(soep_fertility_biography_1[[var_y]]) <- paste("year of birth of ", i, "child")
  labelled::var_label(soep_fertility_biography_1[[var_s]]) <- paste("sex of ", i, "child")
  labelled::var_label(soep_fertility_biography_1[[var_i]]) <- paste("birth order ", i, "child")
}

var_label(soep_fertility_biography_1$NR_KIDS) <- "Number of children in LIB"

# add STATA labels for each variable 
for (i in 1:max(soep_fertility_biography_1$NR_KIDS)) {
  
  var_y <- paste0("KID_Y", i)
  var_m <- paste0("KID_M", i)
  var_i <- paste0("KID_", i)
  var_s <- paste0("KID_S", i)
  
  labelled::var_label(soep_fertility_biography_1[[var_y]]) <- paste("year of birth of ", i, "child")
  labelled::var_label(soep_fertility_biography_1[[var_m]]) <- paste("month of birth of ", i, "child")
  labelled::var_label(soep_fertility_biography_1[[var_s]]) <- paste("sex of ", i, "child")
  labelled::var_label(soep_fertility_biography_1[[var_i]]) <- paste("birth order ", i, "child")
}


# add value labels
labs_ind <- c("no child"=0, "child"=1)
labs_y   <- c("unknown year"=-1)
labs_m   <- c("unknown month"=-1)
labs_s   <- c("male"=1, "female"=2, "unknown"=-1)

ci <- grep("^KID_\\d+$",  names(soep_fertility_biography_1), value = TRUE)
cy <- grep("^KID_Y\\d+$", names(soep_fertility_biography_1), value = TRUE)
cm <- grep("^KID_M\\d+$", names(soep_fertility_biography_1), value = TRUE)
cs <- grep("^KID_S\\d+$", names(soep_fertility_biography_1), value = TRUE)

soep_fertility_biography_1[ci] <- lapply(soep_fertility_biography_1[ci], labelled, labs_ind)
soep_fertility_biography_1[cy] <- lapply(soep_fertility_biography_1[cy], labelled, labs_y)
soep_fertility_biography_1[cm] <- lapply(soep_fertility_biography_1[cm], labelled, labs_m)
soep_fertility_biography_1[cs] <- lapply(soep_fertility_biography_1[cs], labelled, labs_s)


# Save results 
saveRDS(soep_fertility_biography_1,"output/soep_fertility.rds")


 
if (type_you_want==".csv") {
  write.csv(soep_fertility_biography_1, "output/soep_fertility.csv")
}else if(type_you_want==".dta"){
  write_dta(soep_fertility_biography_1, "output/soep_fertility.dta")

}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)

