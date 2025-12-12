############################# HILDA fertility history ##########################
# This script processes fertility history data from the Household, Income and Labour Dynamics in Australia (HILDA) Survey. 
# It combines data on resident and non-resident children, transforms it into a consistent wide format, and fills missing information where necessary 
# to create a comprehensive dataset on fertility history.
# Steps in the code
# 1. Prepare initial objects such as: 
# working directories, list pof folder files we will use, vector of all waves,
# demographic characteristics of each individual: readRDS("output/master_hilda_wide.rds") and interview files as well as 
# number of kids in each wave form survey
# 2. Prepare non-resident children in long format
# 3. Prepare resident children in long format 
# 4. Combine non-resident children with resident children in long format
# 5. Recalculate children parity order taking  account resident and non-resident children
# 6. Transform to wide format (all resident and non-resident children)
# 7. Fill missing values for year of birth of each child and create once again the children dummies
# 8. Add childless individuals 
# 9. create child dummies
# 10. Add sum of children dummy 
# 11.  Order variables 
# 12. save data set

################################################################################
# 1. Prepare initial objects such as: 
# working directories, paths to folders, vector of all waves,
# demographic characteristics of each individual: readRDS("output/master_hilda_wide.rds") and interview files as well as 
# number of kids in each wave form survey

# List of required packages for this task: "dplyr", "haven", "tidyr"

# Establish working directory
source("00_setting_work_space.R")

# Creating list with the name of Combined_letter200c data sets that we will use.  
Combined_<-list.files(folder_Australia_1,pattern="*Combined_")

Rperson_<-list.files(folder_Australia_2,pattern="*Rperson_")

# We create variable to indicate in functions for which wave we perform calculations Hilda started in 2001. 
years<-c(2001:c(2000+length(Combined_)))


################################################################################
# 2. Prepare non-resident children in long format
amount_of_max_children_non_resident<-c(rep(13,length(years)))

list_non_resident<-c() # wide format
for (i in 1:length(Combined_)) {
  
    non_resident_column_var<-c("xwaveid",
                               paste0(letters[i], "ncage", 1:amount_of_max_children_non_resident[i]), # age of non-resident child 
                               paste0(letters[i], "ncsex", 1:amount_of_max_children_non_resident[i]), # Sex of child - Non-resident child)
                               paste0(letters[i],"ncany"),
                               paste0(letters[i],"tchad"),
                               paste0(letters[i],"tcnr")
                               )
    
    x <- haven::read_dta(paste0(folder_Australia_1, Combined_[i]), 
                         col_select = non_resident_column_var)
    # Delete first letter from variable names in a downloaded dataset
    names(x)[-1] <- substring(names(x)[-1], 2) 
    
    # rename columns 
    x <- dplyr::rename_with(x, ~ gsub("^ncage", "KID_Y", .x), starts_with("ncage"))
    x <- dplyr::rename_with(x, ~ gsub("^ncsex", "KID_S", .x), starts_with("ncsex"))
    
    x = x %>% mutate(ncany = sjlabelled::as_character(ncany))
    
    x =  haven::zap_labels(x, user_na = TRUE)
    
    x <- x %>%
      mutate(across(
        starts_with("KID_Y"), # Dynamically selects all columns that start with "KID_Y"
        ~ case_when(
          # First condition: col < 0 and ncany has specific values
          as.numeric(as.character(.)) %in% c(-10,-1)  ~ NA_real_, # Assign NA
          
          # Second condition: col < 0, is not NA, and ncany matches
          as.numeric(as.character(.)) %in%  c(-4,-3) ~ -1, # Assign -1
          
          # Default case: keep the original value
          TRUE ~ as.numeric(as.character(.))
        )
      ))
     
     x <- x %>%
       mutate(across(
         starts_with("KID_S"), # Dynamically selects all columns that start with "KID_Y"
         ~ case_when(
           # First condition: col < 0 and ncany has specific values
           as.numeric(as.character(.)) %in% c(-10,-1)  ~ NA_real_, # Assign NA
           
           # Second condition: col < 0, is not NA, and ncany matches
           as.numeric(as.character(.)) %in%  c(-4,-3) ~ -1, # Assign -1
           
           # Default case: keep the original value
           TRUE ~ as.numeric(as.character(.))
         )
       ))
    

    # change age to the year of birth 
    x[,paste0("KID_Y", 1:amount_of_max_children_non_resident[i])]<-apply(x[,paste0("KID_Y", 1:amount_of_max_children_non_resident[i])], 2, function(y)
      ifelse(y>0, years[i]-y, y))
    
    x[,paste0("KID_Y", 1:amount_of_max_children_non_resident[i])]<-apply(x[,paste0("KID_Y", 1:amount_of_max_children_non_resident[i])], 2, function(y)
      ifelse(y==0, years[i], y))
    

    # create dummy child check if the same number of sum dummy as tcnr
    
    for ( c in 1:13) {
      
      x[,paste0("KID_",c)]<-ifelse(!is.na(x[,paste0("KID_Y",c)]), 1,0)
      
    }
    
    x$tot_tcnr<-rowSums(x[,paste0("KID_",1:13)])
    
    # add year wave 
    x$year_wave<-years[i]
    
    list_non_resident<-rbind(list_non_resident,x)

    rm(x, v, true_len, non_resident_column_var)
    # print(i)

}


rm(amount_of_max_children_non_resident, i)

# non-resident children file to long format 
non_resident_long<-
  tidyr::pivot_longer(
    list_non_resident, 
    cols = paste0("KID_Y",1:13), 
    names_to = "id",
    values_to = "KID_Y", 
    values_drop_na = TRUE) %>% select(c("xwaveid", "year_wave","id","KID_Y")) %>% 
  mutate(non_resident = "non_resident")

rm(list_non_resident)


################################################################################
# 3. Prepare resident children in long format 
resident_long<-c()
for (i in 1:length(Combined_)) {
  
  # We select parents id and household id 
  resident_vars<-c(
    "xwaveid",
    paste0(letters[i], "hhrhid"),  # hhrhid Wave randomised household ID, 
    paste0(letters[i],"hhbmxid"),  # hhbmxid - Biological Mother's cross wave id
    paste0(letters[i],"hhbfxid")   # hhbfxid - Biological Father's cross wave id
    )
  
  # For each child’s “ever-co-resident biological parents” 
  # (same household, own-parent or own-child relationship), 
  # the xwaveid of the parent (text, 7-digit). The biological parent’s 
  # xwaveid is carried back or forward to all waves even if the parent is 
  # not co-resident but the child has been enumerated.
  
  x <- haven::read_dta(paste0(folder_Australia_1, Combined_[i]), col_select = resident_vars)
  # Delete first letter from variable names in a downloaded dataset
  names(x)[-1] <- substring(names(x)[-1], 2) 
  
  # add SEX, BORN_Y, FIRTOBS_Y, LASTOBS_Y
  x<-merge(readRDS("output/master_hilda_wide.rds"), x, by = c("xwaveid"), all.y = TRUE) %>% 
    # Renaming the variables 
    dplyr::rename(hsh_id = hhrhid,
                  mother_id = hhbmxid, 
                  father_id = hhbfxid)
    
  # A. pair children with their father 
  parent_set<-x
  father_mother_id <-"father_id"
  
  # prepare children data set
  children_set<-x %>% select("xwaveid" ,father_mother_id, "BORN_Y", "SEX","hsh_id") %>% 
    rename(id_child = xwaveid,
           KID_Y = BORN_Y,
           KID_S = SEX)
  
  # merge resident children with their parents by father_mother_id and hsh_id
  fathers<-merge(parent_set, children_set, by.x = c("xwaveid","hsh_id"),
                 by.y = c(father_mother_id, "hsh_id"), all=FALSE) %>% 
    group_by(xwaveid) %>% arrange(KID_Y, .by_group = T) %>% mutate(KID_Y=as.numeric(KID_Y))

  # B. pair children with their mother 
  parent_set<-x
  father_mother_id <-"mother_id"
  
  children_set<-x %>% select("xwaveid" ,father_mother_id, "BORN_Y", "SEX", "hsh_id") %>% 
    rename(id_child = xwaveid,
           KID_Y = BORN_Y,
           KID_S = SEX)

  mothers<-merge(parent_set, children_set, by.x = c("xwaveid","hsh_id"),
                 by.y = c(father_mother_id,"hsh_id"), all=FALSE) %>% 
    group_by(xwaveid) %>% arrange(KID_Y, .by_group = T)  %>% mutate(KID_Y=as.numeric(KID_Y))

  # Combine mothers and fathers  
  parents_x<-bind_rows(mothers, fathers)
  
  # add year ave and append it to previous wave
  parents_x$year_wave<-years[i]
  resident_long<-rbind(resident_long, parents_x)
  
  # remove object
  rm(mothers, fathers, resident_vars, father_mother_id, x, children_set,  parent_set, parents_x)
  
  # print(i)
  
}

# select needed columns from resident children file 
resident_long<-resident_long %>% select("xwaveid", "year_wave", "KID_Y", "KID_S") %>% 
  # add flag variable that those children are resident children
  mutate(non_resident = "resident")

################################################################################        
# 4. Combine non-resident children with resident children in long format
final_resid_non_resid<-
  # we combine resident and non-resident children files which are in long format 
  bind_rows(non_resident_long, resident_long) %>% 
  group_by(xwaveid,year_wave) %>% 
  # we recalculate the order of children parity using their year of birth.
  arrange(KID_Y, .by_group = T) %>% 
# 5. Recalculate children parity order taking into account resident and non-resident children
  mutate(parity = 1:n(),
         KID_S = as.numeric(KID_S))

rm(non_resident_long, resident_long)
  
# number of kids in each wave 
nr_kids<-c()
for (i in 1:length(Combined_)) {
  tot_kid_var<-c("xwaveid",
                 paste0(letters[i], "tcr"),   # tcr Number of own resident children
                 paste0(letters[i], "tcnr"),  # tcnr Number of own non-resident children
                 paste0(letters[i], "tchad")) # Total children respondent ever had
  
  x <- haven::read_dta(paste0(folder_Australia_1, Combined_[i]), col_select = tot_kid_var)
  
  # Delete first letter from variable names in a downloaded dataset
  names(x)[-1] <- substring(names(x)[-1], 2)
  
  # add year wave 
  x$year_wave<-years[i]
  
  nr_kids<-rbind(nr_kids, x)
  
  rm(x, tot_kid_var)
  
}

nr_kids %>% 
  mutate(tcr  = ifelse(tcr<0,NA,tcr),
         tcnr = ifelse(tcnr<0,NA,tcnr),
         tchad = ifelse(tchad<0,NA,tchad),
         sum_tcr_tcnr = tcnr+tcr,
         check = tchad ==sum_tcr_tcnr
         ) %>% filter(!is.na(check)) %>% 
  group_by(check)%>% summarise(n=n()) %>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))


examine = nr_kids %>% 
  mutate(tcr  = ifelse(tcr<0,NA,tcr),
         tcnr = ifelse(tcnr<0,NA,tcnr),
         tchad = ifelse(tchad<0,NA,tchad),
         sum_tcr_tcnr = tcnr+tcr,
         check = tchad ==sum_tcr_tcnr
  ) 

final_resid_non_resid_1 = final_resid_non_resid %>% merge(nr_kids, by = c("xwaveid", "year_wave"))

final_resid_non_resid = final_resid_non_resid %>% 
  mutate(
    KID_Y = ifelse((!is.na(KID_S))& is.na(KID_Y),-1,KID_Y)
  )

################################################################################
# 6. Transform to wide format (all resident and non-resident children)
list_wide<-c()
for (i in 1:length(years)) {
  # select i-th wave
  x<-final_resid_non_resid %>% filter(year_wave == years[i])

  # year of birth of children to wide 
  wide_y<-reshape2::dcast(x , xwaveid ~parity, value.var="KID_Y")
  
  colnames(wide_y)[-1] <- paste0("KID_Y", colnames(wide_y)[-1])
  
  # sex of children to wide 
  wide_s<-reshape2::dcast(x , xwaveid ~parity, value.var="KID_S")
  
  colnames(wide_s)[-1] <- paste0("KID_S", colnames(wide_s)[-1])
  
  # merge all wide format information on year of birthm sexm residency status of children for each individual
  
  wide <- merge(wide_y,wide_s, by = c("xwaveid")) 
  
  wide$year_wave <- years[i]
  
  rm(x)

  list_wide<-bind_rows(list_wide,wide)
  
  rm(wide, wide_y, wide_s)
  
}

rm(final_resid_non_resid)

# choose the last row for each parent
parents = list_wide %>% group_by(xwaveid) %>% arrange(year_wave, .by_group = T) %>% select(-year_wave) %>% slice(n()) 

################################################################################
# 7. Add childless respondents
hilda_fertility_biography<-bind_rows(parents,
               data.frame(xwaveid = 
               filter(readRDS("output/master_hilda_wide.rds"), xwaveid %notin% parents$xwaveid)$xwaveid))

rm(list_wide, parents)
# 8. create child dummies 
hilda_fertility_biography<-hilda_fertility_biography %>% 
  mutate(across(.cols = paste0("KID_Y",1:13),
                .fns = ~ifelse(is.na(.), 0, 1),
                .names = "KID_{col}") ) %>% 
  rename_with(
    ~ gsub("KID_KID_Y", "KID_", .x, fixed = TRUE),
    starts_with("KID_KID_Y"))

# 9. Add sum of children dummy 
hilda_fertility_biography$NR_KIDS<-rowSums(hilda_fertility_biography[,paste0("KID_",1:13)])


################################################################################
# 10.  Order variables 
base_variables<-c("xwaveid", "NR_KIDS")      

kids_order_var<-c()
for (i in 1:13) {
  kids_order_var<-rbind(kids_order_var,paste0("KID_Y",i),paste0("KID_S",i),paste0("KID_",i))
}

all_needed_vars<-c(base_variables, kids_order_var)

names(hilda_fertility_biography)

hilda_fertility_biography<-hilda_fertility_biography %>% select(all_needed_vars)


hilda_fertility_biography  = hilda_fertility_biography %>% mutate(across(
  where(~ is.numeric(.) && all(is.na(.) | . == floor(.))),
  as.integer
))


rm(kids_order_var, all_needed_vars, base_variables)

labs_ind <- c("no child" = 0, "child" = 1)
labs_y   <- c("unknown year"=-1)
labs_m   <- c("unknown month"=-1)
labs_s   <- c("male"=1, "female"=2, "unknown"=-1)
ci <- grep("^KID_\\d+$",  names(hilda_fertility_biography), value = TRUE)
cy <- grep("^KID_Y\\d+$", names(hilda_fertility_biography), value = TRUE)
cm <- grep("^KID_M\\d+$", names(hilda_fertility_biography), value = TRUE)
cs <- grep("^KID_S\\d+$", names(hilda_fertility_biography), value = TRUE)

hilda_fertility_biography[ci] <- lapply(hilda_fertility_biography[ci], labelled, labs_ind)
hilda_fertility_biography[cy] <- lapply(hilda_fertility_biography[cy], labelled, labs_y)
hilda_fertility_biography[cm] <- lapply(hilda_fertility_biography[cm], labelled, labs_m)
hilda_fertility_biography[cs] <- lapply(hilda_fertility_biography[cs], labelled, labs_s)

# add STATA labels for each variable 
for (i in 1:13) {
  
  var_y <- paste0("KID_Y", i)
  var_i <- paste0("KID_", i)
  var_s <- paste0("KID_S", i)
  
  labelled::var_label(hilda_fertility_biography[[var_y]]) <- paste("year of birth of ", i, "child")
  labelled::var_label(hilda_fertility_biography[[var_s]]) <- paste("sex of ", i, "child")
  labelled::var_label(hilda_fertility_biography[[var_i]]) <- paste("birth order ", i, "child")
}

var_label(hilda_fertility_biography$NR_KIDS) <- "Number of children in LIB"


################################################################################
# 11. save data set
saveRDS(hilda_fertility_biography, "output/hilda_fertility.rds")


if (type_you_want==".csv") {
  write.csv(hilda_fertility_biography, "output/hilda_fertility.csv")
}else if(type_you_want==".dta"){
  write_dta(hilda_fertility_biography, "output/hilda_fertility.dta")

}

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script", "history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)

