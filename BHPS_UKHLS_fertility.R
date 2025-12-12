##################### BHPS/UKHLS fertility history ############################
# This R script is designed to process and clean data related to fertility of individuals 
# in BHPS and UKHS study datasets. The script performs various data transformations and merges 
# to construct a detailed history of fertility for individuals, incorporating two main sources 
# of information: family matrix, non-resident children files. 
# The strategy it to transform all those two sources together in long format recalculate 
# the parity and at the end transform it to wide format. Afterward add childless individuals 
# (those who did not appear in any of those aforementioned files).

# 1. Load Required Packages:
source("00_setting_work_space.R")

# 2. Prepare family matrix xhhrel_protect.dta
# Extract information on biological children from a family matrix file 
# (`xhhrel_protect.dta`) containing children IDs. From this data - family matrix xhhrel_protect.dta 
# biological children, we take only biological children.

biological_children <- haven::read_dta(paste0(folder_uk_fertility_1,"xhhrel_protect.dta"), 
                   col_select = c(
                     "pidp",
                     "bcx_N",                   # total no. of biological children	xhhrel
                     starts_with("bcx_pidp_"))) # biological child 1-16 pidp

# - Individuals with no children id are filtered into a separate data frame (`childless`). 
# But in fact they may have non-resident children. We will verify it by including information from files: 
# _natchild_protect.dta and f_natchild_protect.dta for ukhls and "b","k","l"_childnt_protect.dta for bhps.

childless<-filter(biological_children, bcx_N==0)

biological_children<-biological_children %>% filter(bcx_N!=0)

nr_kids<-length(names(biological_children))-2 # it should be 16

# - transform those data with child id to long format to add children 
# characteristics from master file like: SEX and BORN_Y.

# Transform the biological_children dataset into long format for child-level analysis
biological_children_long<-biological_children %>% 
  tidyr::pivot_longer(cols=paste0("bcx_pidp_",1:nr_kids), # select which column to transform form  wide to long: bcx_pidp_1:16 - biological child 1:16 pidp
                      names_to='parity',                  # parity - Name of new column that will store old column names
                      values_to='kid_pid'                 # kid_pid - Name of new column that will store the values
                      ) %>% 
  # Clean and recode:
  # - Remove the "bcx_pidp_" prefix from 'parity' to keep just the numeric birth order
  # - Convert special missing code (-8) in 'kid_pid' to NA
  mutate(parity = str_replace(parity, "bcx_pidp_",""),
         kid_pid = ifelse(kid_pid==-8,NA, kid_pid)) %>% 
  # Rename the total number of biological children column for clarity
  rename(nr_kids_bcx = bcx_N) 

nr_kids_bcx<-biological_children_long %>% select(pidp,nr_kids_bcx) %>% distinct()

parents<-filter(biological_children_long, !is.na(kid_pid))

# Merge children id with master to extract date of birth 
# Load the master dataset and select only unique individual IDs and their birth details
master_uk_1 = readRDS("output/master_bhps_ukhls_wide.rds") %>% select( pidp,SEX,BORN_Y,BORN_M) %>% distinct()

# Prepare a child-level dataset by renaming variables to reflect child-specific info:

children<-master_uk_1  %>% 
  rename(kid_pid = pidp, # - pidp becomes kid_pid (child's ID)
         KID_S = SEX,  # - SEX becomes KID_S (child's sex)
         KID_Y = BORN_Y, # - BORN_Y and BORN_M become KID_Y and KID_M (child's year and month of birth)
         KID_M = BORN_M )

parents<-
  # Merge parent-child relationship data with child birth and sex info using child ID (kid_pid)
  merge(parents, children, by= c("kid_pid")) %>% 
  select( pidp, nr_kids_bcx, parity, KID_S,KID_M,KID_Y,kid_pid) %>% 
  # Clean up child-level data: Replace invalid or missing values (< 0) with -1 to flag as problematic
  mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y), #  -9 missing or wild
         KID_M = ifelse(KID_M<0,-1,KID_M), #  -9 missing or wild
         KID_S = ifelse(KID_S<0,-1,KID_S))

parents_family_matrix <- parents %>% 
  # Group data by parent ID (pidp) to process each family separately
  group_by(pidp) %>% 
  # Within each parent group, sort children by year of birth (KID_Y)
  arrange(KID_Y, .by_group = T) %>% 
  # Assign birth order (parity) based on the sorted order
  mutate(parity=1:n(),               # 1 for first child, 2 for second, etc.
         KID_S = as.numeric(KID_S),  # Ensure sex is numeric    
         KID_M = as.numeric(KID_M),  # Ensure birth month is numeric   
         KID_Y = as.numeric(KID_Y)   # Ensure birth year is numeric
         ) %>% 
  # Remove the grouping structure for downstream processing
  ungroup()

rm(biological_children_long, biological_children, children, parents, master_uk_1)

# 3. Add Non-Resident Children:
# for ukhls:a_natchild_protect.dta and f_natchild_protect.dta
# where we have information on children gender and year of birth (KID_S = a_lchsx,  KID_Y =a_lchdoby)

non_resident_a = read_dta(paste0(folder_uk_fertility_1,"a_natchild_protect.dta"), 
                        col_select = c("pidp","a_lchsx","a_lchdoby", "a_childno", "a_lchlv")) %>% filter(a_lchlv!=1 ) %>% 
  rename(parity=a_childno, KID_S = a_lchsx,  KID_Y =a_lchdoby ) %>% 
  # f_lchlv # Child still lives with parent
  # 1              yes
  # 2               no
  # 3 SPONTANEOUS Died
  # 4        stillborn
  # -9          missing
  # -8     inapplicable
  # -2          refusal
  # -1       don't know
  filter(a_lchlv %in%  c(2,3,4)) %>% # non-resident or dead: 2 no; 3 SPONTANEOUS Died; 4 stillborn
  mutate(KID_S = as.numeric(KID_S),
         KID_Y = as.numeric(KID_Y)) %>% select(-parity, -a_lchlv) %>% 
  mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y),
         KID_S = ifelse(KID_S<0,-1,KID_S)
  )
  
non_resident_f = read_dta(paste0(folder_uk_fertility_1,"f_natchild_protect.dta"), 
                        col_select = c("pidp","f_lchsx","f_lchdoby", "f_childno", "f_lchlv")) %>% 
  # f_lchlv # Child still lives with parent
  # 1              yes
  # 2               no
  # 3 SPONTANEOUS Died
  # 4        stillborn
  # -9          missing
  # -8     inapplicable
  # -2          refusal
  # -1       don't know
  filter(f_lchlv %in%  c(2,3,4)) %>% # non-resident or dead: 2 no; 3 SPONTANEOUS Died; 4 stillborn
  rename(parity=f_childno, KID_S = f_lchsx,  KID_Y =f_lchdoby ) %>% 
  mutate(KID_S = as.numeric(KID_S),
         KID_Y = as.numeric(KID_Y)) %>% select(-parity, -f_lchlv) %>% 
  mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y),
         KID_S = ifelse(KID_S<0,-1,KID_S)
         )

# for bhps:
# b_childnt_protect.dta, k_childnt_protect.dta, l_childnt_protect.dta where we have
# information on children gender, month and year of birth (KID_M=lchbm, KID_Y=lchby4, KID_S=lchsx)

bhps_letter = c("b","k","l")

for (letter in bhps_letter) {
  
  non_resident_bhps = read_dta(paste0(folder_bhps_1 ,"b", 
                                      letter,"_childnt_protect.dta"),
                               col_select = c("pidp","pid",
                                              paste0("b",letter,"_","lchbm"), 
                                              paste0("b",letter,"_","lchby4"),
                                              paste0("b",letter,"_","lchsx"),
                                              paste0("b",letter,"_","lchlv")
                               ))
  
  colnames(non_resident_bhps) <- gsub(paste0("^b",letter,"_"), "", colnames(non_resident_bhps))
  
  non_resident_bhps = non_resident_bhps %>% 
    rename(KID_M=lchbm,
           KID_Y=lchby4,
           KID_S=lchsx) %>% 
    group_by(pidp) %>% arrange(KID_Y, .by_group = T) %>% 
    # bl_lchlv natural child still lives in resp. hh
    # -9      missing
    # -8 inapplicable
    # -7        proxy
    # -2      refusal
    # -1   don't know
    # 1          yes
    # 2           no
    # 3         died
    # 4    stillborn
    filter(lchlv %in% c(2,3,4)) # non-resident or dead: 2 no; 3 SPONTANEOUS Died; 4 stillborn
  
  assign(x = paste0("non_resident_bhps_",letter),value = non_resident_bhps)

}

non_resident_bhps = merge(get(paste0("non_resident_bhps_","b")),  
                          get(paste0("non_resident_bhps_","k")),by = c( "pidp","pid"), all = T) %>% 
  mutate(KID_M  =  coalesce(KID_M.x, KID_M.y),
         KID_Y = coalesce(KID_Y.x, KID_Y.y),
         KID_S = coalesce(KID_S.x,KID_S.y)) %>% select(pid, pidp,KID_M, KID_Y, KID_S) %>% 
  merge(get(paste0("non_resident_bhps_","l")), by = c( "pidp","pid"), all = T) %>% 
  mutate(KID_M  =  coalesce(KID_M.x, KID_M.y),
         KID_Y = coalesce(KID_Y.x, KID_Y.y),
         KID_S = coalesce(KID_S.x,KID_S.y)) %>% select(pid, pidp,KID_M, KID_Y, KID_S)


non_resident_bhps = non_resident_bhps %>% select(pidp, pidp,KID_M, KID_Y, KID_S)%>% distinct() %>% 
  mutate(
    KID_Y = ifelse(KID_Y<0,-1,KID_Y),
    KID_S = ifelse(KID_S<0,-1,KID_S),
  )

# - Non-resident children data from bhps and ukhls are combined into one file on 
# non-resident children (appended, bind - stacking them vertically or side by side) 

non_resident = bind_rows(non_resident_a,non_resident_f) %>% distinct()

# 4. Combining family matrix file on children with non-resident children 
parents_family_matrix_non_resid_ukhks_bhps =
  bind_rows(parents_family_matrix, non_resident, non_resident_bhps) %>%
  group_by(pidp) %>% 
  arrange(KID_Y,  .by_group = T) %>%
  mutate(parity=1:n(),
         KID_S = as.numeric(KID_S),
         KID_M = as.numeric(KID_M),
         KID_Y = as.numeric(KID_Y)) %>% ungroup() %>% 
  mutate(
    KID_M = case_when(
      KID_M %in% c(-2, 15) ~ -1,
      TRUE ~ KID_M 
    )
  )


parents_family_matrix_non_resid_ukhks_bhps = parents_family_matrix_non_resid_ukhks_bhps %>% 
  mutate(
    KID_Y = ifelse((!is.na(KID_S) | !is.na(KID_M))& is.na(KID_Y),-1,KID_Y),
    KID_M = ifelse((!is.na(KID_S) | !is.na(KID_Y))& is.na(KID_M),-1,KID_M),
  )


# 5. Reshape to Wide Format:
# - Transforms the combined data on children and parents back to wide format, creating columns for each child’s birth year, month, and gender by birth order.
# - Additional columns are created to accommodate multiple children, with placeholders for up to 16 children.

variables <- c("KID_S", "KID_M")

data_long = parents_family_matrix_non_resid_ukhks_bhps

wide_fomrat<-reshape2::dcast(data_long , pidp ~ parity, value.var="KID_Y")

colnames(wide_fomrat)[-1] <- paste0("KID_Y", colnames(wide_fomrat)[-1])

for (var in variables) {

  x<-reshape2::dcast(data_long , pidp ~ parity, value.var=var)

  colnames(x)[-1] <- paste0(var, colnames(x)[-1])

  wide_fomrat = merge(wide_fomrat, x, by = c("pidp"))
}

# 6. Add childless respondents
x = unique(data_long$pidp)

master_uk = haven::read_dta(paste0(folder_uk_fertility_1,"xhhrel_protect.dta"), 
                col_select = c(
                  "pidp",
                  "bcx_N",                   # total no. of biological children	xhhrel
                  starts_with("bcx_pidp_"))) # biological child 1-16 pidp


childless<-master_uk[master_uk$pidp %notin% x, ]

uk_fertility_biography = bind_rows(wide_fomrat,  data.frame(pidp = unique(childless$pidp)))

# 7. Add Dummy Variables and Merge Additional Child Information:
# create dummies
max_kids = length(select(uk_fertility_biography, starts_with("KID_Y")) %>% names())

uk_fertility_biography_1<-uk_fertility_biography %>% 
  mutate(across(.cols = paste0("KID_Y",1:max_kids ),
                .fns = ~ifelse(is.na(.), 0, 1),
                .names = "KID_{col}") ) %>% 
  rename_with(
    ~ gsub("KID_KID_Y", "KID_", .x, fixed = TRUE),
    starts_with("KID_KID_Y"))

# Additional 
# Number of children in LIB
uk_fertility_biography_1$NR_KIDS<-rowSums(uk_fertility_biography_1[,paste0("KID_",1:16)])

# order variables 
kids_order_var<-c()
for (i in 1:max_kids) {
  kids_order_var<-rbind(kids_order_var,paste0("KID_Y",i),paste0("KID_M",i),paste0("KID_S",i),paste0("KID_",i))
}

kids_order_var = as.vector(kids_order_var)

uk_fertility_biography_1 = uk_fertility_biography_1 %>% select("pidp","NR_KIDS",kids_order_var)

# add STATA labels for each variable 

var_label(uk_fertility_biography_1$NR_KIDS) <- "Number of children in LIB"

uk_fertility_biography_1[,paste0("KID_",1:max_kids)]<-apply(uk_fertility_biography_1[,paste0("KID_",1:max_kids)], 2, function(x)
  as.integer(x)
)

uk_fertility_biography_1[,paste0("KID_S",1:max_kids)]<-apply(uk_fertility_biography_1[,paste0("KID_S",1:max_kids)], 2, function(x)
  as.integer(x)
)


uk_fertility_biography_1[,paste0("KID_Y",1:max_kids)]<-apply(uk_fertility_biography_1[,paste0("KID_Y",1:max_kids)], 2, function(x)
  as.integer(x)
)

uk_fertility_biography_1[,paste0("KID_M",1:max_kids)]<-apply(uk_fertility_biography_1[,paste0("KID_M",1:max_kids)], 2, function(x)
  as.integer(x)
)


# add value labels
labs_ind <- c("no child"=0, "child"=1)
labs_y   <- c("unknown year"=-1)
labs_m   <- c("unknown month"=-1)
labs_s   <- c("male"=1, "female"=2, "unknown"=-1)

ci <- grep("^KID_\\d+$",  names(uk_fertility_biography_1), value = TRUE)
cy <- grep("^KID_Y\\d+$", names(uk_fertility_biography_1), value = TRUE)
cm <- grep("^KID_M\\d+$", names(uk_fertility_biography_1), value = TRUE)
cs <- grep("^KID_S\\d+$", names(uk_fertility_biography_1), value = TRUE)

uk_fertility_biography_1[ci] <- lapply(uk_fertility_biography_1[ci], labelled, labs_ind)
uk_fertility_biography_1[cy] <- lapply(uk_fertility_biography_1[cy], labelled, labs_y)
uk_fertility_biography_1[cm] <- lapply(uk_fertility_biography_1[cm], labelled, labs_m)
uk_fertility_biography_1[cs] <- lapply(uk_fertility_biography_1[cs], labelled, labs_s)


for (i in 1:max_kids) {
  
  var_y <- paste0("KID_Y", i)
  var_m <- paste0("KID_M", i)
  var_i <- paste0("KID_", i)
  var_s <- paste0("KID_S", i)
  
  labelled::var_label(uk_fertility_biography_1[[var_m]]) <- paste("month of birth of ", i, "child")
  labelled::var_label(uk_fertility_biography_1[[var_y]]) <- paste("year of birth of ", i, "child")
  labelled::var_label(uk_fertility_biography_1[[var_s]]) <- paste("sex of ", i, "child")
  labelled::var_label(uk_fertility_biography_1[[var_i]]) <- paste("birth order ", i, "child")
}

# Save results
saveRDS(uk_fertility_biography_1, "output/bhps_ukhls_fertility.rds")


if (type_you_want==".csv") {
  write.csv(uk_fertility_biography_1, "output/bhps_ukhls_fertility.csv")
}else if(type_you_want==".dta"){
  write_dta(uk_fertility_biography_1, "output/bhps_ukhls_fertility.dta")

}

# Clean the environment for next calculations 

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)





