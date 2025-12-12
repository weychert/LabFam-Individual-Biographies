############################# PSID fertility history ###########################
# 1. Download necessary packages and Establish necessary directories
# This script processes fertility history data from the Panel Study of Income Dynamics (PSID),
# using the official "1985–2021 Childbirth and Adoption History File" provided by the PSID team.
# The script transforms raw fertility and adoption records into a structured panel dataset
# suitable for longitudinal and survival analysis of childbearing behavior.

source("00_setting_work_space.R")

if (file.exists(paste0(folder_family_files , "new_family_folder_psid"))){
  # print("ok")
} else {
  dir.create(file.path(paste0(folder_family_files , "/new_family_folder_psid")))
  
}

outDir <- paste0(folder_family_files , "/new_family_folder_psid")

################################################################################
# 2. prepare fertility file CAH85 (Childbirth and Adoption History 1985-2021)
# https://psidonline.isr.umich.edu/documents/psid/intro/cah85_21intro.pdf

if ( (dir.exists(paste0(folder_fertility)) & 
    file.exists(paste0(folder_fertility, "/CAH85.rds"))) ==T) {
 
  psid_fertility<-readRDS(paste0(folder_fertility, "/CAH85.rds"))
   
}else{
  
  easyPSID::convert_to_rds(
    in_direc= folder_fertility, # Directory containing unzipped PSID .txt and .do files
    out_direc = folder_fertility # Directory to place PSID .rds files into
  )
  
  psid_fertility<-readRDS(paste0(folder_fertility, "/CAH85.rds"))
}

################################################################################
# 3. Rename columns in fertility history and create pid 

psid_fertility <- readRDS(paste0(folder_fertility, "/CAH85.rds")) %>% 
              rename(
                ER30001 = CAH3,    # "1968 INTERVIEW NUMBER OF PARENT"  
                ER30002 = CAH4,    # "PERSON NUMBER OF PARENT" 
                BORN_Y = CAH7,     # "YEAR PARENT BORN"
                BORN_M = CAH6,     # "MONTH PARENT BORN"
                SEX = CAH5,        # "SEX OF PARENT" 
                PARITY = CAH9,     # "BIRTH ORDER" (99 - Inap.: adoption record (CAH2=2); no births)
                KID_ER30001 = CAH11,   # "PERSON NUMBER OF CHILD"
                KID_ER30002 = CAH10,   # Child's 1968 Interview Number 
                KID_M = CAH13,         # "OS2. YEAR CHILD BORN"
                KID_Y = CAH15,         # "OS2. MONTH CHILD BORN" 
                KID_S = CAH12,
                adoption = CAH2       # (1 Childbirth record, 2 Adoption record)
                # NR_KIDS_1 = CAH108,    # the number of childbirth or adoption records. 
                # # One record for this individual; only one child, no children, or an unknown number of children)
                # NR_KIDS_2 = CAH106     
                # # CAH106 NUMBER OF NATURAL OR ADOPTED CHILDREN (0 - None, 98 - NA; DK; was reported to have had children but NA, DK the total number)
                ) %>% 
  select(ER30001,ER30002, adoption,CAH106,CAH108,BORN_Y, SEX, PARITY,KID_ER30001,KID_ER30002,KID_Y,KID_M,KID_S) %>% 
  # How is an individual uniquely identified? 
  # The combination of the 1968 ID and the person number uniquely identify each individual.
  # To identify an individual across waves we use the 1968 ID and Person Number 
  # (Summary Variables ER30001 and ER30002). Following the literature we combine these 
  # two variables using the following method: (ER30001 * 1000) + ER30002
  # (1968 ID multiplied by 1000) plus Person Number
  mutate(childless = ifelse(CAH106==0 & CAH108== 1 & adoption!=2, "childless", "parent")) %>% 
  mutate(pid     = ER30001*1000 + ER30002,
         KID_Y = ifelse(KID_Y=="9998" & childless =="parent", -2, KID_Y),     # NA; DK; RF
         KID_Y = ifelse(KID_Y=="9999"& childless =="parent", -2, KID_Y),     # Inap.: no child
         KID_M = ifelse(KID_M=="98" & childless =="parent",-2, KID_M),     # NA; DK; RF
         KID_M = ifelse(KID_M=="99"  & childless =="parent",-2, KID_M),     # Inap.: no child
         ##
         KID_Y = ifelse(KID_Y=="9998" & childless =="childless", NA, KID_Y),     # NA; DK; RF
         KID_Y = ifelse(KID_Y=="9999"& childless =="childless", NA, KID_Y),     # Inap.: no child
         KID_M = ifelse(KID_M=="98" & childless =="childless",NA, KID_M),     # NA; DK; RF
         KID_M = ifelse(KID_M=="99"  & childless =="childless",NA, KID_M),     # Inap.: no child
         ) %>% 
  select(pid, BORN_Y,  SEX, PARITY, KID_Y, KID_M, KID_S,CAH106,CAH108,adoption,childless) %>% 
  filter(adoption!=2)

psid_fertility_1 = psid_fertility %>% 
  select(pid, PARITY,KID_Y, KID_M, KID_S,childless) %>% 
  # create dummy based on PARITY  PARITY==99 childless individuals 
  mutate(KID = ifelse(PARITY!=99,1,0)) %>% 
  # CAH9 variable # "BIRTH ORDER" (99 - Inap.: adoption record (CAH2=2); no births)
  # CAH9 variable 98 - NA; DK; RF if we do not know the order we don't know the order of children then, 
  # we assume the 'missing' child was the first child
  mutate(PARITY = ifelse(PARITY==99,1,PARITY)) %>% 
  mutate(PARITY_1 = ifelse(PARITY==98,0,PARITY)) %>% 
  group_by(pid) %>% arrange(PARITY_1, .by_group = T) %>% 
  mutate(PARITY_2 = 1:n())

# here add labels and make it integer 

psid_fertility_1 = psid_fertility_1 %>% mutate(
  KID_Y = ifelse(KID_Y<0, -1, KID_Y),
  KID_S = as.integer(KID_S),
  KID_S = case_when(
    KID_S ==1 ~ 1,
    KID_S ==2 ~ 2,
    KID_S %in% c(8) ~ -1,
    KID_S %in% c(9) ~ NA
  ),
  KID_S = as.integer(KID_S),
  KID_M = as.integer(KID_M),
  KID_M = case_when(
    KID_M ==21  ~  1,
    KID_M ==22  ~  3,
    KID_M ==23  ~  7,
    KID_M ==24  ~  9,
    KID_M ==-2  ~  -1,
    TRUE ~KID_M
  ),
  KID_M = as.integer(KID_M),
  KID_Y = as.integer(KID_Y),
  KID_Y = ifelse(KID_Y<0, -1,KID_Y)
)


# Kid_y, kid_m – some cases coded as -2, but -2 not explained in table 5
# Kid_m some values bigger than 12 (even equal 23)
# Kid_s – some cases coded as 8 or 9 (many!)


psid_fertility_1 = psid_fertility_1 %>%
  mutate(
    across(
      c(KID_Y, KID_M, KID_S, KID),  # specify column names
      as.integer                    # convert each to integer
    ),
    KID_Y = labelled(KID_Y, c("unknown year" = -1)),
    KID_S = labelled(KID_S, c("male" = 1, "female" = 2, "unknown" = -1)),
    KID_M = labelled(KID_M, c("unknown month" = -1)),
    KID = labelled(KID, c("no child" = 0, "child" = 1))
  )

wide_fomrat = psid_fertility_1 %>%
  pivot_wider(
    id_cols = pid,
    names_from = PARITY_2,
    values_from = c(KID_Y, KID_S, KID_M, KID),
    names_glue = "{.value}{PARITY_2}"
  )

names(wide_fomrat) <- sub("^KID(\\d+)$", "KID_\\1", names(wide_fomrat))

# save results
max = sum(grepl("^KID_Y\\d+$", names(wide_fomrat)))

kids_order_var<-c()
for (i in 1:max) {
  kids_order_var<-rbind(kids_order_var,paste0("KID_Y",i),paste0("KID_M",i),paste0("KID_S",i),paste0("KID_",i))
}

wide_fomrat[,paste0("KID_",1:max)] <- apply(wide_fomrat[,paste0("KID_",1:max)],2, function (x) ifelse(is.na(x), 0, x))

wide_fomrat$NR_KIDS = rowSums(wide_fomrat[,paste0("KID_",1:max)])

wide_fomrat = wide_fomrat[,c("pid","NR_KIDS",kids_order_var)]

# add STATA labels for each variable 
for (i in 1:max) {
  
  var_y <- paste0("KID_Y", i)
  var_m <- paste0("KID_M", i)
  var_i <- paste0("KID_", i)
  var_s <- paste0("KID_S", i)
  
  labelled::var_label(wide_fomrat[[var_m]]) <- paste("month of birth of ", i, "child")
  labelled::var_label(wide_fomrat[[var_y]]) <- paste("year of birth of ", i, "child")
  labelled::var_label(wide_fomrat[[var_s]]) <- paste("sex of ", i, "child")
  labelled::var_label(wide_fomrat[[var_i]]) <- paste("birth order ", i, "child")
}

var_label(wide_fomrat$NR_KIDS) <- "Number of children in LIB"

# save results

saveRDS(wide_fomrat, "output/psid_fertility.rds")


if (type_you_want==".csv") {
  write.csv(wide_fomrat, "output/psid_fertility.csv")
}else if(type_you_want==".dta"){
  write_dta(wide_fomrat, "output/psid_fertility.dta")
  
}

# Cleaning Up Workspace

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_3",
                "folder_shp_1","folder_shp_3","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)





