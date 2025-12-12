############################# SHP fertility history ############################
# This R script is designed to process and clean data related to fertility of individuals in SHP study datasets. 
# The script performs various data transformations and merges to construct a detailed history of fertility for individuals, 
# incorporating three main sources of information: master file (master_mp.dta), yearly survey information (shp_wave_nr_p_user.dta) 
# and retrospective file (shpiii_fa_user.dta).
# The strategy it to transform all those three sources together in long format recalculate the parity and at the end transform it to wide format. 
# Steps in the code
# 1. Read master file and other necessary objects
# 2. Match Biological resident children to their mothers and fathers by parent_id
# 3. Preparation of non-resident children for waves 1-14 (1999-2012)
# 4. Prepare non-resident for merging with resident children 
# 5. Merge master children with non-resident children by wave 1-14
# 6. Prepare retrospective data from 2013 - "shpiii_fa_user.dta"
# 7. Merge parent_master_non_resident with retrospective data from 2013 
# 8. Add parity of children (sort for each individual children from oldest to youngest)
# 9. Transform to wide format
# 10. Add childless individuals 
# 11. Put variables in the right order
################################################################################
# 1. read master file and other necessary objects 
# Establish working directory
source("00_setting_work_space.R")
master_shp_wide = readRDS("output/master_shp_wide.rds")

# Create list of waves to iterate over and download master file 

folder_waves_avaiable<-as.data.frame(list.files(folder_shp_1)) %>% rename(folder_name = `list.files(folder_shp_1)`)

folder_waves_avaiable[c('wave_nr', 'year_wave')] <-str_split_fixed(folder_waves_avaiable$folder_name, "_", 2)

folder_waves_avaiable<-folder_waves_avaiable %>% 
  mutate(year_wave = as.numeric(year_wave)) %>% 
  arrange(year_wave) %>% 
  mutate(wave_nr = ifelse(year_wave == 1999, "99", str_remove(year_wave, "20")))

################################################################################
# 2.	Match Biological resident children to their mothers and fathers by parent_id
# using master_long_all in which we have 
# identification number of mother (idmoth__) and father (idfath__)
################################################################################
# Function match_child_with_parent aims at pairing children to their mothers and fathers using 
# idfath__ (ID of father) and idmoth__ (ID of mother). This variables are stored in master file
# ...\Data_STATA\Data_STATA\SHP-Data-WA-STATA\shp_mp.dta 
# Paring children with their parents will provide information about birth year, sex of every ever-resident child, 
# but there might be some missing still especially for older cohorts 
# From manual page 69:
# Identification numbers of parents refer to their personal ID. For example, to
# match parents and children, one can attach the info of the parent to the info of the child,
# by matching idmoth__ and idfath__ to idpers.
match_child_with_parent <- function(father_mother_id = "idfath__",
                                    personal_id = "idpers",
                                    data_1 = basic) {
  
  basic %>% head()
  
  # a. create parent data set
  parent_set<-data_1
  # b. create children data set
  children_set<-data_1[,c("idpers",father_mother_id, "BORN_Y", "BORN_M","SEX")]
  
  children_set<-rename(children_set,
                       id_child = personal_id,
                       KID_Y = BORN_Y, 
                       KID_M = BORN_M,
                       KID_S = SEX)
  
  ###################### Merge parent with children ############################
  # natural join only common both in both sets
  # an inner join, which means only the rows that have matching keys in both data frames 
  # are retained in the result. Rows from either data frame that do not have a match 
  # in the other data frame are excluded from the result.
  parent<-merge(parent_set, children_set, by.x = c(personal_id),
                by.y = c(father_mother_id), all=FALSE) %>% 
    mutate(
      # -2          no answer
      # -1      does not know
      KID_Y = ifelse(as.numeric(KID_Y) < 0, -1, as.numeric(KID_Y)),
      KID_M  = ifelse(as.numeric(KID_M) < 0, -1, as.numeric(KID_M)))
  

  # create order of a child variable  --> parity (from the oldest to the youngest)
  parent<-parent %>%
    group_by(idpers) %>% arrange(KID_Y,.by_group = T) %>% 
    mutate(parity = 1:n())
  
  
  # create non-resident status based on comparison of hsh_id of a child and parent and other variables 
  
  parent$non_resident<- "resident"

  return(parent)

}

# create mother and father dataset

# Individual Master File
id_parents <- read_dta(paste0(folder_shp_2, "shp_mp.dta")) %>% 
  select(idpers, idmoth__, idfath__)

basic = readRDS("output/master_shp_wide.rds") %>% 
  merge(id_parents ,  by = c("idpers")) %>% 
  select(idpers, idmoth__, idfath__, BORN_Y, BORN_M, SEX,FIRSTOBS_Y,LASTOBS_Y)

fathers <- match_child_with_parent(father_mother_id = "idfath__")
mothers <-match_child_with_parent(father_mother_id = "idmoth__")

parents<-rbind(fathers, mothers) %>%
  select("idpers", "id_child", "KID_Y", "KID_M", "KID_S", "parity", "non_resident")
  
rm(list = c("fathers", "mothers"))

################################################################################
# 3. Preparation of non-resident children for waves 1-14 (1999-2012)
################################################################################
# Question on the year of birth (with some processing by the SHP) of every nonresident child, 
# asked up to the 14th wave. I combine the information on these nonresident children based on the birth year; 
# children reported in different waves with birth years within 2 years (inclusive)
# of one another are assumed to be the same child. 
# No questions on nonresidential children were asked from wave 15 (2013) onwards.

dataset_1999_2003<-data.frame(variable_name_from_manual = 
                        c("ownkid",	"d36",	"d40",	"d41",	"d45",	"d46",	"d50",	"d51",	
                          "d55",	"d56",	"d60",	"d61",	"d65",	"d66",	"d70",	"d71"),
                      official_name = c("totNrKids",	"nrNonResidKids",	"KID_Y1",	"WHOSE_KID_1",
                                        "KID_Y2",	"WHOSE_KID_2",	"KID_Y3",	"WHOSE_KID_3",	"KID_Y4",	"WHOSE_KID_4",
                                        "KID_Y5",	"WHOSE_KID_5",	"KID_Y6",	"WHOSE_KID_6",	"KID_Y7",	"WHOSE_KID_7"))


dataset_2004_2012<-data.frame(variable_name_from_manual = c("ownkid",	"d110",	"d113",	"d114",	"d115",	
                                                            "d118",	"d119",	"d120",	"d123",	"d124",	"d125",	
                                                            "d128",	"d129",	"d130",	"d133",	"d134",	"d135",	
                                                            "d138",	"d139",	"d140",	"d143",	"d144",	"d145",	
                                                            "d148",	"d149",	"d150"),
                              official_name =  c("totNrKids",	"nrNonResidKids",	"KID_Y1",	
                                                 "WHOSE_KID_1",	"IF_KID_SWISS_1",	"KID_Y2",	"WHOSE_KID_2",	
                                                 "IF_KID_SWISS_2",	"KID_Y3",	"WHOSE_KID_3",	"IF_KID_SWISS_3",	
                                                 "KID_Y4",	"WHOSE_KID_4",	"IF_KID_SWISS_4",	"KID_Y5",	"WHOSE_KID_5",	
                                                 "IF_KID_SWISS_5",	"KID_Y6",	"WHOSE_KID_6",	"IF_KID_SWISS_6",	"KID_Y7",	
                                                 "WHOSE_KID_7",	"IF_KID_SWISS_7",	"KID_Y8",	"WHOSE_KID_8",	"IF_KID_SWISS_8"))


list_non_resident_by_wave<-c()
for (wave in 1:14) {
  
  # choose variables for this wave
  if (wave %in% c(1:5)) {
    variable_name_from_manual_1<-dataset_1999_2003
    
  }else{
    variable_name_from_manual_1<-dataset_2004_2012
  }
  
  variable_name_from_manual_1$variable_name_from_manual<-ifelse(variable_name_from_manual_1$variable_name_from_manual =="ownkid",
                                                                paste0("ownkid", folder_waves_avaiable$wave_nr[wave]),
                                                                variable_name_from_manual_1$variable_name_from_manual)
  variable_name_from_manual_1$variable_name_from_manual<-ifelse(variable_name_from_manual_1$variable_name_from_manual==paste0("ownkid", folder_waves_avaiable$wave_nr[wave]),
                                                                variable_name_from_manual_1$variable_name_from_manual,
                                                                paste0("p",folder_waves_avaiable$wave_nr[wave],
                                                                       variable_name_from_manual_1$variable_name_from_manual))
  
  # download the data 
  # If child(ren) in the household: in addition to these children do you have any 
  # other children (biological or adopted)? If so, how many? If no children in the household: 
  # do you have any children (biological or adopted)? If so, how many?
  # Filter : if AGE >= 20
  
  data_personal <- read_dta(paste0(folder_shp_1,
                                   folder_waves_avaiable$folder_name[wave],
                                   "/",
                                   "shp",
                                   folder_waves_avaiable$wave_nr[wave],
                                   "_p_user.dta"),
                            col_select = c("idpers",ends_with(variable_name_from_manual_1$variable_name_from_manual)))
  
  # rename variables 
  data_personal<-select(data_personal, "idpers",variable_name_from_manual_1$variable_name_from_manual)
  
  names(data_personal)<-c("idpers",variable_name_from_manual_1$official_name)
  
  data_personal <- select(data_personal, idpers, totNrKids,nrNonResidKids, starts_with("KID_Y"))
  
  max_kid <- length(names( select(data_personal,starts_with("KID_Y"))))
  
  
  long = data_personal%>%
    pivot_longer(cols = starts_with("KID_Y"), # Select columns to pivot
                 
                 names_to = "parity",        # Name for new categorical column
                 values_to = "KID_Y",        # Name for values column
                 ) %>% 
    mutate(parity = stringr::str_remove(parity,"KID_Y")) %>% 
    filter(nrNonResidKids!=0 & nrNonResidKids!=-3) %>% 
    filter(KID_Y!=-3) %>% 
    mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y))
    
  # add flag about residency status of a child
  long$non_resident<-"non-resident"
  
  long$year_wave<-folder_waves_avaiable$year_wave[wave]

  # save dataset in R environment 
  list_non_resident_by_wave<-rbind(list_non_resident_by_wave, long)
  
  # remove unnecessary objects  
  rm("long")
  rm("data_personal")
  rm("variable_name_from_manual_1")
  rm("max_kid")
  
}

################################################################################
# 4. Prepare non-resident for merging with resident children 
# (there might be overlap as children can move out and move in between wave) 
# We want to check in which waves the individual indicated that a child is non-resident

list_non_resident_by_wave <- list_non_resident_by_wave %>% group_by(idpers) %>% 
  arrange(year_wave, KID_Y, .by_group = T) %>% ungroup() %>% 
  select("idpers", "year_wave", "KID_Y", "non_resident")

original_non_resident <- list_non_resident_by_wave %>% 
  select( -c(year_wave)) %>% distinct()

# 5. Merge master children with non-resident children by wave 1-14
parent_master_non_resident<-merge(select(parents, "idpers",  "KID_S","KID_Y","KID_M"), 
                original_non_resident,
                by = c("idpers","KID_Y"),all = T) %>% 
  select(-"non_resident")

################################################################################
# 6. Prepare retrospective data from 2013 - "shpiii_fa_user.dta"
retro_3 <- read_dta(paste0(folder_retro_shp_2 ,"shpiii_fa_user.dta")) %>% 
  mutate(family_typ = sjlabelled::as_label(family_typ)) %>%
  # ii. choose only level about "year of child birth"
  filter(family_typ %in% c("year of child birth")) %>% 
  select(idpers, family_a) %>% rename(KID_Y = family_a)

# 7. Merge parent_master_non_resident with retrospective data from 2013 
parent_all<-merge(parent_master_non_resident, retro_3, by = c("idpers", "KID_Y"), all = T)

# 8.	Add parity of children (sort for each individual children from oldest to youngest)
parent_all<-parent_all %>% group_by(idpers) %>% 
  arrange(KID_Y, .by_group = T) %>% 
  mutate(parity = 1:n()) %>% 
  mutate(idpers = as.numeric(zap_labels(idpers)), 
         KID_S  = sjlabelled::as_character(KID_S),
         ) %>% 
  mutate(
    KID_S = ifelse(KID_S =="male",1, ifelse(KID_S =="female",2,ifelse(KID_S =="other", -1, NA))),
    KID_S = ifelse(KID_S<0 | is.na(KID_S), -1, KID_S),
    KID_M = ifelse(!is.na(KID_Y) & is.na(KID_M), -1, KID_M)
  )

# 9.	Transform to wide format

parent_all  = parent_all %>% mutate(across(
  where(~ is.numeric(.) && all(is.na(.) | . == floor(.))),
  as.integer
))

parent_all_wide<-parent_all %>%
  pivot_wider(
    id_cols = c(idpers),
    names_from = parity,
    values_from = c("KID_Y", "KID_S","KID_M"),
    names_glue = "{.value}{parity}"
  )

max_nr_kids<-length(grep("^KID_Y", names(parent_all_wide), value = TRUE))

# 10. Add childless individuals 

parent_all_wide = bind_rows(parent_all_wide,
                            data.frame(idpers = usefun::outersect(unique(parent_all_wide$idpers), unique(basic$idpers))))


parent_all_wide[,paste0("KID_",1:max_nr_kids)]<-apply(parent_all_wide[,paste0("KID_Y",1:max_nr_kids)], 2, function(x)
                                                      ifelse(is.na(x),0,1)
                                                      )


parent_all_wide[,paste0("KID_",1:max_nr_kids)]<-apply(parent_all_wide[,paste0("KID_",1:max_nr_kids)], 2, function(x)
  as.integer(x)
)


# add value labels
labs_ind <- c("no child"=0, "child"=1)
labs_y   <- c("unknown year"=-1)
labs_m   <- c("unknown month"=-1)
labs_s   <- c("male"=1, "female"=2, "unknown"=-1)

ci <- grep("^KID_\\d+$",  names(parent_all_wide), value = TRUE)
cy <- grep("^KID_Y\\d+$", names(parent_all_wide), value = TRUE)
cm <- grep("^KID_M\\d+$", names(parent_all_wide), value = TRUE)
cs <- grep("^KID_S\\d+$", names(parent_all_wide), value = TRUE)

parent_all_wide[ci] <- lapply(parent_all_wide[ci], labelled, labs_ind)
parent_all_wide[cy] <- lapply(parent_all_wide[cy], labelled, labs_y)
parent_all_wide[cm] <- lapply(parent_all_wide[cm], labelled, labs_m)
parent_all_wide[cs] <- lapply(parent_all_wide[cs], labelled, labs_s)


parent_all_wide$NR_KIDS <-rowSums(parent_all_wide[,paste0("KID_",1:max_nr_kids)], na.rm = T)

# 11. Put variables in the right order 

ordering_variables_wide_format <- function(x_c) {
  
  lista_order_20<-c()
  for (i in 1:x_c) {
    
    x<-c(paste0("KID_", i),
         paste0("KID_Y", i),
         paste0("KID_M", i),
         paste0("KID_S", i))
    
    lista_order_20<- append(lista_order_20, x)
    
  }
  
  return(lista_order_20)
  
}

parent_all_wide = parent_all_wide[,c("idpers","NR_KIDS", ordering_variables_wide_format(max_nr_kids))]

# add STATA labels for each variable 
for (i in 1:max_nr_kids) {
  
  var_y <- paste0("KID_Y", i)
  var_m <- paste0("KID_M", i)
  var_i <- paste0("KID_", i)
  var_s <- paste0("KID_S", i)
  
  labelled::var_label(parent_all_wide[[var_m]]) <- paste("month of birth of ", i, "child")
  labelled::var_label(parent_all_wide[[var_y]]) <- paste("year of birth of ", i, "child")
  labelled::var_label(parent_all_wide[[var_s]]) <- paste("sex of ", i, "child")
  labelled::var_label(parent_all_wide[[var_i]]) <- paste("birth order ", i, "child")
}

var_label(parent_all_wide$NR_KIDS) <- "Number of children in LIB"

saveRDS(parent_all_wide, "output/shp_fertility.rds")

if (type_you_want==".csv") {
  write.csv(parent_all_wide, "output/shp_fertility.csv")
}else if(type_you_want==".dta"){
  write_dta(parent_all_wide, "output/shp_fertility.dta")
}

x = ls()
x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script","history_set",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)


################################################################################





