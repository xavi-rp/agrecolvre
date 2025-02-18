
##########################################################################
###                                                                   ####
###                            AGRECOLVRE                             ####
###                                                                   ####
### Improvement of the soil health through agroecological management  ####
###                                                                   ####
##########################################################################

# Rosa's document:
# /Users/xavi_rp/Documents/LifeWatchERIC/AgroServ/AgroServ_1stCall_USC/Estudi_Reproduir/SUs-SOIL.docx


library(tidyverse)
library(readxl)


### step 1: Read in LUCAS-soil data ####

LucasS2018_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2018/LUCAS-SOIL-2018-v2/"
LucasS2018 <- read.csv(paste0(LucasS2018_dir, "LUCAS-SOIL-2018.csv"), header = TRUE)

LucasS2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2009/"
LucasS2009 <- read_excel(paste0(LucasS2009_dir, "LUCAS_TOPSOIL_v1.xlsx"))

LucasC2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_CORE_Data/"
LucasC2009 <- read.csv(paste0(LucasC2009_dir, "EU_2009_20200213.CSV.csv"), header = TRUE)



### step 2: Explore LUCAS-soil data ####

## LUCAS core 2009 
#  to get LU and to select 2018 points that have not changed

head(LucasC2009)
nrow(LucasC2009)    # 234624
ncol(LucasC2009)    # 44
names(LucasC2009)

sort(unique(LucasC2009$LU1))  # e.g. "U111", "U112", etc
sort(unique(LucasC2009$LC1))  # e.g. "A11",  "A12", etc     # this is the one to check for changes 2009-2018
sort(unique(LucasC2009$LC2))  # e.g. "A13",  "B11", etc

#
## See LC_claees.txt for the LC classification ##
#

# Fixing some typos

LucasC2009 <- LucasC2009 %>%
  mutate(LC1 = str_trim(LC1, side = "right"))



## LUCAS soil 2018
head(LucasS2018)
nrow(LucasS2018)  # 18984
ncol(LucasS2018)  # 27
names(LucasS2018)

# "Depth"            "POINTID"          "pH_CaCl2"         "pH_H2O"           "EC"              
# "OC"               "CaCO3"            "P"                "N"                "K"               
# "OC..20.30.cm."    "CaCO3..20.30.cm." "Ox_Al"            "Ox_Fe"            "NUTS_0"          
# "NUTS_1"           "NUTS_2"           "NUTS_3"           "TH_LAT"           "TH_LONG"         
# "SURVEY_DATE"      "Elev"             "LC"               "LU"               "LC0_Desc"        
# "LC1_Desc"         "LU1_Desc"   

unique(LucasS2018$NUTS_0)
unique(LucasS2018$NUTS_2)

LucasS2018 %>% 
  filter(NUTS_2 == "ES11") %>%      # 192
  #filter(NUTS_2 == "ES30") %>%      # 51
  #filter(NUTS_1 == "ES1") %>%      # 286
  #filter(NUTS_0 == "NL") %>%        # 99
  nrow() 
  #view() 


sort(unique(LucasS2018$POINTID), decreasing = TRUE) 
sum(sort(unique(LucasS2018$POINTID), decreasing = TRUE) == 57981484)

sort(unique(LucasS2018$LC))  # e.g. "A21", "A22", etc       # this is the one to check for changes 2009-2018
sort(unique(LucasS2018$LC0_Desc))  # e.g. "Cropland", "Grassland", etc
head(sort(unique(LucasS2018$LC1_Desc)))  # e.g. "Apple fruit", "Barley", "Broadleaved woodland", etc        
sort(unique(LucasS2018$LU))  # e.g. "U111", "U112", etc                   
head(sort(unique(LucasS2018$LU1_Desc)))  # e.g. "Agriculture (excluding fallow land and kitchen gardens)", "Forestry", etc                   



### step 3: Select unchanged LUCAS points ####
## Select those points with the same LU between 2009 and 2018

sort(unique(LucasC2009$LC1))  # e.g. "A11",  "A12", etc     # this is the one to check for changes 2009-2018
sort(unique(LucasS2018$LC))  # e.g. "A21", "A22", etc       # this is the one to check for changes 2009-2018

sum(is.na(LucasC2009$LC1))
sum(is.na(LucasS2018$LC))


sort(unique(LucasC2009$LC1)[!unique(LucasC2009$LC1) %in% unique(LucasS2018$LC)])
sort(unique(LucasS2018$LC)[!unique(LucasS2018$LC) %in% unique(LucasC2009$LC1)])


# Changing the forest categories given that in 2009 they were not distinguished
LucasS2018_modified <- LucasS2018 %>%
  mutate(LC_modif_2018 = case_when(
    LC == "C21" ~ "C20",     
    LC == "C22" ~ "C20",     
    LC == "C23" ~ "C20",     
    LC == "C31" ~ "C30",     
    LC == "C32" ~ "C30",     
    LC == "C33" ~ "C30",     
    TRUE ~ LC                       # Keep other values unchanged
  ))

sort(unique(LucasS2018_modified$LC_modif_2018))
sort(unique(LucasS2018_modified$LC_modif_2018)[!unique(LucasS2018_modified$LC_modif_2018) %in% unique(LucasC2009$LC1)])
head(LucasS2018_modified)


# Checking LUCAS-Core 2009 that were sampled in LUCAS-soil 2018
LucasC2009_2018 <- LucasC2009[LucasC2009$POINT_ID %in% LucasS2018$POINTID, ]

nrow(LucasC2009_2018) == nrow(LucasS2018)
nrow(LucasC2009_2018) - nrow(LucasS2018)   # 3351 points in 2018 were not surveyed in 2009
                                           # Here an option would be to use LUCAS-Core 2012 data


# Selecting LUCAS-Soil 2018 that have the same LU than in 2009
LucasS2018_sameLU <- LucasS2018_modified %>%
  inner_join(LucasC2009 %>% select(POINT_ID, LC1), by = c("POINTID" = "POINT_ID")) %>%
  rename(LC1_2009 = LC1)  %>%
  filter(LC_modif_2018 == LC1_2009) 

nrow(LucasS2018_sameLU)
names(LucasS2018_sameLU)
View(LucasS2018_sameLU)



#### Analysing number of points per regions ####

#LucasS2018_sameLU %>%
#  filter(apply(LucasS2018_sameLU, 1, function(row) any(grepl("FRD", row))))

# NUTS_2 %in% c("PT16", "ES11", "ES30", "DE40", "ITI1") |
#           NUTS_1 %in% c("FRD") |
#           NUTS_0 %in% c("NL", "FI", "SK", "EL")

regions_ordered <- c("PT16", "ES11", "ES30", "FRD", "NL", "FI", "DE40", "SK", "EL", "ITI1")

conditions <- list(
  function(df) df$NUTS_2 %in% c("PT16"),          # condition 1
  function(df) df$NUTS_2 %in% c("ES11"),          # condition 2
  function(df) df$NUTS_2 %in% c("ES30"),          # condition 3
  function(df) df$NUTS_1 %in% c("FRD"),           # condition 4
  function(df) df$NUTS_0 %in% c("NL"),            # condition 5
  function(df) df$NUTS_0 %in% c("FI"),            # condition 6
  function(df) df$NUTS_2 %in% c("DE40"),          # condition 7
  function(df) df$NUTS_0 %in% c("SK"),            # condition 8
  function(df) df$NUTS_0 %in% c("EL"),            # condition 9
  function(df) df$NUTS_2 %in% c("ITI1")           # condition 10
  
)

# Apply all conditions and count the number of rows that meet each condition
summary_table1 <- LucasS2018_sameLU %>%
  summarise(
    PT16 = sum(conditions[[1]](LucasS2018_sameLU)),
    ES11 = sum(conditions[[2]](LucasS2018_sameLU)),
    ES30 = sum(conditions[[3]](LucasS2018_sameLU)),
    FRD = sum(conditions[[4]](LucasS2018_sameLU)),
    NL = sum(conditions[[5]](LucasS2018_sameLU)),
    FI = sum(conditions[[6]](LucasS2018_sameLU)),
    DE40 = sum(conditions[[7]](LucasS2018_sameLU)),
    SK = sum(conditions[[8]](LucasS2018_sameLU)),
    EL = sum(conditions[[9]](LucasS2018_sameLU)),
    ITI1 = sum(conditions[[10]](LucasS2018_sameLU))
  ) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("LUCAS_TopSoil" = V1)

summary_table1
desired_cols <- sort(unique(LucasS2018_sameLU$LC))

summary_table1_1 <- data.frame()

for (i in 1:10) {
  summary_table1_1_x <- LucasS2018_sameLU %>%
    filter(conditions[[i]](LucasS2018_sameLU)) %>%
    group_by(LC) %>%
    count() %>% #as.data.frame()
    pivot_wider(names_from = LC, values_from = n) %>% #
    as.data.frame() %>% #colnames()
    mutate(!!!setNames(rep(list(0), length(setdiff(desired_cols, names(.)))), 
                       setdiff(desired_cols, names(.)))) %>%
    select(all_of(desired_cols)) %>% # Ensure correct column order
    {row.names(.) <- regions_ordered[i]; .}
  
  summary_table1_1 <- rbind(summary_table1_1, summary_table1_1_x)  
}
summary_table1_1

#apply(summary_table1_1, 1, sum)  #just to check

#summary_table1 <- cbind(summary_table1, summary_table1_1)
#summary_table1

summary_table1 <- summary_table1 %>%
  #mutate(LUCAS_TopSoil_Broadleaf = rowSums(select(summary_table1_1, starts_with("C1")))) %>%
  mutate(LUCAS_TopSoil_Broadleaf = rowSums(select(summary_table1_1, starts_with("C1"), starts_with("C33")))) %>%
  mutate(LUCAS_TopSoil_Coniferous = rowSums(select(summary_table1_1, starts_with("C2"), matches("^C31$"), matches("^C32$")))) %>%
  mutate(LUCAS_TopSoil_Shrubland = rowSums(select(summary_table1_1, starts_with("D")))) %>%
  mutate(LUCAS_TopSoil_Forests_Shrubland = rowSums(select(summary_table1_1, starts_with("C") | starts_with("D"))))
  



LUCAS_TopSoil_broadleaf



#




### step 4: Select LUCAS-soil variables ####









### step 5: PCA ####

### step 6: ANOVA ####

### step 7: Visualize and export results  ####




