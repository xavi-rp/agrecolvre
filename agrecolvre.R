
###########################################################################
####                                                                   ####
####                            AGRECOLVRE                             ####
####                                                                   ####
#### Improvement of the soil health through agroecological management  ####
####                                                                   ####
###########################################################################

# Rosa's document:
# /Users/xavi_rp/Documents/LifeWatchERIC/AgroServ/AgroServ_1stCall_USC/Estudi_Reproduir/SUs-SOIL.docx


library(tidyverse)
library(readxl)


#### step 1: Read in LUCAS-soil data ####

LucasS2018_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2018/LUCAS-SOIL-2018-v2/"
LucasS2018 <- read.csv(paste0(LucasS2018_dir, "LUCAS-SOIL-2018.csv"))

LucasS2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2009/"
LucasS2009 <- read_excel(paste0(LucasS2009_dir, "LUCAS_TOPSOIL_v1.xlsx"))

LucasC2009 <- 


#### step 2: Explore LUCAS-soil data ####

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



#



#### step 3: Select LUCAS-soil variables ####

#### step 4: PCA ####

#### step 5: ANOVA ####

#### step 6: Visualize and export results  ####





