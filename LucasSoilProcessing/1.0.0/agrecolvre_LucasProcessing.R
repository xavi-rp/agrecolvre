
#########################################################################
###                                                                   ###
###                            AGRECOLVRE                             ###
###                                                                   ###
### Improvement of the soil health through agroecological management  ###
###                                                                   ###
###                        Processing LUCAS data                      ###
###                                                                   ###
#########################################################################


## loading required packages

library(tidyverse)
library(readxl)
library(flextable) 
library(officer) 
library(webshot2)
library(ggplot2)
library(ggpmisc)
library(ggfortify)
library(openxlsx)
#library(missMDA)
library(optparse)



## directories
Lucas_dir <- "/mnt/inputs"
outputs_dir <- "/mnt/outputs"

if (!dir.exists(outputs_dir)) {
  dir.create(outputs_dir)
}



## Arguments
option_list = list(
  make_option(c("-r", "--regions_ordered"), type = "character",
              default = "PT16,ES11,ES30,FRD,NL,FI,DE40,SK,EL,ITI1",
              help = "Comma-separated list of regions to be included for LUCAS points comparison. It can be NUTS0/1/2/3. [default: %default]", 
              metavar = "character")
  )

opt <- parse_args(OptionParser(option_list = option_list))

regions_ordered <- strsplit(opt$regions_ordered, ",")[[1]]



### step 1: Read in and clean LUCAS data ####

LucasS2018 <- read.csv(paste0(Lucas_dir, "/LUCAS-SOIL-2018.csv"), header = TRUE)
LucasS2009 <- read_excel(paste0(Lucas_dir, "/LUCAS_TOPSOIL_v1.xlsx"))   # Currently not used 
LucasC2009 <- read.csv(paste0(Lucas_dir, "/EU_2009_20200213.CSV.csv"), header = TRUE)
LucasC2018 <- read.csv(paste0(Lucas_dir, "/EU_2018_20200213.csv"), header = TRUE)




## LUCAS core 2009 
#  to get LU and to select 2018 points that have not changed

# Fixing some typos
LucasC2009 <- LucasC2009 %>%
  mutate(LC1 = str_trim(LC1, side = "right"))


## LUCAS core 2018 
# removing "" (1 row) and "8" (37 rows) 
LucasC2018 <- LucasC2018 %>%
  filter(!LC1 %in% c("", "8"))


## LUCAS-Topsoil 2018 
# Changing the forest categories given that in 2009 they were not distinguished
LucasS2018_modified <- LucasS2018 %>%
  mutate(LC_modif_2018 = case_when(
    LC == "C21" ~ "C20",     
    LC == "C22" ~ "C20",     
    LC == "C23" ~ "C20",     
    LC == "C31" ~ "C30",     
    LC == "C32" ~ "C30",     
    LC == "C33" ~ "C30",     
    TRUE ~ LC           # Keep other values unchanged
  ))


## Change "below Limits of Detection" with NAs
LucasS2018_modified <- LucasS2018_modified %>%
  mutate(P = na_if(P, "< LOD"))





### Step 2: Data selection and summary tables ####
#### Step 2a: Selecting unchanged LUCAS points ####

#sort(unique(LucasC2009$LC1))  # this is the one to check for changes 2009-2018
#sort(unique(LucasS2018$LC))   # this is the one to check for changes 2009-2018

## Selecting LUCAS-Soil 2018 that have the same LU than in 2009
LucasS2018_sameLU <- LucasS2018_modified %>%
  inner_join(LucasC2009 %>% select(POINT_ID, LC1), by = c("POINTID" = "POINT_ID")) %>%
  rename(LC1_2009 = LC1)  %>%
  filter(LC_modif_2018 == LC1_2009) 

# by default, only points with unchanged LU (2009-2018) are selected, 
# but the option of using all points can be selected by the user --> not implemented yet

select_same_LU <- "no"
select_same_LU <- "yes"

if(select_same_LU == "yes"){
  LucasS2018_sameLU <- LucasS2018_sameLU  
} else if(select_same_LU == "no"){
  LucasS2018_sameLU <- LucasS2018  
} else{
  print("do you want to select only those rows that have the same LU than in 2009?")
}

#LucasS2018_sameLU



##### Summary of sampled points per regions ####

#LucasS2018_sameLU %>%
#  filter(if_any(everything(), ~ . %in% regions_ordered)) %>% View()
  
# Filter all regions and count the number of rows (LUCAS points with same LU) per region 
summary_table1 <- LucasS2018_sameLU %>%
  select(POINTID, NUTS_0, NUTS_1, NUTS_2, NUTS_3, LC) %>% 
  mutate(across(POINTID, as.character)) %>% 
  pivot_longer(everything(), values_to = "region") %>%
  filter(region %in% regions_ordered) %>%
  group_by(region) %>%
  summarise(LUCAS_TopSoil_UnchangedLU = n(), .groups = "drop") %>%
  slice(match(regions_ordered, region))


# total number of LUCAS topsoil (2018) per region
summary_table1 <- LucasS2018 %>%
  select(POINTID, NUTS_0, NUTS_1, NUTS_2, NUTS_3) %>% 
  mutate(across(POINTID, as.character)) %>% 
  pivot_longer(everything(), values_to = "region") %>%
  filter(region %in% regions_ordered) %>%
  group_by(region) %>%
  summarise(LUCAS_TopSoil = n(), .groups = "drop") %>%
  right_join(summary_table1, by = "region") %>%
  slice(match(regions_ordered, region))

#summary_table1



# counting LUCAS points (unchanged 2009-2018) by region and by LC
desired_cols <- sort(unique(LucasS2018_sameLU$LC))

summary_table1_1 <- LucasS2018_sameLU %>%
  select(POINTID, NUTS_0, NUTS_1, NUTS_2, NUTS_3, LC) %>% 
  mutate(across(POINTID, as.character)) %>% 
  pivot_longer(-c(POINTID, LC), values_to = "region") %>%
  filter(region %in% regions_ordered) %>%
  group_by(region, LC) %>%
  count() %>% #as.data.frame()
  pivot_wider(names_from = LC, values_from = n) %>% #
  as.data.frame() %>% #colnames()
  mutate(!!!setNames(rep(list(0), length(setdiff(desired_cols, names(.)))), 
                     setdiff(desired_cols, names(.)))) %>%
  slice(match(regions_ordered, region))

#summary_table1_1

summary_table1 <- summary_table1 %>%
  #mutate(LUCAS_TopSoil_Broadleaf = rowSums(select(summary_table1_1, starts_with("C1")))) %>%
  mutate(LUCAS_TopSoil_Cropland = rowSums(select(summary_table1_1, starts_with("B")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Grassland = rowSums(select(summary_table1_1, starts_with("E")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Broadleaf = rowSums(select(summary_table1_1, starts_with("C1"), starts_with("C33")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Coniferous = rowSums(select(summary_table1_1, starts_with("C2"), matches("^C31$"), matches("^C32$")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Shrubland = rowSums(select(summary_table1_1, starts_with("D")), na.rm = TRUE)) #%>%
  #mutate(LUCAS_TopSoil_Forests_Shrubland = rowSums(select(summary_table1_1, starts_with("C") | starts_with("D")), na.rm = TRUE))
  
#summary_table1



## Total sampled points in LUCAS-Core

summary_table1 <- LucasC2018 %>%
  select(POINT_ID, NUTS0, NUTS1, NUTS2, NUTS3) %>% 
  mutate(across(POINT_ID, as.character)) %>% 
  pivot_longer(everything(), values_to = "region") %>%
  filter(region %in% regions_ordered) %>%
  group_by(region) %>%
  summarise(LUCAS_Core = n(), .groups = "drop") %>%
  right_join(summary_table1, by = "region") %>%
  slice(match(regions_ordered, region))

#summary_table1


### Step 3: Saving outputs ####

##### Summary table ####

table_flex <- summary_table1 %>%
  flextable() %>%
  set_caption("Table1. Summary of surveyed points in the LUCAS-Core and LUCAS-Topsoil modules in 2018. 'LUCAS_TopSoil_UnchangedLU' (and the subsequent columns, split by LU type) represent the number of points where land use remained unchanged between 2009 and 2018.")

save_as_docx(table_flex, path = paste0(outputs_dir, "/summary_table1.docx"))


##### LUCAS-Soil-2018 Same LU ####

write.csv(LucasS2018_sameLU, paste0(outputs_dir, "/LucasS2018_sameLU.csv"), row.names = FALSE)



