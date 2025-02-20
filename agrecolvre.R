
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
library(flextable) 
library(officer) 
library(webshot2)
library(ggplot2)
library(ggpmisc)
library(ggfortify)




setwd("/Users/xavi_rp/Documents/LifeWatchERIC/AgroServ/AgroServ_1stCall_USC")


### step 1: Read in LUCAS-soil data ####

LucasS2018_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2018/LUCAS-SOIL-2018-v2/"
LucasS2018 <- read.csv(paste0(LucasS2018_dir, "LUCAS-SOIL-2018.csv"), header = TRUE)

LucasS2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2009/"
LucasS2009 <- read_excel(paste0(LucasS2009_dir, "LUCAS_TOPSOIL_v1.xlsx"))

LucasC2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_CORE_Data/"
LucasC2009 <- read.csv(paste0(LucasC2009_dir, "EU_2009_20200213.CSV.csv"), header = TRUE)

LucasC2018_dir <- LucasC2009_dir
LucasC2018 <- read.csv(paste0(LucasC2009_dir, "EU_2018_20200213.csv"), header = TRUE)



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


## LUCAS core 2018 

head(LucasC2018)
nrow(LucasC2018)    # 337855
ncol(LucasC2018)    # 97
names(LucasC2018)

sort(unique(LucasC2018$LU1))  # e.g. "U111", "U112", etc
sort(unique(LucasC2018$LC1))  # e.g. "A11",  "A12", etc     # this is the one to check for changes 2009-2018
sum(LucasC2018$LC1 == "8")    # there are also "" (1 row) and "8" (37 rows) 

# removing them
LucasC2018 <- LucasC2018 %>%
  filter(!LC1 %in% c("", "8"))



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



### step 3: Analysing number of sampled points per regions ####
#### Selecting unchanged LUCAS points ####


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



#### Summary of sampled points per regions ####

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
  rename("LUCAS_TopSoil_UnchangedLU" = V1)

summary_table1


summary_table1 <- LucasS2018 %>%
  summarise(
    PT16 = sum(conditions[[1]](LucasS2018)),
    ES11 = sum(conditions[[2]](LucasS2018)),
    ES30 = sum(conditions[[3]](LucasS2018)),
    FRD = sum(conditions[[4]](LucasS2018)),
    NL = sum(conditions[[5]](LucasS2018)),
    FI = sum(conditions[[6]](LucasS2018)),
    DE40 = sum(conditions[[7]](LucasS2018)),
    SK = sum(conditions[[8]](LucasS2018)),
    EL = sum(conditions[[9]](LucasS2018)),
    ITI1 = sum(conditions[[10]](LucasS2018))
  ) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("LUCAS_TopSoil" = V1) %>%
  cbind(summary_table1)

summary_table1



# by LC
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
  

summary_table1

summary_table1 %>%
  bind_rows(summarise(., across(everything(), sum)))



## Total sampled points in LUCAS-Core

conditions1 <- list(
  function(df) df$NUTS2 %in% c("PT16"),          # condition 1
  function(df) df$NUTS2 %in% c("ES11"),          # condition 2
  function(df) df$NUTS2 %in% c("ES30"),          # condition 3
  function(df) df$NUTS1 %in% c("FRD"),           # condition 4
  function(df) df$NUTS0 %in% c("NL"),            # condition 5
  function(df) df$NUTS0 %in% c("FI"),            # condition 6
  function(df) df$NUTS2 %in% c("DE40"),          # condition 7
  function(df) df$NUTS0 %in% c("SK"),            # condition 8
  function(df) df$NUTS0 %in% c("EL"),            # condition 9
  function(df) df$NUTS2 %in% c("ITI1")           # condition 10
  
)

summary_table2 <- LucasC2018 %>%
  summarise(
    PT16 = sum(conditions1[[1]](LucasC2018)),
    ES11 = sum(conditions1[[2]](LucasC2018)),
    ES30 = sum(conditions1[[3]](LucasC2018)),
    FRD = sum(conditions1[[4]](LucasC2018)),
    NL = sum(conditions1[[5]](LucasC2018)),
    FI = sum(conditions1[[6]](LucasC2018)),
    DE40 = sum(conditions1[[7]](LucasC2018)),
    SK = sum(conditions1[[8]](LucasC2018)),
    EL = sum(conditions1[[9]](LucasC2018)),
    ITI1 = sum(conditions1[[10]](LucasC2018))
  ) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("LUCAS_Core" = V1)

summary_table2

desired_cols1 <- sort(unique(LucasC2018$LC1))

summary_table2_1 <- data.frame()

for (i in 1:10) {
  summary_table2_1_x <- LucasC2018 %>%
    filter(conditions1[[i]](LucasC2018)) %>%
    group_by(LC1) %>%
    count() %>% #as.data.frame()
    pivot_wider(names_from = LC1, values_from = n) %>% #
    as.data.frame() %>% #colnames()
    mutate(!!!setNames(rep(list(0), length(setdiff(desired_cols1, names(.)))), 
                       setdiff(desired_cols1, names(.)))) %>%
    select(all_of(desired_cols1)) %>% # Ensure correct column order
    {row.names(.) <- regions_ordered[i]; .}
  
  summary_table2_1 <- rbind(summary_table2_1, summary_table2_1_x)  
}
summary_table2_1

#apply(summary_table2_1, 1, sum)  #just to check


summary_table1
summary_table1 <- cbind(summary_table2, summary_table1)
summary_table1

summary_table1 <- rownames_to_column(summary_table1, var = "Region")



table_flex <- summary_table1 %>%
  flextable() %>%
  set_caption("Table1. Summary of surveyed points in the LUCAS-Core and LUCAS-Topsoil modules in 2018. 'LUCAS_TopSoil_UnchangedLU' (and the subsequent columns, split by forest type) represent the number of points where land use remained unchanged between 2009 and 2018.")

save_as_docx(table_flex, path = "./results/summary_table1.docx")




### step 4: Select LUCAS-soil variables, regions, etc ####

LucasS2018_sameLU
names(LucasS2018_sameLU)


## Selecting region (Galicia --> NUTS_2 == ES11)

LucasS2018_sameLU_reg <- LucasS2018_sameLU %>%
  filter(NUTS_2 %in% c("ES11")) #%>%  # nrow()  # 102
  
LucasS2018_sameLU_reg

## Keeping all the LU together
sort(unique(LucasS2018_sameLU_reg$LC))  # "B11" "B16" "B55" "B71" "C10" "C22" "C32" "C33" "D10" "D20" "E20"

names(LucasS2018_sameLU_reg)

#LucasS2018_sameLU_reg %>%
#  select(pH_CaCl2, pH_H2O) %>%
#  ggplot(aes(x = pH_CaCl2, y = pH_H2O)) + 
#  geom_point() +  
#  #geom_abline(slope = 1, intercept = 0) +
#  geom_smooth(method = "lm", se = TRUE) +  
#  stat_poly_eq()  +
#  labs(title = "", x = "pH_CaCl2", y = "pH_H2O")


variables_lst <-c("pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N", "K")

sort(unique(LucasS2018_sameLU_reg$P))
sort(unique(LucasS2018_sameLU_reg$N))
sort(unique(LucasS2018_sameLU_reg$K))

LucasS2018_sameLU_reg %>% 
  select(all_of(variables_lst)) %>%
  summarise(NAs = sum(is.na(.)),
            NAs_CaCO3 = sum(is.na(CaCO3)),
            LOD_P = sum(P == "< LOD", na.rm = TRUE),
            n = n())
#  NAs  NAs_CaCO3  LOD_P   n
#   46         46     44  102


range(unique(LucasS2018_sameLU_reg$P), na.rm = TRUE)

## Change below Limits of Detection with NAs
LucasS2018_sameLU_reg <- LucasS2018_sameLU_reg %>%
  mutate(P = na_if(P, "< LOD"))

#


### step 5: PCA ####


LucasS2018_sameLU_reg_vrbls <- LucasS2018_sameLU_reg %>% 
  select(all_of(variables_lst)) %>%
  mutate(across(everything(), as.numeric)) %>%
  #scale() %>%           # Standardize variables (important for PCA)
  as_tibble()

LucasS2018_sameLU_reg_vrbls

## Handling missing values
## Option 1: remove rows with NAs
pca_result <- prcomp(na.omit(LucasS2018_sameLU_reg_vrbls),   # to remove rows with NAs (remain 29 rows out of 102) --> probably it's too many
                     center = TRUE, scale. = TRUE)
summary(pca_result)


## Option 2: remove columns with NAs (CaCO3 and P)
pca_result2 <- prcomp(select(LucasS2018_sameLU_reg_vrbls, -c(CaCO3, P)),   # to remove P and CaCO3
                      center = TRUE, scale. = TRUE)
summary(pca_result2)


## Convert PCA scores into a dataframe
pca_scores <- as_tibble(pca_result$x)
pca_scores2 <- as_tibble(pca_result2$x)


## Plotting
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA Analysis: PC1 vs PC2",
       x = "Principal Component 1",
       y = "Principal Component 2")

## Biplot
#autoplot(pca_result, 
#         loadings = TRUE, 
#         loadings.label = TRUE,
#         loadings.colour = NA,
#         geom = "loadings") 

# Extract loadings
loadings <- as.data.frame(pca_result$rotation)  # Loadings data
loadings$Variable <- rownames(loadings)  # Add variable names

# Plot only loadings' labels (no scores, no arrows)
ggplot(loadings, aes(x = PC1, y = PC2, label = Variable)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot of points
  geom_text(size = 3, vjust = 2) +  # Show labels only
  geom_hline(yintercept = 0) +  # Horizontal line
  geom_vline(xintercept = 0) +  # Vertical line
  labs(title = "PCA Loadings (Labels Only)")

## Which variable is most strongly associated with each PC.
loadings1 <- as.data.frame(pca_result$rotation)  # Loadings data
top_vars <- apply(abs(loadings1), 2, function(x) names(sort(x, decreasing = TRUE)))
top_vars

top_1var <- apply(abs(loadings1), 2, function(x) names(which.max(x)))
top_1var


### Explained variance
#explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
#ggplot(data = tibble(PC = paste0("PC", 1:length(explained_var)), Variance = explained_var), 
#       aes(x = PC, y = Variance)) +
#  geom_col(fill = "blue") +
#  geom_text(aes(label = round(Variance, 2)), vjust = -0.5) +
#  labs(title = "Scree Plot: Explained Variance per PC",
#       x = "Principal Components", 
#       y = "Proportion of Variance Explained")





### step 6: ANOVA ####

### step 7: Visualize and export results  ####




