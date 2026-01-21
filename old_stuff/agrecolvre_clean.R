
#########################################################################
###                                                                   ###
###                            AGRECOLVRE                             ###
###                                                                   ###
### Improvement of the soil health through agroecological management  ###
###                                                                   ###
#########################################################################


library(tidyverse)
library(readxl)
library(flextable) 
library(officer) 
library(webshot2)
library(ggplot2)
library(ggpmisc)
library(ggfortify)
library(openxlsx)



setwd("/Users/xavi_rp/Documents/LifeWatchERIC/AgroServ/AgroServ_1stCall_USC")


### step 1: Read in and clean LUCAS data ####

LucasS2018_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2018/LUCAS-SOIL-2018-v2/"
LucasS2018 <- read.csv(paste0(LucasS2018_dir, "LUCAS-SOIL-2018.csv"), header = TRUE)

LucasS2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_SOIL_Data/LucasSoil_2009/"
LucasS2009 <- read_excel(paste0(LucasS2009_dir, "LUCAS_TOPSOIL_v1.xlsx"))

LucasC2009_dir <- "/Users/xavi_rp/Documents/LifeWatchERIC/LUCAS_CORE_Data/"
LucasC2009 <- read.csv(paste0(LucasC2009_dir, "EU_2009_20200213.CSV.csv"), header = TRUE)

LucasC2018_dir <- LucasC2009_dir
LucasC2018 <- read.csv(paste0(LucasC2009_dir, "EU_2018_20200213.csv"), header = TRUE)



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



##### Summary of sampled points per regions ####

# The user should provide the list of regions that wants to analyse.
# If nothing is provided, then the list of regions from SUS-SOIL Living Labs is the one used


if (!exists("regions_ordered")){
  regions_ordered <- c("PT16", "ES11", "ES30", "FRD", "NL", "FI", "DE40", "SK", "EL", "ITI1") # These regions are those from SUS-SOIL Living Labs, keeping Rosa's study order
  
}else{
  # regions_ordered provided by the user as an argument, and into the environment as a vector
  regions_ordered <- regions_ordered
}

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


summary_table1




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

summary_table1_1

summary_table1 <- summary_table1 %>%
  #mutate(LUCAS_TopSoil_Broadleaf = rowSums(select(summary_table1_1, starts_with("C1")))) %>%
  mutate(LUCAS_TopSoil_Broadleaf = rowSums(select(summary_table1_1, starts_with("C1"), starts_with("C33")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Coniferous = rowSums(select(summary_table1_1, starts_with("C2"), matches("^C31$"), matches("^C32$")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Shrubland = rowSums(select(summary_table1_1, starts_with("D")), na.rm = TRUE)) %>%
  mutate(LUCAS_TopSoil_Forests_Shrubland = rowSums(select(summary_table1_1, starts_with("C") | starts_with("D")), na.rm = TRUE))
  

summary_table1



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

summary_table1



table_flex <- summary_table1 %>%
  flextable() %>%
  set_caption("Table1. Summary of surveyed points in the LUCAS-Core and LUCAS-Topsoil modules in 2018. 'LUCAS_TopSoil_UnchangedLU' (and the subsequent columns, split by forest/shrubland type) represent the number of points where land use remained unchanged between 2009 and 2018.")

save_as_docx(table_flex, path = "./results/summary_table1.docx")





#### Step 2b: Selecting Area of Study ####
## Selecting region (Galicia --> NUTS_2 == ES11)

if (!exists("region_study")){
  region_study <- "ES11" # This region is the one from Rosa's study
  
}else{
  # region_study provided by the user as an argument, and into the environment as a vector
  # it can be a NUTS0, NUTS1, NUTS2 or NUTS3
  region_study <- region_study
}

# by default, only points with unchanged LU (2009-2018) are selected, 
# but the option of using all points can be selected by the user

select_same_LU <- "no"
select_same_LU <- "yes"

if(select_same_LU == "yes"){
  LucasS2018_sameLU_reg <- LucasS2018_sameLU %>%
    filter(if_any(everything(), ~ . %in% region_study)) %>% #View()
    #filter(grepl("^B|^C|^D|^E", LC)) 
    filter(grepl("^C|^D|^E", LC)) # forests, shrublands and grasslands 
} else if(select_same_LU == "no"){
  LucasS2018_sameLU_reg <- LucasS2018 %>%
    filter(if_any(everything(), ~ . %in% region_study)) %>% #View()
    #filter(grepl("^B|^C|^D|^E", LC)) 
    filter(grepl("^C|^D|^E", LC)) # forests, shrublands and grasslands 
} else{
  print("do you want to select only those rows that have the same LU than in 2009?")
}
  
LucasS2018_sameLU_reg

# selection of relevant variables for the study (those of the original study)
variables_lst <-c("pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N", "K")

LucasS2018_sameLU_reg_vrbls <- LucasS2018_sameLU_reg %>% 
  select(all_of(variables_lst)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as_tibble()

LucasS2018_sameLU_reg_vrbls



### step 3: PCA ####

## Handling missing values
## Option 1: remove rows with NAs
pca_result <- prcomp(na.omit(LucasS2018_sameLU_reg_vrbls),   # to remove rows with NAs (remain 29 rows out of 102) --> probably it's too many
                     center = TRUE, scale. = TRUE)
summary(pca_result)


## Option 2: remove columns with NAs (CaCO3 and P)
pca_result2 <- prcomp(select(LucasS2018_sameLU_reg_vrbls, -c(CaCO3, P)),   # to remove P and CaCO3
                      center = TRUE, scale. = TRUE)
summary(pca_result2)

## Option 3:  Impute the Missing Values
library(missMDA)
#Estimate number of components (optional)
nb <- estim_ncpPCA(LucasS2018_sameLU_reg_vrbls, ncp.max = 6)
#Impute missing values
imputed <- imputePCA(LucasS2018_sameLU_reg_vrbls, ncp = nb$ncp)
# Step 3: Run PCA
pca_result3 <- prcomp(imputed$completeObs, scale. = TRUE, center = TRUE)



## Convert PCA scores into a dataframe
pca_scores <- as_tibble(pca_result$x)
pca_scores2 <- as_tibble(pca_result2$x)
pca_scores3 <- as_tibble(pca_result3$x)


## Plotting
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA Analysis: PC1 vs PC2",
       x = "Principal Component 1",
       y = "Principal Component 2")


# Extract loadings
loadings <- as.data.frame(pca_result$rotation)  # Loadings data
loadings$Variable <- rownames(loadings)  # Add variable names

# Plot only loadings' labels (no scores, no arrows)
pca_plot <- ggplot(loadings, aes(x = PC1, y = PC2, label = Variable)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot of points
  geom_text(size = 3, vjust = 2) +  # Show labels only
  geom_hline(yintercept = 0) +  # Horizontal line
  geom_vline(xintercept = 0) +  # Vertical line
  labs(title = paste0("Principal Component Analysis - PC1 vs PC2. Region: ", paste(region_study, collapse = ", ")))

pca_plot

## Which variable is most strongly associated with each PC.
top_vars <- apply(abs(select(loadings, -Variable)), 2, function(x) names(sort(x, decreasing = TRUE)))
top_vars

top_1var <- apply(abs(select(loadings, -Variable)), 2, function(x) names(which.max(x)))
top_1var



## Explained variance
explained_var <- summary(pca_result)$importance
explained_df <- as.data.frame(t(explained_var))
explained_df


## saving plots
ggsave(paste0("./results/pca_plot_", paste(region_study, collapse = "_"), ".png"), plot = pca_plot, width = 8, height = 6, dpi = 300)

## saving results
wb <- createWorkbook()

addWorksheet(wb, "PCA_scores")
writeData(wb, "PCA_scores", pca_scores)

addWorksheet(wb, "PCA_loadings")
writeData(wb, "PCA_loadings", loadings)

addWorksheet(wb, "PCA_variance")
writeData(wb, "PCA_variance", explained_df)

saveWorkbook(wb, paste0("./results/pca_summary_", region_study, ".xlsx"), overwrite = TRUE)



### step 6: ANOVA ####

## standardising LC classes and selecting only forests, shrublands and grasslands
LucasS2018_sameLU_reg_LC <- LucasS2018_sameLU_reg %>%
  select(all_of(variables_lst), LC) %>%
  filter(!str_starts(LC, "B")) %>%      # "C10" "C22" "C32" "C33" "D10" "D20" "E20"
  mutate(LC_grouped = case_when(
    LC == "C10" ~ "Broadleaves",     
    LC == "C21" ~ "Coniferous",     
    LC == "C22" ~ "Coniferous",     
    LC == "C23" ~ "Coniferous",     
    LC == "C31" ~ "Coniferous",     
    LC == "C32" ~ "Coniferous",     
    LC == "C33" ~ "Broadleaves",     
    LC == "D10" ~ "Shrubland",     
    LC == "D20" ~ "Shrubland",     
    LC == "E10" ~ "Grassland",     
    LC == "E20" ~ "Grassland",     
    LC == "E30" ~ "Grassland",     
    TRUE ~ LC))  %>%                    # Keep other values unchanged
  select(-LC) %>% 
  mutate_at(variables_lst, as.numeric) 


## boxplots
bxplt_variable_LC <- LucasS2018_sameLU_reg_LC %>% 
  pivot_longer(cols = all_of(variables_lst), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = LC_grouped, y = value, fill = LC_grouped)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # Facet by variable, allowing different y scales
  labs(title = "", y = "Value", x = "LC")
bxplt_variable_LC

## saving boxplots
ggsave(paste0("./results/bxplt_variable_LC_", paste(region_study, collapse = "_"), ".png"), plot = bxplt_variable_LC, width = 12, height = 6, dpi = 300)



## Histograms
histog_variable_LC <- LucasS2018_sameLU_reg_LC %>% 
  pivot_longer(cols = all_of(variables_lst), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = LC_grouped)) +
  geom_histogram(position = "dodge", bins = 5, alpha = 0.7) +  # Adjust the number of bins
  facet_wrap(~ variable + LC_grouped, scales = "free_x") +  # Facet by variable, allowing different y scales
  labs(title = "", y = "Value", x = "LC")

histog_variable_LC

## saving boxplots
ggsave(paste0("./results/histograms_variable_LC_", paste(region_study, collapse = "_"), ".png"), plot = histog_variable_LC, width = 12, height = 12, dpi = 300)


# Variables are not normally distributed, in general.
## Quick check of variances
LucasS2018_sameLU_reg_LC %>%
  group_by(LC_grouped) %>%  
  summarise_at(variables_lst, var, na.rm = TRUE)

# Rule of thumb: Largest variance should not be more than 4 times the smallest variance.
# If ratio ≤ 4, variances are similar enough for ANOVA.
LucasS2018_sameLU_reg_LC %>%
  group_by(LC_grouped) %>% 
  summarise_at(variables_lst, var, na.rm = TRUE) %>%
  summarise_at(variables_lst, ~ max(., na.rm = TRUE) / min(., na.rm = TRUE)) %>%
  round(digits = 1) %>%
  data.frame()

#   pH_CaCl2  pH_H2O  EC     OC  CaCO3   P    N    K
#      2.6    1.5     6.9    2     6.5  14   1.3  15.5


### Normality test for residuals (Shapiro-Wilk)
#normality_results <- lapply(variables_lst, 
#                            function(var) {
#                              model <- aov(as.formula(paste(var, "~ LC_grouped")), data = LucasS2018_sameLU_reg_LC)
#                              shapiro.test(residuals(model))
#                            })
#
### Homogeneity of variances (Levene's test)
#levene_results <- lapply(variables_lst, 
#                         function(var) {
#                           car::leveneTest(as.formula(paste(var, "~ LC_grouped")), data = LucasS2018_sameLU_reg_LC)
#                         })
#
#list(normality_results = normality_results, levene_results = levene_results)


## ANOVA
anova_results <- LucasS2018_sameLU_reg_LC %>%
  pivot_longer(cols = variables_lst, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(ANOVA = list(aov(Value ~ LC_grouped))) %>%
  mutate(TidyResults = map(ANOVA, broom::tidy)) %>% 
  unnest(TidyResults) %>%
  filter(term == "LC_grouped")  # Keep only the main effect

anova_results


## Kruskal-Wallis (non-parametric)
# Run Kruskal–Wallis test for each variable (excluding 'LandUse')
kruskal_results <- LucasS2018_sameLU_reg_LC %>%
  select(-LC_grouped) %>%
  select(where(is.numeric)) %>%
  map_dfr(~ broom::tidy(kruskal.test(.x ~ LucasS2018_sameLU_reg_LC$LC_grouped)), .id = "Soil_Variable")

kruskal_results



## Plotting means 

# Function to compute ANOVA, Tukey test, means & confidence intervals
get_tukey_results <- function(var) {
  formula <- as.formula(paste(var, "~ LC_grouped"))  # Create formula dynamically
  anova_result <- aov(formula, data = LucasS2018_sameLU_reg_LC)
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract mean and CI
  summary_stats <- LucasS2018_sameLU_reg_LC %>%
    group_by(LC_grouped) %>%
    summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      #ci_lower = mean(.data[[var]], na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
      #ci_upper = mean(.data[[var]], na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(.data[[var]], na.rm = TRUE) / sqrt(n())
      ci_lower = mean(.data[[var]], na.rm = TRUE) - qt(0.95, df = n() - 1) * sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
      ci_upper = mean(.data[[var]], na.rm = TRUE) + qt(0.95, df = n() - 1) * sd(.data[[var]], na.rm = TRUE) / sqrt(n())
    ) %>%
    mutate(variable = var,
           Tukey = multcompView::multcompLetters4(anova_result, tukey_result)$LC_grouped$Letters[order(names(multcompView::multcompLetters4(anova_result, tukey_result)$LC_grouped$Letters))]
           )
  
  return(summary_stats)
}

# Apply function to all numeric variables
tukey_results <- bind_rows(lapply(variables_lst, get_tukey_results))

## plots 1 (means, CI and Tukey letters)
anova_means_tukey <- ggplot(tukey_results, aes(x = LC_grouped, y = mean, group = variable)) +
  #geom_point(size = 3, col = "red") +  # Plot mean points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Add CI bars
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "red"
               , aes(color = "Mean")) +  # Mean points
  geom_text(aes(label = Tukey, y = ci_upper + (ci_upper * 0.155)), size = 5) +  # Tukey letters above CI
  facet_wrap(~ variable, scales = "free_y") +  # One plot per variable
  scale_color_manual(name = "", values = c("Mean" = "red")) +
  #theme_minimal() +
  labs(title = "ANOVA Results: Means with 95% CI & Tukey test",
       x = "LC",
       y = "Mean Value")

anova_means_tukey



## saving results

ggsave(paste0("./results/anova_means_tukey_", paste(region_study, collapse = "_"), ".png"), plot = anova_means_tukey, width = 12, height = 6, dpi = 300)


wb <- createWorkbook()

addWorksheet(wb, "ANOVA_results")
writeData(wb, "ANOVA_results", select(anova_results, -ANOVA))

addWorksheet(wb, "Kruskal-Wallis_results")
writeData(wb, "Kruskal-Wallis_results", kruskal_results)

saveWorkbook(wb, paste0("./results/ANOVA_summary_", region_study, ".xlsx"), overwrite = TRUE)



