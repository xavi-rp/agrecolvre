
#########################################################################
###                                                                   ###
###                            AGRECOLVRE                             ###
###                                                                   ###
### Improvement of the soil health through agroecological management  ###
###                                                                   ###
###                              ANOVA                                ###
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
  make_option(c("-s", "--region_study"), type = "character",
              default = "ES11",
              help = "Region for focused study. It can be NUTS0/1/2/3. [default: %default]", 
              metavar = "character"),
  
  make_option(c("-v", "--variables_lst"), type = "character",
              default = "pH_CaCl2,pH_H2O,EC,OC,CaCO3,P,N,K",
              help = "Comma-separated list of soil variables studied [default: %default]", 
              metavar = "character")
  )

opt <- parse_args(OptionParser(option_list = option_list))

region_study <- strsplit(opt$region_study, ",")[[1]]
variables_lst <- strsplit(opt$variables_lst, ",")[[1]]



### step 1: Read in (LUCAS) soil data ####

LucasS2018_sameLU <- read.csv(paste0(Lucas_dir, "/lucas_data.csv"), header = TRUE)




### step 2: ANOVA ####
## For each soil parameter (e.g. OC), see if there are differences between LU (e.g. broadleaves, etc)


## 'LucasS2018_sameLU'  is the dataset to be analysed. It can come from the 1st part of AGRECOLVRE
## or ingested from own data, as long as the format are the same than for LUCAS: 
## i) one column for region (NUTS0, 1, 2, 3)
## ii) several columns for soil attributes
## iii) one colum called 'LC' with the Land cover / Land use, following LUCAS codification (BXX, croplands;
##      CXX, woodland; DXX, shrubland; EXX, grassland)


#### Step 2a: Selecting Area of Study ####
## Selecting region (Galicia --> NUTS_2 == ES11)
## And selecting forests, shrublands and grasslands for the analyses

LucasS2018_sameLU_reg <- LucasS2018_sameLU %>%
  filter(if_any(everything(), ~ . %in% region_study)) %>% #View()
  #filter(grepl("^B|^C|^D|^E", LC)) 
  filter(grepl("^C|^D|^E", LC)) # forests, shrublands and grasslands 

#LucasS2018_sameLU_reg


# selection of relevant variables for the study (those of the original study by default)
LucasS2018_sameLU_reg_vrbls <- LucasS2018_sameLU_reg %>% 
  select(all_of(variables_lst)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as_tibble()

#LucasS2018_sameLU_reg_vrbls


#### Step 2b: Standardising and selecting LC classes ####

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



#### Step 2c: Boxplots ####

bxplt_variable_LC <- LucasS2018_sameLU_reg_LC %>% 
  pivot_longer(cols = all_of(variables_lst), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = LC_grouped, y = value, fill = LC_grouped)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # Facet by variable, allowing different y scales
  labs(title = paste0("Region(s): ", paste(region_study, collapse = ", ")), y = "Value", x = "LC")
#bxplt_variable_LC

## saving boxplots
ggsave(paste0(outputs_dir, "/bxplt_variable_LC", ".png"), plot = bxplt_variable_LC, width = 12, height = 6, dpi = 300)



#### Step 2d: Histograms ####

histog_variable_LC <- LucasS2018_sameLU_reg_LC %>% 
  pivot_longer(cols = all_of(variables_lst), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = LC_grouped)) +
  geom_histogram(position = "dodge", bins = 5, alpha = 0.7) +  # Adjust the number of bins
  facet_wrap(~ variable + LC_grouped, scales = "free_x") +  # Facet by variable, allowing different y scales
  labs(title = paste0("Region(s): ", paste(region_study, collapse = ", ")), y = "Value", x = "LC")

#histog_variable_LC

## saving boxplots
ggsave(paste0(outputs_dir, "/histograms_variable_LC", ".png"), plot = histog_variable_LC, width = 12, height = 12, dpi = 300)


#### Step 2e: Checks ####

# Variables are not normally distributed, in general.
## Quick check of variances
LucasS2018_sameLU_reg_LC_var <- LucasS2018_sameLU_reg_LC %>%
  group_by(LC_grouped) %>%  
  summarise_at(variables_lst, var, na.rm = TRUE)

# Rule of thumb: Largest variance should not be more than 4 times the smallest variance.
# If ratio ≤ 4, variances are similar enough for ANOVA.
LucasS2018_sameLU_reg_LC_ratio <- LucasS2018_sameLU_reg_LC %>%
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


#### Step 2f: ANOVA ####

##### ANOVA ####
anova_results <- LucasS2018_sameLU_reg_LC %>%
  pivot_longer(cols = variables_lst, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(ANOVA = list(aov(Value ~ LC_grouped))) %>%
  mutate(TidyResults = map(ANOVA, broom::tidy)) %>% 
  unnest(TidyResults) %>%
  filter(term == "LC_grouped")  # Keep only the main effect

#anova_results


##### Kruskal-Wallis (non-parametric) ####
# Run Kruskal–Wallis test for each variable (excluding 'LandUse')
kruskal_results <- LucasS2018_sameLU_reg_LC %>%
  select(-LC_grouped) %>%
  select(where(is.numeric)) %>%
  map_dfr(~ broom::tidy(kruskal.test(.x ~ LucasS2018_sameLU_reg_LC$LC_grouped)), .id = "Soil_Variable")

#kruskal_results



##### Plotting means ####

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
      max = max(.data[[var]], na.rm = TRUE),
      ci_lower = mean(.data[[var]], na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
      ci_upper = mean(.data[[var]], na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(.data[[var]], na.rm = TRUE) / sqrt(n())
    ) %>%
    mutate(variable = var,
           Tukey = multcompView::multcompLetters4(anova_result, tukey_result)$LC_grouped$Letters[order(names(multcompView::multcompLetters4(anova_result, tukey_result)$LC_grouped$Letters))]
    )
  
  return(summary_stats)
}

# Apply function to all numeric variables
tukey_results <- bind_rows(lapply(variables_lst, get_tukey_results))


##### Plots 2: boxplots and Tukey letters ####
anova_boxplots_tukey <- LucasS2018_sameLU_reg_LC %>% 
  pivot_longer(cols = variables_lst, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = LC_grouped, y = value)) +
  geom_boxplot(outlier.size = 1) +
  #geom_jitter(width = 0.2, size = .3, alpha = .5) + # show individual points
  geom_text(data = tukey_results, 
            #aes(x = LC_grouped, y = ci_upper + (ci_upper * 0.5), label = Tukey), 
            aes(x = LC_grouped, y = (max + (max * 0.1)), label = Tukey), 
            size = 5, col = "red", 
            fontface = "bold" #,
            #hjust = - 0.05
  ) + # Tukey letters
  facet_wrap(~ variable, scales = "free_y") +  # Facet by variable, allowing different y scales
  labs(title = paste0("ANOVA Results: Boxplots & Tukey HSD Letters. Region(s): ", paste(region_study, collapse = ", ")), y = "Value", x = "LC")

#anova_boxplots_tukey


#### Step 2g: saving results ####


ggsave(paste0(outputs_dir, "/anova_boxplots_tukey", ".png"), plot = anova_boxplots_tukey, width = 12, height = 6, dpi = 300)


wb <- createWorkbook()

addWorksheet(wb, "ANOVA_results")
writeData(wb, "ANOVA_results", select(anova_results, -ANOVA))

addWorksheet(wb, "Kruskal-Wallis_results")
writeData(wb, "Kruskal-Wallis_results", kruskal_results)

saveWorkbook(wb, paste0(outputs_dir, "/ANOVA_summary", ".xlsx"), overwrite = TRUE)







