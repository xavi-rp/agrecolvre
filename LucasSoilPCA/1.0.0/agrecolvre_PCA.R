
#########################################################################
###                                                                   ###
###                            AGRECOLVRE                             ###
###                                                                   ###
### Improvement of the soil health through agroecological management  ###
###                                                                   ###
###             Principal Components Analysis (PCA)                   ###
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




### step 2: PCA ####

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



#### Step 2b: PCA ####

## Handling missing values
## Option 1: remove rows with NAs
pca_result <- prcomp(na.omit(LucasS2018_sameLU_reg_vrbls),   # to remove rows with NAs 
                     center = TRUE, scale. = TRUE)
#summary(pca_result)


### Option 2: remove columns with NAs (CaCO3 and P)
#pca_result2 <- prcomp(select(LucasS2018_sameLU_reg_vrbls, -c(CaCO3, P)),   # to remove P and CaCO3
#                      center = TRUE, scale. = TRUE)
##summary(pca_result2)
#
### Option 3:  Impute the Missing Values
##Estimate number of components (optional)
#nb <- estim_ncpPCA(LucasS2018_sameLU_reg_vrbls, ncp.max = 6)
##Impute missing values
#imputed <- imputePCA(LucasS2018_sameLU_reg_vrbls, ncp = nb$ncp)
## Step 3: Run PCA
#pca_result3 <- prcomp(imputed$completeObs, scale. = TRUE, center = TRUE)



## Convert PCA scores into a dataframe
pca_scores <- as_tibble(pca_result$x)
#pca_scores2 <- as_tibble(pca_result2$x)
#pca_scores3 <- as_tibble(pca_result3$x)


## Plotting
#plot_this <- "yes"
if (exists("plot_this")){
  ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(title = "PCA Analysis: PC1 vs PC2",
         x = "Principal Component 1",
         y = "Principal Component 2")
}
  

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

#pca_plot

## Which variable is most strongly associated with each PC.
top_vars <- apply(abs(select(loadings, -Variable)), 2, function(x) names(sort(x, decreasing = TRUE)))
#top_vars

top_1var <- apply(abs(select(loadings, -Variable)), 2, function(x) names(which.max(x)))
#top_1var



## Explained variance
explained_var <- summary(pca_result)$importance
explained_df <- as.data.frame(t(explained_var))
#explained_df


## saving plots
ggsave(paste0(outputs_dir, "/pca_plot", ".png"), plot = pca_plot, width = 8, height = 6, dpi = 300)


## saving results
wb <- createWorkbook()

addWorksheet(wb, "PCA_scores")
writeData(wb, "PCA_scores", pca_scores)

addWorksheet(wb, "PCA_loadings")
writeData(wb, "PCA_loadings", loadings)

addWorksheet(wb, "PCA_variance")
writeData(wb, "PCA_variance", explained_df)

saveWorkbook(wb, paste0(outputs_dir, "/pca_summary", ".xlsx"), overwrite = TRUE)



