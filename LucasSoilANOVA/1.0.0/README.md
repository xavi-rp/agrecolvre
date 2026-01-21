# LUCAS-Soil Analysis: PCA

## Description
Within the SUS-SOIL and AGROSERV, this service (AGRECOLVRE) lays the foundation for the Soil Health VRE within the LifeWatch ERIC infrastructure, tailored to support research in agroecology and soil carbon dynamics, among others. The VRE will provide an integrated, user-oriented digital workspace for researchers and policy-makers to access, harmonise, visualise and analyse soil-related datasets, such as LUCAS and SUS-SOIL. 
To develop, implement and validate the VRE’s capabilities, a case study on the carbon storage potential of soils across different land use types (forestlands, shrublands and grasslands) will be conducted. This study will use the available EU-wide LUCAS datasets to test data integration, workflow design, and numerical and visual outputs. 
The LUCAS-Soil ANOVA wrapper runs an ANOVA on the provided soil dataset (see the README for dataset structure requirements).

## Input
A clean soil dataset (csv), e.g. the one generated in the LucasSoilProcessing wrapper ('LucasS2018_sameLU.csv'). 
The dataset to be analysed has to have a format similar to the LUCAS-soil datasets, with at least: 
i) one column for region (NUTS0, 1, 2, 3);
ii) several columns for soil attributes;
iii) one colum called 'LC' with the Land cover / Land use, following LUCAS codification (BXX, croplands;
    CXX, woodland; DXX, shrubland; EXX, grassland).


## Output
One tables and 3 figures that summarise the results of the ANOVA: bxplt_variable_LC.png; histograms_variable_LC.png; ANOVA_summary.xlsx; anova_boxplots_tukey.png



