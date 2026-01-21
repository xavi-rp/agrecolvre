# AGRECOLVRE

This Virtual Research Environment (VRE) establishes a user-oriented digital workspace within the LifeWatch ERIC infrastructure (https://my.lifewatch.dev/) to support agroecology and soil carbon research. It enables integrated access, harmonisation, visualisation, and analysis of soil-related datasets, including regional and EU-wide sources such as LUCAS, for researchers and policy-makers. The VRE’s functionality is developed and validated through a case study assessing soil carbon storage potential across major land-use types (forestlands, shrublands, and grasslands), demonstrating data integration, analytical workflows, and numerical and visual outputs.

So far, the VRE has 3 functionalities implemented, tested and validated:

1) The LUCAS-Soil Processing wrapper cleans the LUCAS soil datasets for 2009 and 2018, and selects the points that maintain the same LU across this period.

2) The LUCAS-Soil PCA wrapper runs a Principal Components Analysis (PCA) on the provided soil dataset (CSV). The dataset should have, at least, one column for the region (e.g. NUTS0, 2, or 3), several columns for soil attributes, and one column called 'LC' with the Land Cover/Land Use following the LUCAS codification (BXX, croplands; CXX, woodland; DXX, shrubland; EXX, grassland).

3) The LUCAS-Soil ANOVA wrapper runs an ANOVA on the provided soil dataset (CSV). The dataset should have, at least, one column for the region (e.g. NUTS0, 2, or 3), several columns for soil attributes, and one column called 'LC' with the Land Cover/Land Use following the LUCAS codification (BXX, croplands; CXX, woodland; DXX, shrubland; EXX, grassland).