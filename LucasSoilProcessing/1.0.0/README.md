# LUCAS-Soil Analysis

## Description
Within the SUS-SOIL and AGROSERV, the service AGRECOLVRE lays the foundation for the Soil Health VRE within the LifeWatch ERIC infrastructure, tailored to support research in agroecology and soil carbon dynamics, among others. The VRE will provide an integrated, user-oriented digital workspace for researchers and policy-makers to access, harmonise, visualise and analyse soil-related datasets, such as LUCAS and SUS-SOIL. 
To develop, implement and validate the VRE’s capabilities, a case study on the carbon storage potential of soils across different land use types (forestlands, shrublands and grasslands) will be conducted. This study will use the available EU-wide LUCAS datasets to test data integration, workflow design, and numerical and visual outputs. The LUCAS-Soil Processing wrapper cleans the LUCAS soil datasets for 2009 and 2018, and selects the points that mantains the same LU across this period.

## Data
The LUCAS datasets are publicly available, free of charge, from the European Commission’s Directorates-General Eurostat (ESTAT) and the Joint Research Centre (JRC). AGRECOLVRE users should download them as described below:

 - LUCAS Topsoil 2009/2012, through request form: https://esdac.jrc.ec.europa.eu/content/lucas-2009-topsoil-data
 - LUCAS Topsoil 2015, through request form: https://esdac.jrc.ec.europa.eu/content/lucas2015-topsoil-data
 - LUCAS Topsoil 2018, through request form: https://esdac.jrc.ec.europa.eu/content/lucas-2018-topsoil-data
 
 - LUCAS Core 2009 (direct download): https://ec.europa.eu/eurostat/documents/205002/208938/EU_2009_20200213.CSV
 - LUCAS Core 2015 (direct download): https://ec.europa.eu/eurostat/cache/lucas/EU_2015_20200225.CSV
 - LUCAS Core 2018 (direct download): https://ec.europa.eu/eurostat/cache/lucas/EU_2018_20200213.CSV
 

## Input
LUCAS-soil and LUCAS-core datasets for several years: LUCAS_TOPSOIL_v1.xlsx; LUCAS-SOIL-2018.csv; EU_2009_20200213.CSV; EU_2018_20200213.CSV

## Output
(1) A table that summarises the number of LUCAS points per each region and LU: summary_table1.docx. (2) A csv file with the LUCAS-Soil-2018 points that have the same LU than in 2009, which can be used for the subsequent analysis within the AGRECOLVRE workflow: LucasS2018_sameLU.csv.

## More information on LUCAS survey and data:
 - https://ec.europa.eu/eurostat/web/lucas
 - https://esdac.jrc.ec.europa.eu/projects/lucas

