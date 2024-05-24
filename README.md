# Replication Data and Code for "Modeling how and why aquatic vegetation removal can free rural households from poverty-disease traps" by Molly J Doruska, Christopher B Barrett, and Jason R Rohr

This repository contains all simulation code, data files produced from the simulations, and all code to replicate the tables and figures in "Modeling how and why aquatic vegetation removal can free rural households from poverty-disease traps." 

# Data Avalability Statement 

The data that support the findings of this study that are simulation based are openly available in this repository at https://github.com/mdoruska/Bioeconomic_Model. Information on licensing can be found in the file LICENSE. 

Additional data that support the findings of this study can be found in the repository Data and Code for "A planetary health innovation for disease, food, and water challenges in Africa" in Zenodo: https://doi.org/10.5281/zenodo.7765059, from the World Bank data repository at www.microdata.worldbank.org reference SEN_2018_EHCVM_v02_M, and from the Agence National de la Statistique et de la Démographie de la République du Sénégal at http://www.ansd.sn/index.php?option=com_ansd&view=titrepublication&id=9&Itemid=287. 

Citations: 

Rohr, J., Sack, A., Haggerty, C., Barrett, C., Doruska, M., De Leo, G., Jouanard, N., Remais, J., Riveau, G., & Sokolow, S. (2023). Data and Code for "A planetary health innovation for disease, food, and water challenges in Africa" [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7765059

WAEMU Commission. Harmonized Survey on Households Living Standards, Senegal 2018-2019. Ref. SEN_2018_EHCVM_v02_M. 2018; dataset downloaded from www.microdata.worldbank.org on September 23, 2022.

ANSD. Bulletin Mensuel des Statistiques Economiques. Agence National de la Statistique et de la Démographie de la République du Sénégal. 2018; published online http://www.ansd.sn/index.php?option=com_ansd&view=titrepublication&id=9&Itemid=287 (accessed May 20, 2020).

# File Descriptions 

Bioeconomic_Simulation_Code.jl is the code used for all the bioeconomic model simulations in the paper. Note these simulations take around 55 hours to run.  

Bioeconomic_Model_Main_Results.do is the code used to create the main results figures: Figures 1, 3, 4, S1, S2, and S3. These figures are the result of stochasitic draws and numerical simulations in Bioeconomic_Simulation_Code.jl. Each run of the code might result in slight differences in the final figures. 

Fertilizer_Infection_Results.do is the code used to create Figure 2. 

Disease_Ecology_Simulation.jl simulates the disease ecology model used in the main simulations. This code creates Figure S4. 

Household_Survey_Summary_Statistics.do is the code used to calculated household summary statistics used to calibrate the bioeconomic model. This code creates Tables S1 and S2. 

Expenditure_Shares.do is the code used to estimate the expenditure shares used to calibrate the bioeconomic model. This code creates Table S3. 

Factor_Cost_Shares.do is the code used to estimate the expenditure shares used to calibrate the bioeconmic model. This code creates Table S4. 

Vegetation_Production_Functino.do is the code used to estimate the vegetation production function. This code creates Table S8. 

All .csv files are produced by Bioecnomic_Simualtion_Code.jl and are used by Bioeconomic_Model_Main_Results.do and Fertilizer_Infection_Results.do to create the core results. 

# Computing Environment and Packages 

All code was created in Julia 1.6.2 and Stata 16. 

The Julia code uses the following packages: NLopt, Plots, LinearAlgebra, Distributions, Random, DifferentialEquations, DelimitedFiles

The Stata code uses the following packages: blindschemes, cleanplots, palettes, schemepack, grc1le, estout

# Replication Instructions 

Step 1: Download the necessary data from the World Bank and Zenodo. 

Step 2: Put all data and code in a folder titled "BioeconomicModelResults". All directories should start from where you find the "BioeconomicModelResults" folder. 

Step 3: Run the file Bieconomic_Simulation_Code.jl. This file produces the simulation results for the paper. It outputs the .csv files in this repository which can be used to directly replicate the paper's figures. 

Step 4: Run the additional .do and .jl files to replicate the papers figures and tables. 

# Contact Information

For more information, reach out to Molly Doruska via email at mjd438[at]cornell[dot]edu. 




