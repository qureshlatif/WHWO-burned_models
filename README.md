# "scripts" repository for WHWO burned forest HSI models manuscript

### Basic function of each model script in this repository ###
# Data and tabulation compilation (not necessary to run if working with R workspace) # 
# 0000-CC_compile_UTMs.R - Retrieve coordinates for generating spatial layers at Canyon Creek
# 000-Data_compilation_BP_prelim.R - Initial covariate compilation for Barry Point
# 000-Data_compilation_CC_prelim.R - Initial covariate compilation for Canyon Creek
# 000-Data_compilation_TB_prelim.R - Initial covariate compilation for Toolbox
# 00-Data_compilation.R - Final data compilation

# Analysis implementation and compilation of results #
# 01-Descriptive_stats.R - Generats summary statistics (Table 3 in manuscript).
# 01-Maxent_fit.R - Fits Maxent models.
# 01-WLR_fit.R - Fits weighted logistic regression models
# 01-WLR_fit_combineTB&CC.R - Fits pooled weighted logistic regression after verifying predictive performance between Toolbox and Canyon Creek.
# 02_WLR_parameters.R - Tabulates parameter estimates for weighted logistic regression models.
# 02-HSI_relations.R - Plots habitat relationships represented in HSI models
# 02-Model_evaluation.R - Implements cross-validation to generate AUCs (Table 7 in manuscript).
# 02-Thresholds.R - Finds thresholds that maximize sensitivity + specificity (archived - not used in manuscript).
# 03-HSI_density_relation.R - Generates plots relating HSIs with hatched-nest densities.
# 03-HSI_density_relation_GIS_manual.R - Generates plots relating HSIs with hatched-nest densities for FIRE-BIRD manual (not applicable to manuscript).
# 04-Thresholds2.R - Generates tables relating HSI classes with hatched-nest densities.
# 04-Thresholds2_GISmanual.R - Generates tables relating HSI classes with hatched-nest densities for FIRE-BIRD manual (not applicable to manuscript).
# Maxent_HSI_functions.R - Functions that apply fitted Maxent models to data with required covariates (file can be sourced).
