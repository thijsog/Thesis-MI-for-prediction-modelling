# ---- produce the plots and graphs for the study: The influence of Multiple imputation on prediction modelling. ---- #

## load in the packages ##
library(tidyverse)
library(MLmetrics)
library(parallel)
library(rlecuyer)
library(mvtnorm)
library(mice)
library(mitools)
library(dplyr)
library(magrittr)

## load in the functions
source("Forrest_plot_results_function.R")
source("table_results_intervals.R")
source("table_results_performance_measurements.R")
source("create_heatmap_MSE.R")

### -- For R2 = 0.1 -- ###
## !! first run performance analysis and pool the results for the R2:0.1 condition !! ##

#### Coverage results of the confidence and prediction intervals ####

## Create a Forrest plot of the confidence intervals (CI)
create_forrest_plot_all(calc_outcomes = res,measurement = "cic", graph_title = "CI coverage for R2 of 0.1",lwr = 0.25, 1)

## Create a Forrest plot of the prediction intervals (PI)
create_forrest_plot_all(calc_outcomes = res,measurement = "pic", graph_title = "PI coverage for R2 of 0.1", lwr = 0.75, upr = 1)


#### Heatmap of the MSE bias ####

# Create Heatmap of the MSE bias (bias in relation to the complete dataset MSE)
create_heatmap(calc_outcomes = res,graph_title = "MSE Heatmap for R2 of 0.1")


#### Create the tables for the different measurements ####

## Table results CI
CI_table <- create_table_results_intervals(calc_outcomes = res,interval_type = "ci" )

CI_table <- CI_table[
  order( CI_table[,1], CI_table[,2] ),
]

write.csv(CI_table,"~/folder Thesis MI pred/Results file/R2 of 0.1/CI_table_R2_0.1.csv", row.names = FALSE)

## Table results PI
PI_table <- create_table_results_intervals(calc_outcomes = res,interval_type = "pi" )


PI_table <- PI_table[
  order( PI_table[,1], PI_table[,2] ),
]

write.csv(PI_table,"~/folder Thesis MI pred/Results file/R2 of 0.1/PI_table_R2_0.1.csv", row.names = FALSE)


## Table results MSE, BIAS and MCSD measures 
mse_and_bias_table <-  create_table_results_performance_measurements(res)

mse_and_bias_table <- mse_and_bias_table[
  order( mse_and_bias_table[,1], mse_and_bias_table[,2] ),
]

write.csv(mse_and_bias_table,"~/folder Thesis MI pred/Results file/R2 of 0.1/MSE_and_bias_table_R2_0.1.csv", row.names = FALSE)


### -- For R2 = 0.3 -- ###

#### Coverage results of the confidence and prediction intervals ####

## Create a Forrest plot of the confidence intervals (CI)
create_forrest_plot_all(calc_outcomes = res,measurement = "cic", graph_title = "CI coverage for R2 of 0.3",lwr = 0.25, upr = 1)

## Create a Forrest plot of the prediction intervals (PI)
create_forrest_plot_all(calc_outcomes = res,measurement = "pic", graph_title = "PI coverage for R2 of 0.3",lwr = 0.75, upr = 1)


#### Heatmap of the MSE bias ####

# Create Heatmap of the MSE bias (bias in relation to the complete dataset MSE)
create_heatmap(calc_outcomes = res,graph_title = "MSE Bias Heatmap for R2 of 0.3")


#### Create the tables for the different measurements ####

##  Table results CI
CI_table <- create_table_results_intervals(calc_outcomes = res,interval_type = "ci" )

CI_table <- CI_table[
  order( CI_table[,1], CI_table[,2] ),
]

write.csv(CI_table,"~/folder Thesis MI pred/Results file/R2 of 0.3/CI_table_R2_0.3.csv", row.names = FALSE)

## table results PI
PI_table <- create_table_results_intervals(calc_outcomes = res,interval_type = "pi" )


PI_table <- PI_table[
  order( PI_table[,1], PI_table[,2] ),
]

write.csv(PI_table,"~/folder Thesis MI pred/Results file/R2 of 0.3/PI_table_R2_0.3.csv", row.names = FALSE)

## Table results MSE, BIAS and MCSD measures 
mse_and_bias_table <- create_table_results_performance_measurements(res)

mse_and_bias_table <- mse_and_bias_table[
  order( mse_and_bias_table[,1], mse_and_bias_table[,2] ),
]

write.csv(mse_and_bias_table,"~/folder Thesis MI pred/Results file/R2 of 0.3/MSE_and_bias_table_R2_0.3.csv", row.names = FALSE)
