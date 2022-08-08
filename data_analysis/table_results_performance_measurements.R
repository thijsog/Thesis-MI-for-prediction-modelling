## table for the results of the performance measurements ##
### function to create a table of the MSE, BIAS, PrB, Bias MSE, PrB MSE and MCSD results


create_table_results_performance_measurements <- function(calc_outcomes){
  ## function to create a table for the following performance measurement: MSE, BIAS, PrB, Bias MSE, PrB MSE and MCSD results
  ### calc_outcomes: list containing the outcome values for each condition
  
    # create lists to store the values
    mse <- list()
    bias <- list()
    prb <- list()
    mcsd <- list()
    mse_comp <- list()
    
    impmethod <- list() # we could change study to cond
    missingtype <- list()
    
    # add the values to the lists
    for(i in 1 : length(calc_outcomes)){
      mse[[i]] <- c(mean(unlist(calc_outcomes[[i]][["si"]][["MSE"]])), mean(unlist(calc_outcomes[[i]][["mi"]][["MSE"]])), mean(unlist(calc_outcomes[[i]][["ld"]][["MSE"]])))
      mse_comp[[i]] <- mean(unlist(calc_outcomes[[i]][["comp"]][["MSE"]]))
      bias[[i]] <- c(mean(calc_outcomes[[i]][["si"]][["bias"]]), mean(calc_outcomes[[i]][["mi"]][["bias"]]), mean(calc_outcomes[[i]][["ld"]][["bias"]]),mean(calc_outcomes[[i]][["comp"]][["bias"]]))
      prb[[i]] <- c(mean(calc_outcomes[[i]][["si"]][["prb"]]), mean(calc_outcomes[[i]][["mi"]][["prb"]]), mean(calc_outcomes[[i]][["ld"]][["prb"]]),mean(calc_outcomes[[i]][["comp"]][["prb"]]))
      mcsd[[i]] <- c(mean(calc_outcomes[[i]][["si"]][["mcsd"]]), mean(calc_outcomes[[i]][["mi"]][["mcsd"]]), mean(calc_outcomes[[i]][["ld"]][["mcsd"]]),mean(calc_outcomes[[i]][["comp"]][["mcsd"]]))
      impmethod[[i]] <- calc_outcomes[[i]][["conds"]][["impmethod"]]
      missingtype[[i]] <- calc_outcomes[[i]][["conds"]][["missingtype"]]
    }
    
    
    # aggregrate the outcome selection
    mse <- do.call("rbind", mse)
    mse_comp <- do.call("rbind", mse_comp)
    bias <- do.call("rbind", bias)
    prb <- do.call("rbind", prb)
    impmethod <- do.call("rbind", impmethod)
    missingtype <- do.call("rbind", missingtype)
    mcsd <- do.call("rbind", mcsd)
    
    
    # calculate the prb and bias for the MSE
    bias_si <- mse[,1] - mse_comp
    prb_si <- (bias_si/mse_comp)*100
    
    bias_mi <- mse[,2] - mse_comp
    prb_mi <- (bias_mi/mse_comp)*100
    
    bias_ld <- mse[,3] - mse_comp
    prb_ld <- (bias_ld/mse_comp)*100
    
    bias_comp <- mse_comp - mse_comp
    
    # create df of the aggregrates 
    df1 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mse = mse[,1],
                      bias = bias[,1],
                      prb = prb[,1],
                      bias_MSE = bias_si,
                      prb_MSE = prb_si,
                      mcsd = mcsd[,1])
    df1$model <- "SI"
    
    df2 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mse = mse[,2],
                      bias = bias[,2],
                      prb = prb[,2],
                      bias_MSE = bias_mi,
                      prb_MSE = prb_mi,
                      mcsd = mcsd[,2])
    df2$model <- "MI"
    
    df3 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mse = mse[,3],
                      bias = bias[,3],
                      prb = prb[,3],
                      bias_MSE = bias_ld,
                      prb_MSE = prb_ld,
                      mcsd = mcsd[,3])
    df3$model <- "LD"
    
    df4 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mse = mse_comp,
                      bias = bias[,4],
                      prb = prb[,4],
                      bias_MSE = bias_comp,
                      prb_MSE = bias_comp,
                      mcsd = mcsd[,4])
    df4$model <- "Complete"
    
    # bind the df's 
    df <- rbind(df1, df2)
    df <- rbind(df,df3)
    df <- rbind(df,df4)
  
}

table_test <- create_table_results_performance_measurements(res)


