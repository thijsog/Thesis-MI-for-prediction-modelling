### Create a heatmap of the difference in MSE of each model compared to the complete dataset

create_heatmap <- function(calc_outcomes,graph_title){
  
  mse <- list()
  mcsd <- list()
  mse_comp <- list()
  
  impmethod <- list() # we could change study to cond
  missingtype <- list()
  
  # add the values to the lists
  for(i in 1 : length(calc_outcomes)){
    mse[[i]] <- c(mean(unlist(calc_outcomes[[i]][["si"]][["MSE"]])), mean(unlist(calc_outcomes[[i]][["mi"]][["MSE"]])), mean(unlist(calc_outcomes[[i]][["ld"]][["MSE"]])))
    mse_comp[[i]] <- mean(unlist(calc_outcomes[[i]][["comp"]][["MSE"]]))
    impmethod[[i]] <- calc_outcomes[[i]][["conds"]][["impmethod"]]
    missingtype[[i]] <- calc_outcomes[[i]][["conds"]][["missingtype"]]
    
  }
  
  
  # aggregrate the outcome selection
  mse <- do.call("rbind", mse)
  mse_comp <- do.call("rbind", mse_comp)
  impmethod <- do.call("rbind", impmethod)
  missingtype <- do.call("rbind", missingtype)
  
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
                    condition = paste0(impmethod,"_","SI"),
                    mse = mse[,1],
                    MSE_bias = bias_si,
                    prb_MSE = prb_si)
  df1$model <- "SI"
  
  df2 <- data.frame(missingtype = missingtype,
                    condition = paste0(impmethod,"_","MI"),
                    mse = mse[,2],
                    MSE_bias = bias_mi,
                    prb_MSE = prb_mi)
  df2$model <- "MI"
  
  df3 <- data.frame(missingtype = missingtype,
                    condition = paste0(impmethod,"_","LD"),
                    mse = mse[,3],
                    MSE_bias = bias_ld,
                    prb_MSE = prb_ld)
  df3$model <- "LD"
  
  df4 <- data.frame(missingtype = missingtype,
                    condition = paste0(impmethod,"_","Complete"),
                    mse = mse_comp,
                    MSE_bias = bias_comp,
                    prb_MSE = bias_comp)
  df4$model <- "Complete"
  
  # bind the df's 
  df <- rbind(df1, df2)
  df <- rbind(df,df3)
  df <- rbind(df,df4)
  
  ## createa heatmap
  ggplot(df, aes(missingtype, condition)) + 
    geom_tile(aes(fill= MSE_bias), colour = "black") +
    geom_text(aes(label = round(MSE_bias,3))) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title= graph_title, x= "Mechanism of missingness", y = "Condition") + 
    theme_minimal()

  
}

create_heatmap(calc_outcomes = res,graph_title = "MSE Bias Heatmap for R2 of 0.1")


