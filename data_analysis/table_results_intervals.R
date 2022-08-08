create_table_results_intervals <- function(calc_outcomes, interval_type){
  ## function to create a table for the prediction interval results
  ### calc_outcomes: list containing the outcome values for each condition
  ### interval type: select the type of interval for which a table has to be created; confidence or prediction interval
  
  if(interval_type == "ci"){
    measurement <- "cic"
    
  
  # create lists to store the values
  outcomes <- list()
  upper <- list()
  lower <- list()
  width <- list()
  impmethod <- list() # we could change study to cond
  missingtype <- list()
  
  # add the values to the lists
  for(i in 1 : length(calc_outcomes)){
    outcomes[[i]] <- c(mean(calc_outcomes[[i]][["si"]][[measurement]]), mean(calc_outcomes[[i]][["mi"]][[measurement]]), mean(calc_outcomes[[i]][["ld"]][[measurement]]),mean(calc_outcomes[[i]][["comp"]][[measurement]]))
    upper[[i]] <- c(max(calc_outcomes[[i]][["si"]][[measurement]]),max(calc_outcomes[[i]][["mi"]][[measurement]]), max(calc_outcomes[[i]][["ld"]][[measurement]]),max(calc_outcomes[[i]][["comp"]][[measurement]]))
    lower[[i]] <- c(min(calc_outcomes[[i]][["si"]][[measurement]]), min(calc_outcomes[[i]][["mi"]][[measurement]]), min(calc_outcomes[[i]][["ld"]][[measurement]]),min(calc_outcomes[[i]][["comp"]][[measurement]]))
    width[[i]] <- c(mean(calc_outcomes[[i]][["si"]][["ciw"]]), mean(calc_outcomes[[i]][["mi"]][["ciw"]]), mean(calc_outcomes[[i]][["ld"]][["ciw"]]),mean(calc_outcomes[[i]][["comp"]][["ciw"]]))
    impmethod[[i]] <- calc_outcomes[[i]][["conds"]][["impmethod"]]
    missingtype[[i]] <- calc_outcomes[[i]][["conds"]][["missingtype"]]
  }
  
  # aggregrate the outcome selection
  outcomes <- do.call("rbind", outcomes)
  upper <- do.call("rbind", upper)
  lower <- do.call("rbind", lower)
  width <- do.call("rbind", width)
  impmethod <- do.call("rbind", impmethod)
  missingtype <- do.call("rbind", missingtype)
  
  
  # create df of the aggregrates 
  df1 <- data.frame(missingtype = missingtype,
                    impmethod = impmethod,
                    mean = outcomes[,1],
                    lwr = lower[,1],
                    upr = upper[,1],
                    ciw = width[,1])
  df1$model <- "SI"
  
  df2 <- data.frame(missingtype = missingtype,
                    impmethod = impmethod,
                    mean = outcomes[,2],
                    lwr = lower[,2],
                    upr = upper[,2],
                    ciw = width[,2])
  df2$model <- "MI"
  
  df3 <- data.frame(missingtype = missingtype,
                    impmethod = impmethod,
                    mean = outcomes[,3],
                    lwr = lower[,3],
                    upr = upper[,3],
                    ciw = width[,3])
  df3$model <- "LD"
  
  df4 <- data.frame(missingtype = missingtype,
                    impmethod = impmethod,
                    mean = outcomes[,4],
                    lwr = lower[,4],
                    upr = upper[,4],
                    ciw = width[,4])
  df4$model <- "Complete"
  
  # bind the df's 
  df <- rbind(df1, df2)
  df <- rbind(df,df3)
  df <- rbind(df,df4)
  }else{
    measurement <- "pic"
    
    
    # create lists to store the values
    outcomes <- list()
    upper <- list()
    lower <- list()
    width <- list()
    impmethod <- list() # we could change study to cond
    missingtype <- list()
    
    # add the values to the lists
    for(i in 1 : length(calc_outcomes)){
      outcomes[[i]] <- c(mean(calc_outcomes[[i]][["si"]][[measurement]]), mean(calc_outcomes[[i]][["mi"]][[measurement]]), mean(calc_outcomes[[i]][["ld"]][[measurement]]),mean(calc_outcomes[[i]][["comp"]][[measurement]]))
      upper[[i]] <- c(max(calc_outcomes[[i]][["si"]][[measurement]]),max(calc_outcomes[[i]][["mi"]][[measurement]]), max(calc_outcomes[[i]][["ld"]][[measurement]]),max(calc_outcomes[[i]][["comp"]][[measurement]]))
      lower[[i]] <- c(min(calc_outcomes[[i]][["si"]][[measurement]]), min(calc_outcomes[[i]][["mi"]][[measurement]]), min(calc_outcomes[[i]][["ld"]][[measurement]]),min(calc_outcomes[[i]][["comp"]][[measurement]]))
      width[[i]] <- c(mean(calc_outcomes[[i]][["si"]][["piw"]]), mean(calc_outcomes[[i]][["mi"]][["piw"]]), mean(calc_outcomes[[i]][["ld"]][["piw"]]),mean(calc_outcomes[[i]][["comp"]][["piw"]]))
      impmethod[[i]] <- calc_outcomes[[i]][["conds"]][["impmethod"]]
      missingtype[[i]] <- calc_outcomes[[i]][["conds"]][["missingtype"]]
    }
    
    # aggregrate the outcome selection
    outcomes <- do.call("rbind", outcomes)
    upper <- do.call("rbind", upper)
    lower <- do.call("rbind", lower)
    width <- do.call("rbind", width)
    impmethod <- do.call("rbind", impmethod)
    missingtype <- do.call("rbind", missingtype)
    
    
    # create df of the aggregrates 
    df1 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mean = outcomes[,1],
                      lwr = lower[,1],
                      upr = upper[,1],
                      piw = width[,1])
    df1$model <- "SI"
    
    df2 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mean = outcomes[,2],
                      lwr = lower[,2],
                      upr = upper[,2],
                      piw = width[,2])
    df2$model <- "MI"
    
    df3 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mean = outcomes[,3],
                      lwr = lower[,3],
                      upr = upper[,3],
                      piw = width[,3])
    df3$model <- "LD"
    
    df4 <- data.frame(missingtype = missingtype,
                      impmethod = impmethod,
                      mean = outcomes[,4],
                      lwr = lower[,4],
                      upr = upper[,4],
                      piw = width[,4])
    df4$model <- "Complete"
    
    # bind the df's 
    df <- rbind(df1, df2)
    df <- rbind(df,df3)
    df <- rbind(df,df4)
  }
}

ci_table <- create_table_results_intervals(calc_outcomes = res,interval_type = "ci" )

pi_table <- create_table_results_intervals(calc_outcomes = res,interval_type = "pi" )
