grandmean <- function(yd, col_entry, col_yield, selection_n){
  ##This function creates a dataframe  containing columns Mean, 
  #Mean yield per plant (g) % highest value, Mean/s, % highest value, standard deviation, 
  #(Mean sel-Mean)/s, % highest value, Mean sel and Grand Mean 
  #
  #yd is a dataframe containing entry number and yield at columns denoted by col_entry
  #and col_yield respectively
  #
  #seletion_n is the number of plants that one choses to sel
  
  
  
  
  remove_na_entries <- function(yd, col_entry){
    ##removes empty entries if any
    yd_a <- data.frame()
    nrows_ <- nrow(yd)
    
    for (i in 1:nrows_){
      if (!(is.na(yd[i,col_entry]))){
        row_ <- yd[i,]
        yd_a <- rbind(yd_a, row_)
      }  
    }
    
    return(yd_a)
  }
  
  yd <- remove_na_entries(yd, col_entry)
  num_entry <- length(levels(entr))
  
  yield_list <-  function(yd, col_entry, col_yield){
    #creates a list of lists containing yields in each entry   
    
    yd$Entry  <- as.numeric(yd$Entry)
    yd$Seed.weight  <- as.numeric(yd$Seed.weight)
    entr <- as.factor(yd$Entry) 
    num_entry <- length(levels(entr))
    
    nrows_ <- nrow(yd)
    nreps  <- round(nrows_/num_entry+2)
    
    #yield_mat <- matrix(NA, nrow = num_entry, ncol = nreps)
    
    ylist <- rep( list(list()), num_entry) 
    
    for (i in 1:nrows_){
      entry <- yd[i, col_entry]
      yield <- yd[i, col_yield]
      
      ylist[[entry]] = c(ylist[[entry]], yield)
    }
    return(ylist)
  }
  
  
  y_list <-  yield_list(yd, col_entry, col_yield)
  
  
  mean_vec <- function(y_list){
    #returns a vector of means of entries
    leng <- length(y_list)
    means_ <- rep(0, leng)
    
    for (i in 1:leng){
      mean_ <- mean(unlist(y_list[i]), na.rm = T)
      means_[i] <- mean_
    }
    return(means_)
  }
  
  
  sd_vec <- function(y_list){
    #returns vector of standard deviations of entries
    leng <- length(y_list)
    sds_ <- rep(0, leng)
    
    for (i in 1:leng){
      sd_ <- sd(unlist(y_list[i]), na.rm = T)
      sds_[i] <- sd_
    }
    return(sds_)
  }
  
  
  means_ <- mean_vec(y_list)
  sdvx <- sd_vec(y_list)
  
  max_mean <- max(means_)
  mean_by_s <- means_/sdvx
  max_ratio <- max(mean_by_s)
  per100_highest_val_mean <- means_/max_mean * 100
  per100_highest_val_ratio <- mean_by_s/max_ratio * 100
  
  mean_sel <- function(y_list, selection_n){
    #returns the means of the top selection_n number of plants
    
    num_entry <-  length(y_list)
    mean_s <- rep(list(list()), num_entry)
    
    for ( i in 1:num_entry){
      #num_retain <- round(length(y_list[[i]])*selection_ratio)
      
      sorted <- sort(na.exclude(unlist(y_list[i])), decreasing = T)
      #mean_s[i] <- mean(sorted[1:num_retain])
      mean_s[i] <- mean(sorted[1:selection_n])
      
    }
    return(unlist(mean_s))
  }
  
  
  mean_selected <- mean_sel(y_list, selection_n)
  
  mean_sel_minus_mean_by_s  <- (mean_selected - means_)/sdvx
  
  max_ms_m <- max(mean_sel_minus_mean_by_s)
  per100_highest_val_ms_m <-  mean_sel_minus_mean_by_s/max_ms_m *100
  
  grand_mean <- (per100_highest_val_ms_m + per100_highest_val_mean + per100_highest_val_ratio)/3
  
  
  
  final <- cbind(1:num_entry,means_, per100_highest_val_mean, mean_by_s, per100_highest_val_ratio, sdvx, mean_sel_minus_mean_by_s, per100_highest_val_ms_m, mean_selected, grand_mean)
  
  colnames(final) <- c("Entry", "Mean", "Mean yield per plant (g) % highest value", "Mean/s", "% highest value", "s", "(Mean sel-Mean)/s", "% highest value", "Mean sel", "Grand Mean")
  
  
  
  order_ent <- final[ order(-final[,10])]
  final_df <-  final[order_ent, ]
  
  return(final_df)
  
}
