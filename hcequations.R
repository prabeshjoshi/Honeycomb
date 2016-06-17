hcequations <- function(layoutmat, ydata, rownum, colnum, yieldnum, type_){
  
  num_rows <- nrow(layoutmat)
  num_col <- ncol(layoutmat)
  
  yield_matrix <- function(layoutmat, ydata, rownum, colnum, yieldnum){
    ## creates a matrix placing yields in their respective positions
    yield_mat <- matrix(NA,num_rows,num_col)
    
    num_yrows <- nrow(ydata)
    for (i in 1:num_yrows){
      
      r <- as.numeric(as.character(ydata[i, rownum]))
      c <- as.numeric(as.character(ydata[i, colnum]))
      yield_mat[r, c] = as.numeric(as.character(ydata[i, yieldnum]))
    }
    return(yield_mat)
  }
  
  
  addition_vec_r_19 <- c(0,0,0,0,-1, -1, -1, -1, -1, -2, -2, -2, 1, 1, 1, 1, 1, 2)
  addition_vec_c_19 <- c(-1, -2, 1, 2, -2, -1, 0, 1, 2, -1, 0, 1, -2, -1, 0, 1, 2)
  
  addition_vec_r_31 <- c(0, 0, 0, 0, 0, 0, -1, -1, -1,-1,-1, -2, -2, -2, -2, -2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3)
  addition_vec_c_31 <- c(-1, -2, -3, 1, 2, 3, 0, -2, -1, 1, 2, -2, -1, 0, 1, 2, -3, -2, -1, 0, 1, 2, 3, -2, -1, 0, 1, 2, -1, 1)
  
  
  all_positions <- function(r,c, type_){
    type_r <- paste("addition_vec_","r_",type_, sep="")
    type_c <- paste("addition_vec_c_",type_, sep="")
    type_r <- get(type_r)
    type_c <- get(type_c)
    xs <- r +  type_r
    ys <- c +  type_c
    position_ <- cbind(xs, ys)
    return(position_)
  }
  
  accepted_ <- all_positions(r,c, type_)
  
  yield_mat <- yield_matrix(layoutmat, ydata, rownum, colnum, yieldnum)
  
  all_yields <- function(accepted_, ydata, rownum, colnum, yieldnum, yield_mat){
    accepted_r <- accepted_[,1]
    accepted_c <- accepted_[,2]
    yields <- vector()
    leng <- nrow(accepted_)
    
    for (i in 1:leng){
      r <- accepted_[i, 1]
      c <- accepted_[i, 2]
      if ((r>0) & (c>0) & (r<(num_rows+1)) & (c<(num_col+1))) {
        yield <- yield_mat[r, c]
        yields <- c(yields, yield)  
      }
    }
    yields <- yields[!is.na(yields)]
    return(yields)
  }
  
  
  
  entry_means <- function(layout_, yield_mat, ydata, type_){
    rownum <- nrow(yield_mat)
    colnum <- ncol(yield_mat)
    
    numentries <- rep(0, type_)
    sumentries <- rep(0, type_)
    
    lng <- length(ydata)
    
    
    for (r in 1:rownum){
      for (c in 1:colnum){
        
        nentry <- layout_[r, c]
        yld <- yield_mat[r, c]
        if (!(is.na(yld))){
        sumentries[nentry] <-  sumentries[nentry] + yld
        numentries[nentry] <- numentries[nentry] +1
      }
      }
    }
    
    #return (cbind(numentries,sumentries))
    meanent <- sumentries/numentries
    return(meanent)
  }
  
  
  
  entry_means_ <- entry_means(layoutmat, yield_mat, ydata, type_)
  
  
  
  lengths <- vector()
  nyields <-   nrow(ydata)
  rowvec <- as.numeric(as.character(ydata[,rownum]))
  colvec <- as.numeric(as.character(ydata[,colnum]))
  
  is.complete_circle <- vector()
  
  X_bar_r <- vector()
  
  entry <- vector()
  
  leng <- nrow(ydata)
  X_r <- vector()
  sds_ <- vector()
  is_complete_circle <- vector()
  
  mu_s <- vector()
  
  for (i in 1:nyields){
    
    r <- as.numeric(as.character(ydata[i, rownum]))
    c <- as.numeric(as.character(ydata[i, colnum]))
    
    pos_ <- all_positions(r, c, type_)
    
    yields <- all_yields(pos_, ydata, rownum, colnum, yieldnum, yield_mat)
    
    X_bar_r = c(X_bar_r, mean(yields))
    sd_ <- sd(yields)
    
    lengths  <- c(lengths, length(yields))
    is.complete_circle <- c(is.complete_circle, as.logical(length(yields)==(type_-1)))
    sds_ <- c(sds_,sd_ )
    entry <- c(entry, layoutmat[r, c])
    mus_ <- entry_means_[layoutmat[r, c]]
    mu_s <- c(mu_s, mus_)
  }
  
  x_t <- mean(as.numeric(as.character((ydata[,yieldnum]))), na.rm = T)
  
  
  x_s <- ydata[,yieldnum]
  
  eqn_A <- (mu_s/sds_)^(2) * (x_s/X_bar_r)^(2)
  eqn_B <- (mu_s/sds_)^(2) * (mu_s/x_t)^(2)
  
  final <- cbind(entry, X_bar_r,sds_, lengths, eqn_A, eqn_B )
  final <- cbind(ydata, final)
  return(final)
}