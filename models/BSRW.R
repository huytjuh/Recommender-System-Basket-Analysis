bsrw_matrix <- function(obj){
  d <- seq(0.1, 0.9, by = 0.1)

  test <- length(d)

  R_bsrw_bn_0.5 <- list()
  R_bsrw_bn_0.7 <- list()
  R_bsrw_bn_0.9 <- list()
  for (i in 1:test){

    R_bsrw_bn_0.5[[i]] <- solve(diag(1, nrow = nrow(R_item)) - d[i]*P_transition_list[[1]]) %*% t((1-d[i])*diag(1, nrow = nrow(R_item)))
    R_bsrw_bn_0.7[[i]] <- solve(diag(1, nrow = nrow(R_item)) - d[i]*P_transition_list[[2]]) %*% t((1-d[i])*diag(1, nrow = nrow(R_item)))
    R_bsrw_bn_0.9[[i]] <- solve(diag(1, nrow = nrow(R_item)) - d[i]*P_transition_list[[3]]) %*% t((1-d[i])*diag(1, nrow = nrow(R_item)))

    colnames(R_bsrw_bn_0.5[[i]]) <- colnames(R_item)
    colnames(R_bsrw_bn_0.7[[i]]) <- colnames(R_item)
    colnames(R_bsrw_bn_0.9[[i]]) <- colnames(R_item)
  }
  
  return(c(R_bsrw_bn_0.5, R_bsrw_bn_0.7, R_bsrw_bn_0.9))
}

