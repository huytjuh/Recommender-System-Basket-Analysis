# SIMILARITY MATRIX BASED ON COSINE
similarity_matrix <- function(obj) {
  UseMethod("similarity_matrix")
}

similarity_matrix.cosine <- function(obj){
  R_item <- lsa::cosine(as.matrix(obj[, c(2:ncol(obj))]))
  return(R_item)
}

# SIMILARITY MATRIX BASED ON CONDITIONAL PROBABILITY THEOREM
similarity_matrix.conditional <- function(obj, R_item){
  R_item2 <- matrix(NA, nrow = nrow(R_item), ncol = ncol(R_item), dimnames = list(colnames(R_item), colnames(R_item)))
  
  df_train3 <- obj[, c(2:ncol(obj))]
  df_train3 <- df_train3 / rowSums(df_train3)
  
  freq <- colSums(df_train3 != 0)
  alpha <- 1

  for (i in 1:nrow(R_item2)){
    R_item2[i, ] <- colSums(df_train3[which(df_train3[, i] > 0), ]) / (freq[i] * freq^alpha)
  }
  return(R_item2)
}

# SIMILARITY MATRIX BASED ON BIPARTITE NETWORK THEOREM
similarity_matrix.bipartite <- function(obj){
  alpha_full <- c(0.5, 0.7, 0.9)
  
  df_train3 <- df_train2[, c(2:ncol(obj))]
  
  P_transition_list <- list()
  for (n in 1:length(alpha_full)){
    alpha <- alpha_full[n]
    
    P_pc_top <- df_train3
    P_pc_bot <- rowSums(df_train3)^alpha
    P_pc <- P_pc_top / P_pc_bot
    
    P_cp_top <- df_train3
    P_cp_bot <- colSums(df_train3)^alpha
    P_cp <- matrix(NA, nrow = nrow(df_train3), ncol = ncol(df_train3), dimnames = list(row.names(df_train3), colnames(df_train3)))
    for (i in 1:ncol(df_train3)){
      P_cp[, i] <- as.matrix(P_cp_top[, i] / P_cp_bot[i])
    }
    
    P_transition <- matrix(NA, nrow = nrow(R_item), ncol = ncol(R_item), dimnames = list(colnames(R_item), colnames(R_item)))
    for (i in 1:nrow(R_item)){
      P_transition[i, ] <- colSums(P_pc * P_cp[, i])
    }
    P_transition_list[[n]] <- t(P_transition)
  }
  
  return(P_transition_list)
}
