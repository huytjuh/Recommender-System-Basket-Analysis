personalized_weight <- function(a, n){
  df_train3 <-df_train2[, -1]
  P_pc_top <- df_train3
  P_pc_bot <- rowSums(df_train3)^a
  P_pc <- P_pc_top / P_pc_bot 
  row.names(P_pc) <- df_train2$customer_ID
  
  if (a == 0.5){
    w <- as.matrix(P_pc) %*% as.matrix(R_bsrw_bn_0.5[[n]])
  } else if (a == 0.7){
    w <- as.matrix(P_pc) %*% as.matrix(R_bsrw_bn_0.7[[n]])
  } else {
    w <- as.matrix(P_pc) %*% as.matrix(R_bsrw_bn_0.9[[n]])
  }
  row.names(w) <- df_train2$customer_ID
  
  return(w)
}

prediction_weighted_sum <- function(sim, basket, weight, customer_ID, evidence){
  output <- sim %*% c(basket) / rowSums(sim) * c(weight[row.names(weight) == customer_ID, ])
  output <- output[order(output, decreasing = TRUE), ]
  output <- output[!(names(output) %in% evidence)]
  
  return (names(output))
}

bHR <- function(type, n, input_list, name, boolean=TRUE){
  if (type == "pop"){
    target_list <- df_test2_pop_target #pop
  } else{
    target_list <- df_test2_rnd_target #rnd
  }
  
  counter <- 0
  for (i in 1:n){
    if (any(input_list[[i]][1:3] %in% target_list[[i]]) == TRUE){
      counter <- counter + 1
    }
  }
  
  output <- counter / n
  if (boolean == TRUE){
    output <- paste(name, counter / n, sep = ": ")
  }
  
  return (output)
}

wHR <- function(recommendation, target, pop){
  output <- data.frame(target = recommendation, value = as.numeric(recommendation == target))
  pop <- data.frame(target = names(pop), boolean = (1 - pop))
  output <- merge(x = output, y = pop, all = TRUE)
  
  return ((output$value %*% output$boolean) / sum(output$boolean))
}

macroHR <- function(recommendation, target){
  output <- data.frame(target = recommendation, value = as.numeric(recommendation == target))
  
  return(sum(output$value) / length(output$value))
}
