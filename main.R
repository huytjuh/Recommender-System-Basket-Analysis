data <- read.csv(file = "ta_feng_all_months_merged.csv")

df <- data.frame(customer_ID = as.character(data$CUSTOMER_ID),
                 transaction_date = as.factor(data$TRANSACTION_DT),
                 product_subclass = as.factor(data$PRODUCT_SUBCLASS),
                 product_ID = as.factor(data$PRODUCT_ID),
                 frequencies = as.numeric(data$AMOUNT))

setDT(df)
df_matrix <- as.data.frame(dcast(df, transaction_date + customer_ID ~ product_subclass, value.var = "frequencies"))

popularity <- names(colSums(df_matrix[, c(3:ncol(df_matrix))])[order(colSums(df_matrix[, c(3:ncol(df_matrix))]), decreasing = TRUE)]) #get all names sorted by popularity
df_matrix <- cbind(df_matrix[, c(1:2)], df_matrix[, popularity]) #sort by popularity
df_matrix <- df_matrix[rowSums(df_matrix[, c(1096:ncol(df_matrix))]) == 0, c(1:1095)] #select the 1093 most popular items
df_matrix <- df_matrix[order(as.Date(df_matrix$transaction_date, format="%d/%m/%Y")), c(1:1095)] #chronological order

head(df_matrix)


set.seed(1234)

# split baskets longitudinally
df_train <- df_matrix[c(1:85684), ]
df_test <- df_matrix[c(85685:nrow(df_matrix)), ]


# Aggregate training baskets
df_train2 <- df_train %>% group_by(customer_ID) %>% summarize_at(2:(ncol(df_train)-1), sum)
df_train2 <- df_train2 %>% mutate(count = rowSums(df_train2[, c(2:ncol(df_train2))]!=0))
df_train2 <- df_train2[!(df_train2$count < 2), c(1:(ncol(df_train2)-1))]

for (i in 2:ncol(df_train2)){
  df_train2[[i]] <- as.numeric(log(df_train2[[i]] + 1))
}

head(df_train2)


# Filter out test baskets with less than 4 items
df_test2 <- df_test %>% mutate(count = rowSums(df_test[, c(3:ncol(df_test))]!=0))
df_test2 <- df_test2[!(df_test2$count < 4), c(1:(ncol(df_test2)-1))]
df_test2 <- df_test2[df_test2$customer_ID %in% df_train2$customer_ID, ]

for (i in 3:ncol(df_test2)){
  df_test2[[i]] <- as.numeric(log(df_test2[[i]] + 1))
}

head(df_test2)


set.seed(1234)

df_test2_pop_evidence <- list()
df_test2_pop_target <- list()
df_test2_rnd_evidence <- list()
df_test2_rnd_target <- list()
pb <- txtProgressBar(min = 0, max = nrow(df_test2), style = 3)
for (i in 1:nrow(df_test2)){
  basket <- df_test2[i, ]
  basket_products <- colnames(basket[, -c(1:2)][, basket[, -c(1:2)] > 0])
  
  #split the already sorted baskets by popularity
  df_test2_pop_target[[i]] <- basket_products[(length(basket_products)-2):length(basket_products)]
  df_test2_pop_evidence[[i]] <- basket
  df_test2_pop_evidence[[i]][, df_test2_pop_target[[i]]] <- 0 
  
  #create random sample
  rnd <- sample(length(basket_products), 3) 
  df_test2_rnd_target[[i]] <- basket_products[rnd]
  df_test2_rnd_evidence[[i]] <- basket
  df_test2_rnd_evidence[[i]][, df_test2_rnd_target[[i]]] <- 0
  
  setTxtProgressBar(pb, i)
}
df_test2_pop_evidence <- as.data.frame(rbindlist(df_test2_pop_evidence))
df_test2_rnd_evidence <- as.data.frame(rbindlist(df_test2_rnd_evidence))


df_test2_weighted <- list()
df_test2_weighted_customer_ID <- list()
pb <- txtProgressBar(min = 0, max = nrow(df_test2), style = 3)
for (i in 1:nrow(df_test2)){
  basket <- df_test2[i, ]
  basket_products <- colnames(basket[, -c(1:2)][, basket[, -c(1:2)] > 0])
  
  # loop through all basket items in which we take out a single target item for each iteration
  df_test2_weighted_temp <- list()
  for(j in 1:length(basket_products)){
    df_test2_weighted_temp$target[[j]] <- basket_products[j]
    df_test2_weighted_temp$evidence[[j]] <- basket
    df_test2_weighted_temp$evidence[[j]][, df_test2_weighted_temp$target[[j]]] <- 0
  }
  # store all evidence and target items into a list
  df_test2_weighted[[i]] <- df_test2_weighted_temp
  df_test2_weighted_customer_ID[[i]] <- as.data.frame(basket$customer_ID)
  
  setTxtProgressBar(pb, i)
}


R_item <- lsa::cosine(as.matrix(df_train2[, c(2:ncol(df_train2))]))
head(R_item)


R_item2 <- matrix(NA, nrow = nrow(R_item), ncol = ncol(R_item), dimnames = list(colnames(R_item), colnames(R_item)))

df_train3 <- df_train2[, c(2:ncol(df_train2))]
df_train3 <- df_train3 / rowSums(df_train3)

freq <- colSums(df_train3 != 0)
alpha <- 1

pb <- txtProgressBar(min = 0, max = nrow(R_item2), style = 3)
for (i in 1:nrow(R_item2)){
  R_item2[i, ] <- colSums(df_train3[which(df_train3[, i] > 0), ]) / (freq[i] * freq^alpha)
  setTxtProgressBar(pb, i)
}

head(R_item2)


alpha_full <- c(0.5, 0.7, 0.9)

df_train3 <- df_train2[, c(2:ncol(df_train2))]

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

  pb <- txtProgressBar(min = 0, max = nrow(R_item), style = 3)
  P_transition <- matrix(NA, nrow = nrow(R_item), ncol = ncol(R_item), dimnames = list(colnames(R_item), colnames(R_item)))
  for (i in 1:nrow(R_item)){
    P_transition[i, ] <- colSums(P_pc * P_cp[, i])
    setTxtProgressBar(pb, i)
  }
  P_transition_list[[n]] <- t(P_transition)
}
head(t(P_transition))


d <- seq(0.1, 0.9, by = 0.1)

test <- length(d)

R_bsrw_bn_0.5 <- list()
R_bsrw_bn_0.7 <- list()
R_bsrw_bn_0.9 <- list()
pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){

  R_bsrw_bn_0.5[[i]] <- solve(diag(1, nrow = nrow(R_item)) - d[i]*P_transition_list[[1]]) %*% t((1-d[i])*diag(1, nrow = nrow(R_item)))
  R_bsrw_bn_0.7[[i]] <- solve(diag(1, nrow = nrow(R_item)) - d[i]*P_transition_list[[2]]) %*% t((1-d[i])*diag(1, nrow = nrow(R_item)))
  R_bsrw_bn_0.9[[i]] <- solve(diag(1, nrow = nrow(R_item)) - d[i]*P_transition_list[[3]]) %*% t((1-d[i])*diag(1, nrow = nrow(R_item)))
  
  colnames(R_bsrw_bn_0.5[[i]]) <- colnames(R_item)
  colnames(R_bsrw_bn_0.7[[i]]) <- colnames(R_item)
  colnames(R_bsrw_bn_0.9[[i]]) <- colnames(R_item)

  setTxtProgressBar(pb, i)
}


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


test <- nrow(df_test2)

d <- seq(0.1, 0.9, by = 0.1)

trace_0.5 <- list()
trace_0.7 <- list()
trace_0.9 <- list()

for (n in 1:length(d)){

  recommendations_bsrw_bn_0.5 <- list()
  recommendations_bsrw_bn_0.7 <- list()
  recommendations_bsrw_bn_0.9 <- list()

  w_0.5 <- personalized_weight(0.5, n)
  w_0.7 <- personalized_weight(0.7, n)
  w_0.9 <- personalized_weight(0.9, n)

  pb <- txtProgressBar(min = 0, max = test, style = 3)
  for (i in 1:test){
    basket_test <- df_test2_pop_evidence[i, -c(1:2)]
    basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]

    # use the offline R_item matrix, and select the corresponding columns and sum it up
    R_bsrw_basket_0.5 <- rowSums(cbind(R_bsrw_bn_0.5[[n]][, basket_test_products], 0))
    R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
    R_bsrw_basket_0.9 <- rowSums(cbind(R_bsrw_bn_0.9[[n]][, basket_test_products], 0))

    # calculate the recommendations of BSRW for the appropriate parameters
    recommendations_bsrw_bn_0.5[[i]] <- prediction_weighted_sum(P_transition_list[[1]], R_bsrw_basket_0.5, w_0.5, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
    recommendations_bsrw_bn_0.7[[i]] <- prediction_weighted_sum(P_transition_list[[2]], R_bsrw_basket_0.7, w_0.7, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
    recommendations_bsrw_bn_0.9[[i]] <- prediction_weighted_sum(P_transition_list[[3]], R_bsrw_basket_0.9, w_0.9, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)

    setTxtProgressBar(pb, i)
  }  

  # store the results
  trace_0.5[[n]] <- bHR("pop", test, recommendations_bsrw_bn_0.5, "", FALSE)
  trace_0.7[[n]] <- bHR("pop", test, recommendations_bsrw_bn_0.7, "", FALSE)  
  trace_0.9[[n]] <- bHR("pop", test, recommendations_bsrw_bn_0.9, "", FALSE)

}
cbind(trace_0.5, trace_0.7, trace_0.9)


test <- length(df_test2_weighted)

d <- seq(0.1, 0.9, by = 0.1)

trace_0.5 <- list()
trace_0.7 <- list()
trace_0.9 <- list()

for (n in 1:length(d)){

  recommendations_bsrw_bn_0.5 <- list()
  recommendations_bsrw_bn_0.7 <- list()
  recommendations_bsrw_bn_0.9 <- list()

  w_0.5 <- personalized_weight(0.5, n)
  w_0.7 <- personalized_weight(0.7, n)
  w_0.9 <- personalized_weight(0.9, n)

  pb <- txtProgressBar(min = 0, max = test, style = 3)
  for (i in 1:test){
    
    hit <- list()
    for (j in 1:length(df_test2_weighted[[i]]$target)){
      basket_test <- df_test2_weighted[[i]]$evidence[[j]][, -c(1:2)]
      basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]

      # use the offline R_item matrix, and select the corresponding columns and sum it up
      R_bsrw_basket_0.5 <- rowSums(cbind(R_bsrw_bn_0.5[[n]][, basket_test_products], 0))
      R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
      R_bsrw_basket_0.9 <- rowSums(cbind(R_bsrw_bn_0.9[[n]][, basket_test_products], 0))

      # calculate the recommendations of BSRW for the appropriate parameters
      hit$bsrw_bn_0.5[j] <- prediction_weighted_sum(P_transition_list[[1]], R_bsrw_basket_0.5, w_0.5, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
      hit$bsrw_bn_0.7[j] <- prediction_weighted_sum(P_transition_list[[2]], R_bsrw_basket_0.7, w_0.7, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
      hit$bsrw_bn_0.9[j] <- prediction_weighted_sum(P_transition_list[[3]], R_bsrw_basket_0.9, w_0.9, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
    }
    
    # get the corresponding macroHR evaluation metrics
    recommendations_bsrw_bn_0.5[i] <- macroHR(hit$bsrw_bn_0.5, df_test2_weighted[[i]]$target)
    recommendations_bsrw_bn_0.7[i] <- macroHR(hit$bsrw_bn_0.7, df_test2_weighted[[i]]$target)
    recommendations_bsrw_bn_0.9[i] <- macroHR(hit$bsrw_bn_0.9, df_test2_weighted[[i]]$target)
    
    setTxtProgressBar(pb, i)
  }  

  # store the results
  trace_0.5[[n]] <- sum(unlist(recommendations_bsrw_bn_0.5)) / length(recommendations_bsrw_bn_0.5)
  trace_0.7[[n]] <- sum(unlist(recommendations_bsrw_bn_0.7)) / length(recommendations_bsrw_bn_0.7) 
  trace_0.9[[n]] <- sum(unlist(recommendations_bsrw_bn_0.9)) / length(recommendations_bsrw_bn_0.9)
}
cbind(trace_0.5, trace_0.7, trace_0.9)


d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_pop <- list()
recommendations_cos <- list()
recommendations_cp <- list()
recommendations_bn_0.7 <- list()
recommendations_bsrw_cos <- list()
recommendations_bsrw_cp <- list()
recommendations_bsrw_bn_0.7 <- list()

pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  basket_test <- df_test2_rnd_evidence[i, -c(1:2)]
  basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]
  
  # calculate recommendations of pop
  recommendations_pop[[i]] <- colnames(basket_test[, !(colnames(basket_test) %in% basket_test_products)])
  
  # calculate recommendations of traditional CF models
  recommendations_cos[[i]] <- prediction_weighted_sum(t(R_item), t(basket_test), w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_cp[[i]] <- prediction_weighted_sum(t(R_item2), t(basket_test), w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_bn_0.7[[i]] <- prediction_weighted_sum(P_transition_list[[2]], t(basket_test), w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  
  # calculate recommendations of respective BSRW models
  R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
  
  recommendations_bsrw_cos[[i]] <- prediction_weighted_sum(t(R_item), R_bsrw_basket_0.7, w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_bsrw_cp[[i]] <- prediction_weighted_sum(t(R_item2), R_bsrw_basket_0.7, w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_bsrw_bn_0.7[[i]] <- prediction_weighted_sum(P_transition_list[[2]], R_bsrw_basket_0.7, w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
     
  setTxtProgressBar(pb, i)
}

# print results
bHR("pop", test, recommendations_pop, "pop")
bHR("pop", test, recommendations_cos, "cos")
bHR("pop", test, recommendations_cp, "cp")
bHR("pop", test, recommendations_bn_0.7, "bn")
bHR("pop", test, recommendations_bsrw_cos, "cos_bsrw")
bHR("pop", test, recommendations_bsrw_cp, "cp_bsrw")
bHR("pop", test, recommendations_bsrw_bn_0.7, "bn_bsrw")


d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_rnd <- list()
recommendations_cos <- list()
recommendations_cp <- list()
recommendations_bn_0.7 <- list()
recommendations_bsrw_cos <- list()
recommendations_bsrw_cp <- list()
recommendations_bsrw_bn_0.7 <- list()

pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  basket_test <- df_test2_rnd_evidence[i, -c(1:2)]
  basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]
  
  # calculate recommendations of pop
  recommendations_rnd[[i]] <- colnames(basket_test[, !(colnames(basket_test) %in% basket_test_products)])
  
  # calculate recommendations of traditional CF models
  recommendations_cos[[i]] <- prediction_weighted_sum(t(R_item), t(basket_test), w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_cp[[i]] <- prediction_weighted_sum(t(R_item2), t(basket_test), w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_bn_0.7[[i]] <- prediction_weighted_sum(P_transition_list[[2]], t(basket_test), w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
  
  # calculate recommendations of respective BSRW models
  R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
  
  recommendations_bsrw_cos[[i]] <- prediction_weighted_sum(t(R_item), R_bsrw_basket_0.7, w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_bsrw_cp[[i]] <- prediction_weighted_sum(t(R_item2), R_bsrw_basket_0.7, w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
  recommendations_bsrw_bn_0.7[[i]] <- prediction_weighted_sum(P_transition_list[[2]], R_bsrw_basket_0.7, w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
     
  setTxtProgressBar(pb, i)
}

# print results
bHR("rnd", test, recommendations_rnd, "pop")
bHR("rnd", test, recommendations_cos, "cos")
bHR("rnd", test, recommendations_cp, "cp")
bHR("rnd", test, recommendations_bn_0.7, "bn")
bHR("rnd", test, recommendations_bsrw_cos, "cos_bsrw")
bHR("rnd", test, recommendations_bsrw_cp, "cp_bsrw")
bHR("rnd", test, recommendations_bsrw_bn_0.7, "bn_bsrw")


d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_pop <- list()
recommendations_cos <- list()
recommendations_cp <- list()
recommendations_bn_0.7 <- list()
recommendations_bsrw_cos <- list()
recommendations_bsrw_cp <- list()
recommendations_bsrw_bn_0.7 <- list()

popularity <- t(colSums(exp(df_train2[, -1])-1) / sum(colSums(exp(df_train2[, -1])-1)))

pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  hit <- list()
  for (j in 1:length(df_test2_weighted[[i]]$target)){
    basket_test <- df_test2_weighted[[i]]$evidence[[j]][, -c(1:2)]
    basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]
    
    # calculate recommendation of pop based model
    hit$pop[j] <- colnames(basket_test[, !(colnames(basket_test) %in% basket_test_products)])[1]
  
    # calculate recommendations of traditional models
    hit$cos[j] <- prediction_weighted_sum(t(R_item), t(basket_test), w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
    hit$cp[j] <- prediction_weighted_sum(t(R_item2), t(basket_test), w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
    hit$bn_0.7[j] <- prediction_weighted_sum(P_transition_list[[2]], t(basket_test), w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]

    # calculate recommendations of the respective BSRW based models
    R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))

    hit$bsrw_cos[j] <- prediction_weighted_sum(t(R_item), R_bsrw_basket_0.7, w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
    hit$bsrw_cp[j] <- prediction_weighted_sum(t(R_item2), R_bsrw_basket_0.7, w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
    hit$bsrw_bn_0.7[j] <- prediction_weighted_sum(P_transition_list[[2]], R_bsrw_basket_0.7, w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
  }
  
  # store the wHR evaluation of pop based models
  recommendations_pop[i] <- wHR(hit$pop, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$pop])
  
  # store the wHR evaluation of the traditional based models
  recommendations_cos[i] <- wHR(hit$cos, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$cos])
  recommendations_cp[i] <- wHR(hit$cp, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$cp])
  recommendations_bn_0.7[i] <- wHR(hit$bn_0.7, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$bn_0.7])
  
  # store the wHR evaluation of the respective BSRW based models
  recommendations_bsrw_cos[i] <- wHR(hit$bsrw_cos, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$bsrw_cos])
  recommendations_bsrw_cp[i] <- wHR(hit$bsrw_cp, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$bsrw_cp])
  recommendations_bsrw_bn_0.7[i] <- wHR(hit$bsrw_bn_0.7, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$bsrw_bn_0.7])
  
  setTxtProgressBar(pb, i)
}
# print results
print(paste("pop", sum(unlist(recommendations_pop)) / length(recommendations_pop), sep = ": "))
print(paste("cos", sum(unlist(recommendations_cos)) / length(recommendations_cos), sep = ": "))
print(paste("cp", sum(unlist(recommendations_cp)) / length(recommendations_cp), sep = ": "))
print(paste("bn_0.7", sum(unlist(recommendations_bn_0.7)) / length(recommendations_bn_0.7), sep = ": "))

print(paste("bsrw_cos", sum(unlist(recommendations_bsrw_cos)) / length(recommendations_bsrw_cos), sep = ": "))
print(paste("bsrw_cp", sum(unlist(recommendations_bsrw_cp)) / length(recommendations_bsrw_cp), sep = ": "))
print(paste("bsrw_bn_0.7", sum(unlist(recommendations_bsrw_bn_0.7)) / length(recommendations_bsrw_bn_0.7), sep = ": "))


item_factors <- implicit_als(as.matrix(df_train2[, -1]))

R_als <- item_factors %*% t(item_factors)
colnames(R_als) <- colnames(R_item)
row.names(R_als) <- colnames(R_item)
head(R_als)


d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_als <- list()
recommendations_bsrw_als <- list()

pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  basket_test <- df_test2_pop_evidence[i, -c(1:2)]
  basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]

  # calculate recommendation of ALS based model
  recommendations_als[[i]] <- prediction_weighted_sum(R_als, t(basket_test), w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  
  # calculate recommendations of hybrid model
  R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
  
  recommendations_bsrw_als[[i]] <- prediction_weighted_sum(R_als, R_bsrw_basket_0.7, w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)

  setTxtProgressBar(pb, i)
}
# print results
bHR("pop", test, recommendations_als, "als")
bHR("pop", test, recommendations_bsrw_als, "als_bsrw")


d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_als <- list()
recommendations_bsrw_als <- list()

pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  basket_test <- df_test2_rnd_evidence[i, -c(1:2)]
  basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]
  
  # calculate recommendation of ALS based model
  recommendations_als[[i]] <- prediction_weighted_sum(R_als, t(basket_test), w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)
  
  # calculate recommendations of hybrid model
  R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
  
  recommendations_bsrw_als[[i]] <- prediction_weighted_sum(R_als, R_bsrw_basket_0.7, w, df_test2_rnd_evidence[i, ]$customer_ID, basket_test_products)

  setTxtProgressBar(pb, i)
}
# print results
bHR("rnd", test, recommendations_als, "als")
bHR("rnd", test, recommendations_bsrw_als, "als_bsrw")


d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_als <- list()
recommendations_bsrw_als <- list()

popularity <- t(colSums(exp(df_train2[, -1])-1) / sum(colSums(exp(df_train2[, -1])-1)))

pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  hit <- list()
  for (j in 1:length(df_test2_weighted[[i]]$target)){
    basket_test <- df_test2_weighted[[i]]$evidence[[j]][, -c(1:2)]
    basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]

    # calculate recommendation of ALS based model
    hit$als[j] <- prediction_weighted_sum(R_als, t(basket_test), w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]

    R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))

    # calculate recommendations of hybrid model
    hit$bsrw_als[j] <- prediction_weighted_sum(R_als, R_bsrw_basket_0.7, w, df_test2_weighted[[i]]$evidence[[j]]$customer_ID, basket_test_products)[1]
  }
  # store the wHR evaluation of the ALS based model
  recommendations_als[i] <- wHR(hit$als, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$als])

  # store the wHR evaluation of the hybrid model
  recommendations_bsrw_als[i] <- wHR(hit$bsrw_als, df_test2_weighted[[i]]$target, popularity[, colnames(popularity) %in% hit$bsrw_als])
  
  setTxtProgressBar(pb, i)
}
# print results
print(paste("als", sum(unlist(recommendations_als)) / length(recommendations_als), sep = ": "))
print(paste("bsrw_als", sum(unlist(recommendations_bsrw_als)) / length(recommendations_bsrw_als), sep = ": "))


# initialize one-hot encoding tuples

N <- 1
M <- nrow(R_item)
p <- 2*M

h_user <- matrix(0, nrow = N, ncol = 1, dimnames = list("customer_ID"))
h_target <- matrix(0, nrow = M, ncol = 1, dimnames = list(paste("target", row.names(R_item), sep = "_")))
h_basket <- matrix(0, nrow = M, ncol = 1, dimnames = list(paste("basket", row.names(R_item), sep = "_")))
h <- t(rbind(rbind(h_user, h_target), h_basket))

set.seed(1234)

test <- nrow(df_test2)*0.1
target_list <- list()
predictions_list <- list()
target_list_rnd <- list()
predictions_list_rnd <- list()
counter <- 0
pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  customer_unique <- unique(df_test2[, "customer_ID"])[i]
  basket_train <- df_train2[df_train2$customer_ID == customer_unique, ]
  basket_train_products <- colnames(basket_train[, 2:ncol(basket_train)][, c(basket_train[, 2:ncol(basket_train)]) > 0])
  
  
  # generate tuples for training for each customer
  tuple_pos <- list()
  tuple_neg <- list()
  for (t in 1:length(basket_train_products)){
    target_train <- basket_train_products[t]
    evidence_train <- basket_train_products[-t]
    
    input <- h
    input[, 1] <- customer_unique
    input[, colnames(input) %in% paste("basket", evidence_train, sep = "_")] <- 1
    input[, colnames(input) %in% paste("target", target_train, sep = "_")] <- 1
    tuple_pos[[t]] <- as.data.frame(input)
    tuple_pos[[t]]$target <- 1
    
    sample <- sample(colnames(R_item)[!(colnames(R_item) %in% basket_train_products)], length(basket_train_products) + 1)
    input2 <- h
    input2[, 1] <- customer_unique
    input2[, colnames(input2) %in% paste("basket", sample[1:length(basket_train_products)], sep = "_")] <- 1
    input2[, colnames(input2) %in% paste("target", sample[length(basket_train_products) + 1], sep = "_")] <- 1
    tuple_neg[[t]] <- as.data.frame(input2)
    tuple_neg[[t]]$target <- -1
  }
  tuple_pos_matrix <- as.data.frame(rbindlist(tuple_pos))
  tuple_neg_matrix <- as.data.frame(rbindlist(tuple_neg))
  training_set <- rbind(tuple_pos_matrix, tuple_neg_matrix)
  
  # perform AdaGrad called from Python file in order to learn the parameters
  params <- FM_GD(training_set[-1])
  
  # obtain values of the factorization machine for every tuple
  basket_test_all <- df_test2[df_test2$customer_ID %in% customer_unique, ]
  for (j in 1:nrow(basket_test_all)){
    counter <- counter + 1
    basket_test <- basket_test_all[j, ]
    basket_test_products <- colnames(basket_test[, 3:ncol(basket_test)][, c(basket_test[, 3:ncol(basket_test)]) > 0]) 
  
    # calculate all test tuple values corresponding for bHR(pop)
    evidence_test <- basket_test_products[1:(length(basket_test_products) - 3)]
    target_list[[counter]] <- as.data.frame(basket_test_products[(length(basket_test_products)-2):length(basket_test_products)])
    tuple_test <- matrix(0, nrow = ncol(R_item) - length(evidence_test), ncol = 2*M, dimnames = list(NULL, colnames(h)[-1]))
    tuple_test[, colnames(tuple_test) %in% paste("basket", evidence_test, sep = "_")] <- 1
    tuple_test[, !(colnames(tuple_test) %in% paste("target", evidence_test, sep = "_") | colnames(tuple_test) %like% "basket")] <- diag(1, nrow = nrow(tuple_test))
    testing_set <- as.data.frame(tuple_test)
    testing_set$target <- 1
    testing_set <- cbind(customer_ID = customer_unique, testing_set)
    
    # convert to python code in order to speeden things up as R is very slow in matrix multiplications
    output <- FM_value(as.matrix(testing_set[, -c(1, ncol(h))]), as.matrix(params[[3]]), as.matrix(params[[2]]), as.integer(M), as.integer(p))
    output <- as.data.frame(t(unlist(output, use.names=FALSE)))
    colnames(output) <- colnames(R_item)[!(colnames(R_item) %in% evidence_test)]

    predictions <- colnames(output[, order(output, decreasing = TRUE)])
    predictions_list[[counter]] <- as.data.frame(predictions[1:3])
    
    # calculate all test tuple values corresponding for bHR(rnd)
    rnd <- sample(length(basket_test_products), 3)
    evidence_test_rnd <- basket_test_products[-c(rnd)]
    target_list_rnd[[counter]] <- as.data.frame(basket_test_products[c(rnd)])
    tuple_test_rnd <- matrix(0, nrow = ncol(R_item) - length(evidence_test_rnd), ncol = 2*M, dimnames = list(NULL, colnames(h)[-1]))
    tuple_test_rnd[, colnames(tuple_test_rnd) %in% paste("basket", evidence_test_rnd, sep = "_")] <- 1
    tuple_test_rnd[, !(colnames(tuple_test_rnd) %in% paste("target", evidence_test_rnd, sep = "_") | colnames(tuple_test_rnd) %like% "basket")] <- diag(1, nrow = nrow(tuple_test_rnd))
    testing_set_rnd <- as.data.frame(tuple_test_rnd)
    testing_set_rnd$target <- 1
    testing_set_rnd <- cbind(customer_ID = customer_unique, testing_set_rnd)
    
    # convert to python code in order to speeden things up as R is very slow in matrix multiplications
    output_rnd <- FM_value(as.matrix(testing_set_rnd[, -c(1, ncol(h))]), as.matrix(params[[3]]), as.matrix(params[[2]]), as.integer(M), as.integer(p))
    output_rnd <- as.data.frame(t(unlist(output_rnd, use.names=FALSE)))
    colnames(output_rnd) <- colnames(R_item)[!(colnames(R_item) %in% evidence_test_rnd)]

    predictions_rnd <- colnames(output_rnd[, order(output_rnd, decreasing = TRUE)])
    predictions_list_rnd[[counter]] <- as.data.frame(predictions_rnd[1:3])
  }  
  setTxtProgressBar(pb, i)
}


# initialize one-hot encoding tuples

N <- 1
M <- nrow(R_item)
p <- 2*M

h_user <- matrix(0, nrow = N, ncol = 1, dimnames = list("customer_ID"))
h_target <- matrix(0, nrow = M, ncol = 1, dimnames = list(paste("target", row.names(R_item), sep = "_")))
h_basket <- matrix(0, nrow = M, ncol = 1, dimnames = list(paste("basket", row.names(R_item), sep = "_")))
h <- t(rbind(rbind(h_user, h_target), h_basket))

test <- nrow(df_test2)*0.01
target_list <- list()
predictions_list <- list()
target_list_rnd <- list()
predictions_list_rnd <- list()

# initialize popularity based chance p(x)
popularity <- t(colSums(exp(df_train2[, -1])-1) / sum(colSums(exp(df_train2[, -1])-1)))

counter <- 0
pb <- txtProgressBar(min = 0, max = test, style = 3)
for (i in 1:test){
  customer_unique <- unique(df_test2[, "customer_ID"])[i]
  basket_train <- df_train2[df_train2$customer_ID == customer_unique, ]
  basket_train_products <- colnames(basket_train[, 2:ncol(basket_train)][, c(basket_train[, 2:ncol(basket_train)]) > 0])
  
  # generate tuples for training for each customer
  tuple_pos <- list()
  tuple_neg <- list()
  for (t in 1:length(basket_train_products)){
    target_train <- basket_train_products[t]
    evidence_train <- basket_train_products[-t]
    
    input <- h
    input[, 1] <- customer_unique
    input[, colnames(input) %in% paste("basket", evidence_train, sep = "_")] <- 1
    input[, colnames(input) %in% paste("target", target_train, sep = "_")] <- 1
    tuple_pos[[t]] <- as.data.frame(input)
    tuple_pos[[t]]$target <- 1
    
    sample <- sample(colnames(R_item)[!(colnames(R_item) %in% basket_train_products)], length(basket_train_products) + 1)
    input2 <- h
    input2[, 1] <- customer_unique
    input2[, colnames(input2) %in% paste("basket", sample[1:length(basket_train_products)], sep = "_")] <- 1
    input2[, colnames(input2) %in% paste("target", sample[length(basket_train_products) + 1], sep = "_")] <- 1
    tuple_neg[[t]] <- as.data.frame(input2)
    tuple_neg[[t]]$target <- -1
  }
  tuple_pos_matrix <- as.data.frame(rbindlist(tuple_pos))
  tuple_neg_matrix <- as.data.frame(rbindlist(tuple_neg))
  training_set <- rbind(tuple_pos_matrix, tuple_neg_matrix)
  
  # call Python code to learn the optimal parameters
  params <- FM_GD(training_set[-1])
  
  # loop all factorzation machine value of all basket items in order to calculate wHR(loo)
  user_index <- df_test2_weighted_customer_ID[df_test2_weighted_customer_ID$customer_ID %in% customer_unique, ]$index
  for (q in 1:length(user_index)){
    index <- user_index[q]
    hit <- list()
    counter <- counter + 1
    for (j in 1:length(df_test2_weighted[[index]]$target)){
      basket_test <- df_test2_weighted[[index]]$evidence[[j]][, -c(1:2)]
      basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]

      # generate test tuples
      evidence_test <- basket_test_products
      tuple_test <- matrix(0, nrow = ncol(R_item) - length(evidence_test), ncol = 2*M, dimnames = list(NULL, colnames(h)[-1]))
      tuple_test[, colnames(tuple_test) %in% paste("basket", evidence_test, sep = "_")] <- 1
      tuple_test[, !(colnames(tuple_test) %in% paste("target", evidence_test, sep = "_") | colnames(tuple_test) %like% "basket")] <- diag(1, nrow = nrow(tuple_test))
      testing_set <- as.data.frame(tuple_test)
      testing_set$target <- 1
      testing_set <- cbind(customer_ID = customer_unique, testing_set)
      
      # convert to python code in order to speeden things up as R is very slow in matrix multiplications
      output <- FM_value(as.matrix(testing_set[, -c(1, ncol(h))]), as.matrix(params[[3]]), as.matrix(params[[2]]), as.integer(M), as.integer(p))
      output <- as.data.frame(t(unlist(output, use.names=FALSE)))
      colnames(output) <- colnames(R_item)[!(colnames(R_item) %in% evidence_test)]

      predictions <- colnames(output[, order(output, decreasing = TRUE)])
      hit$predictions[j] <- predictions[1]
    }
    # calculate wHR(loo) 
    predictions_list[[counter]] <- wHR(hit$predictions, df_test2_weighted[[index]]$target, popularity[, colnames(popularity) %in% hit$predictions])    
  }
  
  setTxtProgressBar(pb, i)
}
