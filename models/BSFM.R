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
}
