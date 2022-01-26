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
