# SIMILARITY MATRIX BASED ON ALS
similarity_ALS <- function(obj){
  item_factors <- implicit_als(as.matrix(df_train2[, -1]))

  R_als <- item_factors %*% t(item_factors)
  colnames(R_als) <- colnames(R_item)
  row.names(R_als) <- colnames(R_item)
}

# APPLY ALS BASED RECOMMENDATIONS
d <- seq(0.1, 0.9, by = 0.1)
test <- nrow(df_test2)
n <- 1

w <- personalized_weight(0.7, n)

recommendations_als <- list()
recommendations_bsrw_als <- list()

for (i in 1:test){
  basket_test <- df_test2_pop_evidence[i, -c(1:2)]
  basket_test_products <- names(basket_test)[which(basket_test > 0, arr.ind = TRUE)[, "col"]]

  # calculate recommendation of ALS based model
  recommendations_als[[i]] <- prediction_weighted_sum(R_als, t(basket_test), w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)
  
  # calculate recommendations of hybrid model
  R_bsrw_basket_0.7 <- rowSums(cbind(R_bsrw_bn_0.7[[n]][, basket_test_products], 0))
  
  recommendations_bsrw_als[[i]] <- prediction_weighted_sum(R_als, R_bsrw_basket_0.7, w, df_test2_pop_evidence[i, ]$customer_ID, basket_test_products)

}
