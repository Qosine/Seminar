coef_matrix <- matrix(data=0, nrow = 6, ncol = 6)
colnames(coef_matrix) = colnames(true_target_variables$predictors)
rownames(coef_matrix) = cbind("Awareness real","Awareness sim","Familiarity real ","Familiarity sim", "Consideration real", "Consideration sim")
for( i in 1:6){
  coef_matrix[1,i] <- logit.target.awareness$coefficients[[i]]
  coef_matrix[2,i] <- glm( sim_target_variables$awareness
                           ~ simulated_population,
                           family=binomial(link="logit"))$coefficients[[i]]
  coef_matrix[3,i] <- logit.target.familiarity$coefficients[[i]]
  coef_matrix[4,i] <- glm( sim_target_variables$familiarity
                           ~ simulated_population,
                           family=binomial(link="logit"))$coefficients[[i]]
  coef_matrix[5,i] <- logit.target.consideration$coefficients[[i]]
  coef_matrix[6,i] <- glm( sim_target_variables$consideration
                           ~ simulated_population,
                           family=binomial(link="logit"))$coefficients[[i]]
}
res <- list()
res[[1]] <- as.data.frame(coef_matrix)
res[[2]] <- as.data.frame(cor(simulated_population))
res[[3]] <-  cor(true_target_variables$predictors)
new_pred <- cbind(true_target_variables$predictors[,-3][,-2],true_target_variables$predictors[,3]+true_target_variables$predictors[,2])
vif <- vector()
for (i in 1:5){
  vif[i] <- 1/(1-summary(lm(new_pred[,i] ~ new_pred[,-i]))$r.squared)
}
res[[4]] <- vif
res <- list("coef_matrix" = res[[1]], "cor_sim" = res[[2]], "cor_real" = res[[3]], "vif" = vif)
mean(true_target_variables$predictors[,3])
write.xlsx(res[[1]],"./result1.xlsx")
write.xlsx(res[[2]],"./result2.xlsx")
write.xlsx(res[[3]],"./result3.xlsx")
write.xlsx(res[[4]],"./result4.xlsx")