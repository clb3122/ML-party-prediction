library(tidyverse)
library(glmnet)
set.seed(123)

fit_party_ridge <- function(predictors, data, label = "") {
  dat <- data |>
    filter(party_id_3 %in% c("Democrat", "Independent", "Republican")) |>
    mutate(party_id_3 = droplevels(factor(party_id_3))) |>
    select(party_id_3, any_of(predictors)) |>
    drop_na()
  X <- model.matrix(party_id_3 ~ ., data = dat)[, -1]
  Y <- dat$party_id_3
  # Cross-validated ridge regression
  cvfit <- cv.glmnet(
    X, Y,
    alpha = 0,
    family = "multinomial",
    type.measure = "class")
  best.lambda <- cvfit$lambda.min
  # Plot lambdas against standard deviation
  plot(CV$lambda, CV$cvsd,
       xlab = "lambda", ylab = "CV Standard Deviation",
       main = paste(label, "- Ridge regression accuracy by lambda"))
  abline(v = best.lambda, lty = 2, col = "red")
  # Predictions (on training data)
  pred.class <- predict(cvfit, newx = X, s = best.lambda, type = "class")
  pred.class <- as.vector(pred.class)
  # Confusion matrix
  conf.mat <- table(Predicted = pred.class, Actual = Y)
  # Cross-validated error
  cv.error <- cvfit$cvm[which(cvfit$lambda == best.lambda)]
  # Final model refit at best lambda
  final.model <- glmnet(
    X, Y,
    alpha = 0,
    family = "multinomial",
    lambda = best.lambda
  )
  cat("\n", label, "Ridge Regression\n")
  cat("Optimal lambda:", best.lambda, "\n")
  cat("CV Accuracy:", round(1 - cv.error, 4), "\n")
  cat("CV Misclassification:", round(cv.error, 4), "\n\n")
  print(conf.mat)
  cat("\n\nLargest Coefficients:\n")
  coef_list <- coef(final.model)
  for (cls in names(coef_list)) {
    cat("\nClass:", cls, "\n")
    coefs <- as.matrix(coef_list[[cls]])
    coefs <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]
    top <- head(coefs[order(abs(coefs[,1]), decreasing = TRUE), , drop = FALSE], 10)
    print(top)
  }
  invisible(list(
    model = final.model,
    cvfit = cvfit,
    best.lambda = best.lambda,
    confusion.matrix = conf.mat,
    cv.error = cv.error
  ))
}

issue_ridge <- fit_party_ridge(issue_vars, data = anes_model_data, label = "ISSUE") # Issue Ridge Regression with Ind voters
issue_ridge <- fit_party_ridge(issue_vars, data = anes_model_data, label = "ISSUE") # Issue Ridge Regression with Ind voters

# without independents
demo_ridge_2party  <- fit_party_ridge(demo_vars,  data = anes_data_2party, label = "DEMO 2-party")
issue_ridge_2party <- fit_party_ridge(issue_vars, data = anes_data_2party, label = "ISSUE 2-party")

