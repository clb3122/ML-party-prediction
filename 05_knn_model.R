library(tidyverse)
library(class)

set.seed(123)

fit_party_knn <- function(predictors, data, label = "", k_max = 20) {
  dat <- data |>
    filter(party_id_3 %in% c("Democrat", "Independent", "Republican")) |>
    mutate(party_id_3 = droplevels(factor(party_id_3))) |>
    select(party_id_3, any_of(predictors)) |>
    drop_na()
  # convert all predictors to numeric with dummies
  X <- model.matrix(party_id_3 ~ ., data = dat)[, -1]
  Y <- dat$party_id_3
  n <- nrow(X)
  Z <- sample(n, n / 2)
  X.training <- X[Z, ]
  X.testing <- X[-Z, ]
  Y.training <- Y[Z]
  Y.testing <- Y[-Z]
  # find optimal K
  class.rate <- rep(0, k_max)
  for (K in 1:k_max) {
    knn.result <- knn(X.training, X.testing, Y.training, k = K)
    class.rate[K] <- mean(knn.result == Y.testing)
  }

  best_k <- which.max(class.rate)
  best_rate <- class.rate[best_k]

  plot(1:k_max, class.rate, type = "b",
       xlab = "K", ylab = "Classification rate",
       main = paste(label, "- KNN accuracy by K"))
  abline(v = best_k, lty = 2, col = "red")
  cat("\n", label, "KNN \n")
  cat("Optimal K:", best_k, "\n")
  cat("Accuracy: ", round(best_rate, 4), "\n")
  cat("Misclassification:", round(1 - best_rate, 4), "\n")
  knn.best <- knn(X.training, X.testing, Y.training, k = best_k)
  print(table(Predicted = knn.best, Actual = Y.testing))
  invisible(list(class.rate = class.rate, best_k = best_k, best_rate = best_rate))
}

demo_knn  <- fit_party_knn(demo_vars,  data = anes_model_data, label = "DEMO") # Demo KNN with Ind voters
issue_knn <- fit_party_knn(issue_vars, data = anes_model_data, label = "ISSUE") # Issue KNN with Ind voters

# without independents
demo_knn_2party  <- fit_party_knn(demo_vars,  data = anes_model_data_2party, label = "DEMO 2-party")
issue_knn_2party <- fit_party_knn(issue_vars, data = anes_model_data_2party, label = "ISSUE 2-party")
