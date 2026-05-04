fit_party_logistic <- function(predictors, data, label = "") {
  dat <- data |>
    filter(party_id_3 %in% c("Democrat", "Republican")) |>
    mutate(party_id_3 = droplevels(factor(party_id_3))) |>
    select(party_id_3, any_of(predictors)) |>
    drop_na()
  n <- nrow(dat)
  Z <- sample(n, n / 2)
  train <- dat[Z, ]
  test  <- dat[-Z, ]
# fit logistic regression
  fit <- glm(party_id_3 ~ ., data = train, family = binomial)  
  print(summary(fit))
# predicted probabilities
  prob <- predict(fit, newdata = test, type = "response")       
  pred <- ifelse(prob > 0.5, levels(dat$party_id_3)[2], levels(dat$party_id_3)[1])
  pred <- factor(pred, levels = levels(dat$party_id_3))
  err  <- mean(pred != test$party_id_3)
  cat("\n", label, "LOGISTIC REGRESSION \n")
  cat("Test misclass:", round(err, 4), "| accuracy:", round(1 - err, 4), "\n")
  print(table(Predicted = pred, Actual = test$party_id_3))
  invisible(list(fit = fit, err = err))
}

# logreg can only be done without independents
demo_logistic_2party <- fit_party_logistic(demo_vars,  data = anes_data_2party, label = "DEMO 2-party")
issue_logistic_2party <- fit_party_logistic(issue_vars, data = anes_data_2party, label = "ISSUE 2-party")
