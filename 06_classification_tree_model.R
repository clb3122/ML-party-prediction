library(tidyverse)
library(tree)

set.seed(123)

fit_party_tree <- function(predictors, data, label = "") {
    dat <- data |>
        filter(party_id_3 %in% c("Democrat", "Independent", "Republican")) |>
        mutate(party_id_3 = droplevels(factor(party_id_3))) |>
        select(party_id_3, any_of(predictors))
    n <- nrow(dat)
    train_id <- sample(seq_len(n), size = floor(0.8 * n))
    train <- dat[train_id, ]
    test <- dat[-train_id, ]
    # fit tree
    fit <- tree(party_id_3 ~ ., data = train)
    # summary and text output
    cat("\n", label, "TREE SUMMARY \n")
    print(summary(fit))
    print(fit)
    # plot unpruned tree
    plot(fit)
    text(fit, pretty = 0)
    title(main = paste(label, "- Unpruned"))
    # training and test error
    pred_train <- predict(fit, newdata = train, type = "class")
    train_err <- mean(pred_train != train$party_id_3)
    pred_test <- predict(fit, newdata = test, type = "class")
    test_err <- mean(pred_test != test$party_id_3)
    cat("Train misclass:", round(train_err, 4), "| accuracy:", round(1 - train_err, 4), "\n")
    cat("Test  misclass:", round(test_err,  4), "| accuracy:", round(1 - test_err,  4), "\n")
    print(table(Predicted = pred_test, Actual = test$party_id_3))
    # CV + prune
    cv <- cv.tree(fit, FUN = prune.misclass)
    plot(cv$size, cv$dev, type = "b",
         xlab = "Tree size (# terminal nodes)",
         ylab = "CV misclassification",
         main = paste(label, "- CV"))
    best_size <- cv$size[which.min(cv$dev)]
    pruned <- prune.tree(fit, best = best_size)
    plot(pruned)
    text(pruned, pretty = 0)
    title(main = paste(label, "- Pruned (size =", best_size, ")"))
    pred_pruned <- predict(pruned, newdata = test, type = "class")
    pruned_err <- mean(pred_pruned != test$party_id_3)
    cat("Pruned misclass:", round(pruned_err, 4), "| accuracy:", round(1 - pruned_err, 4), "\n")
    print(table(Predicted = pred_pruned, Actual = test$party_id_3))
    invisible(list(unpruned = fit, pruned = pruned, cv = cv,
                   train_err = train_err, test_err = test_err, pruned_err = pruned_err))
}

# with independents
demo_trees  <- fit_party_tree(demo_vars,  data = anes_model_data, label = "DEMO")
issue_trees <- fit_party_tree(issue_vars, data = anes_model_data, label = "ISSUE")

# without independents
demo_trees_2party  <- fit_party_tree(demo_vars,  data = anes_model_data_2party, label = "DEMO 2-party")
issue_trees_2party <- fit_party_tree(issue_vars, data = anes_model_data_2party, label = "ISSUE 2-party")
