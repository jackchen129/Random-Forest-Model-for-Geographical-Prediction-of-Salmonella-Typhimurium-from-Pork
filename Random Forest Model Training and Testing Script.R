# MODEL TRAINING FUNCTION -----------------------------------------------
train_random_forest_model <- function(train_data) {
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    allowParallel = TRUE
  )
  
  model <- train(
    x = select(train_data, -SampleID, -country),
    y = train_data$country,
    method = "ranger",
    tuneGrid = expand.grid(
      mtry = floor(seq(2, sqrt(ncol(train_data) - 2), length.out = 5)),
      splitrule = "gini",
      min.node.size = 1
    ),
    metric = "ROC",
    importance = "permutation",
    num.trees = 1000,
    trControl = ctrl
  )
  
  model
}

# MODEL TESTING FUNCTION -----------------------------------------------
predict_random_forest <- function(model, test_data) {
  predictions <- predict(model, test_data)
  prob_predictions <- predict(model, test_data, type = "prob")
  
  list(
    predicted_class = predictions,
    predicted_probabilities = prob_predictions
  )
}