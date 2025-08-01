train_and_analyze <- function(train_data) {
  ctrl <- trainControl(
    method = "repeatedcv", number = 5, repeats = 3,
    classProbs = TRUE, summaryFunction = twoClassSummary,
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
  
  imp_data <- varImp(model)$importance %>%
    rownames_to_column("Plasmid") %>%
    arrange(desc(Overall))
  
  list(model = model, importance = imp_data)
}