train_model <- function(train_data) {
  ctrl <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary,
    allowParallel = TRUE
  )
  model <- caret::train(
    x = dplyr::select(train_data, -Isolate, -country, -country_clean),
    y = train_data$country_clean,
    method = "ranger",
    importance = "permutation",
    num.trees = 750,
    metric = "ROC",
    trControl = ctrl
  )
  return(model)
}

evaluate_model <- function(model, test_data, clean2orig, output_dir) {
  probmat <- predict(model, test_data, type = "prob")
  predcls <- predict(model, test_data)
  
  preds <- dplyr::bind_cols(
    tibble::tibble(SampleID = test_data$Isolate, actual = test_data$country_clean),
    tibble::as_tibble(probmat),
    tibble::tibble(predicted = predcls)
  ) %>%
    dplyr::mutate(
      actual = factor(actual, levels = colnames(probmat)),
      predicted = factor(predicted, levels = colnames(probmat)),
      actual_orig    = clean2orig[actual],
      predicted_orig = clean2orig[predicted]
    )
  
  # ROC
  pos_class <- levels(preds$actual)[2] # USA
  roc_obj <- pROC::roc(preds$actual, preds[[pos_class]], levels = rev(levels(preds$actual)))
  auc_value <- pROC::auc(roc_obj)
  
  cm <- caret::confusionMatrix(preds$predicted, preds$actual, positive = pos_class)
  
  var_imp <- caret::varImp(model)$importance %>%
    tibble::rownames_to_column("Feature") %>%
    dplyr::arrange(desc(Overall))
  
  # Plots
  plot_confusion_matrix(cm, output_dir)
  plot_feature_importance(var_imp, output_dir)
  plot_predicted_probabilities(preds, pos_class, output_dir)
  
  list(
    predictions = preds,
    confusion_matrix = cm,
    variable_importance = var_imp,
    auc = as.numeric(auc_value)
  )
}