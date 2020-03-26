# This functin is used to select best model by cross validation

cv = function(x, y, parameters, model, predict, loss, n_fold = 5){
  loss_fold = list()
  for (fold in 0:(n_fold - 1)) {
    fold = c()
    for (parameter in parameters) {
      test_index = ((fold * length(y) / n_fold) + 1):(((fold + 1) * length(y) / n_fold))
      train_index = (1:length(y))[-test_index]
      train_x = x[train_index, ]
      train_y = y[train_index]
      test_x = x[test_index, ]
      test_y = y[test_index]
      train_model = model(train_x, train_y, parameter)
      predict_y = predict(train_model, test_x)
      test_loss = loss(predict_y, test_y)
      fold = c(fold, test_loss)
    }
    loss_fold = list(loss_fold, paste("fold_", fold) = fold)
  }
  
  return(list(lambda = parameter, loss = loss_fold))
}