
# Threshold Independent Stochastic Primal-Dual Algorithm for L1 Regularized Logistic Regression------------

TISP = function( lambda,n_iter, data_train, data_test)
{
  eta = 1 / nrow(data_train$y) # Set learning rate based on the number of rows in training data
  w_new = as.matrix(rep(0, ncol(data_train$x))) # Initialize weights
  for (n in 1:n_iter) {
    w = w_new
    pred = data_train$x %*% w
    v = as.matrix(data_train$y - (1 + exp(-pred))^(-1))
    # Updating coefficients with L1 regularization
    w_new_theta = w + (1 / eta) * t(data_train$x) %*% v
    w_new = ifelse(abs(w_new_theta) > lambda, w_new_theta, 0)
  }

  # Function to calculate metrics and construct the output data frame
  calculate_metrics <- function(w, data, y_true) {
    link = data$x %*% w
    roc_obj = roc(as.numeric(y_true), as.numeric(link))
    threshold = as.numeric(coords(roc_obj, "best", ret = "threshold", drop=TRUE)[1])
    y_hat = as.factor(ifelse(link > threshold, 1, 0))
    levels(y_hat) = c("0", "1")
    cm = confusionMatrix(as.factor(y_true), y_hat)
    return(as.numeric(1 - cm$byClass['Balanced Accuracy']))
  }

  train_miss = calculate_metrics(w_new, data_train, data_train$y)
  test_miss = calculate_metrics(w_new, data_test, data_test$y)
  num_feature = length(which(w_new != 0))

  out = data.frame(
    lambda = lambda,
    Feature = num_feature,
    Miss.Train = train_miss,
    Miss.Test = test_miss
  )

  colnames(out) = c("lambda", "Feature", "Miss.Train", "Miss.Test")
  return(out)
}

# Missclassification Plot---------------

Miss_plot = function(n_iter,lambda,eta,data)
{
  w_new = as.matrix(rep(0, ncol(data$x)))
  n = 1
  train_miss = array()
  iteration = array()
  y_train = data$y
  invisible(while(n <= n_iter){
    w = w_new
    pred = data$x %*% w
    v = as.matrix(data$y - (1+exp(-pred))^(-1))
    #updating coefficients
    w_new_theta = w + (1/eta)*t(data$x)%*%v
    w_new = ifelse(abs(w_new_theta) > lambda,w_new_theta,0)
    link_train = data$x %*% w_new
    roc_train = roc(as.numeric(y_train), as.numeric(link_train))
    threshold_train = as.numeric(coords(roc_train, "best", ret = "threshold",drop=TRUE)[1])
    y_hat_train = as.factor(ifelse(link_train > threshold_train, 1, 0))
    levels(y_hat_train) = c("0", "1")
    cm_train = confusionMatrix(as.factor(y_train), as.factor(y_hat_train))
    train_miss[n] = as.numeric(1 - cm_train$byClass['Balanced Accuracy'])
    #keeping iteration number
    iteration[n] = n
    #updating iteration
    n = n + 1
  } )
  data1=data.frame(iteration,train_miss)
  miss.plot = ggplot(data1, aes(x = iteration)) +
    geom_line(aes(y = train_miss))+
    ylab('Missclassification Errors')+xlab('Iteration')
  return(miss.plot)
}

# Train vs Test ROC Plot----------------

Roc_plot = function(n_iter,lambda,eta,train,test)
{
  w_new = as.matrix(rep(0, ncol(train$x)))
  n = 1
  my.list = list()
  y_train = train$y
  y_test = test$y
  while (n <= n_iter)
  {
    w = w_new
    pred = train$x %*% w
    v = as.matrix(train$y - (1+exp(-pred))^(-1))
    #updating coefficients
    w_new_theta = w + (1/eta)*t(train$x)%*%v
    w_new = ifelse(abs(w_new_theta) > lambda,w_new_theta,0)
    n = n + 1
  }
  link_train = train$x %*% w_new
  roc_train = roc(as.numeric(y_train), as.numeric(link_train))
  link_test = test$x %*% w_new
  roc_test = roc(as.numeric(y_test), as.numeric(link_test))
  plot = ggroc(list(Train = roc_train, Test = roc_test ))+
    geom_abline(slope=1,intercept = 1,color = "blue")
  return(plot)
}
