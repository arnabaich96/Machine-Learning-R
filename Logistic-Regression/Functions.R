logistic = function(n_iter=300,
                    data_train,
                    data_test,
                    eta=0.001,
                    lambda = 0.0001,
                    w_init = rep(0, ncol(data_train$x)))
{
  w_new = as.matrix(w_init)
  X = as.matrix(data_train$x)
  n = 1
  my.list = list()
  log_likelihood = array()
  iteration = array()
  # Updating w
  while (n <= n_iter)
  {
    w = w_new
    #calculating log likelihood
    pred = X %*% w
    lik = data_train$y * pred - log(1 + exp(pred))
    log_likelihood[n] = sum(as.numeric(lik))

    #Calculating Gradient
    v = as.matrix(data_train$y - (exp(pred) / 1 + exp(pred)))
    grad = t(X) %*% v
    #updating coefficients
    w_new = w - eta * lambda * w + (eta / nrow(data_train$y)) * grad
    #keeping iteration number
    iteration[n] = n
    #updating iteration
    n = n + 1
  }

  #plotting log likelihood
  data = data.frame(iteration,log_likelihood)
  p <- ggplot(data, aes(x=iteration, y=log_likelihood)) +
    geom_line() +
    labs(title = "Log Likelihood vs Iteration",
         x = "Iteration",
         y = "Log Likelihood")
  my.list$Log.likelihood = ggplotly(p)
  #train data
  link_train = X %*% w_new
  prob_train = exp(link_train)/(1+exp(link_train))
  y_train = data_train$y
  roc_train = roc(as.numeric(y_train), as.numeric(prob_train))
  threshold_train = as.numeric(coords(roc_train, "best", ret = "threshold"))
  y_hat_train = as.factor(ifelse(prob_train > threshold_train, 1, 0))
  levels(y_hat_train) = c("0", "1")
  cm_train = confusionMatrix(as.factor(y_train), as.factor(y_hat_train))
  train_miss = as.numeric(1 - cm_train$byClass['Balanced Accuracy'])
  # test_data
  link_test = data_test$x %*% w_new
  y_test = data_test$y
  prob_test= exp(link_test)/(1+exp(link_test))
  roc_test = roc(as.numeric(y_test), as.numeric(prob_test))
  threshold_test = as.numeric(coords(roc_test, "best", ret = "threshold"))
  y_hat_test = as.factor(ifelse(prob_test > threshold_test, 1, 0))
  levels(y_hat_test) = c("0", "1")
  cm_test = confusionMatrix(as.factor(y_test), as.factor(y_test))
  test_miss = as.numeric(1 - cm_test$byClass['Balanced Accuracy'])
  # Thresholds and Misclassification
  my.list$Table = matrix(c(train_miss,threshold_train,test_miss,threshold_test),nrow=2,byrow=TRUE)
  rownames(my.list$Table) = c("Train","Test")
  colnames(my.list$Table) = c("Misclassification.Prob","Threshold")
  # Roc plot
  my.list$ROC.plot = ggroc(list(Train = roc_train, Test = roc_test ))+geom_abline(slope=1,intercept = 1,color = "blue")
  return(my.list)
}

