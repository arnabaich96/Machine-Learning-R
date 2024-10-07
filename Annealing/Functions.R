# FSA variable selection method for linear models and binary classification with the logistic loss,
annealing = function(k=30, data_train, data_test,n_iter=300, eta=1/nrow(data_train$y), mu=100)
{
  w_new = as.matrix(rep(0, ncol(data_train$x)))
  n = 1
  x_tr_new = data_train$x
  x_te_new = data_test$x
  my.list = list()
  M = array()
  y_train = data_train$y
  y_test = data_test$y

  while (n <= n_iter) {
    x_tr = as.matrix(x_tr_new)
    x_te = as.matrix(x_te_new)
    w = w_new
    pred = x_tr %*% w
    v = as.matrix(y_train - (1 + exp(-pred))**(-1))
    grad = t(x_tr) %*% v
    w_new_theta = w + eta * grad
    w_new_sign = sign(w_new_theta)

    M[n] = k + (nrow(w_new) - k) * max(0, (n_iter - 2 * n) / (2 * n * mu + n_iter))

    df1 = data.frame(cbind(w_new_theta**2, w_new_sign, t(x_tr)))
    df2 = data.frame(cbind(w_new_theta**2, w_new_sign, t(x_te)))

    d1 = top_n(df1, M[n], X1)
    d2 = top_n(df2, M[n], X1)

    x_tr_new = t(d1[,-c(1,2)])
    x_te_new = t(d2[,-c(1,2)])
    w_new = as.matrix(sqrt(d1[,1]) * d1[,2])
    n = n + 1
  }

  link_train = x_tr_new %*% w_new
  roc_train = roc(as.numeric(y_train), as.numeric(link_train))
  threshold_train = as.numeric(coords(roc_train, "best", ret = "threshold", drop=TRUE)[1])
  y_hat_train = as.factor(ifelse(link_train > threshold_train, 1, -1))
  levels(y_hat_train) = c("-1", "1")
  cm_train = confusionMatrix(as.factor(y_train), y_hat_train)
  train_miss = as.numeric(1 - cm_train$byClass['Balanced Accuracy'])

  link_test = x_te_new %*% w_new
  roc_test = roc(as.numeric(y_test), as.numeric(link_test))
  threshold_test = as.numeric(coords(roc_test, "best", ret = "threshold", drop=TRUE)[1])
  y_hat_test = as.factor(ifelse(link_test > threshold_test, 1, -1))
  levels(y_hat_test) = c("-1", "1")
  cm_test = confusionMatrix(as.factor(y_test), y_hat_test)
  test_miss = as.numeric(1 - cm_test$byClass['Balanced Accuracy'])

  table = data.frame(
    Feature = k,
    Miss.Train = train_miss,
    Miss.Test = test_miss
  )

  return(table)
}


# Loss function Curve
Loss_plot = function(data,n_iter=300,k=300,s=0.001,mu=100)
{
  w_new = as.matrix(rep(0, ncol(data$x)))
  eta=1/nrow(data$y)
  n = 1
  x_new = data$x
  iteration = array()
  y = data$y
  loss = array()
  M = array()
  # Updating w

  while (n <= n_iter)
  {
    x = x_new
    w = w_new
    link = x %*% w
    #Calculating Gradient
    v = as.matrix(y- (1+exp(-link))^(-1))
    grad = t(x) %*% v
    #updating coefficients
    w_new_theta = w - (eta*grad)
    w_new_sign = sign(w_new_theta)
    M[n] = k + (nrow(w_new)-k)*max(0,(n_iter-2*n)/(2*n*mu + n_iter))
    df = data.frame(cbind(w_new_theta^2,w_new_sign,t(x)))
    d = top_n(df,M[n],X1)
    x_new = t(d[,-c(1,2)])
    w_new = as.matrix(sqrt(d[,1])*d[,2])
    loss[n] = nrow(y)**(-1)*(log(1+exp(t(y) %*% x_new %*% w_new))- t(y) %*% x_new %*% w_new)
    +s*sqrt(sum(w_new**2))
    iteration[n]=n
    n = n + 1
  }
  out=data.frame(iteration,loss)
 ggplot(out, aes(x = iteration)) + geom_line(aes(y = loss)) +
    ylab('Loss') + xlab('Iteration')+ ylim(min(loss)-1,max(loss)+1)
}


# ROC Plot
Roc_plot = function(train,test,n_iter=300,eta=1/nrow(data_train$y),k=300,mu=100)
{

    w_new = as.matrix(rep(0, ncol(data_train$x)))
    n = 1
    x_tr_new = train$x
    x_te_new  = test$x
    M = array()
    train_miss = array()
    iteration = array()
    y_train =train$y
    y_test = test$y

    # Updating w

    while (n <= n_iter)
    {
      x_tr = as.matrix(x_tr_new)
      x_te = as.matrix(x_te_new)
      w = w_new
      pred = x_tr %*% w
      v = as.matrix(y_train- (1+exp(-pred))^(-1))
      grad = t(x_tr) %*% v
      w_new_theta = w - as.matrix(eta*grad)
      w_new_sign = sign(w_new_theta)
      M[n] = k + (nrow(w_new)-k)*max(0,(n_iter-2*n)/(2*n*mu + n_iter))
      df1 = data.frame(cbind(w_new_theta**2,w_new_sign ,t(x_tr)))
      df2 = data.frame(cbind(w_new_theta**2,w_new_sign ,t(x_te)))
      d1= top_n(df1,M[n],X1)
      d2= top_n(df2,M[n],X1)
      x_tr_new = t(d1[,-c(1,2)])
      x_te_new = t(d2[,-c(1,2)])
      w_new = as.matrix(sqrt(d1[,1])*d1[,2])
      n = n + 1
    }
    link_train = x_tr_new %*% w_new
    roc_train = roc(as.numeric(y_train), as.numeric(link_train))
    link_test = x_te_new%*% w_new
    roc_test = roc(as.numeric(y_test), as.numeric(link_test))
   ggroc(list(Train = roc_train, Test = roc_test ))+
      geom_abline(slope=1,intercept = 1,color = "blue")+
      ggtitle('For 300 Features')
  }
