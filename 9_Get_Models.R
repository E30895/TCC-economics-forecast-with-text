
###############################################
######### ERRO QUADRATICO MEDIO ###############
###############################################

f_rmse = function(x, y){
  sqrt(mean((x - y)^2))
}


###############################################
################ ERRO MEDIO ###################
###############################################

f_me = function(x,y){
  mean(y - x)
}

###############################################
################ SARIMA MODEL #################
###############################################

get_sarima = function(ind, df, variable, horizon, n_lags){
  
  library(tidyverse)
  library(forecast)
  
  data_in = dataprep(
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags)
  
  #INICIANDO AS VARIAVEIS
  y_in = data_in$y_in
  
  reg_arima = auto.arima(
    y = y_in, 
    stepwise = F, 
    approximation = F, 
    stationary = F,
    seasonal = T,
    start.p = 0,
    start.q = 0)
  
  print(reg_arima)
  
  for_arima_aux = forecast(
    object = reg_arima, 
    h = horizon)
  
  forecasts = for_arima_aux$mean
  
  results = list(forecasts = forecasts)
  
  return (results)
  
}

###############################################
############### LASSO FUNCTION ################
###############################################

get_lasso = function(ind, df, variable, horizon, n_lags){
  
  set.seed(100)
  
  library(glmnet)
  library(forecast)
  
  #PREPARANDO OS DADOS
  data_in = dataprep(
    type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = 4)
  
  #INICIANDO AS VARIAVEIS
  y_in = data_in$y_in
  x_in = data_in$x_in
  x_out = data_in$x_out
  
  #ESTIMANDO O MODELO
  cv_lasso = cv.glmnet(
    x = as.matrix(x_in),
    y = y_in,
    alpha = 1,
    intercept = T,
    standardize = T,
    nfolds = 5)
  
  grid <- 10^seq(10, -2, length = 100)
  out = glmnet(x_in, y_in, alpha = 1, lambda = grid)
  
  lasso.coef = predict(
    out, 
    type = "coefficients",
    s = cv_lasso$lambda.min)[1:41,]
  
  print(lasso.coef[lasso.coef != 0])
  
  #PREVISAO
  opt_lasso = predict(
    cv_lasso,
    s = cv_lasso$lambda.min,
    newx = as.matrix(x_out, nrow = 1))
  
  results = list(forecast = opt_lasso)
  
  return(results)
  
}

###############################################
############# ELASTICNET MODEL ################
###############################################

get_elasticnet <- function(ind, df, variable, horizon, n_lags) {
  library(forecast)
  library(glmnet)
  library(caret)
  
  # PREPARANDO OS DADOS
  data_in <- dataprep(
    type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  # INICIANDO AS VARIAVEIS
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  set.seed(100)
  cv_5 <- trainControl(method = "cv", number = 5) # CROSS VALIDATION
  
  myGrid <- expand.grid(
    alpha = seq(0, 1, length = 10), # range for alpha
    lambda = exp(seq(from = log(0.5), to = log(25000), length = 200)) # Broad range for lambda
  )
  
  cv_enet <- train(
    x = as.data.frame(x_in),
    y = as.numeric(y_in),
    method = "glmnet",
    trControl = cv_5,
    tuneGrid = myGrid,
    metric = "RMSE",
    intercept = T,
    standardize = T
  )
  
  # Assuming 'cv_enet' is your trained model object
  best_lambda <- cv_enet$finalModel$lambdaOpt %>% print()
  best_alpha <- cv_enet$finalModel$tuneValue[1] %>% as.numeric() %>% print()
  opt_enet <- coef(cv_enet$finalModel, cv_enet$finalModel$lambdaOpt)
  
  x <- rep(0, ncol(x_in) + 1)
  x[opt_enet@i + 1] <- opt_enet@x
  names(x) <- c("cte", colnames(x_in))
  
  glmnet_aux <- glmnet(
    x = x_in,
    y = y_in,
    family = "gaussian",
    lambda = best_lambda,
    alpha = best_alpha,
    standardize = T,
    intercept = T
  )
  
  opt_elasticnet <- predict(
    glmnet_aux,
    s = best_lambda,
    newx = x_out,
    type = "response"
  ) # acho que retorna "the fitted values"
  
  results <- list(forecast = opt_elasticnet)
  
  return(results)
}
###############################################
############ BOOSTING FUNCTION ################
###############################################

#get_boosting = function(ind, df, variable, horizon, n_lags) {
#  
#  library(mboost)
#  library(forecast)
#  
#  # INICIALIZACAO DE VARIAVEIS
#  set.seed(100)
#  
#  data_in = dataprep(
#    type = 'tb',
#    ind = ind,
#    df = df,
#    variable = variable,
#    horizon = horizon,
#    n_lags = n_lags
#  )
#  
#  y_in = data_in$y_in
#  x_in = data_in$x_in
#  x_out = data_in$x_out
#  
#  # AJUSTE DO MODELO DE BOOSTING
#  reg_full = glmboost(
#    y = y_in,
#    x = as.matrix(x_in),
#    # offset = 0,
#    center = TRUE,
#    control = boost_control(mstop = 100, nu = 0.1)
#  )
#  
#  # DETERMINACAO DO NUMERO OTIMO DE ITERACOES
#  cv10f = cv(model.weights(reg_full), type = "kfold", B = 5)
#  cv_seq = cvrisk(reg_full, folds = cv10f, papply = lapply)
#  m_opt = mstop(cv_seq)
#  
#  # AJUSTE DO MODELO COM O NUMERO OTIMO DE ITERACOES
#  
#  reg_opt = reg_full[m_opt]
#  
#  # PREVISAO PARA A JANELA DE TESTE
#  opt_boosting = predict(
#    object = reg_opt,
#    newdata = matrix(x_out, nrow = 1)
#  )
#  
#  # RESULTADOS
#  results = list(
#    forecast = opt_boosting,
#    outputs = list(
#      m_opt = m_opt,
#      reg_opt = reg_opt
#    )
#  )
#  return(results)
#}


get_boosting <- function(ind, df, variable, horizon, n_lags) {
  library(mboost)
  library(forecast)
  
  # INICIALIZACAO DE VARIAVEIS
  set.seed(100)
  
  data_in <- dataprep(
    type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  # AJUSTE DO MODELO DE BOOSTING
  reg_full <- glmboost(
    y = y_in,
    x = as.matrix(x_in),
    offset = 0, # mean(y_in),
    center = TRUE,
    control = boost_control(mstop = 300, nu = 0.1)
  )
  
  # DETERMINACAO DO NUMERO OTIMO DE ITERACOES
  cv5f <- cv(model.weights(reg_full), type = "kfold", B = 5)
  cv_seq <- cvrisk(reg_full, folds = cv5f, papply = lapply)
  m_opt <- mstop(cv_seq)
  
  # AJUSTE DO MODELO COM O NUMERO OTIMO DE ITERACOES
  
  reg_opt <- reg_full[m_opt]
  
  # PREVISAO PARA A JANELA DE TESTE
  opt_boosting <- predict(
    object = reg_opt,
    newdata = matrix(x_out, nrow = 1)
  ) %>% as.vector() + mean(y_in)
  
  # RESULTADOS
  results <- list(
    forecast = opt_boosting,
    outputs = list(
      m_opt = m_opt,
      reg_opt = reg_opt
    )
  )
  return(results)
}

