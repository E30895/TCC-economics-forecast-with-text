

call_models = function(data, model_name, model_function, variable, type = "default"){
  
  #' Chamadas para Modelos de Previsão
  #'
  #' Esta função aplica um modelo de previsão a um conjunto de dados, utilizando uma janela deslizante para gerar previsões.
  #'
  #' @param data Um `data.frame` ou matriz contendo os dados a serem modelados.
  #' @param model_name O nome do modelo a ser utilizado (apenas um rótulo).
  #' @param model_function A função que implementa o modelo de previsão a ser aplicado.
  #' @param variable O nome da variável dependente que será prevista.
  #' @param type O tipo de dados. O padrão é "default".
  #' @return Uma lista contendo os seguintes elementos:
  #' - me: O erro médio das previsões.
  #' - rmse: A raiz do erro quadrático médio das previsões.
  #' - forecasts: As previsões geradas pelo modelo para o horizonte definido.
  #'
  #' @examples
  #' df <- data.frame(time = 1:100, variable = rnorm(100))
  #' results <- call_models(data = df, model_name = "MyModel", model_function = my_model_function, variable = "variable")
  #'
  #' @export
  
  data = as.matrix(data)
  
  model_name <- model_name
  model_function <- model_function
  
  nwindows = nrow(data)*0.25
  y_out <- tail(data[, variable], nwindows)
  #y_out <- tail(data[, "BZEAMOM"], nwindows)

  #if (type == 'tb'){
  #  data <- as.data.frame(data) %>%
  #    select(-c(1)) %>% 
  #    as.matrix()
  #}
  
  
  model_list <- list()
  for_ind <- c(1, 2, 3, 4, 5, 6, 9, 12)
  #for_ind = c(1)
  
  for (i in for_ind) {
    model <- rolling_window(
      fn = model_function,
      df = data,
      nwindow = nwindows + i - 1,
      horizon = i,
      variable = variable,
      n_lags = 4
    )
    model_list[[i]] <- model
    cat(i, "\n")
  }
  
  
  forecasts <- Reduce(
    f = cbind,
    x = lapply(model_list, function(x) head(x$forecast, nwindows))
  ) %>% as.matrix()
  
  
  plot.ts(y_out)
  lines(forecasts[, 1], col = 2)
  
  rmse <- apply(forecasts, 2, f_rmse, y = y_out) %>% print()
  me = apply(forecasts, 2, f_me, y = y_out) %>% print()
  
  results = list(me = me, rmse = rmse, forecasts = forecasts)
  
  return(results)
}



