f_csfe <- function(x, y_bench, y_real) {
  
  #' Calcula o Cumulative Squared Forecast Error (CSFE)
  #'
  #' Esta função calcula o erro quadrático acumulado da previsão em relação a um benchmark e 
  #' os valores reais. O resultado permite avaliar a precisão do modelo em comparação com o benchmark.
  #'
  #' @param x Um vetor numérico contendo as previsões do modelo.
  #' @param y_bench Um vetor numérico contendo as previsões do benchmark.
  #' @param y_real Um vetor numérico contendo os valores reais observados.
  #' @return Um vetor numérico que contém o erro quadrático acumulado para cada ponto no tempo.
  #'
  #' @examples
  #' f_csfe(c(1, 2, 3), c(1.5, 2.5, 3.5), c(1, 2, 3)) # Retorna o erro quadrático acumulado
  #'
  
  error_bench <- (y_bench - y_real)^2
  error_x <- (x - y_real)^2
  result <- cumsum(error_bench - error_x)
  return(result)
}

csfe = function(model, benchmark, y_real){
  
  #' Calcula CSFE para Diferentes Horizontes
  #'
  #' Esta função calcula o Cumulative Squared Forecast Error (CSFE) para diferentes horizontes de previsão
  #' a partir das previsões de um modelo e de um benchmark.
  #'
  #' @param model Um objeto contendo as previsões do modelo, com colunas representando diferentes horizontes.
  #' @param benchmark Um objeto contendo as previsões do benchmark, com colunas correspondendo aos mesmos horizontes.
  #' @param y_real Um vetor numérico contendo os valores reais observados.
  #' @return Uma matriz com os erros quadráticos acumulados para cada horizonte de previsão.
  #'
  #' @examples
  #' csfe_results <- csfe(model, benchmark, y_real)
  #' print(csfe_results)
  #'
  
  h1 = f_csfe(model$forecast[,1], benchmark$forecasts[,1], y_real = y_real)
  h2 = f_csfe(model$forecast[,2], benchmark$forecasts[,2], y_real = y_real)
  h3 = f_csfe(model$forecast[,3], benchmark$forecasts[,3], y_real = y_real)
  h4 = f_csfe(model$forecast[,4], benchmark$forecasts[,4], y_real = y_real)
  h5 = f_csfe(model$forecast[,5], benchmark$forecasts[,5], y_real = y_real)
  h6 = f_csfe(model$forecast[,6], benchmark$forecasts[,6], y_real = y_real)
  h9 = f_csfe(model$forecast[,7], benchmark$forecasts[,7], y_real = y_real)
  h12 = f_csfe(model$forecast[,7], benchmark$forecasts[,7], y_real = y_real)
  
  cbind(h1, h2, h3, h4, h5, h6, h9, h12)
  
}