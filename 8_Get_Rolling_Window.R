rolling_window <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  
  #' Janela Movel para Modelagem
  #'
  #' Esta função aplica uma função especificada a uma janela deslizante de dados em um `data.frame` para realizar previsões.
  #'
  #' @param fn A função a ser aplicada em cada janela deslizante. Esta função deve retornar um objeto com as previsões e saídas.
  #' @param df Um `data.frame` contendo os dados a serem processados.
  #' @param nwindow O tamanho da janela deslizante. O padrão é 1.
  #' @param horizon O horizonte de previsão, que determina quantas observações devem ser projetadas para frente.
  #' @param variable O nome da variável dependente a ser utilizada na análise.
  #' @param ... Argumentos adicionais a serem passados para a função `fn`.
  #' @return Uma lista com dois elementos:
  #' - forecast: Um vetor com as previsões resultantes de cada aplicação da função nas janelas deslizantes.
  #' - outputs: Uma lista contendo as saídas resultantes de cada aplicação da função nas janelas deslizantes.
  #'
  #' @examples
  #' df <- data.frame(data = 1:100, variable = rnorm(100))
  #' result <- rolling_window(fn = my_forecast_function, df = df, nwindow = 5, horizon = 1, variable = 'variable')
  #'
  #' @export
  
  #ind <- seq_len(nrow(df))
  window_size <- nrow(df) - nwindow
  indmat <- matrix(NA, window_size, nwindow)
  indmat[1, ] <- seq_len(ncol(indmat))
  for (i in 2:nrow(indmat)) {
    indmat[i, ] <- indmat[i - 1, ] + 1
  }
  
  rw <- apply(
    X = indmat,
    MARGIN = 2,
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs <- lapply(rw, function(x) x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}

