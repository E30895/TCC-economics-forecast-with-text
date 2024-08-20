dataprep <- function(type = '...', ind, df, variable, horizon, n_lags = 4) {
  
  #' Preparação de Dados para Modelagem
  #'
  #' Esta função prepara os dados para análise, criando variáveis de defasagem e separando as variáveis dependentes e independentes.
  #'
  #' @param type Um caractere que define o tipo de preparação de dados. Pode ser 'tb' para uma estrutura de tabela ou outro valor conforme necessário.
  #' @param ind Um vetor de índices para selecionar as linhas do `data.frame`.
  #' @param df Um `data.frame` contendo os dados a serem preparados.
  #' @param variable O nome da variável dependente a ser utilizada na análise.
  #' @param horizon O horizonte de previsão, que determina quantas observações devem ser projetadas para frente.
  #' @param n_lags O número de defasagens a serem criadas. Deve ser um inteiro maior que 1. O padrão é 4.
  #' @return Uma lista com três elementos:
  #' - x_in: Um `data.frame` contendo as variáveis independentes de entrada, excluindo os últimos `horizon` registros.
  #' - x_out: Um vetor contendo a última observação das variáveis independentes (a previsão).
  #' - y_in: Um vetor com os valores correspondentes da variável dependente para as observações de entrada.
  #'
  #' @examples
  #' df <- data.frame(data = 1:100, variable = rnorm(100))
  #' result <- dataprep(type = 'tb', ind = 1:80, df = df, variable = 'variable', horizon = 1, n_lags = 4)
  #'
  #' @export

  if (n_lags <= 1) {
    stop("n_lags deve ser um inteiro maior do que 1.")
  }

  df <- df[ind, ]
  y <- df[, variable]
  
  if (type == 'tb'){
    x_aux = df[,-1]
  } else{
    x_aux = df
  }
  
  x <- embed(as.matrix(x_aux), n_lags)

  names_x <- NULL
  for (i in seq_len(n_lags)) {
    names_x <- c(
      names_x,
      paste(colnames(x_aux), "_lag_", horizon + i - 1, sep = "")
    )
  }
  colnames(x) <- names_x

  x_in <- x[-c((nrow(x) - horizon + 1):nrow(x)), ]
  x_out <- x[nrow(x), ]
  x_out <- t(as.vector(x_out))
  y_in <- tail(y, nrow(x_in))

  return(list(x_in = x_in, x_out = x_out, y_in = y_in))
}
