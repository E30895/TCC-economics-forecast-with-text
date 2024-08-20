
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)


get_tesi = function(dataset, how=c('proportional', 'proportionalPol', 'TFIDF')){
  
  #' Esta função calcula uma série temporal de sentimentos a partir de um conjunto de dados textuais utilizando diferentes métodos de ponderação.
  #'
  #' @param dataset DataFrame. Um conjunto de dados com pelo menos duas colunas: `Data` (datas) e `Traducao` (textos).
  #' @param how Vetor de caracteres. Especifica o método de ponderação a ser utilizado para calcular o sentimento. Pode ser "proportional", "proportionalPol", ou "TFIDF". O valor padrão é c("proportional", "proportionalPol", "TFIDF").
  #'
  #' @return DataFrame com duas colunas: `date`, representando a data, e `sentiment`, que é a média dos valores de sentimento calculados para cada data.
  #'
  #' @details A função começa processando o `dataset` de entrada, removendo valores ausentes e criando um corpus textual. Em seguida, aplica um lexicon de sentimentos específico para calcular a série temporal de sentimentos usando o método especificado em `how`. Finalmente, a função agrupa os resultados por data e calcula a média dos sentimentos diários.
  #'
  #' @examples
  #' # Exemplo de uso da função get_tesi:
  #' dataset <- data.frame(Data = as.Date(c('2023-01-01', '2023-01-02')), Traducao = c("bom", "ruim"))
  #' sentiment_series <- get_tesi(dataset, how = "proportional")
  #' print(sentiment_series)
  #'
  #' @import dplyr sentometrics
  #' @export
  
  dataset = data.frame(
    id = 1:dim(dataset)[1],
    date = dataset$Data,
    texts = dataset$Traducao) %>% 
    na.omit()
  
  l3 <- sento_lexicons(list_lexicons["LM_en"], list_valence_shifters[["en"]][, c("x", "t")])
  corpus <- sento_corpus(corpusdf = dataset, do.clean = T)
  sentiment_series <- compute_sentiment(corpus, l3, how = how, nCore = 4)
  
  sentiment_series <- sentiment_series %>%
    group_by(date) %>%
    summarise(sentiment = mean(`LM_en--dummyFeature`, na.rm = TRUE))
  
  return(sentiment_series)
}
