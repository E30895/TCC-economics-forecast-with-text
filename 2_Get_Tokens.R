
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)


get_tokens = function(dataset) {
  
  #' Obter Tokens
  #'
  #' Esta função extrai tokens (palavras) de um conjunto de dados fornecido.
  #'
  #' @param dataset O conjunto de dados contendo as informações de texto.
  #' @return Um conjunto de dados contendo os tokens extraídos.
  #' @examples
  #' dataset <- data.frame(
  #'   Texto = c("Esta é uma frase de exemplo.", "Outra frase aqui."),
  #'   Traducao = c("This is an example sentence.", "Another sentence here.")
  #' )
  #' get_tokens(dataset)
  #'
  #' @importFrom dplyr mutate select
  #' @importFrom tidyr unnest_tokens
  #' @importFrom tidytext unnest_tokens
  #' @export
  
  tokens = dataset %>% 
    mutate(traducao = Traducao) %>% 
    select(-c('endereco', 'Texto', 'Num_Palavras-Chave')) %>% 
    tidytext::unnest_tokens(output = 'token', input = 'Traducao', token = 'words')

  
  return(tokens)
}

