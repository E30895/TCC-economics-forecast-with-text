library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)

get_tepu = function(tokens, tepu_dict){
  
  #' Análise de Sentimento com Dicionário TEPU
  #'
  #' Esta função realiza a análise de sentimento de um conjunto de tokens utilizando um dicionário de termos TEPU.
  #'
  #' @param tokens Um `data.frame` contendo tokens e suas traduções, incluindo uma coluna chamada `Data`.
  #' @param tepu_dict Um `data.frame` com os termos do dicionário TEPU e seus sentimentos associados.
  #' @return Um `data.frame` contendo as seguintes colunas:
  #' - traducao: A tradução dos tokens.
  #' - Data: As datas correspondentes.
  #' - Noticias: O número total de notícias por data.
  #' - EPU: O número de notícias que possuem termos 'P' e 'U'.
  #' - TEPU: A proporção de termos TEPU em relação ao total de notícias.
  #'
  #' @examples
  #' tokens <- data.frame(Data = as.Date('2024-01-01') + 0:9, 
  #'                      traducao = c("Accountability", "Democracy", "Vote", "Ballot", 
  #'                                   "Campaign", "Government", "Elections", "Transparency", 
  #'                                   "Policy", "Corruption"),
  #'                      stringsAsFactors = FALSE)
  #' tepu_dict <- get_tepu_dict()
  #' resultado <- get_tepu(tokens, tepu_dict)
  #'
  #' @export
  
  dataset = dplyr::inner_join(
    x = tokens,
    y = tepu_dict) %>% 
    dplyr::group_by(Data, traducao) %>% 
    dplyr::count(sentimento)
  
  analise_sentimento = dataset %>% 
    tidyr::pivot_wider(
      id_cols = c("traducao", 'Data'),
      names_from = "sentimento",
      values_from = "n") %>% 
    group_by(Data) %>% 
    summarise(Noticias = n(),
              EPU = sum(!is.na(P) & !is.na(U))) %>% 
    mutate(TEPU = EPU/Noticias) %>% 
    arrange(Data)
  
}
