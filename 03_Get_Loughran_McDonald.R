
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)


get_Loughran_McDonald = function(){
  
  #' Obter Dicionário de Loughran-McDonald
  #'
  #' Esta função retorna o dicionário de Loughran-McDonald contendo termos relacionados à incerteza.
  #'
  #' @return Um conjunto de dados contendo o dicionário de Loughran-McDonald para termos relacionados à incerteza.
  #' @examples
  #' get_Loughran_McDonald()
  #'
  #' @importFrom textdata lexicon_loughran
  #' @importFrom dplyr mutate group_by distinct ungroup filter
  #' @importFrom tm stemDocument
  #' @export
  
  dicionario = textdata::lexicon_loughran() %>% 
    dplyr::mutate(
      token = tm::stemDocument(x = word, language = "english"),
      sentimento = sentiment,
      .keep = "none"
    ) %>% 
    dplyr::group_by(token) %>% 
    dplyr::distinct(sentimento) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(sentimento %in% 'uncertainty')
  
  return(dicionario)
}

get_tepu_dict = function(){
  
  #' Cria um dicionário de termos econômicos e políticos com sentimentos associados
  #'
  #' A função `get_tepu_dict` constrói um dicionário que combina termos econômicos e políticos, classificando cada termo de acordo com um sentimento associado. O dicionário inclui termos econômicos do dicionário de Loughran-McDonald, classificados como 'U', e uma seleção de termos políticos, classificados como 'P'.
  #'
  #' @details
  #' A função primeiro extrai os primeiros 84 termos do dicionário de Loughran-McDonald utilizando a função `get_Loughran_McDonald()`. Estes termos são associados ao sentimento 'U', indicando sua natureza econômica. Em seguida, uma lista de termos políticos é manualmente definida e associada ao sentimento 'P'.
  #' 
  #' Após a união desses dois conjuntos de dados em um único `data.frame`, os termos são convertidos para letras minúsculas para garantir a consistência ao comparar com outros textos ou tokens.
  #' 
  #' @return Um `data.frame` com duas colunas:
  #' - token: Termos econômicos e políticos em letras minúsculas.
  #' - sentimento: Sentimento associado a cada termo: 'U' para termos econômicos e 'P' para termos políticos.
  #'
  #' @note
  #' O dicionário gerado pode ser utilizado em análises de sentimento ou categorização de texto, especialmente em estudos que envolvem linguagem econômica e política.
  #'
  #' @examples
  #' # Gerar o dicionário TEPU e visualizar os primeiros termos
  #' tepu_dict <- get_tepu_dict()
  #' head(tepu_dict)
  #'
  #' @export
  
  U = get_Loughran_McDonald()[1:84,]
  U$sentimento = 'U'
  P = data.frame(
    token = c("Accountability", "Ambassador", "Autocracy", "Ballot", "Bipartisanship", "Bureaucracy", "Campaign", "Campaigning", "Cabinet", "Checks and balances",
              "Chancellor", "Central Bank", "Civil servant", "Civil service", "Coalition", "Congress", "Congressman", "Constituency", "Constituent", "Constitution", "Corruption",
              "Democracy", "Dictatorship", "Diplomat", "Diplomacy", "Domestic policy", "Electoral", "Electoral college", "Election", "Executive", "Federalism",
              "Foreign policy", "Government", "Governing party", "Gridlock", "House of Representatives", "Human rights", "Impeachment", "Initiative", "International relations",
              "Legislation", "Legislative", "Legislative", "Lobby", "Lobbyist", "Lawmaker", "Manifesto", "Majority", "Minority", "Monarch", "Monarchy", "Oligarchy",
              "Opposition", "Opposition party", "Parliament", "Petition", "Policy", "Political campaign", "Political party", "Politics", "Poll", "President",
              "President-elect", "Public servant", "Recall", "Recall election", "Referendum", "Republic", "Ruling party",
              "Senate", "Separation of powers", "Suffrage", "Transparency", "Vote"),
    sentimento = "P")
  
  tepu_dict = rbind(U, P)
  tepu_dict$token = tolower(tepu_dict$token)
  
  return(tepu_dict)
  
}
