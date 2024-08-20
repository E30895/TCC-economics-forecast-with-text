
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)


get_stationarity = function(dataset){
  
  #' Teste de Estacionariedade com o Teste ADF
  #'
  #' Esta função verifica a estacionariedade de um conjunto de dados em um `data.frame` utilizando o Teste Augmented Dickey-Fuller (ADF).
  #'
  #' @param dataset Um `data.frame` contendo as séries temporais a serem testadas.
  #' @return Uma lista com dois elementos:
  #' - results: Uma lista com as séries temporais estacionárias, onde cada série é ajustada de acordo com o número de diferenças necessárias.
  #' - ndiffs: Um `data.frame` com o nome de cada série e o número de diferenças necessárias para alcançar a estacionariedade.
  #'
  #' @examples
  #' dataset <- data.frame(serie1 = rnorm(100), serie2 = rnorm(100), serie3 = rnorm(100))
  #' stationarity_results <- get_stationarity(dataset)
  #'
  #' @export
  
  #dataset = dataset[-c(1)]

  results = list()
  suport_df = data.frame(Serie = character(), ndiffs = integer())
  
  # Loop através de cada coluna do dataframe
  for (col_name  in names(dataset)){
    serie = dataset[[col_name]]
    
    #Realizando o teste ADF (Questionar ao Hudson)
    adf_test = forecast::ndiffs(serie, alpha = 0.05, test = 'adf', type = 'level')
    
    temp_df = data.frame(Serie = col_name, ndiffs = adf_test)
    suport_df = rbind(suport_df, temp_df)
    
    #Iterando para realizar o número de diferenças necessárias
    if (adf_test !=0){
      diff_serie = serie
      
      for (i in 1:adf_test){
        diff_serie = diff(diff_serie)
      }
      
      results[[col_name]] = diff_serie
      
    } else {
      
      results[[col_name]] = serie
      
    }
  }
  
  return(list(results = results, ndiffs = suport_df))
  
}


