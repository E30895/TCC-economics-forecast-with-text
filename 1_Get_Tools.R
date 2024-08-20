
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)

get_normalize = function(dataset, type = c('TESI', 'TEPU')){
  
  #' Normalização dos Dados
  #'
  #' Esta função realiza a normalização dos dados em um conjunto de dados fornecido.
  #'
  #' @param dataset O conjunto de dados a ser normalizado.
  #' @param type O tipo de normalização a ser aplicado. Pode ser "TESI" ou "TEPU". O padrão é "TESI".
  #' @return O conjunto de dados normalizado.
  #' @examples
  #' dataset <- data.frame(sentiment = c(1, 2, 3, 4, 5),
  #'                       TEPU = c(10, 20, 30, 40, 50))
  #' get_normalize(dataset)
  #'
  #' dataset <- data.frame(sentiment = c(1, 2, 3, 4, 5),
  #'                       TEPU = c(10, 20, 30, 40, 50))
  #' get_normalize(dataset, "TEPU")
  #' @export
  
  if (type == 'TESI'){
    dataset$SSN = ((dataset$sentiment - mean(dataset$sentiment)) / sd(dataset$sentiment))} 
  
  else if (type == "TEPU"){
    dataset$TEPU = ((dataset$TEPU - mean(dataset$TEPU)) / sd(dataset$TEPU))}
  
  else {
    paste("Escolha um tipo: TESI ou TEPU")
  }
  
  return(dataset)
  
}


get_resample = function(dataset, type =c('TESI', 'TEPU'),timer=c('weeek', 'month', 'quarter', 'year')){
  
  #' Amostragem e Sumarização de Dados Temporais
  #'
  #' Esta função realiza amostragem e sumarização de dados temporais em um conjunto de dados fornecido.
  #'
  #' @param dataset O conjunto de dados a ser amostrado e sumarizado.
  #' @param type O tipo de dados a ser amostrado e sumarizado. Pode ser "TESI" ou "TEPU". O padrão é "TESI".
  #' @param timer O período de tempo para agrupamento dos dados. Pode ser "week" (semana), "month" (mês), "quarter" (trimestre) ou "year" (ano). O padrão é "week".
  #' @return O conjunto de dados com a amostragem e sumarização aplicadas.
  #' @examples
  #' dataset <- data.frame(date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
  #'                       SSN = rnorm(365),
  #'                       TEPU = rnorm(365))
  #' get_resample(dataset)
  #'
  #' dataset <- data.frame(date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
  #'                       SSN = rnorm(365),
  #'                       TEPU = rnorm(365))
  #' get_resample(dataset, "TEPU", "month")
  #' @export
  
  
  if (type == "TESI"){
    dataset = subset(dataset, date <= as.Date("2023-12-31"))
    dataset = dataset %>%
      mutate(date = floor_date(date, timer)) %>%
      group_by(date) %>%
      summarise(TESI_Z = mean(SSN, na.rm = TRUE))}
  
  else if (type == "TEPU"){
    dataset = subset(dataset, Data <= as.Date("2023-12-31"))
    dataset = dataset %>%
      mutate(Data = floor_date(Data, timer)) %>%
      group_by(Data) %>%
      summarise(TEPU_Z = mean(TEPU, na.rm = TRUE))}
  
  else {
    paste("Seletione um tipo:  TESI ou TEPU")
  }
  
  return(dataset)
  
}