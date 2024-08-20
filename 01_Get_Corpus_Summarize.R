
library(tidyverse)
library(sentometrics)


setwd('C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\10. Github Upload\\Datasets')

agronegocio = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_Agronegocio.xlsx")
mercadoF = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_MercadoFinanceiro.xlsx")
mercadoT = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_MercadoTrabalho.xlsx")
servicos = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_Serviços.xlsx")
industria = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticicas_tratadas_industria.xlsx")

temp_dataset = rbind(agronegocio, mercadoF, mercadoT, servicos, industria)

dataset = data.frame(
  id = 1:dim(temp_dataset)[1],
  date = temp_dataset$Data,
  texts = temp_dataset$Traducao) %>% 
  na.omit()

corpus <- sento_corpus(corpusdf = dataset, do.clean = T)
teste = corpus_summarize(corpus, by = "year")
