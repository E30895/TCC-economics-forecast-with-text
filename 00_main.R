library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)
library(lmtest)
library(sandwich)
library(forecast)

setwd('C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\10. Github Upload')
source('1_Get_Tools.R')
source('2_Get_Tokens.R')
source('3_Get_Loughran_McDonald.R')
source('4_Get_TESI.R')
source('5_Get_TEPU.R')
source('6_Get_Stationary.R')
source('7_Get_Data_Prep.R')
source('8_Get_Rolling_Window.R')
source('9_Get_Models.R')
source('10_Call_Model.R')
source('11_Get_GW.r')
source('12_Compute_GW.r')
source('12_Compute_DM.r')
source('13_get_Performance.r')

##################################################
############### Loading Dataset #################
##################################################
dataset = readxl::read_excel("Datasets\\dataset_economic.xlsx") %>% na.omit()
agronegocio = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_Agronegocio.xlsx")
mercadoF = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_MercadoFinanceiro.xlsx")
mercadoT = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_MercadoTrabalho.xlsx")
servicos = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticias_tratadas_Serviços.xlsx")
industria = readxl::read_xlsx("C:\\Users\\eusou\\OneDrive\\Documentos\\TCC\\08. G1 Dataset\\3.noticicas_tratadas_industria.xlsx")


##################################################
##################### TESI #######################
##################################################
agronegocio_tesi = get_tesi(agronegocio, how = "proportionalPol") %>% get_normalize(type = "TESI")  %>% get_resample(type = "TESI", timer = "month")
mercadoF_tesi = get_tesi(mercadoF, how = "proportionalPol") %>% get_normalize(type = "TESI") %>% get_resample(type = "TESI", timer = "month")
mercadoT_tesi = get_tesi(mercadoT, how = "proportionalPol") %>% get_normalize(type = "TESI") %>% get_resample(type = "TESI", timer = "month")
servicos_tesi = get_tesi(servicos, how = "proportionalPol") %>% get_normalize(type = "TESI") %>% get_resample(type = "TESI", timer = "month")
industria_tesi = get_tesi(industria, how = "proportionalPol") %>% get_normalize(type = "TESI") %>% get_resample(type = "TESI", timer = "month")


##################################################
##################### TEPU #######################
##################################################

#GETING TOKENS
agronegocio_tokens = get_tokens(agronegocio)
mercadoF_tokens = get_tokens(mercadoF)
mercadoT_tokens = get_tokens(mercadoT)
servicos_tokens = get_tokens(servicos)
industria_tokens = get_tokens(industria)

#GETING TEPU DICT
tepu_dict = get_tepu_dict()

#COMPUTING TEPU
agronegocio_tepu = get_tepu(agronegocio_tokens, tepu_dict) %>% get_normalize(type = "TEPU") %>% get_resample(type = 'TEPU', timer = 'month')
mercadoF_tepu = get_tepu(mercadoF_tokens, tepu_dict) %>% get_normalize(type = "TEPU") %>% get_resample(type = 'TEPU', timer = 'month')
mercadoT_tepu = get_tepu(mercadoT_tokens, tepu_dict) %>% get_normalize(type = "TEPU") %>% get_resample(type = 'TEPU', timer = 'month')
servicos_tepu = get_tepu(servicos_tokens, tepu_dict) %>% get_normalize(type = "TEPU") %>% get_resample(type = 'TEPU', timer = 'month')
industria_tepu = get_tepu(industria_tokens, tepu_dict) %>% get_normalize(type = "TEPU") %>% get_resample(type = 'TEPU', timer = 'month')


##################################################
############### AGREGATE DATASETS ################
##################################################

#DATASET ECONOMIC
dataset_economic = dataset[-c(1)]
dataset_economic = get_stationarity(dataset_economic)
dataset_economic = do.call(cbind, dataset_economic$results) %>% as.data.frame()


#DATASET TEXT-BASE
dataset_tb = data.frame(
  agronegocio_tesi = agronegocio_tesi$TESI_Z,
  mercadoF_tesi = mercadoF_tesi$TESI_Z,
  mercadoT_tesi = mercadoT_tesi$TESI_Z,
  servicos_tesi = servicos_tesi$TESI_Z,
  industria_tesi = industria_tesi$TESI_Z,
  agronegocio_tepu = agronegocio_tepu$TEPU_Z,
  mercadoT_tepu = mercadoT_tepu$TEPU_Z,
  mercadoF_tepu = mercadoF_tepu$TEPU_Z,
  servicos_tepu = servicos_tepu$TEPU_Z,
  industria_tepu = industria_tepu$TEPU_Z
)

dataset_tb = get_stationarity(dataset_tb)
dataset_tb = do.call(cbind, dataset_tb$results) %>% as.data.frame()

#DATASET ECONOMIC-TEXT-BASE
dataset_economic_tb = cbind(dataset_economic, 
                       agronegocio_tesi$TESI_Z,
                       mercadoF_tesi$TESI_Z, 
                       mercadoT_tesi$TESI_Z, 
                       servicos_tesi$TESI_Z, 
                       industria_tesi$TESI_Z,
                       agronegocio_tepu$TEPU_Z, 
                       mercadoF_tepu$TEPU_Z, 
                       mercadoT_tepu$TEPU_Z, 
                       servicos_tepu$TEPU_Z, 
                       industria_tepu$TEPU_Z)

dataset_economic_tb = get_stationarity(dataset_economic_tb)
dataset_economic_tb = do.call(cbind, dataset_economic_tb$results) %>% as.data.frame()


##################################################
################# Forecasting ####################
##################################################

#BENCHMARK
benchmark = call_models(dataset_economic, 'Sarima', get_sarima, "BZEAMOM%")

#MODELS FOR TEXT-BASE
lasso_tb = call_models(dataset_tb, 'Lasso', get_lasso, "BZEAMOM")
enet_tb = call_models(dataset_tb, 'Enet', get_elasticnet, "BZEAMOM")
boosting_tb = call_models(dataset_tb, 'Boosting', get_boosting, "BZEAMOM")

#MODELS FOR ECONOMIC DATASET
lasso_economic = call_models(dataset_economic, 'Lasso', get_lasso, "BZEAMOM%")
enet_economic = call_models(dataset_economic, 'enet', get_elasticnet, "BZEAMOM%")
boosting_economic = call_models(dataset_economic, 'Boosting', get_boosting, "BZEAMOM%") 

#MODELS FOR ECONOMIC TEXT BASE
lasso_economic_tb = call_models(dataset_economic_tb, 'Lasso', get_lasso, "BZEAMOM%") 
enet_economic_tb = call_models(dataset_economic_tb, 'Enet', get_elasticnet, "BZEAMOM%")
boosting_economic_tb = call_models(dataset_economic_tb, 'Boosting', get_boosting, "BZEAMOM%") 

##################################################
############ Diebold-Mariano Test ################
##################################################

dm_tests = compute_dm()

##################################################
############ Giacomini-White Test ################
##################################################

gw_tests = compute_gw()


##################################################
#################### CSFE ########################
##################################################

csfe_boosting_tb = csfe(boosting_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_lasso_tb = csfe(lasso_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_enet_tb = csfe(enet_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])

csfe_boosting_economic = csfe(boosting_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_lasso_economic = csfe(lasso_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_enet_economic = csfe(enet_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])

csfe_boosting_economic_tb = csfe(boosting_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_lasso_economic_tb = csfe(lasso_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_enet_economic_tb = csfe(enet_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])


