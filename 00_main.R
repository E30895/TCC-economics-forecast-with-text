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
#save(dataset_economic, file = "Datasets\\dataset_economic.Rdata")


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
save(dataset_tb, file = "Datasets\\dataset_tb.Rdata")

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
save(dataset_economic_tb, file = "Datasets\\dataset_economic_tb.Rdata")


##################################################
################# Forecasting ####################
##################################################

#LOAD DATASETS FOR SKIP INIT PROCESS
#load("Datasets\\dataset_economic.Rdata")
#load("Datasets\\dataset_tb.Rdata")
#load("Datasets\\dataset_economic_tb.Rdata")

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

lasso_tb$rmse
boosting_tb$rmse

#save(
#  lasso_tb,
#  enet_tb,
#  boosting_tb,
#  lasso_economic,
#  enet_economic,
#  boosting_economic,
#  lasso_economic_tb,
#  enet_economic_tb,
#  boosting_economic_tb,
#  file = "Forecasting\\models_forecasting_new.Rdata"
#)


##################################################
############ Diebold-Mariano Test ################
##################################################
#LOAD DATA FOR SKIP PROCESS
#load("Forecasting\\models_forecasting.Rdata")
#load("Datasets\\dataset_economic.Rdata")
#load("Datasets\\dataset_tb.Rdata")
#load("Datasets\\dataset_economic_tb.Rdata")

#COMPUTING DM TEST
dm_tests = compute_dm()
dm_tests$pvalues_eco_tb



##################################################
#################### CSFE ########################
##################################################
#load("Forecasting\\models_forecasting_new.Rdata")
#benchmark = call_models(dataset_economic, 'Sarima', get_sarima, "BZEAMOM%")

csfe_boosting_tb = csfe(boosting_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_lasso_tb = csfe(lasso_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_enet_tb = csfe(enet_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])

csfe_boosting_economic = csfe(boosting_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_lasso_economic = csfe(lasso_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_enet_economic = csfe(enet_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])

csfe_boosting_economic_tb = csfe(boosting_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_lasso_economic_tb = csfe(lasso_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
csfe_enet_economic_tb = csfe(enet_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])


##################################################
############ Giacomini-White Test ################
##################################################
#LOAD DATA FOR SKIP PROCESS
#load("Forecasting\\models_forecasting_new.Rdata")
#load("Datasets\\dataset_economic.Rdata")
#load("Datasets\\dataset_tb.Rdata")
#load("Datasets\\dataset_economic_tb.Rdata")

#COMPUTING GW TEST
gw_tests = compute_gw()
gw_tests$pvalues_tb

benchmark$forecasts


################################################
#GRAFICOS TESI
plot.ts(agronegocio_tesi$TESI_Z)
plot.ts(mercadoF_tesi$TESI_Z)
plot.ts(mercadoT_tesi$TESI_Z)
plot.ts(servicos_tesi$TESI_Z)
plot.ts(industria_tesi$TESI_Z)

#GRAFICOS TEPU
plot(x = agronegocio_tepu$Data, y = agronegocio_tepu$TEPU_Z, main = "TEPU - AGRONEGOCIO", type = 'lines')
plot(x = mercadoF_tepu$Data, y = mercadoF_tepu$TEPU_Z, main = "TEPU - MERCADO FINANCEIRO", type = 'lines')
plot(x = mercadoT_tepu$Data, y = mercadoT_tepu$TEPU_Z, main = "TEPU - MERCADO DE TRABALHO", type = 'lines')
plot(x = servicos_tepu$Data, y = servicos_tepu$TEPU_Z, main = "TEPU - SERVIÇOS", type = 'lines')
plot(x = industria_tepu$Data, y = industria_tepu$TEPU_Z, main = "TEPU - INDÚSTRIA", type = 'lines')

save(dataset.notext, file = "dataset_notext.Rdata")
save(dataset_merged_stationary, file = "dataset_text.Rdata")

save(agronegocio_tesi, file = ".agronegocio_tesi.Rdata")
save(mercadoF_tesi, file = ".mercadoF_tesi.Rdata")
save(mercadoT_tesi, file = ".mercadoT_tesi.Rdata")
save(servicos_tesi, file = ".servicos_tesi.Rdata")
save(industria_tesi, file = ".industria_tesi.Rdata")

save(agronegocio_tepu, file = ".agronegocio_tepu.Rdata")
save(mercadoF_tepu, file = ".mercadoF_tepu.Rdata")
save(mercadoT_tepu, file = ".mercadoT_tepu.Rdata")
save(servicos_tepu, file = ".servicos_tepu.Rdata")
save(industria_tepu, file = ".industria_tepu.Rdata")


