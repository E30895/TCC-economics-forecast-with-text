add_stars <- function(pvalues, alpha_levels = c(0.05, 0.01, 0.001)) {
  
  #' Adiciona Estrelas para Indicar Níveis de Significância
  #'
  #' Esta função atribui estrelas (*) aos valores-p com base em níveis de significância especificados.
  #'
  #' @param pvalues Um vetor de valores-p.
  #' @param alpha_levels Um vetor de níveis de significância (padrão: 0.05, 0.01, 0.001).
  #'
  #' @return Um vetor de caracteres com estrelas correspondentes aos valores-p.
  #'
  #' @examples
  #' stars <- add_stars(c(0.02, 0.001, 0.07))
  #'
  #'
  stars <- character(length(pvalues))
  
  for (i in seq_along(alpha_levels)) {
    stars[pvalues <= alpha_levels[i]] <- paste(rep("*", i), collapse = "")
  }
  
  return(stars)
}

compute_gw = function(){
  
  #' Computa Resultados do Teste GW
  #'
  #' Esta função computa os resultados do teste Granger-Wald (GW) para diferentes modelos de previsão 
  #' em conjuntos de dados tanto baseados em texto quanto econômicos.
  #'
  #' @return Uma lista contendo os seguintes elementos:
  #' \item{pvalues_tb}{Uma matriz de valores-p para modelos baseados em texto em diferentes horizontes.}
  #' \item{pvalues_eco}{Uma matriz de valores-p para modelos econômicos em diferentes horizontes.}
  #' \item{pvalues_eco_tb}{Uma matriz de valores-p para modelos que combinam dados econômicos e baseados em texto.}
  #'
  #' @examples
  #' gw_results <- compute_gw()
  #' print(gw_results$pvalues_tb)
  #'
  
  ##################################################
  ############## GLOBAL SETTINGS ###################
  ##################################################
  model_names <- c("LASSO", "ENET", "BOOSTING")
  horizons <- c(1, 3, 6, 9, 12)
  
  
  ##################################################
  ############## GW FOR TEXT BASE ##################
  ##################################################
  model_dataframes <- list(lasso_tb, enet_tb, boosting_tb)
  pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
  results <- list()
  
  results_tb = results
  pvalues_tb = pvalues
  
  for (m in seq_along(model_names)) {
    model_name <- model_names[m]
    model_df <- model_dataframes[[m]]
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      x <- data.frame(benchmark$forecasts)[[i]]
      y <- data.frame(model_df$forecasts)[[i]]
      
      gw <- gw.test(
        x = x,
        y = y,
        p = dataset_economic$`BZEAMOM%`[124:164], #out of sample
        T = nrow(dataset_tb), # Tamanho da amostra
        tau = h, # Horizonte de Previsão
        method = "HAC",
        alternative = "two.sided"
      )
      
      pvalues_tb[m, i] <- gw$p.value
      results_tb[[paste0(model_name, '_tb', h)]] <- gw
    }
  }
  
  rownames(pvalues_tb) <- model_names
  colnames(pvalues_tb) <- horizons
  
  pvalues_eco_stars <- add_stars(pvalues_tb)
  pvalues_tb <- matrix(paste0(format(pvalues_tb, nsmall = 10), pvalues_eco_stars), nrow = nrow(pvalues_tb), dimnames = dimnames(pvalues_tb))
  pvalues_tb
  
  
  ##################################################
  ############## GW FOR ECONOMIC ###################
  ##################################################
  
  model_dataframes <- list(lasso_economic, enet_economic, boosting_economic)
  pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
  results <- list()
  
  results_eco = results
  pvalues_eco = pvalues
  
  for (m in seq_along(model_names)) {
    model_name <- model_names[m]
    model_df <- model_dataframes[[m]]
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      x <- data.frame(benchmark$forecasts)[[i]]
      y <- data.frame(model_df$forecasts)[[i]]
      
      gw <- gw.test(
        x = x,
        y = y,
        p = dataset_economic$`BZEAMOM%`[124:164], #out of sample
        T = nrow(dataset_tb), # Tamanho da amostra
        tau = h, # Horizonte de Previsão
        method = "HAC",
        alternative = "two.sided"
      )
      
      pvalues_eco[m, i] <- gw$p.value
      results_eco[[paste0(model_name, "_eco", h)]] <- gw
    }
  }
  
  rownames(pvalues_eco) <- model_names
  colnames(pvalues_eco) <- horizons
  
  pvalues_eco_stars <- add_stars(pvalues_eco)
  pvalues_eco <- matrix(paste0(format(pvalues_eco, nsmall = 10), pvalues_eco_stars), nrow = nrow(pvalues_eco), dimnames = dimnames(pvalues_eco))
  pvalues_eco
  
  
  ##################################################
  ########GW FOR ECONOMIC AND TEXT BASE ############
  ##################################################
  
  model_dataframes <- list(lasso_economic_tb, enet_economic_tb, boosting_economic_tb)
  pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
  results <- list()
  
  results_eco_tb = results
  pvalues_eco_tb = pvalues
  
  for (m in seq_along(model_names)) {
    model_name <- model_names[m]
    model_df <- model_dataframes[[m]]
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      x <- data.frame(benchmark$forecasts)[[i]]
      y <- data.frame(model_df$forecasts)[[i]]
      
      gw <- gw.test(
        x = x,
        y = y,
        p = dataset_economic$`BZEAMOM%`[124:164], #out of sample
        T = nrow(dataset_tb), # Tamanho da amostra
        tau = h, # Horizonte de Previsão
        method = "HAC",
        alternative = "two.sided"
      )
      
      pvalues_eco_tb[m, i] <- gw$p.value
      results_eco_tb[[paste0(model_name, "_eco", h)]] <- gw
    }
  }
  
  rownames(pvalues_eco_tb) <- model_names
  colnames(pvalues_eco_tb) <- horizons
  
  pvalues_eco_tb_stars <- add_stars(pvalues_eco_tb)
  pvalues_eco_tb <- matrix(paste0(format(pvalues_eco_tb, nsmall = 10), pvalues_eco_tb_stars), nrow = nrow(pvalues_eco_tb), dimnames = dimnames(pvalues_eco_tb))
  pvalues_eco_tb
  
  list = list(
    pvalues_tb = pvalues_tb, 
    pvalues_eco =pvalues_eco,
    pvalues_eco_tb = pvalues_eco_tb
    )
  
  return(list)
}