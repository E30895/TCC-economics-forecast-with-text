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
  
  stars <- character(length(pvalues))
  
  for (i in seq_along(alpha_levels)) {
    stars[pvalues <= alpha_levels[i]] <- paste(rep("*", i), collapse = "")
  }
  
  return(stars)
}


compute_dm = function(){
  
  #' Computa o Teste de Diebold-Mariano para Modelos de Previsão
  #'
  #' Esta função calcula o teste de Diebold-Mariano para diferentes modelos de previsão e horizontes de previsão.
  #'
  #' @return Uma lista contendo matrizes de valores-p formatadas com estrelas para três conjuntos de modelos:
  #'   - pvalues_tb: valores-p para modelos baseados em texto.
  #'   - pvalues_eco: valores-p para modelos econômicos.
  #'   - pvalues_eco_tb: valores-p para modelos que combinam dados econômicos e baseados em texto.
  #'
  #' @examples
  #' dm_results <- compute_dm()
  
  model_names <- c("LASSO", "ENET", "BOOSTING")
  horizons <- c(1, 2, 3, 4, 5, 6, 9, 12)
  
  ##################################################
  ############## DM FOR TEXT BASE ##################
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
      
      #residual = y - ŷ
      dm = dm.test(
        e1 = dataset_economic$`BZEAMOM%`[124:164] - y,
        e2 = dataset_economic$`BZEAMOM%`[124:164] - x,
        h = h,
        alternative = 'two.sided',
        varestimator = 'bartlett'
      )
      
      
      pvalues_tb[m, i] <- dm$p.value
      results_tb[[paste0(model_name, '_tb', h)]] <- dm
    }
  }
  
  rownames(pvalues_tb) <- model_names
  colnames(pvalues_tb) <- horizons
  pvalues_eco_stars <- add_stars(pvalues_tb)
  pvalues_tb <- matrix(paste0(format(pvalues_tb, nsmall = 10), pvalues_eco_stars), nrow = nrow(pvalues_tb), dimnames = dimnames(pvalues_tb))
  pvalues_tb
  
  
  ##################################################
  ############## DM FOR ECONOMIC ###################
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
      
      #residual = y - ŷ
      dm = dm.test(
        e1 = dataset_economic$`BZEAMOM%`[124:164] - y,
        e2 = dataset_economic$`BZEAMOM%`[124:164] - x,
        h = h,
        alternative = 'two.sided',
        varestimator = 'bartlett'
      )
      
      pvalues_eco[m, i] <- dm$p.value
      results_eco[[paste0(model_name, "_eco", h)]] <- dm
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
      
      dm = dm.test(
        e1 = dataset_economic$`BZEAMOM%`[124:164] - y,
        e2 = dataset_economic$`BZEAMOM%`[124:164] - x,
        h = h,
        alternative = 'two.sided',
        varestimator = 'bartlett'
      )
      
      pvalues_eco_tb[m, i] <- dm$p.value
      results_eco_tb[[paste0(model_name, "_eco", h)]] <- dm
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
