# Limpar ambiente e console ----------------------------------------------------------------
rm(list = ls())  
cat("\014")  

# Carregando os módulos necessários -------------------------------------------------------

require(readr)
require(dplyr)
require(writexl)
require(random)
require(caret)
require(ranger)
require(yardstick)

# Trabalhando com clusters (opcional) -----------------------------------------------------

library(doParallel)
cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

# Definindo os caminhos de dados e versão -------------------------------------------------

data_version <- "v000"

input_data_path <- paste0("./data/")
output_models_path <- paste0("./models/", data_version, "/")
output_models_results_path <- paste0("./results/", data_version, "/")

# Criando diretórios caso não existam -----------------------------------------------------

dir.create(output_models_path, recursive = TRUE, showWarnings = FALSE)
dir.create(output_models_results_path, recursive = TRUE, showWarnings = FALSE)

# Importando e dividindo os datasets ------------------------------------------------------

original_data <- read_csv(paste0(input_data_path, "/matriz-", data_version, ".csv"))

# Tratando valores ausentes ----------------------------------------------------------------
original_data <- na.omit(original_data)

original_data <- original_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Criando um vetor de sementes aleatórias -------------------------------------------------

random_seeds <- c(6842, 7045,1359, 4109, 7947, 9122, 2050, 6646, 8143, 8444,
                  6402, 1721, 6955, 3744, 3144, 3681, 9588, 3807, 4464, 1034,
                  950, 8778, 163, 7249, 3181, 9938, 1564, 685, 8560, 8504, 3092,
                  7722, 6351, 2368, 5969, 6367, 3921, 8767, 9040, 1415, 428,
                  4837, 8263, 1631, 4249, 1411, 4747, 3158, 7846, 430, 6366,
                  6428, 1305, 8981, 3461, 6489, 1580, 8997, 8685, 5944, 991,
                  3630, 4472, 9304, 8411, 4961, 6877, 1325, 1507, 6748, 9408,
                  5790, 8395, 6161, 8942, 8907, 329, 2263, 9397, 3317, 6359,
                  8121, 2416, 1121, 9781, 4723, 5186, 3671, 7715, 4939, 4640,
                  9268, 5138, 6258, 8862, 2386, 6146, 879, 6644, 1821)

# Covariáveis -----------------------------------------------------------------------------

covariables <- c(
  # SoilGrids
  'bdod',
  'cec',
  'cfvo',
  'clay',
  'nitrogen',
  'phh2o',
  'sand',
  'silt',
  'soc',
  'Area_Estavel',
  
  # Morfometrica do terreno
  'slope',
  'convergence',
  'cti',
  'eastness',
  'northness',
  'pcurv',
  'roughness',
  'spi',
  'elevation',
  
  # # Probabilidade individual de ocorrencia das classes wrb
  # 'Acrisols',
  # 'Albeluvisols',
  # 'Alisols',
  # 'Andosols',
  # 'Arenosols',
  # 'Calcisols',
  # 'Cambisols',
  # 'Chernozems',
  # 'Cryosols',
  # 'Durisols',
  # 'Ferralsols',
  # 'Fluvisols',
  # 'Gleysols',
  # 'Gypsisols',
  # 'Histosols',
  # 'Kastanozems',
  # 'Leptosols',
  # 'Lixisols',
  # 'Luvisols',
  # 'Nitisols',
  # 'Phaeozems',
  # 'Planosols',
  # 'Plinthosols',
  # 'Podzols',
  # 'Regosols',
  # 'Solonchaks',
  # 'Solonetz',
  # 'Stagnosols',
  # 'Umbrisols',
  # 'Vertisols',
  
  # Agrupamento de classes (Possivel alteração para a próxima versão)
  'Sandysols',
  'Humisols',
  'Thinsols',
  'Wetsols',
  
   'Ferralsols_1', #Usado somente na v000
   'Histosols_1',  #Usado somente na v000
  
  'black_soil_prob',
  
  
  # Clima Koeppen
  'lv1_Tropical',
  'lv1_Dry_season',
  'lv1_Humid_subtropical_zone',
  'lv2_without_dry_season',
  'lv2_monsoon',
  'lv2_with_dry_summer',
  'lv2_with_dry_winter',
  'lv2_semiarid',
  'lv2_oceanic_climate_without_sry_season',
  'lv2_with_dry_winter_1',
  'lv3_low_latitude_and_altitude',
  'lv3_with_hot_summer',
  'lv3_with_temperate_summer',
  'lv3_and_hot',
  'lv3_and_temperate',
  'lv3_and_hot_summer',
  'lv3_and_temperate_summer',  'lv3_and_short_and_cool_summer',
  
  # Bioma
  'Amazonia',
  'Caatinga',
  'Cerrado',
  'Mata_Atlantica',
  'Pampa',
  'Pantanal',
  
  # Fitofisionomia
  'Campinarana',
  'Contato_Ecotono_e_Encrave',
  'Corpo_dagua_continental',
  'Estepe',
  'Floresta_Estacional_Decidual',
  'Floresta_Estacional_Semidecidual',
  'Floresta_Estacional_Sempre_Verde',
  'Floresta_Ombrofila_Aberta',
  'Floresta_Ombrofila_Densa',
  'Floresta_Ombrofila_Mista',
  'Formacao_Pioneira',
  'Savana',
  'Savana_Estepica',
  
  # Províncias
  # 'Amazonas_Solimoes_Provincia',
  # 'Amazonia_Provincia',
  # 'Borborema_Provincia',
  # 'Cobertura_Cenozoica_Provincia',
  # 'Costeira_Margem_Continental_Provincia',
  # 'Gurupi_Provincia',
  # 'Mantiqueira_Provincia',
  # 'Massa_d_agua_Provincia',
  # 'Parana_Provincia',
  # 'Parecis_Provincia',
  # 'Parnaiba_Provincia',
  # 'Reconcavo_Tucano_Jatoba_Provincia',
  # 'Sao_Francisco_Provincia',
  # 'Sao_Luis_Provincia',
  # 'Tocantis_Provincia',
  
  #Indices minerais
  'oxides',
  'clayminerals',
  
  #' #Latitude e Longitude
   'longitude',
   'latitude'

  #Coordenadas Obliquas 
  # 'OGC_0',
  # 'OGC_0_53',
  # 'OGC_1_03',
  # 'OGC_1_57',
  # 'OGC_2_10',
  # 'OGC_2_60'
  
  
)

# Exportando a função para os clusters ----------------------------------------------------

regression_eval <- function(obs, pred){
  ME <- round(mean(obs - pred, na.rm = TRUE), digits = 4)
  MSE <- round(mean((obs - pred)^2, na.rm = TRUE), digits = 4)
  MAE <- round(mean(abs(obs - pred), na.rm = TRUE), digits = 4)
  RMSE <- round(sqrt(mean((obs - pred)^2, na.rm = TRUE)), digits = 4)
  r2 <- round((cor(obs, pred, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 4)
  NSE <- round((1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))), digits = 4)
  CCC <- round(yardstick::ccc_vec(truth = obs, estimate = pred), digits = 4)
  VAR <- round(var(obs - pred, na.rm = TRUE), digits = 4)
  SD <- round(sqrt(VAR), digits = 4)
  
  return(data.frame(ME = ME, MSE = MSE, MAE = MAE, RMSE = RMSE, R2 = r2, NSE = NSE, CCC = CCC, VAR = VAR, SD = SD))
}

clusterExport(cl, varlist = c("regression_eval"))

# Calculando hiperparâmetros ---------------------------------------------------------------

total_samples <- nrow(original_data)
total_covariates <- length(covariables)
mtry_value <- round(total_covariates / 3)
min_node_size_value <- 5
num_trees_value <- round(total_samples / 10)

# Definindo a função de resumo personalizada -----------------------------------------------

my_summary_metrics <- function(data, lev = NULL, model = NULL) {
  pred <- data$pred
  obs <- data$obs
  metrics <- regression_eval(pred, obs)
  out <- c(
    ME = metrics$ME,
    MSE = metrics$MSE,
    MAE = metrics$MAE,
    RMSE = metrics$RMSE,
    R2 = metrics$R2,
    NSE = metrics$NSE,
    CCC = metrics$CCC,
    VAR = metrics$VAR,
    SD = metrics$SD
  )
  return(out)
}

# Função para treinar e avaliar os modelos ------------------------------------------------

train_and_evaluate <- function(target_variable) {
  rf_kFold_cross_validation <- list()
  rf_kFold_best_models <- list()
  rf_predictions <- list()
  
  for (i in seq(along.with = random_seeds)) {
    
    ti <- Sys.time()
    print(paste("Start time:", Sys.time(),"; random seed:", random_seeds[i]))
    
    ## Configurando a semente de randomização
    set.seed(random_seeds[i])
    
    ## Preparando o objeto de controle k-Fold
    cv_control_object <- trainControl(method = "cv", number = 10,
                                      summaryFunction = my_summary_metrics,
                                      returnResamp = 'all')
    
    ## Treinando o modelo
    tuned_RF_kfold_cv <- train(
      as.formula(paste(target_variable, "~",
                       paste(covariables, collapse = '+'))),
      data = original_data,
      method = "ranger",
      num.trees = num_trees_value,
      replace = TRUE,
      sample.fraction = 0.632,
      importance = "permutation",
      trControl = cv_control_object,
      tuneGrid = expand.grid(mtry = mtry_value, 
                             min.node.size = min_node_size_value,
                             splitrule = "variance")
    )
    
    ## Fazendo predições
    predictions <- predict(tuned_RF_kfold_cv, original_data)
    rf_predictions[[i]] <- data.frame(Observed = original_data[[target_variable]], Predicted = predictions)
    
    remove(cv_control_object)
    
    ## Obtendo métricas de treinamento
    ## Obtendo métricas de cross-validation
    cv <- tuned_RF_kfold_cv[["resample"]] %>% mutate(model = i) %>% 
      mutate(map_version = data_version)
    
    rf_kFold_cross_validation[[i]] <- cv
    
    # Obtendo estatísticas dos melhores modelos
    hyperparameters <- tuned_RF_kfold_cv[["bestTune"]]
    
    result <- tuned_RF_kfold_cv[["results"]] %>%
      filter(mtry == hyperparameters[1, 1])
    
    rf_kFold_best_models[[i]] <- result %>% mutate(model = i) %>% 
      mutate(map_version = data_version)
    
    ## Salvando modelos ajustados
    save(tuned_RF_kfold_cv,
         file = paste0(output_models_path,
                       "tuned_RF_cv_model_", target_variable, "_", i, ".RData"))
    
    tf <- Sys.time()
    print(paste0("Time spent training the model ", i, ":"))
    print(tf - ti)
  }
  
  rf_all_cv_results <- bind_rows(rf_kFold_cross_validation)
  rf_all_best_models <- bind_rows(rf_kFold_best_models)
  
  write_xlsx(rf_all_best_models,
             paste0(output_models_results_path,
                    "rf-kFold-results_", target_variable, ".xlsx"),
             col_names = TRUE)
  
  return(rf_predictions)
}

# Executando o treinamento para variáveis "ln_clay_sand" e "ln_silt_sand" ------------------

predictions_ln_clay_sand <- train_and_evaluate("ln_clay_sand")
predictions_ln_silt_sand <- train_and_evaluate("ln_silt_sand")

# Calculando areia, argila e silte com base nas predições ---------------------------------

calculate_granulometry <- function(pred_clay_sand_list, pred_silt_sand_list) {
  granulometry_list <- list()
  
  for (i in seq(along.with = pred_clay_sand_list)) {
    pred_clay_sand <- pred_clay_sand_list[[i]]
    pred_silt_sand <- pred_silt_sand_list[[i]]
    
    sand <- (1 / (exp(pred_clay_sand$Predicted) + exp(pred_silt_sand$Predicted) + 1)) * 100
    clay <- (exp(pred_clay_sand$Predicted) / (exp(pred_clay_sand$Predicted) + exp(pred_silt_sand$Predicted) + 1)) * 100
    silt <- (exp(pred_silt_sand$Predicted) / (exp(pred_clay_sand$Predicted) + exp(pred_silt_sand$Predicted) + 1)) * 100
    
    granulometry <- data.frame(
      Observed_Sand = (1 / (exp(original_data$ln_clay_sand) + exp(original_data$ln_silt_sand) + 1)) * 100,
      Predicted_Sand = sand,
      Observed_Clay = (exp(original_data$ln_clay_sand) / (exp(original_data$ln_clay_sand) + exp(original_data$ln_silt_sand) + 1)) * 100,
      Predicted_Clay = clay,
      Observed_Silt = (exp(original_data$ln_silt_sand) / (exp(original_data$ln_clay_sand) + exp(original_data$ln_silt_sand) + 1)) * 100,
      Predicted_Silt = silt,
      model = i,
      map_version = data_version
    )
    
    granulometry_list[[i]] <- granulometry
  }
  
  return(granulometry_list)
}

granulometry_list <- calculate_granulometry(predictions_ln_clay_sand, predictions_ln_silt_sand)

# Calculando métricas de desempenho para areia, argila e silte ------------------------------

sand_metrics_list <- lapply(seq_along(granulometry_list), function(i) {
  g <- granulometry_list[[i]]
  metrics <- regression_eval(g$Predicted_Sand, g$Observed_Sand)
  metrics$model <- i
  metrics$map_version <- data_version
  metrics$type <- "sand"
  return(metrics)
})

clay_metrics_list <- lapply(seq_along(granulometry_list), function(i) {
  g <- granulometry_list[[i]]
  metrics <- regression_eval(g$Predicted_Clay, g$Observed_Clay)
  metrics$model <- i
  metrics$map_version <- data_version
  metrics$type <- "clay"
  return(metrics)
})

silt_metrics_list <- lapply(seq_along(granulometry_list), function(i) {
  g <- granulometry_list[[i]]
  metrics <- regression_eval(g$Predicted_Silt, g$Observed_Silt)
  metrics$model <- i
  metrics$map_version <- data_version
  metrics$type <- "silt"
  return(metrics)
})

# Salvando métricas de desempenho ----------------------------------------------------------

write_xlsx(list(
  Sand_Metrics = bind_rows(sand_metrics_list),
  Clay_Metrics = bind_rows(clay_metrics_list),
  Silt_Metrics = bind_rows(silt_metrics_list)
), paste0(output_models_results_path, "granulometry_metrics_", data_version, ".xlsx"))

# Salvando todos os dados de métricas ------------------------------------------------------

write_xlsx(bind_rows(granulometry_list), paste0(output_models_results_path, "granulometry_predicted_", data_version, ".xlsx"))

# Finalizando -------------------------------------------------------------------------------

if (exists("cl")) {
  print("Closing clusters.")
  stopCluster(cl)
  rm(cl)
} else {
  print("Code is not working with clusters.")
}

