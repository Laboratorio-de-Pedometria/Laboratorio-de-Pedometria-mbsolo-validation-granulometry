# Pacotes necessários -------------------------------------------------------
require(dplyr)       # Para manipulação de dados
require(readxl)      # Para leitura de arquivos Excel
require(writexl)     # Para escrita de arquivos Excel
require(readr)       # Para leitura de dados
require(tidyr)       # Para manipulação de dados (e.g., spread, gather)
require(tibble)      # Para criação de tibbles (data frames)
require(yardstick)   # Para avaliação de modelos de aprendizado de máquina
require(caret)       # Para treinamento e avaliação de modelos
require(ranger)      # Para treinamento de modelos de florestas aleatórias
library(magrittr)    # Para operações pipe (%>%)

# Data --------------------------------------------------------------------
# Definindo a versão dos dados em uso
data_version <- "v003"

# Caminhos dos dados
cv_results_path <- paste0("./results/", data_version, "/")
input_data_path <- "data/"
models_path <- paste0("./models/", data_version, "/")
output_models_results_path <- paste0("./output/", data_version, "/")

# Listando arquivos .xlsx no caminho especificado
list.files(path = cv_results_path, pattern = ".xlsx")

# Carregando os dados originais
original_data <- read_csv(paste0(input_data_path, "/matriz-", data_version ,".csv"))

# Classificando os dados por bioma
original_data <- original_data %>% mutate(
  biome = ifelse(Amazonia == 1, "Amazônia",
                 ifelse(Caatinga == 1, "Caatinga",
                        ifelse(Mata_Atlantica == 1, "Mata Atlântica",   
                               ifelse(Cerrado == 1, "Cerrado",
                                      ifelse(Pampa == 1, "Pampa",
                                             ifelse(Pantanal == 1, "Pantanal", "NA"))))))
)

# Funções auxiliares -----------------------------------------------------

# Função para avaliação de regressão
regression_eval <- function(pred, obs){
  # Calculando várias métricas estatísticas
  ME <- round(mean(pred - obs, na.rm = TRUE), digits = 4)   # Mean Error
  MSE <- round(mean((pred - obs)^2, na.rm = TRUE), digits = 4)  # Mean Square Error
  MAE <- round(mean(abs(pred - obs), na.rm = TRUE), digits = 4)  # Mean Absolute Error
  RMSE <- round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 4)  # Root Mean Square Error
  r2 <- round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 4)  # Pearson's correlation squared
  NSE <- round((1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)), digits = 4)  # Nash–Sutcliffe model efficiency coefficient
  VAR <- round(var(pred - obs, na.rm = TRUE), digits = 4)
  SD <- round(sqrt(VAR), digits = 4)
  out <- c(ME, MAE, MSE, RMSE, NSE, r2, VAR, SD)
  names(out) <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared", "VAR", "SD")
  if (any(is.nan(out))) 
    out[is.nan(out)] <- NA
  out
}

# Função para calcular areia, argila e silte com base nas predições para cada bioma
calculate_granulometry_by_biome <- function(pred_clay_sand_list, pred_silt_sand_list, original_data) {
  biomes <- unique(original_data$biome)
  granulometry_list <- list()
  
  for (biome in biomes) {
    biome_data <- original_data %>% filter(biome == biome)
    
    for (i in seq_along(pred_clay_sand_list)) {
      pred_clay_sand <- pred_clay_sand_list[[i]]
      pred_silt_sand <- pred_silt_sand_list[[i]]
      
      sand <- (1 / (exp(pred_clay_sand$Predicted) + exp(pred_silt_sand$Predicted) + 1)) * 100
      clay <- (exp(pred_clay_sand$Predicted) / (exp(pred_clay_sand$Predicted) + exp(pred_silt_sand$Predicted) + 1)) * 100
      silt <- (exp(pred_silt_sand$Predicted) / (exp(pred_clay_sand$Predicted) + exp(pred_silt_sand$Predicted) + 1)) * 100
      
      granulometry <- data.frame(
        Observed_Sand = (1 / (exp(biome_data$ln_clay_sand) + exp(biome_data$ln_silt_sand) + 1)) * 100,
        Predicted_Sand = sand,
        Observed_Clay = (exp(biome_data$ln_clay_sand) / (exp(biome_data$ln_clay_sand) + exp(biome_data$ln_silt_sand) + 1)) * 100,
        Predicted_Clay = clay,
        Observed_Silt = (exp(biome_data$ln_silt_sand) / (exp(biome_data$ln_clay_sand) + exp(biome_data$ln_silt_sand) + 1)) * 100,
        Predicted_Silt = silt,
        model = i,
        biome = biome,
        map_version = data_version
      )
      
      granulometry_list[[paste0(biome, "_", i)]] <- granulometry
    }
  }
  
  return(granulometry_list)
}

# Calculando a granulometria separadamente por bioma
granulometry_list <- calculate_granulometry_by_biome(predictions_ln_clay_sand, predictions_ln_silt_sand, original_data)

# Função para calcular as métricas de desempenho para cada bioma e tipo de solo
calculate_metrics_by_biome <- function(granulometry_list, type) {
  metrics_list <- lapply(seq_along(granulometry_list), function(i) {
    g <- granulometry_list[[i]]
    if (type == "sand") {
      metrics <- regression_eval(g$Predicted_Sand, g$Observed_Sand)
    } else if (type == "clay") {
      metrics <- regression_eval(g$Predicted_Clay, g$Observed_Clay)
    } else if (type == "silt") {
      metrics <- regression_eval(g$Predicted_Silt, g$Observed_Silt)
    }
    metrics$model <- i
    metrics$map_version <- data_version
    metrics$type <- type
    metrics$biome <- g$biome[1]  # Adiciona a informação do bioma
    return(metrics)
  })
  return(metrics_list)
}

# Calculando métricas para areia, argila e silte separadamente por bioma
sand_metrics_list <- calculate_metrics_by_biome(granulometry_list, "sand")
clay_metrics_list <- calculate_metrics_by_biome(granulometry_list, "clay")
silt_metrics_list <- calculate_metrics_by_biome(granulometry_list, "silt")

# Salvando métricas de desempenho separadas por bioma
write_xlsx(list(
  Sand_Metrics = bind_rows(sand_metrics_list),
  Clay_Metrics = bind_rows(clay_metrics_list),
  Silt_Metrics = bind_rows(silt_metrics_list)
), paste0(output_models_results_path, "granulometry_metrics_by_biome_", data_version, ".xlsx"))
