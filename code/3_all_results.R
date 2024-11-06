# Instalar e carregar os pacotes necessários
library(readxl)
library(writexl)
library(dplyr)

# Definir o diretório onde os arquivos XLSX estão localizados
dir_path <- "./results/all/"

# Listar todos os arquivos XLSX no diretório
file_list <- list.files(path = dir_path, pattern = "*.xlsx", full.names = TRUE)

# Função para calcular a média e o desvio padrão das métricas fornecidas em um arquivo XLSX, separando por map_version e biome
calculate_stats <- function(file) {
  # Ler as abas
  sand_data <- read_excel(file, sheet = "Sand_Metrics")
  clay_data <- read_excel(file, sheet = "Clay_Metrics")
  silt_data <- read_excel(file, sheet = "Silt_Metrics")
  
  # Adicionar uma coluna indicando o tipo de métrica
  sand_data$type <- "Sand"
  clay_data$type <- "Clay"
  silt_data$type <- "Silt"
  
  # Combinar os dados das três abas
  combined_data <- bind_rows(sand_data, clay_data, silt_data)
  
  # Verificar se a coluna 'biome' existe e adicionar 'Brasil' onde estiver vazia
  if ("biome" %in% colnames(combined_data)) {
    combined_data$biome[is.na(combined_data$biome) | combined_data$biome == ""] <- "Brasil"
  } else {
    combined_data$biome <- "Brasil"
  }
  
  # Filtrar as colunas de interesse
  metrics <- combined_data %>%
    select(map_version, biome, type, ME, MAE, MSE, RMSE, NSE, R2)
  
  # Calcular a média e o desvio padrão das métricas separadas por map_version, biome e tipo
  stats <- metrics %>%
    group_by(map_version, biome, type) %>%
    summarise(across(c(ME, MAE, MSE, RMSE, NSE, R2), list(mean = mean, sd = sd), na.rm = TRUE))
  
  return(stats)
}

# Aplicar a função a todos os arquivos e combinar os resultados em um único data frame
all_stats <- lapply(file_list, calculate_stats) %>%
  bind_rows()

# Salvar os resultados em um novo arquivo XLSX
output_file_xlsx <- "./results/all_results.xlsx"
write_xlsx(all_stats, output_file_xlsx)

# Salvar os resultados em um novo arquivo CSV
output_file_csv <- "./results/all_results.csv"
write.csv(all_stats, output_file_csv)
