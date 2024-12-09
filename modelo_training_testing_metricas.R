# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(Metrics)

# Definir la función calculate_metrics
calculate_metrics <- function(actual, predicted) {
  mae <- mae(actual, predicted)
  rmse <- rmse(actual, predicted)
  mape <- mape(actual, predicted)
  r_squared <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  
  metrics <- data.frame(
    MAE = mae,
    RMSE = rmse,
    MAPE = mape,
    R_squared = r_squared
  )
  return(metrics)
}

# Paso 1: Seleccionar solo las variables numéricas para el análisis de correlación
numerical_vars <- datos_seleccionados %>% 
  select_if(is.numeric)

# Paso 2: Calcular la matriz de correlación
correlation_matrix <- cor(numerical_vars, use = "complete.obs")

# Paso 3: Visualizar la matriz de correlación
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, title = "Matriz de Correlación")

# Paso 4: Identificar correlaciones significativas (>|0.7|)
significant_correlations <- which(abs(correlation_matrix) > 0.7 & correlation_matrix != 1, arr.ind = TRUE)
correlation_table <- data.frame(
  Variable1 = rownames(correlation_matrix)[significant_correlations[, 1]],
  Variable2 = colnames(correlation_matrix)[significant_correlations[, 2]],
  Correlation = correlation_matrix[significant_correlations]
)
correlation_table <- correlation_table[!duplicated(t(apply(correlation_table, 1, sort))), ]
print(correlation_table)

# Paso 5: Visualización de relaciones significativas
if (nrow(correlation_table) > 0) {
  for (i in 1:nrow(correlation_table)) {
    var1 <- correlation_table$Variable1[i]
    var2 <- correlation_table$Variable2[i]
    
    if (var1 == "OverallQual" || var2 == "OverallQual") {
      ggplot(numerical_vars, aes(x = as.factor(.data[["OverallQual"]]), y = .data[["SalePrice"]])) +
        geom_boxplot(fill = "steelblue", color = "black") +
        labs(title = "Relación entre OverallQual y SalePrice",
             x = "OverallQual (Calidad General)", y = "SalePrice (Precio de Venta)") +
        theme_minimal() -> plot
    } else {
      ggplot(numerical_vars, aes(x = .data[[var1]], y = .data[[var2]])) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Relación entre", var1, "y", var2),
             x = var1, y = var2) +
        theme_minimal() -> plot
    }
    ggsave(filename = paste0("graficas/Relacion_", var1, "_", var2, ".png"), 
           plot = plot, width = 8, height = 6)
  }
}

# Paso 6: Evaluación de multicolinealidad con VIF
vif_model <- lm(SalePrice ~ ., data = numerical_vars)
vif_values <- vif(vif_model)
print(vif_values)

# Paso 7: Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_indices <- sample(1:nrow(numerical_vars), size = 0.7 * nrow(numerical_vars))
train_data <- numerical_vars[train_indices, ]
test_data <- numerical_vars[-train_indices, ]

# Guardar los datos de entrenamiento y prueba como CSV
write.csv(train_data, "train_data.csv", row.names = FALSE)
write.csv(test_data, "test_data.csv", row.names = FALSE)

# Paso 8: Ajustar el modelo transformado al conjunto de entrenamiento
train_data$LogSalePrice <- log(train_data$SalePrice)
train_data$LogGrLivArea <- log(train_data$GrLivArea)
transformed_model_train <- lm(LogSalePrice ~ LogGrLivArea + OverallQual, data = train_data)

# Paso 9: Predicciones y métricas en el conjunto de entrenamiento
predicted_train <- predict(transformed_model_train, newdata = train_data)
metrics_train <- calculate_metrics(train_data$LogSalePrice, predicted_train)
print(metrics_train)

# Paso 10: Predicciones y métricas en el conjunto de prueba
test_data$LogSalePrice <- log(test_data$SalePrice)
test_data$LogGrLivArea <- log(test_data$GrLivArea)
predicted_test <- predict(transformed_model_train, newdata = test_data)
metrics_test <- calculate_metrics(test_data$LogSalePrice, predicted_test)
print(metrics_test)

# Paso 11: Guardar el modelo transformado entrenado
saveRDS(transformed_model_train, file = "transformed_model_train.rds")
cat("El modelo transformado entrenado ha sido guardado como 'transformed_model_train.rds'.\n")
