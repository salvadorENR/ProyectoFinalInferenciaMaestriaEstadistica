# Cargar el conjunto de datos filtrado
precios_casas_filtrado <- read.csv("precios_casas_filtrado.csv")

# Paso 1: Análisis inicial del modelo con la variable original (SalePrice)
library(MASS)
library(car)

# Variables predictoras y fórmula del modelo
predictoras_numericas <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars")
formula_completa <- as.formula(paste("SalePrice ~", paste(predictoras_numericas, collapse = " + ")))

# Ajustar el modelo completo con los datos originales
modelo_completo <- lm(formula_completa, data = precios_casas_filtrado)
summary(modelo_completo)

# Diagnóstico inicial del modelo original
# Guardar las gráficas de diagnóstico
png("diagnostico_modelo_original.png")
par(mfrow = c(2, 2))
plot(modelo_completo)
dev.off()

# Validación del modelo original con conjunto de prueba
set.seed(123)
train_indices <- sample(1:nrow(precios_casas_filtrado), 0.8 * nrow(precios_casas_filtrado))
datos_entrenamiento <- precios_casas_filtrado[train_indices, ]
datos_prueba <- precios_casas_filtrado[-train_indices, ]

# Ajustar el modelo original con los datos de entrenamiento
modelo_completo_entrenamiento <- lm(formula_completa, data = datos_entrenamiento)

# Predecir en los datos de prueba (modelo original)
predicciones_original <- predict(modelo_completo_entrenamiento, newdata = datos_prueba)

# Calcular RMSE para el modelo original
rmse_original <- sqrt(mean((datos_prueba$SalePrice - predicciones_original)^2))

# Calcular MAE y MAPE para el modelo original
mae_original <- mean(abs(datos_prueba$SalePrice - predicciones_original))
mape_original <- mean(abs((datos_prueba$SalePrice - predicciones_original) / datos_prueba$SalePrice)) * 100

cat("\nError RMSE en el conjunto de prueba (modelo original):", rmse_original, "\n")
cat("MAE:", mae_original, "| MAPE:", mape_original, "%\n")

# Paso 2: Identificación de puntos influyentes con Cook's Distance
cooks_distances <- cooks.distance(modelo_completo)
influential_points <- which(cooks_distances > (4 / nrow(precios_casas_filtrado)))
cat("Puntos influyentes identificados (Cook's Distance):", influential_points, "\n")

# Visualizar Cook's Distance
# Guardar la gráfica de Cook's Distance
png("cooks_distance.png")
plot(cooks_distances, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / nrow(precios_casas_filtrado), col = "red", lty = 2)
dev.off()

# Crear un nuevo conjunto de datos sin los puntos influyentes
if (length(influential_points) > 0) {
  precios_casas_sin_influencia <- precios_casas_filtrado[-influential_points, ]
} else {
  precios_casas_sin_influencia <- precios_casas_filtrado
}

# Ajustar el modelo sin puntos influyentes
modelo_sin_influencia <- lm(formula_completa, data = precios_casas_sin_influencia)
summary(modelo_sin_influencia)

# Diagnóstico del modelo sin puntos influyentes
# Guardar las gráficas de diagnóstico
png("diagnostico_modelo_sin_influencia.png")
par(mfrow = c(2, 2))
plot(modelo_sin_influencia)
dev.off()

# Validación del modelo sin puntos influyentes con conjunto de prueba
datos_entrenamiento_sin_influencia <- precios_casas_sin_influencia[train_indices, ]
datos_prueba_sin_influencia <- precios_casas_sin_influencia[-train_indices, ]
modelo_sin_influencia_entrenamiento <- lm(formula_completa, data = datos_entrenamiento_sin_influencia)

# Predecir en los datos de prueba (modelo sin puntos influyentes)
predicciones_sin_influencia <- predict(modelo_sin_influencia_entrenamiento, newdata = datos_prueba_sin_influencia)

# Calcular RMSE, MAE y MAPE para el modelo sin puntos influyentes
rmse_sin_influencia <- sqrt(mean((datos_prueba_sin_influencia$SalePrice - predicciones_sin_influencia)^2))
mae_sin_influencia <- mean(abs(datos_prueba_sin_influencia$SalePrice - predicciones_sin_influencia))
mape_sin_influencia <- mean(abs((datos_prueba_sin_influencia$SalePrice - predicciones_sin_influencia) / datos_prueba_sin_influencia$SalePrice)) * 100

cat("\nError RMSE en el conjunto de prueba (modelo sin puntos influyentes):", rmse_sin_influencia, "\n")
cat("MAE:", mae_sin_influencia, "| MAPE:", mape_sin_influencia, "%\n")

# Paso 3: Transformación logarítmica (antes de dividir en entrenamiento y prueba)
precios_casas_sin_influencia$LogSalePrice <- log(precios_casas_sin_influencia$SalePrice)

# Ajustar el modelo con la variable transformada
formula_transformada <- as.formula(paste("LogSalePrice ~", paste(predictoras_numericas, collapse = " + ")))

# Dividir los datos con la transformación aplicada
datos_entrenamiento_sin_influencia <- precios_casas_sin_influencia[train_indices, ]
datos_prueba_sin_influencia <- precios_casas_sin_influencia[-train_indices, ]

# Ajustar el modelo transformado con datos de entrenamiento
modelo_transformado_entrenamiento <- lm(formula_transformada, data = datos_entrenamiento_sin_influencia)
summary(modelo_transformado_entrenamiento)

# Predecir en el conjunto de prueba (modelo transformado)
predicciones_log <- predict(modelo_transformado_entrenamiento, newdata = datos_prueba_sin_influencia)

# Transformar de nuevo las predicciones al valor original
predicciones_orig <- exp(predicciones_log)

# Calcular RMSE, MAE y MAPE para el modelo transformado
rmse_transformado <- sqrt(mean((datos_prueba_sin_influencia$SalePrice - predicciones_orig)^2))
mae_transformado <- mean(abs(datos_prueba_sin_influencia$SalePrice - predicciones_orig))
mape_transformado <- mean(abs((datos_prueba_sin_influencia$SalePrice - predicciones_orig) / datos_prueba_sin_influencia$SalePrice)) * 100

cat("\nError RMSE en el conjunto de prueba (modelo transformado):", rmse_transformado, "\n")
cat("MAE:", mae_transformado, "| MAPE:", mape_transformado, "%\n")

# Paso 4: Comparación de modelos
cat("Resumen de Comparaciones:\n")
cat("\nModelo Original (con puntos influyentes):\n")
cat("RMSE:", rmse_original, "| MAE:", mae_original, "| MAPE:", mape_original, "%\n")
cat("\nModelo Sin Puntos Influyentes:\n")
cat("RMSE:", rmse_sin_influencia, "| MAE:", mae_sin_influencia, "| MAPE:", mape_sin_influencia, "%\n")
cat("\nModelo Transformado:\n")
cat("RMSE:", rmse_transformado, "| MAE:", mae_transformado, "| MAPE:", mape_transformado, "%\n")

