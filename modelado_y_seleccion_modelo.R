# Cargar las librerías necesarias
library(MASS)
library(dplyr)
library(car)

# Paso 1: Preparar los datos para el modelado
# Seleccionar las variables significativas según el análisis previo
selected_vars <- c("SalePrice", "GrLivArea", "OverallQual")
model_data <- numerical_vars[, selected_vars]

# Verificar si hay valores faltantes en los datos seleccionados
if (any(is.na(model_data))) {
  stop("Existen valores faltantes en los datos seleccionados para el modelado.")
}

# Paso 2: Construir los modelos
# Modelo completo (ambas variables independientes)
full_model <- lm(SalePrice ~ GrLivArea + OverallQual, data = model_data)

# Modelo con solo GrLivArea
model_grlivarea <- lm(SalePrice ~ GrLivArea, data = model_data)

# Modelo con solo OverallQual
model_overallqual <- lm(SalePrice ~ OverallQual, data = model_data)

# Paso 3: Resumen de los modelos
cat("\nResumen del modelo completo:\n")
print(summary(full_model))

cat("\nResumen del modelo con GrLivArea:\n")
print(summary(model_grlivarea))

cat("\nResumen del modelo con OverallQual:\n")
print(summary(model_overallqual))

# Paso 4: Comparación de modelos usando AIC
aic_full <- AIC(full_model)
aic_grlivarea <- AIC(model_grlivarea)
aic_overallqual <- AIC(model_overallqual)

cat("\nComparación de AIC:\n")
cat("Modelo completo AIC:", aic_full, "\n")
cat("Modelo con GrLivArea AIC:", aic_grlivarea, "\n")
cat("Modelo con OverallQual AIC:", aic_overallqual, "\n")

# Paso 5: Evaluación de modelos (R^2 ajustado y AIC)
cat("\nComparación de modelos (R^2 ajustado y AIC):\n")
comparison_table <- data.frame(
  Modelo = c("Completo", "Solo GrLivArea", "Solo OverallQual"),
  R2_Ajustado = c(summary(full_model)$adj.r.squared,
                  summary(model_grlivarea)$adj.r.squared,
                  summary(model_overallqual)$adj.r.squared),
  AIC = c(aic_full, aic_grlivarea, aic_overallqual)
)
print(comparison_table)

# Paso 6: Guardar los resultados de comparación
write.csv(comparison_table, "comparacion_modelos.csv", row.names = FALSE)

# Paso 7: Guardar diagnósticos del modelo completo
png("diagnostico_modelo_completo.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Configurar para mostrar múltiples gráficas
plot(full_model, main = "Diagnóstico del Modelo Completo")
dev.off()

# Paso 8: Guardar diagnósticos del modelo con GrLivArea
png("diagnostico_modelo_grlivarea.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Configurar para mostrar múltiples gráficas
plot(model_grlivarea, main = "Diagnóstico del Modelo con GrLivArea")
dev.off()

# Paso 9: Guardar diagnósticos del modelo con OverallQual
png("diagnostico_modelo_overallqual.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Configurar para mostrar múltiples gráficas
plot(model_overallqual, main = "Diagnóstico del Modelo con OverallQual")
dev.off()

# Paso 10: Comparación adicional y selección del modelo completo
cat("\nEl modelo completo se selecciona debido a un balance entre menor AIC y mayor R^2 ajustado.\n")

# Paso 11: Aplicar transformaciones (etapa posterior)
# Aplicar log-transformación para abordar no linealidad y heteroscedasticidad
model_data$LogSalePrice <- log(model_data$SalePrice)
model_data$LogGrLivArea <- log(model_data$GrLivArea)

# Modelo con transformaciones
transformed_model <- lm(LogSalePrice ~ LogGrLivArea + OverallQual, data = model_data)
cat("\nResumen del modelo con transformaciones:\n")
print(summary(transformed_model))

# Diagnósticos del modelo transformado
cat("\nDiagnósticos del modelo transformado:\n")
png("diagnosticos_transformed_model.png", width = 1200, height = 1000)
par(mfrow = c(2, 2))  # Configurar para mostrar múltiples gráficas
plot(transformed_model)
dev.off()

# Paso 12: Comparación final de AIC
aic_transformed <- AIC(transformed_model)
cat("\nComparación final de AIC:\n")
cat("Modelo completo AIC:", aic_full, "\n")
cat("Modelo transformado AIC:", aic_transformed, "\n")

# Evaluación final
if (aic_transformed < aic_full) {
  cat("\nEl modelo transformado es preferible debido a un menor AIC.\n")
} else {
  cat("\nEl modelo completo se mantiene como preferido.\n")
}

# Guardar el modelo transformado
saveRDS(transformed_model, file = "modelo_transformado.rds")
cat("\nEl modelo transformado ha sido guardado como 'modelo_transformado.rds'.\n")
