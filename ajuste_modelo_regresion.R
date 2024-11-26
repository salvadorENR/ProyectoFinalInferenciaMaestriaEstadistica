# Cargar el conjunto de datos filtrado
precios_casas_filtrado <- read.csv("precios_casas_filtrado.csv")

# Paso 1: Transformación de la variable dependiente
# Aplicar transformación logarítmica a SalePrice para estabilizar la varianza
precios_casas_filtrado$LogSalePrice <- log(precios_casas_filtrado$SalePrice)

# Paso 2: Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
train_indices <- sample(1:nrow(precios_casas_filtrado), 0.8 * nrow(precios_casas_filtrado))
datos_entrenamiento <- precios_casas_filtrado[train_indices, ]
datos_prueba <- precios_casas_filtrado[-train_indices, ]

# Paso 3: Modelo de regresión lineal con variable transformada
library(MASS)
library(car)

# Variables predictoras y fórmula del modelo
predictoras_numericas <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars")
formula_completa <- as.formula(paste("LogSalePrice ~", paste(predictoras_numericas, collapse = " + ")))

# Ajustar el modelo completo con los datos de entrenamiento
modelo_completo <- lm(formula_completa, data = datos_entrenamiento)
summary(modelo_completo)

# Paso 4: Verificar y manejar puntos influyentes
# Identificar puntos influyentes usando Cook's Distance
cooks_distances <- cooks.distance(modelo_completo)
influential_points <- which(cooks_distances > (4 / nrow(datos_entrenamiento)))
cat("Puntos influyentes identificados:", influential_points, "\n")

# Crear un nuevo conjunto de datos sin los puntos influyentes
datos_sin_influencia <- datos_entrenamiento[-influential_points, ]

# Ajustar el modelo nuevamente sin puntos influyentes
modelo_sin_influencia <- lm(formula_completa, data = datos_sin_influencia)
summary(modelo_sin_influencia)

# Paso 5: Diagnóstico del modelo ajustado
# Graficar los diagnósticos del modelo
png("diagnostico_modelo_sin_influencia.png")
par(mfrow = c(2, 2))
plot(modelo_sin_influencia)
dev.off()

# Paso 6: Comparar modelo con y sin puntos influyentes
anova(modelo_completo, modelo_sin_influencia)

# Paso 7: Validación del modelo con datos de prueba
# Predecir en los datos de prueba
predicciones <- predict(modelo_sin_influencia, newdata = datos_prueba)

# Volver a transformar las predicciones (exponencial inversa del logaritmo)
predicciones_orig <- exp(predicciones)

# Calcular el error RMSE
error_rmse <- sqrt(mean((datos_prueba$SalePrice - predicciones_orig)^2))
cat("Error RMSE en los datos de prueba:", error_rmse, "\n")

# Paso 8: Guardar resultados y métricas
capture.output(
  {
    cat("Resumen del Modelo Completo (con puntos influyentes):\n")
    print(summary(modelo_completo))
    
    cat("\nResumen del Modelo Sin Puntos Influyentes:\n")
    print(summary(modelo_sin_influencia))
    
    cat("\nError RMSE en los datos de prueba (modelo ajustado):", error_rmse, "\n")
  },
  file = "resultados_modelo_mejorado.txt"
)
