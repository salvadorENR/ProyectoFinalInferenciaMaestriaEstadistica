# Cargar el conjunto de datos filtrado
precios_casas_filtrado <- read.csv("precios_casas_filtrado.csv")

# Modelo de regresión lineal
library(MASS)
library(car)

# Variables predictoras y fórmula del modelo
predictoras_numericas <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars")
formula_completa <- as.formula(paste("SalePrice ~", paste(predictoras_numericas, collapse = " + ")))

# Ajustar el modelo completo
modelo_completo <- lm(formula_completa, data = precios_casas_filtrado)
summary(modelo_completo)

# Modelo óptimo usando Stepwise
modelo_optimo <- stepAIC(modelo_completo, direction = "both", trace = FALSE)
summary(modelo_optimo)

# Diagnóstico y visualización
par(mfrow = c(2, 2))
plot(modelo_optimo)

# Guardar resultados
capture.output(
  {
    cat("Resumen del Modelo Completo:\n")
    print(summary(modelo_completo))
    cat("\nResumen del Modelo Óptimo:\n")
    print(summary(modelo_optimo))
  },
  file = "resultados_modelo_regresion.txt"
)
