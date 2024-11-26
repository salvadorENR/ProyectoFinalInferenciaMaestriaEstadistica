# Cargar el conjunto de datos filtrado
precios_casas_filtrado <- read.csv("precios_casas_filtrado.csv")

# Analizar correlaciones y multicolinealidad
library(Hmisc)
library(ggcorrplot)
library(car)

# Variables predictoras numéricas
predictoras_numericas <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars")

# Matriz de correlación y significancia
matriz_correlacion <- cor(precios_casas_filtrado[, predictoras_numericas], use = "complete.obs")
resultados_corr <- rcorr(as.matrix(precios_casas_filtrado[, predictoras_numericas]))

# Guardar resultados de correlación
sink("resultados_correlacion.txt")
cat("Matriz de Correlación:\n")
print(round(matriz_correlacion, 3))
cat("\n\nMatriz de P-Valores:\n")
print(round(resultados_corr$P, 5))
sink()

# Evaluar el VIF
modelo_vif <- lm(SalePrice ~ ., data = precios_casas_filtrado[, c(predictoras_numericas, "SalePrice")])
valores_vif <- vif(modelo_vif)
print("Valores del VIF:")
print(valores_vif)
