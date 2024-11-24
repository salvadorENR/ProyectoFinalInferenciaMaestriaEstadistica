# Cargar librerías necesarias
if (!requireNamespace("Hmisc", quietly = TRUE)) {
  install.packages("Hmisc")
}
library(Hmisc)  # Para obtener las significancias

if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}
library(ggcorrplot)

if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)    # Para calcular el VIF (Factor de Inflación de la Varianza)

# Seleccionar las variables predictoras numéricas relevantes
variables_predictoras <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars")

# Calcular la matriz de correlación entre las variables predictoras
matriz_correlacion <- cor(datos_filtrados[, variables_predictoras], use = "complete.obs")

# Calcular los valores de p para evaluar la significancia
resultado_corr <- rcorr(as.matrix(datos_filtrados[, variables_predictoras]))
p_values <- resultado_corr$P  # Extraer la matriz de p-valores

# Guardar los resultados en un archivo de texto
sink("correlation_results.txt")
cat("Matriz de Correlación:\n")
print(round(matriz_correlacion, 3))
cat("\n\nMatriz de P-Valores:\n")
print(round(p_values, 5))
sink()  # Finalizar la escritura en el archivo

# Visualizar la matriz de correlación con significancia
ggcorrplot(
  matriz_correlacion,
  method = "circle",        # Método de visualización: círculos
  type = "lower",           # Mostrar solo la parte inferior
  lab = TRUE,               # Mostrar valores de correlación
  lab_size = 4,             # Tamaño de las etiquetas
  p.mat = p_values,         # Agregar la matriz de p-valores
  sig.level = 0.05,         # Nivel de significancia
  insig = "blank",          # Dejar en blanco correlaciones no significativas
  colors = c("red", "white", "blue"),  # Colores para correlaciones
  title = "Matriz de Correlación con Significancia",
  legend.title = "Correlación"
) +
  labs(x = "Variable 1", y = "Variable 2") +  # Cambiar etiquetas de los ejes
  theme_minimal()

# Evaluar multicolinealidad con el VIF
modelo_vif <- lm(SalePrice ~ ., data = datos_filtrados[, c(variables_predictoras, "SalePrice")])
vif_valores <- vif(modelo_vif)

# Mostrar los valores del VIF
print("Valores del VIF:")
print(vif_valores)

# Interpretación de los valores del VIF:
print("Interpretación:")
if (any(vif_valores > 10)) {
  print("Algunas variables presentan problemas graves de multicolinealidad (VIF > 10).")
} else if (any(vif_valores > 5)) {
  print("Hay indicios de multicolinealidad moderada (5 < VIF < 10).")
} else {
  print("No hay indicios significativos de multicolinealidad (VIF < 5).")
}

