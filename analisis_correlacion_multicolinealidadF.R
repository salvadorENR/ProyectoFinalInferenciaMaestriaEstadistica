# Cargar las librerías necesarias
library(dplyr)
library(GGally)
library(car)

# Paso 1: Seleccionar solo las variables numéricas para el análisis de correlación
numerical_vars <- datos_seleccionados %>% 
  select_if(is.numeric)

# Paso 2: Calcular la matriz de correlación
correlation_matrix <- cor(numerical_vars, use = "complete.obs")

# Paso 3: Visualizar la matriz de correlación con formato "pairplot"
pairplot <- ggpairs(
  numerical_vars,
  title = "Relaciones entre Variables Numéricas",
  upper = list(continuous = wrap("cor", size = 3)),
  diag = list(continuous = wrap("densityDiag")),
  lower = list(continuous = wrap("smooth", alpha = 0.3))
)

# Guardar la visualización
ggsave(filename = "matriz_correlacion_pairplot.png", plot = pairplot, width = 10, height = 10)

# Paso 4: Identificar correlaciones significativas (>|0.7|)
# Transformar la matriz de correlación en una tabla para identificar valores altos
significant_correlations <- which(abs(correlation_matrix) > 0.7 & correlation_matrix != 1, 
                                  arr.ind = TRUE)
correlation_table <- data.frame(
  Variable1 = rownames(correlation_matrix)[significant_correlations[, 1]],
  Variable2 = colnames(correlation_matrix)[significant_correlations[, 2]],
  Correlation = correlation_matrix[significant_correlations]
)

# Eliminar duplicados (pares repetidos de correlaciones)
correlation_table <- correlation_table[!duplicated(t(apply(correlation_table, 1, sort))), ]

# Mostrar correlaciones significativas
cat("\nCorrelaciones significativas (>|0.7|):\n")
print(correlation_table)

# Paso 5: Visualización de relaciones significativas
if (nrow(correlation_table) > 0) {
  for (i in 1:nrow(correlation_table)) {
    var1 <- correlation_table$Variable1[i]
    var2 <- correlation_table$Variable2[i]
    
    if (var1 == "OverallQual" || var2 == "OverallQual") {
      # Usar un gráfico de cajas para OverallQual
      ggplot(numerical_vars, aes(x = as.factor(.data[["OverallQual"]]), y = .data[["SalePrice"]])) +
        geom_boxplot(fill = "steelblue", color = "black") +
        labs(title = "Relación entre OverallQual y SalePrice",
             x = "OverallQual (Calidad General)", y = "SalePrice (Precio de Venta)") +
        theme_minimal() -> plot
    } else {
      # Usar un gráfico de dispersión para otras relaciones
      ggplot(numerical_vars, aes(x = .data[[var1]], y = .data[[var2]])) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Relación entre", var1, "y", var2),
             x = var1, y = var2) +
        theme_minimal() -> plot
    }
    
    # Guardar las visualizaciones
    ggsave(filename = paste0("graficas/Relacion_", var1, "_", var2, ".png"), 
           plot = plot, width = 8, height = 6)
  }
} else {
  cat("\nNo se encontraron correlaciones significativas mayores a 0.7.\n")
}

# Paso 6: Evaluación de multicolinealidad con VIF
vif_model <- lm(SalePrice ~ ., data = numerical_vars)
vif_values <- vif(vif_model)

# Mostrar los valores de VIF
vif_table <- data.frame(Variable = names(vif_values), VIF = vif_values)
cat("\nValores de VIF para detectar multicolinealidad:\n")
print(vif_table)

# Identificar variables con VIF > 10
high_vif <- vif_table %>% filter(VIF > 10)
cat("\nVariables con alta multicolinealidad (VIF > 10):\n")
print(high_vif)

# Recomendación para manejo de multicolinealidad
if (nrow(high_vif) > 0) {
  cat("\nSugerencia: Considerar eliminar una de las variables con alta multicolinealidad o realizar transformaciones.\n")
} else {
  cat("\nNo se detectaron problemas significativos de multicolinealidad (VIF <= 10).\n")
}
