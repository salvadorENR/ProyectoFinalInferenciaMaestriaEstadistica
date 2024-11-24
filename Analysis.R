# Instalar paquetes necesarios
install.packages("randomForest")

# Cargar bibliotecas necesarias
library(tidyverse)  # Para manipulación y visualización de datos
library(caret)      # Para modelos de aprendizaje automático
library(pROC)       # Para curvas ROC y AUC
library(e1071)      # Para modelos de clasificación como SVM
library(randomForest)  # Para Random Forest
library(ggplot2)    # Para gráficos
library(dplyr)      # Para manipulación de datos

# Cargar el dataset
datos <- read.csv("house_prices.csv")

# Especificar las variables a incluir
variables_seleccionadas <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", 
                             "GarageCars", "SalePrice", "Neighborhood", "BldgType", 
                             "HouseStyle", "ExterQual", "CentralAir", "MSZoning")

# Crear un nuevo dataset con solo las variables seleccionadas
datos_filtrados <- datos[, variables_seleccionadas]

# Ver las primeras filas del nuevo dataset
head(datos_filtrados)

# Guardar el nuevo dataset en un archivo CSV
write.csv(datos_filtrados, "precios_casas_filtrado.csv", row.names = FALSE)

# Cargar el dataset filtrado
datos_filtrados <- read.csv("precios_casas_filtrado.csv")

# 1. Resumen estadístico para variables numéricas
variables_numericas <- c("LotArea", "GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars", "SalePrice")
summary(datos_filtrados[, variables_numericas])

# 2. Tablas de frecuencia para variables categóricas
variables_categoricas <- c("Neighborhood", "BldgType", "HouseStyle", "ExterQual", "CentralAir", "MSZoning")
lapply(datos_filtrados[, variables_categoricas], table)

# 3. Visualizaciones para variables numéricas
# Histograma para cada variable numérica
# Definir un mapeo de nombres de variables a etiquetas representativas
etiquetas_variables <- list(
  LotArea = "Área del Lote (pies cuadrados)",
  GrLivArea = "Área Habitable (pies cuadrados)",
  TotalBsmtSF = "Área Total del Sótano (pies cuadrados)",
  YearBuilt = "Año de Construcción",
  GarageCars = "Número de Coches en el Garaje",
  SalePrice = "Precio de Venta (USD)"
)

for (var in variables_numericas) {
  # Obtener la etiqueta representativa para la variable
  etiqueta <- etiquetas_variables[[var]]
  
  # Crear el gráfico
  p <- ggplot(datos_filtrados, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribución de", etiqueta), x = etiqueta, y = "Frecuencia") +
    theme_minimal()
  
  # Guardar el gráfico
  ggsave(paste0(etiqueta, "_histograma.png"), plot = p)
}

# 4. Diagramas de caja para mostrar la relación entre variables categóricas y SalePrice
for (var in variables_categoricas) {
  ggplot(datos_filtrados, aes_string(x = var, y = "SalePrice")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(title = paste("Precio de Venta según", var), x = var, y = "Precio de Venta") +
    theme_minimal() +
    ggsave(paste0(var, "_boxplot.png"))
}

# 5. Diagramas de dispersión para variables numéricas contra SalePrice
for (var in variables_numericas[-length(variables_numericas)]) { # Excluir SalePrice
  ggplot(datos_filtrados, aes_string(x = var, y = "SalePrice")) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste("Precio de Venta vs", etiquetas_variables[[var]]), 
         x = etiquetas_variables[[var]], y = "Precio de Venta") +
    theme_minimal() +
    ggsave(paste0(etiquetas_variables[[var]], "_diagrama_dispersion.png"))
}

# Guardar el resumen estadístico y las tablas de frecuencia en un archivo de texto
capture.output(
  {
    cat("Resumen Estadístico para Variables Numéricas:\n")
    print(summary(datos_filtrados[, variables_numericas]))
    cat("\nTablas de Frecuencia para Variables Categóricas:\n")
    print(lapply(datos_filtrados[, variables_categoricas], table))
  },
  file = "analisis_descriptivo.txt"
)
