# Instalar paquetes necesarios
install.packages("randomForest")
install.packages("ggplot2")


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
# Crear un diccionario de etiquetas descriptivas para las variables categóricas
etiquetas_categoricas <- list(
  Neighborhood = "Vecindario",
  BldgType = "Tipo de Vivienda",
  HouseStyle = "Estilo de Vivienda",
  ExterQual = "Calidad Exterior",
  CentralAir = "Aire Acondicionado Central",
  MSZoning = "Clasificación de Zonificación"
)

for (var in variables_categoricas) {
  # Obtener la etiqueta en español para la variable
  etiqueta <- etiquetas_categoricas[[var]]
  
  # Crear el gráfico
  p <- ggplot(datos_filtrados, aes(x = !!sym(var), y = SalePrice)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Precio de Venta según", etiqueta),
      x = etiqueta,
      y = "Precio de Venta"
    ) +
    theme_minimal()
  
  # Guardar el gráfico con un nombre basado en la etiqueta
  ggsave(filename = paste0(etiqueta, "_boxplot.png"), plot = p)
}

# 5. Diagramas de dispersión para variables numéricas contra SalePrice
# Variables numéricas relevantes (excluyendo SalePrice)
variables_relevantes <- c("GrLivArea", "TotalBsmtSF", "YearBuilt", "GarageCars")

# Etiquetas en español
etiquetas_variables <- list(
  GrLivArea = "Área habitable (pies cuadrados)",
  TotalBsmtSF = "Área total del sótano (pies cuadrados)",
  YearBuilt = "Año de construcción",
  GarageCars = "Número de coches en el garaje"
)

# Generar gráficos de dispersión para todas las variables relevantes
for (var in variables_relevantes) {
  if (var != "GarageCars") {
    # Crear el gráfico de dispersión
    etiqueta <- etiquetas_variables[[var]]
    p <- ggplot(datos_filtrados, aes_string(x = var, y = "SalePrice")) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "lm", color = "red") +
      labs(
        title = paste("Precio de Venta vs", etiqueta),
        x = etiqueta,
        y = "Precio de Venta"
      ) +
      theme_minimal()
    
    # Guardar el gráfico
    ggsave(paste0(etiqueta, "_scatterplot.png"), plot = p)
  } else {
    # Crear el boxplot para GarageCars
    etiqueta <- etiquetas_variables[[var]]
    p <- ggplot(datos_filtrados, aes(x = factor(GarageCars), y = SalePrice)) +
      geom_boxplot(fill = "lightblue", color = "black") +
      labs(
        title = paste("Precio de Venta según", etiqueta),
        x = etiqueta,
        y = "Precio de Venta"
      ) +
      theme_minimal()
    
    # Guardar el gráfico
    ggsave(paste0(etiqueta, "_boxplot.png"), plot = p)
  }
}

# Calcular y mostrar la correlación y el p-valor para cada variable relevante
for (var in variables_relevantes) {
  if (var != "GarageCars") {
    cor_test <- cor.test(datos_filtrados[[var]], datos_filtrados$SalePrice, method = "pearson")
    
    # Imprimir los resultados
    print(paste("La correlación entre", etiquetas_variables[[var]], "y Precio de Venta es:", round(cor_test$estimate, 3)))
    print(paste("El p-valor asociado es:", round(cor_test$p.value, 5)))
  } else {
    # Para GarageCars (variable categórica), no se calcula la correlación
    print(paste("Para", etiquetas_variables[[var]], "se generó un boxplot en lugar de un cálculo de correlación."))
  }
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
