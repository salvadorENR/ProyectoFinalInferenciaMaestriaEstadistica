# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# Paso 1: Cargar el conjunto de datos
# Reemplazar 'house_prices.csv' con el nombre o ruta del archivo
datos <- read.csv("house_prices.csv", stringsAsFactors = FALSE)

# Paso 2: Seleccionar las variables clave justificadas en el análisis teórico
variables_clave <- c("SalePrice", "Neighborhood", "MSZoning", "GrLivArea", "LotArea", 
                     "OverallQual", "GarageArea", "TotalBsmtSF", "YearBuilt")
datos_seleccionados <- datos[, variables_clave]

# Paso 3: Detectar el tipo de cada variable
# Clasificar las variables en numéricas o categóricas
tipo_variable <- sapply(datos_seleccionados, function(x) {
  if (is.character(x) || is.factor(x)) {
    "Categorical"
  } else if (is.numeric(x)) {
    "Numerical"
  } else {
    "Other"
  }
})

# Mostrar el tipo de cada variable
cat("\nTipos de variables seleccionadas:\n")
print(tipo_variable)

# Paso 4: Análisis de valores faltantes 
# Calcular valores faltantes por columna
valores_faltantes <- colSums(is.na(datos_seleccionados))
datos_faltantes <- data.frame(Variable = names(valores_faltantes), 
                              Faltantes = valores_faltantes, 
                              Tipo = tipo_variable)
cat("\nResumen de valores faltantes:\n")
print(datos_faltantes)

# Paso 5: Análisis descriptivo ajustado al tipo de variable

# Crear carpeta para guardar las gráficas
if (!dir.exists("graficas")) {
  dir.create("graficas")
}

# Variables numéricas
numerical_vars <- names(datos_seleccionados)[tipo_variable[names(datos_seleccionados)] == "Numerical"]
if (length(numerical_vars) > 0) {
  cat("\nResumen estadístico de variables numéricas:\n")
  print(summary(datos_seleccionados[numerical_vars]))
  
  # Guardar histogramas de variables numéricas
  for (var in numerical_vars) {
    p <- ggplot(datos_seleccionados, aes(x = !!sym(var))) +
      geom_histogram(fill = "steelblue", color = "black", bins = 30) +
      labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
      theme_minimal()
    ggsave(filename = paste0("graficas/Distribucion_", var, ".png"), plot = p, width = 8, height = 6)
  }
}

# Variables categóricas
categorical_vars <- names(datos_seleccionados)[tipo_variable[names(datos_seleccionados)] == "Categorical"]
if (length(categorical_vars) > 0) {
  cat("\nTablas de frecuencia para variables categóricas:\n")
  for (var in categorical_vars) {
    cat("\nVariable:", var, "\n")
    print(table(datos_seleccionados[[var]]))
  }
  
  # Guardar gráficos de barras de variables categóricas
  for (var in categorical_vars) {
    p <- ggplot(datos_seleccionados, aes(x = !!sym(var))) +
      geom_bar(fill = "steelblue", color = "black") +
      labs(title = paste("Frecuencia de", var), x = var, y = "Cantidad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(filename = paste0("graficas/Frecuencia_", var, ".png"), plot = p, width = 8, height = 6)
  }
}

# Paso 6: Visualización de relaciones clave
# Relación entre variables numéricas y la variable dependiente (SalePrice)
if ("SalePrice" %in% numerical_vars) {
  for (var in numerical_vars) {
    if (var != "SalePrice") {
      p <- ggplot(datos_seleccionados, aes(x = !!sym(var), y = SalePrice)) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Relación entre", var, "y Precio de Venta"), x = var, y = "Precio de Venta")
      ggsave(filename = paste0("graficas/Relacion_", var, "_SalePrice.png"), plot = p, width = 8, height = 6)
    }
  }
}

# Paso final: Confirmar que todas las variables fueron procesadas
cat("\nTodas las variables seleccionadas fueron analizadas sin problemas.\n")
