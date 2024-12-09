# Definir la función de transformación Box-Cox
box_cox <- function(y, lambda) {
  if (lambda == 0) {
    return(log(y))  # Transformación logarítmica cuando lambda = 0
  } else {
    return((y^lambda - 1) / lambda)
  }
}

# Generar datos de ejemplo
y <- seq(0.1, 10, by = 0.1)  # Datos de ejemplo (evitar ceros para prevenir problemas con log)

# Aplicar la transformación Box-Cox para diferentes valores de lambda
valores_lambda <- c(-1, 0, 0.5, 1, 2)  # Diferentes valores de lambda
valores_transformados <- lapply(valores_lambda, function(l) box_cox(y, l))

# Graficar las transformaciones
plot(y, y, type = "l", col = "black", lwd = 2, ylim = c(-5, 15),
     main = "Transformación Box-Cox para Diferentes Valores de Lambda",
     xlab = "Variable Original (y)", ylab = "Variable Transformada")
colores <- c("red", "blue", "green", "orange", "purple")
etiquetas_leyenda <- c(expression(lambda == -1), expression(lambda == 0), 
                       expression(lambda == 0.5), expression(lambda == 1), 
                       expression(lambda == 2))

# Agregar los valores transformados al gráfico
for (i in seq_along(valores_lambda)) {
  lines(y, valores_transformados[[i]], col = colores[i], lwd = 2)
}

# Agregar la leyenda
legend("topleft", legend = etiquetas_leyenda, col = colores, lwd = 2, bty = "n")




# Simular datos para replicar
set.seed(123)  # Para reproducibilidad
x <- seq(1, 20, length.out = 50)  # Variable predictora
y <- 10 * x + rnorm(50, mean = 0, sd = 20)  # Variable dependiente con ruido

# Ajustar un modelo lineal
modelo <- lm(y ~ x)

# Extraer residuos y valores ajustados
residuos <- resid(modelo)
valores_ajustados <- fitted(modelo)

# Crear las gráficas una al lado de la otra
png("graficas_replicadas_exactas.png", width = 1000, height = 500)  # Guardar como archivo
par(mfrow = c(1, 2))  # Organizar gráficas en una fila y dos columnas

# Gráfica 1: Diagrama de dispersión de y vs x
plot(x, y, main = "", xlab = expression(x), ylab = expression(y), pch = 16, col = "black")

# Gráfica 2: Gráfica de residuos (e vs valores ajustados)
plot(valores_ajustados, residuos, main = "", xlab = expression(hat(y)), ylab = expression(e), pch = 16, col = "black")
abline(h = 0, lty = 2, col = "gray")  # Línea horizontal en residual = 0

dev.off()  # Cerrar el dispositivo gráfico







