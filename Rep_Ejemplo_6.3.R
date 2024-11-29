# Cargar los datos del ejercicio
# Nota: asegúrate de tener un archivo "trabajadores.dat" en el directorio de trabajo
data <- read.table("trabajadores.dat", header = FALSE)

# Renombrar las columnas según la descripción
colnames(data) <- c("Supervisores", "Trabajadores")

# Visualización inicial de los datos
head(data)

# Ajustar el modelo inicial (sin transformaciones)
modelo_inicial <- lm(Supervisores ~ Trabajadores, data = data)
summary(modelo_inicial)

# Gráficas iniciales de residuos
png("residuos_iniciales.png")
par(mfrow = c(2, 2))
plot(modelo_inicial, main = "Diagnóstico del modelo inicial")
dev.off()

# Transformación cuadrática (Trabajadores^2)
data$Trabajadores_cuadrado <- data$Trabajadores^2
modelo_cuadratico <- lm(Supervisores ~ Trabajadores + Trabajadores_cuadrado, data = data)
summary(modelo_cuadratico)

# Gráficas para el modelo cuadrático
png("residuos_modelo_cuadratico.png")
par(mfrow = c(2, 2))
plot(modelo_cuadratico, main = "Diagnóstico del modelo cuadrático")
dev.off()

# Transformación logarítmica en Supervisores
data$LogSupervisores <- log(data$Supervisores)
modelo_log <- lm(LogSupervisores ~ Trabajadores, data = data)
summary(modelo_log)

# Gráficas para el modelo logarítmico
png("residuos_modelo_log.png")
par(mfrow = c(2, 2))
plot(modelo_log, main = "Diagnóstico del modelo logarítmico")
dev.off()

# Transformación log-log (logaritmo en Supervisores y Trabajadores)
data$LogTrabajadores <- log(data$Trabajadores)
modelo_log_log <- lm(LogSupervisores ~ LogTrabajadores, data = data)
summary(modelo_log_log)

# Gráficas para el modelo log-log
png("residuos_modelo_log_log.png")
par(mfrow = c(2, 2))
plot(modelo_log_log, main = "Diagnóstico del modelo log-log")
dev.off()

# Transformación inversa (1/Trabajadores)
data$InvTrabajadores <- 1 / data$Trabajadores
modelo_inverso <- lm(LogSupervisores ~ InvTrabajadores, data = data)
summary(modelo_inverso)

# Gráficas para el modelo con transformación inversa
png("residuos_modelo_inverso.png")
par(mfrow = c(2, 2))
plot(modelo_inverso, main = "Diagnóstico del modelo con transformación inversa")
dev.off()

# Comparación final
cat("Resumen de modelos ajustados:\n")
cat("Modelo Inicial:\n")
summary(modelo_inicial)
cat("Modelo Cuadrático:\n")
summary(modelo_cuadratico)
cat("Modelo Logarítmico:\n")
summary(modelo_log)
cat("Modelo Log-Log:\n")
summary(modelo_log_log)
cat("Modelo Inverso:\n")
summary(modelo_inverso)
