library(ggplot2)
library(dplyr)
library(readr)

epa_http <- read_table("epa-http.csv", col_names = FALSE)


nombres_columnas <- c("Origen", "Tiempo",  "Tipo" , "URL" , "Protocolo", "Código", "Bytes")
colnames(epa_http) <- nombres_columnas

epa_http$Tiempo <- gsub("\\[|\\]", "", epa_http$Tiempo)
epa_http$Tipo <- gsub("\"", "", epa_http$Tipo)
epa_http$Protocolo <- gsub("\"", "", epa_http$Protocolo)
epa_http$Bytes <- ifelse(is.na(as.numeric(epa_http$Bytes)), NA, as.numeric(epa_http$Bytes))


#2

# Crear tabla con columnas "Origen", "Código" y "Status"
nueva_tabla <- data.frame(Origen = epa_http$Origen, Código = epa_http$Código, Status = ifelse(epa_http$Código == 200, "Conectado", "Sin Conexión"))
nueva_tabla_sin_repetidos <- unique(nueva_tabla)

nueva_tabla_agrupada <- aggregate(Código ~ Origen + Status, data = nueva_tabla_sin_repetidos, FUN = function(x) ifelse(all(x == 200), 200, paste(unique(x), collapse = ",")))

nueva_tabla_agrupada <- nueva_tabla_agrupada[order(nueva_tabla_agrupada$Origen, nueva_tabla_agrupada$Código, nueva_tabla_agrupada$Status), ]

# Contar las filas por valor de "Status"
conectados <- sum(nueva_tabla_agrupada$Status == "Conectado")
sin_conexion <- sum(nueva_tabla_agrupada$Status == "Sin Conexión")

# Mostrar los resultados
cat("Filas con el valor 'Conectado':", conectados, "\n")
cat("Filas con el valor 'Sin Conexión':", sin_conexion, "\n")


#3

tipo_URL <- subset(epa_http, select = c("Tipo", "URL"))
tipo_URLimagenes <- subset(tipo_URL, grepl("\\.(png|jpg|gif|ico|JPG|GIF|ICO|PNG)$", URL))

frecuencia_tipo <- table(tipo_URLimagenes$Tipo)

print(frecuencia_tipo)

#4

# contar las ocurrencias de cada código de respuesta y guardar el resultado en un data frame
resumen_respuestas <- as.data.frame(table(epa_http$Código))

# renombrar las columnas del data frame
colnames(resumen_respuestas) <- c("Codigo", "Frecuencia")

# crear el gráfico de barras y agregar los valores encima de la gráfica
ggplot(data = resumen_respuestas, aes(x = Codigo, y = Frecuencia, fill = Codigo)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Frecuencia), vjust = -0.5) +
  labs(x = "Código de respuesta", y = "Frecuencia", title = "Distribución de respuestas del servidor")

# calcular el porcentaje correspondiente a cada código de respuesta
resumen_respuestas$Porcentaje <- prop.table(resumen_respuestas$Frecuencia) * 100

# crear el gráfico de dona con etiquetas de porcentaje
ggplot(data = resumen_respuestas, aes(x = "", y = Frecuencia, fill = Codigo)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, title = "Distribución de respuestas del servidor") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5))

#5 

# Separar el tiempo en días, horas, minutos y segundos
tiempo_separado <- strsplit(epa_http$Tiempo, ":")
dias <- sapply(tiempo_separado, "[[", 1)
horas <- sapply(tiempo_separado, "[[", 2)
minutos <- sapply(tiempo_separado, "[[", 3)
segundos <- sapply(tiempo_separado, "[[", 4)

# Calcular la hora a partir de los días, horas y minutos
hora <-  as.numeric(horas)

# Crear nueva tabla con la hora y la columna Bytes
Tabla_Hora_Bytes <- data.frame(Hora = hora, Bytes = epa_http$Bytes)

# Reemplazar valores NA por ceros en la columna "Bytes"
Tabla_Hora_Bytes$Bytes <- ifelse(is.na(Tabla_Hora_Bytes$Bytes), 0, Tabla_Hora_Bytes$Bytes)

Tabla_Hora_Bytes$Caracteres_URL <- nchar(epa_http$URL)

# seleccionar las columnas que se utilizarán para el clustering
datos_clustering <- Tabla_Hora_Bytes[, c("Caracteres_URL", "Bytes")]

# normalizar los datos
datos_normalizados <- scale(datos_clustering)

# realizar el clustering con k=3
set.seed(123) # establecer una semilla para reproducibilidad
kmeans_model_3 <- kmeans(datos_normalizados, centers = 3)

# realizar el clustering con k=5
set.seed(123) # establecer una semilla para reproducibilidad
kmeans_model_5 <- kmeans(datos_normalizados, centers = 5)

# imprimir los resultados
print(kmeans_model_3)
print(kmeans_model_5)

#6

# agregar la asignación de cluster como una columna adicional
datos_clustering$Cluster <- factor(kmeans_model_3$cluster)

# generar el scatter plot
ggplot(datos_clustering, aes(x = Caracteres_URL, y = Bytes, color = Cluster)) +
  geom_point() +
  ggtitle("Clustering con k=3") +
  labs(x = "Caracteres_URL", y = "Bytes")


# agregar la asignación de cluster como una columna adicional
datos_clustering$Cluster <- factor(kmeans_model_5$cluster)

# generar el scatter plot
ggplot(datos_clustering, aes(x = Caracteres_URL, y = Bytes, color = Cluster)) +
  geom_point() +
  ggtitle("Clustering con k=5") +
  labs(x = "Caracteres_URL", y = "Bytes")

