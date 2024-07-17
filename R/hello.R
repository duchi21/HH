library(readxl)
library(ggplot2)

# Función para graficar histogramas de columnas numéricas de un archivo Excel
graficar_histogramas <- function(archivo_excel, hoja = 1, carpeta_salida = "histogramas") {
  # Crear carpeta de salida si no existe
  if (!dir.exists(carpeta_salida)) {
    dir.create(carpeta_salida)
  }

  # Leer el archivo de Excel
  datos <- read_excel(archivo_excel, sheet = hoja)

  # Seleccionar solo las columnas numéricas
  datos_numericos <- datos %>% select_if(is.numeric)

  # Graficar las columnas numéricas
  for (columna in colnames(datos_numericos)) {
    p <- ggplot(datos, aes_string(x = columna)) +
      geom_histogram(binwidth = 30, fill = "blue", color = "black") +
      labs(title = paste("Histograma de", columna),
           x = columna, y = "Frecuencia") +
      theme_minimal()

    # Guardar el histograma como archivo PNG
    ggsave(filename = paste0(carpeta_salida, "/histograma_", columna, ".png"), plot = p)
  }

  message("Histograms saved in folder: ", carpeta_salida)
}
