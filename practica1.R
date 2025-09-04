library(readxl)   # leer archivos Excel
library(tidyr)    # reestructuración de datos
library(dplyr)    # manipulación de datos
library(writexl)  # exportar Excel
library(plotly)   # gráficos 3D interactivos

# Establecer directorio de trabajo
setwd("/home/merli_nx19/Documents/Mineria")
cat("Directorio de trabajo:", getwd(), "\n")

# Función para transformar Excel a formato MOLAP
transformar_a_molap <- function(file, indicador_nombre) {
  df <- read_excel(file, sheet = 1)
  
  # Normalizar nombres de columnas
  colnames(df)[1:2] <- c("Country_Name", "Country_Code")
  
  # Convertir a formato largo
  df_long <- df %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),   # columnas que son años
      names_to = "Year",
      values_to = "Value"
    ) %>%
    mutate(Indicator = indicador_nombre)  # agregar columna indicador
  
  return(df_long)
}

# Leer y transformar archivos Excel
df_industriales <- transformar_a_molap("2.xlsx", "Industriales")
df_quimicas     <- transformar_a_molap("1.xlsx", "Reacciones Quimicas")
df_total        <- transformar_a_molap("Total 3.xlsx", "Total")

# Unir todos los datos
df_molap <- bind_rows(df_industriales, df_quimicas, df_total) %>%
  mutate(Year = as.integer(Year))  # convertir año a número

# Mostrar primeras filas
print(head(df_molap, 15))

# Exportar archivos

write_xlsx(df_molap, "MOLAP_CO2.xlsx")
write.csv(df_molap, "MOLAP_CO2.csv", row.names = FALSE)
cat("Archivos exportados: MOLAP_CO2.xlsx y MOLAP_CO2.csv\n")

# Mapear índices para gráfico 3D
x_map <- setNames(0:(length(unique(df_molap$Year)) - 1), sort(unique(df_molap$Year)))
y_map <- setNames(0:(length(unique(df_molap$Country_Name)) - 1), unique(df_molap$Country_Name))
z_map <- setNames(0:(length(unique(df_molap$Indicator)) - 1), unique(df_molap$Indicator))

# Crear dataframe para plotly
df_plot <- df_molap %>%
  mutate(
    x = x_map[as.character(Year)],
    y = y_map[Country_Name],
    z = z_map[Indicator]
  )

# Gráfico 3D interactivo
fig <- plot_ly(
  df_plot,
  x = ~x,
  y = ~y,
  z = ~z,
  type = "scatter3d",
  mode = "markers",
  text = ~paste(
    "País:", Country_Name,
    "<br>Año:", Year,
    "<br>Indicador:", Indicator,
    "<br>Valor:", Value
  ),
  marker = list(
    size = 6,
    color = ~Value,
    colorscale = "Viridis",
    opacity = 0.8
  )
)

fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "Año", tickvals = 0:(length(x_map)-1), ticktext = names(x_map)),
    yaxis = list(title = "País", tickvals = 0:(length(y_map)-1), ticktext = names(y_map)),
    zaxis = list(title = "Indicador", tickvals = 0:(length(z_map)-1), ticktext = names(z_map))
  ),
  title = "Cubo OLAP - Emisiones de CO₂"
)

# Mostrar gráfico
fig
