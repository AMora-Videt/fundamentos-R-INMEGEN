# Cargar librerías necesarias
library(vroom)
library(dplyr)
library(ggplot2)


# Cargar los datos
OVNIS <- vroom(file = "https://data.biofreelancer.com/ovnis")

OVNIS$latitude <- as.numeric(OVNIS$latitude)
OVNIS$longitude <- as.numeric(OVNIS$longitude)

# Inspección de datos (Obligatorio usar V mayúscula)
View(OVNIS)
summary(OVNIS)

### Ejercicio 
# Filtar NAs
OVNIS_limpio <- na.omit(OVNIS)

### Ejercicio 
# Seleccionar y contar las 10 formas de OVNIS más comunes
top_formas <- OVNIS_limpio %>%
  count(shape) %>%          
  arrange(-n) %>%            
  slice(1:10)

print(top_formas)

### Ejercicio 
# Gráfico de barras con ggplot de los avistamientos más comunes
# Eje x = forma, Eje y número avistamientos por forma, quitar legenda
ggplot(data = top_formas, aes(x = shape, y = n, fill = shape)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")

## Mas ordenado para que se lean los nombres
ggplot(data = top_formas, aes(x = reorder(shape, n), y = n, fill = shape)) +
  geom_col() +                     
  coord_flip() +                   
  theme(legend.position = "none") + 
  labs(
    title = "Top 10 Formas de OVNIS",
    x = "Forma",
    y = "Número de Avistamientos")

### Ejercicio 
# Contar los países con más avistamientos
paises_top <- OVNIS_limpio %>%
  count(country) %>%
  arrange(-n)

print(paises_top)

### Ejercicio 
# Mapa de calor con los países con más avistamientos, x= país, y= número avistamientos
# cambiar el color de barras
#comentario# No entendi? es un  mapa y tambien un grafico de barras o solo el grafico de barras?
#grafica
ggplot(data = paises_top, aes(x = country, y = n)) +
  geom_col(fill = "darkred") +  
  labs(
    title = "Avistamientos por País", 
    x = "País", 
    y = "Número de Avistamientos"
  )

####### Parte 2

### Ejercicio 
# Crear una nueva variable de duración en minutos
OVNIS_limpio <- OVNIS_limpio %>%
  mutate(duracion_min = as.numeric(duration_seconds) / 60)

summary(OVNIS_limpio$duracion_min)

### Ejercicio 
### 1. Boxplot de duración de avistamientos por país ### 
### poner outliers en rojo y de for de asterisco
ggplot(data = OVNIS_limpio, aes(x = country, y = duracion_min)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) +
  coord_flip() + 
  labs(
    title = "Boxplot de duración de avistamientos por país", 
    x = "País", 
    y = "Duración (minutos)"
  )

##donde se ve mejro
ggplot(data = OVNIS_limpio, aes(x = country, y = duracion_min)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) +
  scale_y_log10() + 
  coord_flip() +
  labs(title = "Boxplot en Escala Logarítmica", y = "Duración (Escala Log10)")

### Ejercicio 
### 2. Histograma de duración de los avistamientos ###
ggplot(data = OVNIS_limpio, aes(x = duracion_min)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  labs(
    title = "Histograma de duración de los avistamientos",
    x = "Duración (minutos)",
    y = "Número de casos (Frecuencia)"
  ) +
  theme_minimal()

##donde se ve mejor usando log 
ggplot(data = OVNIS_limpio, aes(x = duracion_min)) +
  geom_histogram(fill = "darkgreen", color = "white", bins = 30) +
  scale_x_log10() + 
  labs(title = "Histograma en Escala Logarítmica", x = "Minutos (Log10)", y = "Frecuencia")

### Ejercicio 
### 3. Gráfico de correlación entre variables numéricas ###
# Seleccionamos solo columnas numéricas para calcular la correlación
num_vars <- OVNIS_limpio %>% 
  select(latitude, longitude, duration_seconds, duracion_min)

cor_matrix <- cor(num_vars, use = "complete.obs")

print(cor_matrix)
### Ejercicio 
# Mapa de calor de correlaciones 
#instale ggcorrplot
#install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(cor_matrix, 
           hc.order = TRUE,       
           type = "lower",        
           lab = TRUE,             
           colors = c("red", "white", "blue"), 
           title = "Mapa de calor de correlaciones")

### Ejercicio 
### 4. Distribución de probabilidad (densidad) de la duración ###
ggplot(data = OVNIS_limpio, aes(x = duracion_min)) +
  geom_density(fill = "blue", alpha = 0.4, color = "darkblue") +
  scale_x_log10() +
  labs(
    title = "Distribución de probabilidad (densidad) de la duración",
    subtitle = "Escala logarítmica para visualización de outliers",
    x = "Duración (minutos en Log10)",
    y = "Densidad"
  ) +
  theme_minimal()

