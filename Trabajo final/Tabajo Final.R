# Instala el paquete 'readr' si aún no está instalado
# install.packages("readr")

# Carga la biblioteca readr
library(readr)
library(dplyr)
library(ggplot2)

# Especifica la ruta de tu archivo CSV
ruta_del_csv <- "Ocupados.csv"

# Carga el archivo CSV en un data frame y usa la primera fila como nombres de variables, con el separador ";"
data <- read_csv2(ruta_del_csv, col_names = TRUE)

# Muestra las primeras filas de la base de datos para verificar la carga
head(data)

# Muestra la estructura de la base de datos
str(data)

# Selecciona solo algunas columnas del data frame original
columnas_seleccionadas <- c("DPTO", "P6424S5", "P6460", "P6500", "P6790", "P6426")
Datos.muestreo <- data[, columnas_seleccionadas, drop = FALSE]

# Muestra las primeras filas y un resumen de las nuevas columnas seleccionadas
head(Datos.muestreo)
summary(Datos.muestreo)


# Cambia los nombres de las variables según tus preferencias
nuevos_nombres <- c("Departamento", "Licencia_enfermedad", "Tipo_contrato", "Cuanto_mes", "Trabjo_año", "Tiempo_trabajo")
colnames(Datos.muestreo) <- nuevos_nombres

# Muestra las primeras filas con los nuevos nombres
head(Datos.muestreo)

# Elimina todas las filas que contienen valores NA
Datos.sin_NA <- na.omit(Datos.muestreo)

# Muestra las primeras filas de la base de datos sin NA
head(Datos.sin_NA)

Datos.sin_NA <- mutate(Datos.sin_NA, Departamento = as.numeric(Departamento))
Datos.sin_NA <- Datos.sin_NA %>%
  filter(Tipo_contrato != 9)
Datos.sin_NA <- Datos.sin_NA %>%
  mutate(Tipo_contrato = Tipo_contrato - 1,
         Licencia_enfermedad = Licencia_enfermedad - 1)

head(Datos.sin_NA)
summary(Datos.sin_NA)

# Calcula el rango intercuartílico para Cuanto_mes y Tiempo_trabajo
iqr_cuanto_mes <- IQR(Datos.sin_NA$Cuanto_mes)
iqr_tiempo_trabajo <- IQR(Datos.sin_NA$Tiempo_trabajo)

# Calcula los límites superior e inferior para Cuanto_mes y Tiempo_trabajo
limite_superior_cuanto_mes <- quantile(Datos.sin_NA$Cuanto_mes, 0.75) + 1.5 * iqr_cuanto_mes
limite_inferior_cuanto_mes <- quantile(Datos.sin_NA$Cuanto_mes, 0.25) - 1.5 * iqr_cuanto_mes

limite_superior_tiempo_trabajo <- quantile(Datos.sin_NA$Tiempo_trabajo, 0.75) + 1.5 * iqr_tiempo_trabajo
limite_inferior_tiempo_trabajo <- quantile(Datos.sin_NA$Tiempo_trabajo, 0.25) - 1.5 * iqr_tiempo_trabajo

# Filtra las observaciones sin datos atípicos
Datos.sin_outliers <- Datos.sin_NA %>%
  filter(
    Cuanto_mes >= limite_inferior_cuanto_mes & Cuanto_mes <= limite_superior_cuanto_mes,
    Tiempo_trabajo >= limite_inferior_tiempo_trabajo & Tiempo_trabajo <= limite_superior_tiempo_trabajo
  )

# Muestra las primeras filas de la base de datos sin datos atípicos
head(Datos.sin_outliers)
summary(Datos.sin_outliers)


# Crea un diagrama de caja para la variable "Cuanto_mes"
ggplot(Datos.sin_outliers, aes(x = "", y = Cuanto_mes)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box plot Cuánto ganó en el último mes", y = "Pesos colombianos")


# Crea un diagrama de caja para la variable "Tiempo_trabajo"
ggplot(Datos.sin_outliers, aes(x = "", y = Tiempo_trabajo)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box plot tiempo trabajado en la empresa.", y = "Tiempo en meses")

# Crea un gráfico de barras para la variable "Departamento"
ggplot(Datos.sin_outliers, aes(x = Departamento)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribución de Departamentos", x = "Código departamento", y = "Frecuencia")

# Crea un gráfico de barras para la variable "Licencia_enfermedad"
ggplot(Datos.sin_outliers, aes(x = Licencia_enfermedad)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Histograma de si tiene o no licencia médica remunerada", x = "0 no recibe - 1 recibe", y = "Frecuencia")

# Crea un gráfico de barras para la variable "Tipo_contrato"
ggplot(Datos.sin_outliers, aes(x = Tipo_contrato)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Histograma de Tipos de Contrato", x = "0 contrato indefinido - 1 contrato fijo", y = "Frecuencia")




###### primera etapa 

## Calculo del S y p necesarios de la prueba piloto

# Selecciona aleatoriamente 200 observaciones
muestra_200 <- sample_n(Datos.sin_outliers, 200)

# Calcula el promedio y la desviación estándar de las variables especificadas
muestra_200 <- muestra_200 %>%
  mutate(Licencia_enfermedad = mean(Licencia_enfermedad),
         Tipo_contrato = mean(Tipo_contrato),
         Cuanto_mes = mean(Cuanto_mes),
         Trabjo_año = mean(Trabjo_año),
         Tiempo_trabajo = mean(Tiempo_trabajo),
         Desviacion_Cuanto_mes = sd(Cuanto_mes),
         Desviacion_Trabjo_año = sd(Trabjo_año),
         Desviacion_Tiempo_trabajo = sd(Tiempo_trabajo))

# Muestra las primeras filas de la nueva base de datos
head(muestra_200)


n.mas=function(tipo,N,s,e,p,alpha){
  if(tipo=="t"){n=round((qnorm(1-alpha/2)^2*N^2*s^2)/(e^2+qnorm(1-alpha/2)^2*N*s^2),0)}
  if(tipo=="t"){return(n)}
  if(tipo=="m"){n=round((qnorm(1-alpha/2)^2*s^2)/(e^2+((qnorm(1-alpha/2)^2*s^2)/N)),0)}
  if(tipo=="m"){return(n)}
  if(tipo=="p"){n=round((qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p))/(e^2+(qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p)*(1/N))),0)
  if(tipo=="p"){return(n)}
  }
}

# tipo contrato
tipo="p"
N=33
p<-0.28
e=0.1
alpha=0.05
n1<-n.mas(tipo,N,s,e,p,alpha)

# licencia enfermedad remunerada
tipo="p"
N=33
p<-0.13
e=0.1
alpha=0.1
n2<-n.mas(tipo,N,s,e,p,alpha)

# Ingressos ultimo mes
tipo="m"
N=33
s=1881638
e=300000
alpha=0.05
n3<-n.mas(tipo,N,s,e,p,alpha)

# Cuantos meses tranajo en el ultimo año
tipo="m"
N=33
s=11.2 
e=2
alpha=0.05
n4<-n.mas(tipo,N,s,e,p,alpha) 

# Tiempo_trabjado en meses
tipo="m"
N=33
s=73.7
e=10
alpha=0.05
n5<-n.mas(tipo,N,s,e,p,alpha)

n=max(n1,n2,n3,n4,n5)

# Define el vector de departamentos únicos
todos_los_departamentos <- unique(Datos.sin_outliers$Departamento)

# Define el número de departamentos a seleccionar
n_departamentos_a_seleccionar <- n

# Configura la semilla para reproducibilidad
set.seed(123)

# Crea un vector de números aleatorios uniformes entre 0 y 1
numeros_aleatorios <- runif(length(todos_los_departamentos))

# Combina los departamentos y los números aleatorios en un data frame
departamentos_aleatorios <- data.frame(Departamento = todos_los_departamentos,
                                       NumeroAleatorio = numeros_aleatorios)

# Ordena el data frame por el número aleatorio de manera descendente
departamentos_aleatorios <- departamentos_aleatorios %>%
  arrange(desc(NumeroAleatorio))

# Selecciona los primeros n departamentos
departamentos_seleccionados <- head(departamentos_aleatorios$Departamento, n_departamentos_a_seleccionar)

# Filtra la muestra original para incluir solo los departamentos seleccionados
muestra_departamentos <- Datos.sin_outliers %>%
  filter(Departamento %in% departamentos_seleccionados)

# Muestra la nueva base de datos con las observaciones seleccionadas
head(muestra_departamentos)


## Segunda fase
# Instalar y cargar el paquete dplyr si aún no está instalado
# install.packages("dplyr")

# Contar las observaciones por cada departamento
observaciones_por_departamento <- muestra_departamentos %>%
  count(Departamento)

# Ver el resultado
print(n=28,observaciones_por_departamento)

# Definir la función n.mas
n.mas <- function(tipo, N, s, e, p, alpha) {
  if (tipo == "t") {
    n = round((qnorm(1 - alpha/2)^2 * N^2 * s^2) / (e^2 + qnorm(1 - alpha/2)^2 * N * s^2), 0)
  }
  if (tipo == "m") {
    if (s == 0 || N == 0) {
      n = 0  # Manejar división por cero
    } else {
      n = round((qnorm(1 - alpha/2)^2 * s^2) / (e^2 + ((qnorm(1 - alpha/2)^2 * s^2) / N)), 0)
    }
  }
  if (tipo == "p") {
    if (N == 1) {
      n = 0  # Manejar división por cero
    } else {
      n = round((qnorm(1 - alpha/2)^2 * (N/(N-1)) * p * (1 - p)) / (e^2 + (qnorm(1 - alpha/2)^2 * (N/(N-1)) * p * (1 - p) * (1/N))), 0)
    }
  }
  return(n)
}

# Definir los valores dados
departamentos <- c(5, 8, 11, 13, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 52, 66, 68, 70, 73, 76, 81, 85, 86, 88, 91, 94, 95, 97)
N_vector <- c(834, 401, 450, 375, 579, 141, 210, 216, 222, 157, 301, 178, 314, 262, 488, 512, 211, 259, 574, 25, 63, 270, 38, 23, 26)
p_values <- c(0.28, 0.13)
s_values <- c(1881638, 11.2, 73.7)
e_values <- c(300000, 2, 10)
alpha_values <- c(0.5, 0.5, 0.5)

# Crear un dataframe para almacenar los resultados
resultados <- data.frame(Departamento = integer(), n = integer())

# Iterar sobre cada departamento y calcular el tamaño de muestra
for (i in seq_along(departamentos)) {
  dep <- departamentos[i]
  N <- N_vector[i]
  
  n1 <- n.mas("p", N, s_values[1], e_values[1], p_values[1], alpha_values[1])
  n2 <- n.mas("p", N, s_values[2], e_values[2], p_values[2], alpha_values[2])
  n3 <- n.mas("m", N, s_values[3], e_values[3], 0, alpha_values[3])
  
  n_max <- max(n1, n2, n3)
  
  resultados <- rbind(resultados, data.frame(Departamento = dep, n = n_max))
}

# Ver los resultados
print(resultados)


# Instalar y cargar el paquete dplyr si aún no está instalado
# install.packages("dplyr")
library(dplyr)

# Definir los tamaños de muestra para cada departamento
tamaños_muestra <- resultados

# Filtrar departamentos con NA en n (94, 95, 97)
tamaños_muestra <- tamaños_muestra[complete.cases(tamaños_muestra), ]

# Crear un dataframe para almacenar la muestra final
muestra_final <- data.frame()

# Iterar sobre cada departamento y seleccionar la muestra correspondiente
for (i in 1:nrow(tamaños_muestra)) {
  dep <- tamaños_muestra$Departamento[i]
  n <- tamaños_muestra$n[i]
  
  # Seleccionar la muestra aleatoria simple sin reemplazo para el departamento actual
  muestra_dep <- muestra_departamentos %>%
    filter(Departamento == dep) %>%
    slice_sample(n = n, replace = FALSE)
  
  # Agregar la muestra del departamento actual a la muestra final
  muestra_final <- bind_rows(muestra_final, muestra_dep)
}

# Ver la muestra final
print(muestra_final)


departamentos <- c(5, 8, 11, 13, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 52, 66, 68, 70, 73, 76, 81, 85, 86, 88, 91, 94, 95, 97)
N_vector <- c(834, 401, 450, 375, 579, 141, 210, 216, 222, 157, 301, 178, 314, 262, 488, 512, 211, 259, 574, 25, 63, 270, 38, 23, 26)


# Crear un dataframe para almacenar las probabilidades de inclusión
probabilidades <- data.frame(Departamento = integer(), Probabilidad = numeric())

# Crear un dataframe para almacenar las probabilidades de inclusión
probabilidades <- data.frame(Departamento = integer(), Probabilidad = numeric())

# Iterar sobre cada departamento y calcular las probabilidades
for (i in 1:length(departamentos)) {
  dep <- departamentos[i]
  n_dep <- tamaños_muestra$n[tamaños_muestra$Departamento == dep]
  N_dep <- N_vector[departamentos == dep]
  
  # Establecer las probabilidades a 1 para los departamentos 94, 95 y 97
  if (dep %in% c(94, 95, 97)) {
    prob_dep <- 1
  } else {
    prob_dep <- n_dep / N_dep
  }
  
  # Agregar las probabilidades del departamento actual al dataframe
  probabilidades <- bind_rows(probabilidades, data.frame(Departamento = dep, Probabilidad = prob_dep))
}

# Ver las probabilidades de inclusión
print(probabilidades)

####Estimacion de los totales por unidades primarias de muestreo a partir de las unidades 
### secundarias de muestreo 
# Definir una función para calcular el estimador del total de Hansen por variable y departamento
# Definir una función para calcular el estimador del total de Hansen por variable y departamento
calcular_estimador_hansen <- function(variable, muestra_final, probabilidades) {
  resultados_hansen_por_departamento <- data.frame(Departamento = integer(), Estimador_Total = numeric())
  
  for (dep in unique(muestra_final$Departamento)) {
    total_estimado_dep <- 0
    
    muestra_dep <- muestra_final[muestra_final$Departamento == dep, ]
    prob_dep <- probabilidades$Probabilidad[probabilidades$Departamento == dep]
    
    total_estimado_variable_dep <- sum(muestra_dep[, variable] / prob_dep, na.rm = TRUE)
    total_estimado_dep <- total_estimado_dep + total_estimado_variable_dep
    
    resultados_hansen_por_departamento <- bind_rows(resultados_hansen_por_departamento, data.frame(Departamento = dep, Estimador_Total = total_estimado_dep))
  }
  
  return(resultados_hansen_por_departamento)
}

# Iterar sobre cada variable de interés
resultados_hansen_por_variable <- list()

for (variable in variables_interes) {
  resultados_hansen_por_variable[[variable]] <- calcular_estimador_hansen(variable, muestra_final, probabilidades)
}

# Ver los resultados del estimador del total de Hansen por variable y departamento
print(resultados_hansen_por_variable)



### Calculo de los totales globales estimados 
# Nueva probabilidad de inclusión
nueva_probabilidad <- 25/33

# Crear un dataframe para almacenar los resultados del estimador del total de Hansen por variable
resultados_hansen_por_variable_total <- data.frame(Variable = character(), Estimador_Total = numeric())

# Iterar sobre cada variable de interés
for (variable in variables_interes) {
  # Inicializar el estimador del total por variable
  total_estimado_variable <- 0
  
  # Iterar sobre cada departamento y calcular el estimador del total de Hansen para la variable actual
  for (dep in unique(muestra_final$Departamento)) {
    # Filtrar los resultados anteriores para el departamento actual y la variable actual
    resultados_dep_variable <- resultados_hansen_por_variable[[variable]]
    resultados_dep_variable <- resultados_dep_variable[resultados_dep_variable$Departamento == dep, ]
    
    # Obtener el estimador del total de Hansen para el departamento actual y la variable actual
    total_estimado_variable_dep <- resultados_dep_variable$Estimador_Total * nueva_probabilidad
    
    # Agregar al total estimado global para la variable
    total_estimado_variable <- total_estimado_variable + total_estimado_variable_dep
  }
  
  # Agregar el resultado al dataframe
  resultados_hansen_por_variable_total <- bind_rows(resultados_hansen_por_variable_total, data.frame(Variable = variable, Estimador_Total = total_estimado_variable))
}

# Ver los resultados del estimador del total de Hansen por variable
print(resultados_hansen_por_variable_total)

###### calculo de los promedios y proporciones globales estimados 
# Tamaño de la población completa
tamaño_poblacion <- 8727

# Dividir los resultados por el tamaño de la población completa para obtener proporciones
resultados_hansen_por_variable_total$Proporcion_Estimada <- resultados_hansen_por_variable_total$Estimador_Total / tamaño_poblacion

# Imprimir los resultados
print(resultados_hansen_por_variable_total)

# Lista de variables de interés
# Lista de variables de interés
variables_interes <- c("Licencia_enfermedad", "Tipo_contrato", "Cuanto_mes", "Trabjo_año", "Tiempo_trabajo")

# Crear un dataframe para almacenar los resultados de la varianza para cada variable
resultados_varianza <- data.frame(Variable = character(), Varianza_Total_Estimado = numeric())

# Iterar sobre cada variable de interés
for (variable in variables_interes) {
  # Filtrar la muestra final solo para la variable actual
  muestra_variable <- muestra_final[, c("Departamento", variable)]
  
  # Obtener los tamaños de muestra correspondientes a la variable actual
  tamaños_muestra_variable <- tamaños_muestra
  
  # Obtener la cuasivarianza muestral intra-grupos para la variable actual
  cuasivarianza_intragrupo <- sd(muestra_variable[[variable]], na.rm = TRUE)^2
  
  # Obtener la varianza del total estimado en dos etapas para la variable actual
  varianza_total_estimado_dos_etapas <- ((N_vector - tamaños_muestra_variable) / (N_vector - 1)) * cuasivarianza_intragrupo +
    ((tamaños_muestra_variable - 1) / (N_vector - 1)) * (cuasivarianza_intragrupo / tamaños_muestra_variable)
  
  # Agregar el resultado al dataframe
  resultados_varianza <- bind_rows(resultados_varianza, data.frame(Variable = variable, Varianza_Total_Estimado = sum(varianza_total_estimado_dos_etapas)))
}

# Ver los resultados de la varianza para cada variable
print(resultados_varianza)

### Coeficientes de variacion 
# Crear un dataframe para almacenar los resultados del coeficiente de variación
resultados_cv <- data.frame(Variable = character(), Coeficiente_Variacion = numeric())

# Iterar sobre cada variable de interés
for (variable in variables_interes) {
  # Obtener la varianza total estimada para la variable actual
  varianza_total_estimada <- resultados_varianza$Varianza_Total_Estimado[resultados_varianza$Variable == variable]
  
  # Obtener el total estimado para la variable actual
  total_estimado <- resultados_hansen_por_variable_total$Estimador_Total[resultados_hansen_por_variable_total$Variable == variable]
  
  # Calcular el coeficiente de variación
  coeficiente_variacion <- (sqrt(varianza_total_estimada) / total_estimado) * 100
  
  # Agregar el resultado al dataframe
  resultados_cv <- bind_rows(resultados_cv, data.frame(Variable = variable, Coeficiente_Variacion = coeficiente_variacion))
}

# Ver los resultados del coeficiente de variación para cada variable
print(resultados_cv)

