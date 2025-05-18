# Taller 1 muestreo estadístico Jhon Ramírez
## Primer punto
#Punto 3

# Crear un conjunto de datos y calcular todas las combinaciones de tamaño 3
y <- c(60, 65, 67, 72, 77)
muestras <- combn(y, 3)

# Definir proinp
facexp <- 5/3

# Inicializar el vector para almacenar los resultados
pi_estimador <- numeric(10)

# Calcular el pi-estimador para cada muestra
for (i in 1:10) {
  pi_estimador[i] <- facexp * sum(muestras[, i])
}

# Mostrar el resultado
pi_estimador


##Punto 4 valor esperado
#Calculamos el valor esperado usando
E_pi_estimador=mean(pi_estimador)
E_pi_estimador
##Mientras que el valor real es 
yt=sum(y)
yt

#punto 5 varianza
v_pi_estimador=((sum(pi_estimador^2))/10)-yt^2
v_pi_estimador


## Segundo punto 
#Funcion partes para ver todas las muestras posibles
powerset <- function(vec) {
  n <- length(vec)
  indices <- 1:n
  conjunto_partes <- list()
  
  for (i in 0:n) {
    combinaciones <- combn(indices, i)
    for (j in 1:ncol(combinaciones)) {
      conjunto_partes <- c(conjunto_partes, list(vec[combinaciones[, j], drop = FALSE]))
    }
  }
  
  return(conjunto_partes)
}
Muestrasy=powerset(y)
Muestrasy

calcular_probabilidad <- function(subconjunto) {
  tamaño_mn=length(subconjunto)
  return(0.7^(tamaño_mn)*(1-0.7)^(5-tamaño_mn))
}

probabilidades_m=sapply(Muestrasy, calcular_probabilidad)
probabilidades_m
sum(probabilidades_m)



suma_de_probabilidades <- function(elemento) {
  indices_contiene_elemento <- sapply(Muestrasy, function(Muestrasy) elemento %in% Muestrasy)
  suma <- sum(probabilidades_m[indices_contiene_elemento])
  return(suma)
}
pro_Pi_or=numeric(length(y))

for (i in 1:length(y)) {
  pro_Pi_or[i] <- suma_de_probabilidades(y[i])
}
pro_Pi_or


# Función para calcular la suma de probabilidades de subconjuntos que contienen una pareja de elementos

# Función para calcular la suma de probabilidades de subconjuntos que contienen una pareja de elementos
suma_de_probabilidades_pareja <- function(elemento1, elemento2) {
  # Verificar si tanto elemento1 como elemento2 están en cada subconjunto
  indices_contiene_pareja <- sapply(Muestrasy, function(Muestrasy) all(c(elemento1, elemento2) %in% Muestrasy))
  
  suma <- sum(probabilidades_m[indices_contiene_pareja])
  return(suma)
}

# Ejemplo de uso
y <- c(60, 65, 67, 72, 77)

# Crear una matriz para almacenar los resultados
pro_Pi_or_seg <- matrix(NA, nrow = length(y), ncol = length(y))

# Aplicar la función a cada pareja de elementos diferentes de y y almacenar los resultados en la matriz
for (i in 1:(length(y) - 1)) {
  for (j in (i + 1):length(y)) {
    elemento1 <- y[i]
    elemento2 <- y[j]
    pro_Pi_or_seg[i, j] <- suma_de_probabilidades_pareja(elemento1, elemento2)
    pro_Pi_or_seg[j, i] <- pro_Pi_or_seg[i, j] # Para llenar la parte simétrica de la matriz
  }
}

# Mostrar los resultados
print(pro_Pi_or_seg)


# 3 calculo de los pi estimadores
# Los factores de expancion que son constentes en este caso.
A <- 1/0.7

# Crear un vector para almacenar los resultados
pi_est <- numeric()

# Recorrer los subconjuntos
for (i in 1:length(Muestrasy)) {
  subconjunto <- Muestrasy[[i]]
  suma <- sum(subconjunto)
  resultado <- A * suma
  pi_est <- c(pi_est, resultado)
}

# Mostrar los resultados
print(pi_est)


##4 Valor esperado:
pi_est_es=probabilidades_m %*% pi_est
pi_est_es

#5 varianza
pi_est2=pi_est^2
v_pi_est= (pi_est2 %*% probabilidades_m)-(pi_est_es)^2
v_pi_est


## Ejercicio 3
#Varianzas de las indicadoras
3/4*(1-3/4)
1/2*(1-1/2)
p1=3/4
p2=3/4
p3=1/2
p12=1/2
p13=1/4
p23=1/4
cov12=p12-p1*p2;cov12
cov13=p13-p1*p3;cov13
cov23=p23-p3*p2;cov23




###################################################
#Ejercicio 4
## Modelo 1 

y=c(98,102,154,133,190,175)
u=c(1,2,3,4,5,6)
mediaU=mean(y);mediaU
