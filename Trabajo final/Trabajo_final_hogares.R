depto<-unique(Base$Dpto) #Esto  da que son 33 departamentos


#### Limpieza Datos ####
#imputacion y eliminacion de datos

Base$Contrato[is.na(Base$Contrato)] <- 9  #aplica segun definicion de variable
cantidad_na0 <- sum(is.na(Base$Contrato))
cantidad_na0

cantidad_na1 <- sum(is.na(Base$Tiempo_despl))
cantidad_na1 #4324
media_columna <- mean(Base$Tiempo_despl, na.rm = TRUE)  # 25.04087

cantidad_na2 <- sum(is.na(Base$Tiempo_trabj))
cantidad_na2 #0

cantidad_na3 <- sum(is.na(Base$Ingreso))
cantidad_na3   #14314
media_columna3 <- mean(Base$Ingreso, na.rm = TRUE)  # 1634002
media_columna3

#(eliminacion y comparacion)
base1<-Base

# Eliminar los individuos con valores NA en la variable tiempo_despl
datos_completos <- base1[complete.cases(base1$Tiempo_despl), ]

#  la base de datos resultante
cantidad_na3.1 <- sum(is.na(datos_completos$Ingreso))
cantidad_na3.1   #10775
media_columna3.1 <- mean(datos_completos$Ingreso, na.rm = TRUE)  # 1609868
media_columna3.1

# Imputar los valores NA en la columna variable1 con el número 1609868
datos_completos$Ingreso <- ifelse(is.na(datos_completos$Ingreso), 1609868, datos_completos$Ingreso)
# el dataframe actualizado

cantidad_na3.2 <- sum(is.na(datos_completos$Ingreso))
cantidad_na3.2

diccionario <- c(
  "5" = 2,   # Antioquia - Región Andina
  "8" = 3,   # Atlántico - Región Caribe
  "11" = 2,   # Bogotá, D.C. - Región Andina
  "13" = 3,   # Bolívar - Región Caribe
  "15" = 2,   # Boyacá - Región Andina
  "17" = 2,   # Caldas - Región Andina
  "18" = 1,   # Caquetá - Región Amazonia
  "19" = 6,   # Cauca - Región Pacífico
  "20" = 3,   # Cesar - Región Caribe
  "23" = 3,   # Córdoba - Región Caribe
  "25" = 2,   # Cundinamarca - Región Andina
  "27" = 6,   # Chocó - Región Pacífico
  "41" = 2,   # Huila - Región Andina
  "44" = 3,   # La Guajira - Región Caribe
  "47" = 3,   # Magdalena - Región Caribe
  "50" = 5,   # Meta - Región Orinoquía
  "52" = 6,   # Nariño - Región Pacífico
  "54" = 2,   # Norte de Santander - Región Andina
  "63" = 2,   # Quindío - Región Andina
  "66" = 2,   # Risaralda - Región Andina
  "68" = 2,   # Santander - Región Andina
  "70" = 3,   # Sucre - Región Caribe
  "73" = 2,   # Tolima - Región Andina
  "76" = 6,   # Valle del Cauca - Región Pacífico
  "81" = 5,   # Arauca - Región Orinoquía
  "85" = 5,   # Casanare - Región Orinoquía
  "86" = 1,   # Putumayo - Región Amazonia
  "88" = 4,   # Archipiélago de San Andrés, Providencia y Santa Catalina - Región Insular
  "91" = 1,   # Amazonas - Región Amazonia
  "94" = 1,   # Guainía - Región Amazonia
  "95" = 1,   # Guaviare - Región Amazonia
  "97" = 1,   # Vaupés - Región Amazonia
  "99" = 5    # Vichada - Región Orinoquía
)
datos_completos$region <- diccionario[as.character(datos_completos$Dpto)]


DATOS<-datos_completos

################################################################################
#PRIMERA ETAPA
DATOS$region<-as.factor(DATOS$region)
table(DATOS$region)

#Tamaños de muestra
####  tamaño de muestra MAS ####
n.mas=function(tipo,N,s,e,p,alpha){
  if(tipo=="t"){n=round((qnorm(1-alpha/2)^2*N^2*s^2)/(e^2+qnorm(1-alpha/2)^2*N*s^2),0)}
  if(tipo=="t"){return(n)}
  if(tipo=="m"){n=round((qnorm(1-alpha/2)^2*s^2)/(e^2+((qnorm(1-alpha/2)^2*s^2)/N)),0)}
  if(tipo=="m"){return(n)}
  if(tipo=="p"){n=round((qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p))/(e^2+(qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p)*(1/N))),0)
  if(tipo=="p"){return(n)}
  }
}

# Satisfecho
tipo="p"
N=33
p<-aggregate(Satisf ~ region, data = DATOS, FUN = function(x) prop.table(table(x)))
p<-sum(p$Satisf[,1])/6
e=0.1
alpha=0.05
n1<-n.mas(tipo,N,s,e,p,alpha)

# Contrato fijo
tipo="p"
N=33
p<-aggregate(Contrato ~ region, data = DATOS, FUN = function(x) prop.table(table(x)))
p<-sum(p$Contrato[,1])/6
e=0.1
alpha=0.1
n2<-n.mas(tipo,N,s,e,p,alpha)

# Ingresos
tipo="p"
N=33
DATOS$Ingreso_ctg <- ifelse(DATOS$Ingreso < 1000000, 1, 2)
DATOS$Ingreso_ctg<-as.factor(DATOS$Ingreso_ctg)
prop_ing<-aggregate(Ingreso_ctg ~ region, data = DATOS, FUN = function(x) prop.table(table(x)))
p<-sum(prop_ing$Ingreso_ctg[,1])/6
e=0.1
alpha=0.05
n3<-n.mas(tipo,N,s,e,p,alpha)

# Tiempo_despl
tipo="p"
N=33
DATOS$Tiempo_despl_ctg <- ifelse(DATOS$Tiempo_despl > 60, 1, 2) 
DATOS$Tiempo_despl_ctg<-as.factor(DATOS$Tiempo_despl_ctg)
prop_tiempdes<-aggregate(Tiempo_despl_ctg ~ region, data = DATOS, FUN = function(x) prop.table(table(x)))
p<-sum(prop_tiempdes$Tiempo_despl_ctg[,1])/6
e=0.1
alpha=0.05
n4<-n.mas(tipo,N,s,e,p,alpha) 

# Tiempo_trabj
tipo="p"
N=33
DATOS$Tiempo_trabj_ctg <- ifelse(DATOS$Tiempo_trabj < 12, 1, 2)
DATOS$Tiempo_trabj_ctg<-as.factor(DATOS$Tiempo_trabj_ctg)
prop_tiemptra<-aggregate(Tiempo_trabj_ctg ~ region, data = DATOS, FUN = function(x) prop.table(table(x)))
p<-sum(prop_tiemptra$Tiempo_trabj_ctg[,1])/6
e=0.1
alpha=0.05
n5<-n.mas(tipo,N,s,e,p,alpha)

n=max(n1,n2,n3,n4,n5)

tamaño_neyman<-function(Ne,n,var){
  num<-NULL
  ne<-NULL
  for (i in 1:length(Ne)) {
    num[i]<-(Ne[i]*var[i])
    ne[i]=n*(num[i]/(sum(Ne*var)))
  }
  return(ne)
}

estrato=NULL
Ne=NULL
var1=NULL
var2=NULL
var3=NULL
for (i in 1:6) {
  estrato[[i]]<-subset(DATOS,region==i)
  Ne[i]<-length(unique(estrato[[i]]$Dpto))
  var1[i]<-var(estrato[[i]]$Tiempo_despl)
  var2[i]<-var(estrato[[i]]$Ingreso)
  var3[i]<-var(estrato[[i]]$Tiempo_trabj)
  tmñ1<-round(tamaño_neyman(Ne,n,var1),0)
  tmñ2<-round(tamaño_neyman(Ne,n,var2),0)
  tmñ3<-round(tamaño_neyman(Ne,n,var3),0)
}
reg<-seq(1,6,1)
tamños_est<-data.frame(reg,tmñ1,tmñ2,tmñ3)
#elegimos el 3

#SACAR MUESTRA DE DEPARTAMENTOS

#Coordinado negativo
s.mas=function(base,n,seed){
  N=nrow(base)
  set.seed(seed)
  base$u=runif(nrow(base))
  base=base[with(base,order(base$u)),]
  base=base[1:n,]
  base$pik=nrow(base)/N
  return(base)
}

dept_region <- lapply(split(DATOS$Dpto, DATOS$region),unique)
muestrar1=s.mas(as.data.frame(dept_region$'1'),n=tmñ3[1],seed=1234)
muestrar2=s.mas(as.data.frame(dept_region$'2'),n=tmñ3[2],seed=1234)
muestrar3=s.mas(as.data.frame(dept_region$'3'),n=tmñ3[3],seed=1234)
muestrar4=s.mas(as.data.frame(dept_region$'4'),n=tmñ3[4],seed=1234)
muestrar5=s.mas(as.data.frame(dept_region$'5'),n=tmñ3[5],seed=1234)
muestrar6=s.mas(as.data.frame(dept_region$'6'),n=tmñ3[6],seed=1234)

UPM_selc<-c(muestrar1[,1],muestrar2[,1],muestrar3[,1],muestrar4[,1]
            ,muestrar5[,1],muestrar6[,1])
DATOS2<-subset(DATOS, DATOS$Dpto %in% UPM_selc)

UPM=NULL
for (i in 1:length(UPM_selc)) {
  UPM[[i]]<-subset(DATOS, DATOS$Dpto == UPM_selc[i]) 
}


#SEGUNDA ETAPA
nUPM=NULL
for (i in 1:length(UPM_selc)) {
tipo="p"
N=nrow(UPM[[i]])
p<-nrow(subset(UPM[[i]],Contrato==1))/N
e=0.05
alpha=0.05
nUPM[i]<-n.mas(tipo,N,s,e,p,alpha)
}

N_UPMS<-NULL
for (i in 1:length(UPM_selc)) {
  N_UPMS[i] <-nrow(UPM[[i]])
}

sum(nUPM)
USM=NULL
Muestra_final=NULL
for (i in 1:length(UPM_selc)) {
  USM[[i]]=s.mas(UPM[[i]],n=nUPM[i],seed=1234)
  Muestra_final<-rbind(Muestra_final,USM[[i]])
}

dim(Muestra_final)
USM[[1]]
##############################################################################
#ESTIMACIONES
Nd_reg<-c(length(dept_region$`1`),length(dept_region$`2`),length(dept_region$`3`),
          length(dept_region$`4`),length(dept_region$`5`),length(dept_region$`6`))

#INGRESO
pi_est_dep<-NULL
for (i in 1:23) {
  pi_est_dep[i]<-((N_UPMS[i]/nUPM[i])*sum(USM[[i]]$Ingreso_ctg==1, na.rm = TRUE))
}

pik<-c(rep((Nd_reg[1]/tmñ3[1]),tmñ3[1]),
  rep((Nd_reg[2]/tmñ3[2]),tmñ3[2]),
  rep((Nd_reg[3]/tmñ3[3]),tmñ3[3]),
  rep((Nd_reg[4]/tmñ3[4]),tmñ3[4]),
  rep((Nd_reg[5]/tmñ3[5]),tmñ3[5]),
  rep((Nd_reg[6]/tmñ3[6]),tmñ3[6]))

p_est1<-sum(pik*pi_est_dep)/25371
sum(DATOS$Ingreso_ctg==1, na.rm = TRUE)/25371

#Varianza
Nh<-c(Nd_reg[1],Nd_reg[2],Nd_reg[3],Nd_reg[4],Nd_reg[5],Nd_reg[6])
nh<-c(tmñ3[1],tmñ3[2],tmñ3[3],tmñ3[4],tmñ3[5],tmñ3[6])

for (i in 1:6) {
  var1<-sum((Nh[i]^2/nh[i])*(1-(nh[i]/Nh[i]))*var(pi_est_dep))
  m<-(Nh[i]/nh[i])
}

for (i in 1:23) {  
  var2<-sum((N_UPMS[i]^2/nUPM[i])*(1-(nUPM[i]/N_UPMS[i])))*
    var(USM[[i]]$Ingreso_ctg==1, na.rm = TRUE)
}
var=var1+m*var2
varianza1=var/25371^2

CV1=(sqrt(varianza1)/p_est1)*100
ICinf1<-p_est1-qnorm(0.975)*sqrt(varianza1)
ICsup1<-p_est1+qnorm(0.975)*sqrt(varianza1)

#Tiempo desplazamiento
pi_est_dep<-NULL
for (i in 1:23) {
  pi_est_dep[i]<-((N_UPMS[i]/nUPM[i])*sum(USM[[i]]$Tiempo_despl_ctg==1, na.rm = TRUE))
}

p_est2<-sum(pik*pi_est_dep)/25371
mean(DATOS$Tiempo_despl_ctg==1, na.rm = TRUE)

#Varianza
for (i in 1:6) {
  var1<-sum((Nh[i]^2/nh[i])*(1-(nh[i]/Nh[i]))*var(pi_est_dep))
  m<-(Nh[i]/nh[i])
}

for (i in 1:23) {  
  var2<-sum((N_UPMS[i]^2/nUPM[i])*(1-(nUPM[i]/N_UPMS[i])))*
    var(USM[[i]]$Tiempo_despl_ctg==1, na.rm = TRUE)
}
var=var1+m*var2
varianza2=var/25371^2

CV2=(sqrt(varianza2)/p_est2)*100
ICinf2<-p_est2-qnorm(0.975)*sqrt(varianza2)
ICsup2<-p_est2+qnorm(0.975)*sqrt(varianza2)

#Tiempo trabajo
pi_est_dep<-NULL
for (i in 1:23) {
  pi_est_dep[i]<-((N_UPMS[i]/nUPM[i])*sum(USM[[i]]$Tiempo_trabj_ctg==1, na.rm = TRUE))
}

p_est3<-sum(pik*pi_est_dep)/25371
mean(DATOS$Tiempo_trabj_ctg==1, na.rm = TRUE)

#Varianza
for (i in 1:6) {
  var1<-sum((Nh[i]^2/nh[i])*(1-(nh[i]/Nh[i]))*var(pi_est_dep))
  m<-(Nh[i]/nh[i])
}

for (i in 1:23) {  
  var2<-sum((N_UPMS[i]^2/nUPM[i])*(1-(nUPM[i]/N_UPMS[i])))*
    var(USM[[i]]$Tiempo_trabj_ctg==1, na.rm = TRUE)
}
var=var1+m*var2
varianza3=var/25371^2

CV3=(sqrt(varianza3)/p_est3)*100
ICinf3<-p_est3-qnorm(0.975)*sqrt(varianza3)
ICsup3<-p_est3+qnorm(0.975)*sqrt(varianza3)

# Satisf
pi_est_dep<-NULL
for (i in 1:23) {
  pi_est_dep[i]<-((N_UPMS[i]/nUPM[i])*sum(USM[[i]]$Satisf==1, na.rm = TRUE))
}

p_est4<-sum(pik*pi_est_dep)/25371
mean(DATOS$Satisf==1, na.rm = TRUE)

#Varianza
for (i in 1:6) {
  var1<-sum((Nh[i]^2/nh[i])*(1-(nh[i]/Nh[i]))*var(pi_est_dep))
  m<-(Nh[i]/nh[i])
}

for (i in 1:23) {  
  var2<-sum((N_UPMS[i]^2/nUPM[i])*(1-(nUPM[i]/N_UPMS[i])))*
    var(USM[[i]]$Satisf==1, na.rm = TRUE)
}
var=var1+m*var2
varianza4=var/25371^2

CV4=(sqrt(varianza4)/p_est4)*100
ICinf4<-p_est4-qnorm(0.975)*sqrt(varianza4)
ICsup4<-p_est4+qnorm(0.975)*sqrt(varianza4)


# Contrato
pi_est_dep<-NULL
for (i in 1:23) {
  pi_est_dep[i]<-((N_UPMS[i]/nUPM[i])*sum(USM[[i]]$Contrato==1, na.rm = TRUE))
}

p_est5<-sum(pik*pi_est_dep)/25371
mean(DATOS$Contrato==1, na.rm = TRUE)

#Varianza
for (i in 1:6) {
  var1<-sum((Nh[i]^2/nh[i])*(1-(nh[i]/Nh[i]))*var(pi_est_dep))
  m<-(Nh[i]/nh[i])
}

for (i in 1:23) {  
  var2<-sum((N_UPMS[i]^2/nUPM[i])*(1-(nUPM[i]/N_UPMS[i])))*
    var(USM[[i]]$Contrato==1, na.rm = TRUE)
}
var=var1+m*var2
varianza5=var/25371^2

CV5=(sqrt(varianza5)/p_est5)*100
ICinf5<-p_est5-qnorm(0.975)*sqrt(varianza5)
ICsup5<-p_est5+qnorm(0.975)*sqrt(varianza5)

resultados<-data.frame(
  c(p_est1,p_est2,p_est3,p_est4,p_est5),
  c(CV1,CV2,CV3,CV4,CV5),
  c(ICinf1,ICinf2,ICinf3,ICinf4,ICinf5),
  c(ICsup1,ICsup2,ICsup3,ICsup4,ICsup5)
)
names(resultados)<-c("Estimación", "CV %", "IC-Inf (95%)", "IC-Sup (95%)")
resultados

library(xtable)
xtable(resultados)