getwd()
setwd("C:/Users/mayol/OneDrive/Documents/Cursos 2023/Muestreo_Estadistico/PracticaR")


rm(list=ls())  
ls()

install.packages("survey")
install.packages("sampling")

library(survey)
library(sampling)

#1 lectura marco
base <- readRDS("C:/Users/mayol/Dropbox/PC/Downloads/Marco (1).rds")
#base<-readRDS(file = "Marco.rds")
str(base)

# 2 Base Bogotá
base_bogota=subset(base,base$COLE_COD_MCPIO_UBICACION==11001)
str(base_bogota)
base_bogota$academico=ifelse(base_bogota$COLE_CARACTER=="ACADÉMICO",1,0)
names(base_bogota)
attach(base_bogota)

sum(base_bogota$academico)
mean(PUNT_MATEMATICAS)
sqrt(var(PUNT_MATEMATICAS))

boxplot(PUNT_MATEMATICAS)

# 3. tamaño de muestra
n.mas=function(tipo,N,s,e,p,alpha){
  if(tipo=="t"){n=round((qnorm(1-alpha/2)^2*N^2*s^2)/(e^2+qnorm(1-alpha/2)^2*N*s^2),0)}
  if(tipo=="t"){return(n)}
  if(tipo=="m"){n=round((qnorm(1-alpha/2)^2*s^2)/(e^2+(qnorm(1-alpha/2)^2*s^2/N)),0)}
  if(tipo=="m"){return(n)}
  if(tipo=="p"){n=round((qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p))/(e^2+(qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p)*(1/N))),0)
  if(tipo=="p"){return(n)}
  }
}


VarMas<-
tipo="t"
N=nrow(base_bogota)
s=11.28
e=7000
alpha=0.05
p=NULL
n.mas(tipo,N,s,e,p,alpha)



tipo="m"
s=100
e=10
n.mas(tipo,N,s,e,p,alpha)

tipo="p"
p=0.5
e=0.05
n.mas(tipo,N,s,e,p,alpha)


# 4. Selección de la muestra

#Coordinado negativo
s.mas=function(base,n,seed){
  N=nrow(base)
  set.seed(seed)
  base$u=runif(nrow(base))
  base=base[with(base,order(base$u)),]
  base=base[1:n,]
  base$pik=n/N
  return(base)
}

n=350
seed=123
muestra=s.mas(base_bogota,n,seed)
str(muestra)
View(muestra)
names(muestra)

#Selección y rechazo
Fan_Muller=function(base,n,seed){
  N=nrow(base)
  j = 0
  m = numeric(N)
  set.seed(seed)
  for (k in 1:N) if (runif(1) < (n - j)/(N - k + 1)) {
    j = j + 1
    m[k] = 1}
  return(m)
}

n=350
seed=123
m=Fan_Muller(base,n,seed)
muestra2=base[which(m==1),]

head(muestra2)

# 5. Estimación MAS en una etapa

salida=function(est,alpha){
  est=as.data.frame(est)
  names(est)[2]="se"
  est$cv=100*(est$se/est[,1])
  est$ic_low=est[,1]-qnorm(1-alpha/2)*est$se 
  est$ic_upp=est[,1]+qnorm(1-alpha/2)*est$se
  return(round(est,2))
}


N=nrow(base_bogota)
muestra$ind=rep(1,nrow(muestra))
muestra$Fexp=1/muestra$pik

#Especificación del diseño
dsgn=svydesign(id=~1,fpc=~rep(N,n),data=muestra,weights=~Fexp)

#Estimación del total
(est=svytotal(~academico,dsgn,deff=T))
alpha=0.05
(tabla=salida(est,alpha))

#Estimación de la media
(est1=svymean(~PUNT_GLOBAL,dsgn,deff=T))
alpha=0.05
(tabla=salida(est1,alpha))

#Estimación de proporción
(est2=svymean(~DESEMP_INGLES,dsgn,deff=T))
alpha=0.05
(tabla=salida(est2,alpha))

#Estimación más de una variable
(est3=svymean(~PUNT_GLOBAL+PUNT_LECTURA_CRITICA+PUNT_MATEMATICAS+PUNT_C_NATURALES+
               PUNT_SOCIALES_CIUDADANAS+PUNT_INGLES
             ,dsgn))
alpha=0.05
est=est3
(tabla=salida(est,alpha))

# 6. Estimación tamaño de un dominio usando MAS 
(est4=svyby(~ind,~ESTU_GENERACION.E,dsgn,svytotal))
est4=as.data.frame(est4)


est4=est4[,-1]
alpha=0.05
(tabla=salida(est4,alpha))

#Estimación de una variable en dominios 

#Estimación del total de una variable asociada a un dominio
(est5=svyby(~academico,~ESTU_GENERACION.E,dsgn,deff=T,svytotal))

est5=as.data.frame(est5)
est5=est5[,-1]
alpha=0.05
(tabla=salida(est5,alpha))
muestra$PUNT_MATEMATICAS

#Estimación de la media
(est6=svyby(~PUNT_MATEMATICAS,~ESTU_GENERACION.E,dsgn,deff=T,svymean))
est6=as.data.frame(est6)
est6=est6[,-1]
alpha=0.05
(tabla=salida(est6,alpha))

#Estimación de proporción
salida.p=function(est){
  for(i in 1:length(est)){
    a=as.data.frame(est[1,i])
    categoria=rownames(a)[1:(nrow(a)/2)]
    dominio=rep(rownames(est)[i],(nrow(a)/2))
    p=a[1:(nrow(a)/2),]
    se=a[(nrow(a)/2+1):nrow(a),]
    tab=data.frame(dominio,categoria,p,se)
    ifelse(i==1,(tab2=tab),(tab2=rbind(tab2,tab)))
  }
  return(tab2)
}

(est7=svyby(~DESEMP_INGLES,~ESTU_GENERACION.E,dsgn,deff=T,svymean))
est7=as.data.frame(est7)

table(muestra$DESEMP_INGLES)
est=est7[,-1]
(tabla=salida.p(est))
