getwd()
setwd("C:/")
#################################################################
rm(list=ls())
ls()
##################################################################
library(readxl)
Datos.Dominios<-read_excel("Datos.Muestra.Est.xlsx",col_names=TRUE)
show(Datos.Dominios)
View(Datos.Dominios)
attach(Datos.Dominios)
names(Datos.Dominios)
str(Datos.Dominios)

###################################################################
# Pregunta 1- Estimación del peso promedio Bajo el MAS
N<-400
n<-27
peso_prome_est<-sum(Peso)/n; peso_prome_est

var_est1<-(1/n)*(1-(n/N))*var(Peso)
Coef_varmuestr1<-sqrt(var_est1)/peso_prome_est ; Coef_varmuestr1
#####################################################################
# Pregunta 2- Estimación de la proporción de hombres y mujeres 
Z_M<-ifelse(Genero=="M", 1,0)
Z_F<-ifelse(Genero=="F", 1,0)
sum(Z_F);sum(Z_M)


Datos.Dominios1<-data.frame(Genero,Peso,Estatura,Vota,Z_M,Z_F)
str(Datos.Dominios1)

View(Datos.Dominios1)
Prop_estM<-sum(Z_M)/n;Prop_estM
Prop_estF<-sum(Z_F)/n;Prop_estF

V_e2<-(1-(n/N))*(Prop_estM*Prop_estF)/(n-1)
C_est2<-sqrt(V_e2)/Prop_estM;C_est2

V_e3<-(1-(n/N))*(Prop_estF*Prop_estM)/(n-1)
C_est3<-sqrt(V_e2)/Prop_estF;C_est3

##################################################################
# Pregunta 3- Estimación del peso promedio por género

Y_M<-Peso*Z_M
N_M<-326
P_prM<-(N*sum(Y_M))/(n*N_M);P_prM
V_e4<-(1/N_M^2)* (N^2/n)*(1-(n/N))*var(Y_M) 
C_est4<-sqrt(V_e4)/P_prM;C_est4

N_F<-74
Y_F<-Peso*Z_F
P_prF<-(N*sum(Y_F))/(n*N_F);P_prF
V_e5<-(1/N_F^2)* (N^2/n)*(1-(n/N))*var(Y_F)
C_est5<-sqrt(V_e5)/P_prF;C_est5



