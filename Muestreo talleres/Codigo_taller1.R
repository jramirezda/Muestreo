getwd()
setwd("C:/Users/mayol/OneDrive/Documents/Cursos 2023/Muestreo_Estadistico/PracticaR")

rm(list=ls())  
ls()

# Ejercicio 1
install.packages("combinat")
library(combinat)
options(digits=3)
Pob=c("1","2","3","4","5")
Muestras=t(as.matrix(combn(Pob,3)))
dim(Muestras)

I(Muestras=="1")
I1=rowSums(I(Muestras=="1"))
I2=rowSums(I(Muestras=="2"))
I3=rowSums(I(Muestras=="3"))
I4=rowSums(I(Muestras=="4"))
I5=rowSums(I(Muestras=="5"))

prob_mues<-rep(1/10,10)

Muestras1<-data.frame(Muestras,prob_mues,I1,I2,I3,I4,I5)
Muestras1$n<-rowSums(Muestras1[,5:9])
str(Muestras1)
#################################################################
#i. Probabilidades de Inclusión de Primer orden 

attach(Muestras1)
pi1<-sum(I1*prob_mues)
pi2<-sum(I2*prob_mues)
pi3<-sum(I3*prob_mues)
pi4<-sum(I4*prob_mues)
pi5<-sum(I5*prob_mues)

# ii. Probabilidades de Inclusión de segundo orden 
pi12<-sum(I1*I2*prob_mues)

pikl<-matrix(0.3,5,5)
diag(pikl)<-rep(0.6,5)

pikl

delta<-pikl-(0.6*0.6)
sum(delta)

Y<-c(60,65,67,72,77)
I1Y<-I1*Y[1]
I2Y<-I2*Y[2]
I3Y<-I3*Y[3]
I4Y<-I4*Y[4]
I5Y<-I5*Y[5]

#iii. pi-estimador
Muestras2<-data.frame(Muestras1,I1Y,I2Y,I3Y,I4Y,I5Y)
str(Muestras2)
pi_est<-rowSums(Muestras2[,11:15]/pi1)

Muestras3<-data.frame(Muestras2,pi_est)
Muestras3

#iv. Valor esperado del pi-estimador

totalpob<-sum(Y)
Esp_pi_est<-sum(Muestras3[,4]*Muestras3[,16])

Esp_pi_est
totalpob

#v. Varianza Poblacional del pi-estimador
Esp2_pi_est<-sum(Muestras3[,4]*(Muestras3[,16])^2)
VarP_pi_est<-Esp2_pi_est-(Esp_pi_est^2)

 