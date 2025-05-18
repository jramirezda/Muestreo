set.seed(1)
datos=rnorm(1000,55,7)
datos

muestra1=sample(datos,50)
muestra2=sample(datos,100)
muestra3=sample(datos,150)
muestra4=sample(datos,200)

pik1=50/1000
pik2=100/1000
pik3=150/1000
pik4=200/1000


piest1=(1/pik1)*sum(muestra1);piest1
piest2=(1/pik2)*sum(muestra2);piest2
piest3=(1/pik3)*sum(muestra3);piest3
piest4=(1/pik4)*sum(muestra4);piest4


pikl1=(50*(50-1))/(1000*(999));pikl1
pikl2=(100*(100-1))/(1000*(999));pikl2
pikl3=(150*(150-1))/(1000*(999));pikl3
pikl4=(200*(200-1))/(1000*(999));pikl4


cov1=pikl1-pik1^2;cov1
cov2=pikl2-pik2^2;cov2
cov3=pikl3-pik3^2;cov3
cov4=pikl4-pik4^2;cov4


varpiest1=(cov1/pikl1*pk1^2)