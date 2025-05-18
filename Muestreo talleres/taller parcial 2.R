e1=c(35,28,26,41,43,29,32,37,36,25,29,31,39,38,40,45,28,27,35,34)
a1=c(1,1,1,1,1,0,1,1,1,1,0,1,1,0,0,1,1,1,1,1)
e2=c(27,4,49,10,15,41,25,30)
a2=c(1,0,0,1,0,0,0,0)
e3=c(8,15,21,7,14,30,20,11,12,32,34,24)
a3=c(1,0,1,0,1,1,0,0,1,0,0,1)
n1=length(e1);n1
n2=length(e2);n2
n3=length(e3);n3
n=n1+n2+n3;n
N1 =155; N2 = 62; N3 = 93
N=N1+N2+N3;N


pik1=n1/N1
tpi1=sum(e1/pik1)

pik2=n2/N2
tpi2=sum(e2/pik2)

pik3=n3/N3
tpi3=sum(e3/pik3)

tpi=sum(tpi1+tpi2+tpi3);tpi


ppi1=sum(a1/pik1)
ppi2=sum(a2/pik2)
ppi3=sum(a3/pik3)

pie=ppi1+ppi2+ppi3;pie
pie/N

sym1=1/(n1-1)*sum((a1-mean(a1))^2)
vara1=((N1^2)/n1)*(1-(n1/N1))*sym1;vara1

sym2=2/(n2-2)*sum((a2-mean(a2))^2)
vara2=((N2^2)/n2)*(1-(n2/N2))*sym2;vara2

sym3=3/(n3-3)*sum(a1-mean(a1)^2)
vara3=((N3^2)/n3)*(1-(n3/N3))*sym3;vara3

vara=vara1+vara2+vara3;vara
