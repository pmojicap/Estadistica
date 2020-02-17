## SEMANA 8 - ERROR ESTANDAR ##

##E1: Aumente el valor de desviacion estandar y evalue como cambia el error estandar.
pop <- ceiling(rnorm(5000,mean = 35, sd= 3))
m1<-sample(x=pop,size = 10,replace = T)
m2<-sample(x=pop,size = 10,replace = T)
m3<-sample(x=pop,size = 10,replace = T)
sd(c(m1,m2,m3))/sqrt(length(c(m1,m2,m3)))
##Hay un aumento del error estandar dado que hay una relacion directa, a mayor sd, mayor es el ES

##E2: Aumente el tamaño de la muestra y evalue como cambia el error estandar.
pop <- ceiling(rnorm(5000,mean = 35, sd= 3))
m1<-sample(x=pop,size = 50,replace = T)
m2<-sample(x=pop,size = 50,replace = T)
m3<-sample(x=pop,size = 50,replace = T)
sd(c(m1,m2,m3))/sqrt(length(c(m1,m2,m3)))
##Hay una disminucion del error estandar dado que hay una relacion inversa, a mayor nm, menor es el ES

##EJ3: Cambie el tamaño de muestra y evalue el valor de error estandar. ¿Cuanto deberia aumentar la muestra para que el error estandar disminuya a la mitad?
set.seed(999)
pop <- c(ceiling(rnorm(3500, mean = 30,sd = 3)), ceiling(runif(500, 45,51)))
m1<-sample(x=pop,size = 15,replace = T)
m2<-sample(x=pop,size = 15,replace = T)
m3<-sample(x=pop,size = 15,replace = T)
sd(c(m1,m2,m3))/sqrt(length(c(m1,m2,m3)))
##La muestra deberia aumentar al menos 3 veces para que el ES disminuya a la mitad