## SEMANA 7 - TLC ##

##E1: Tome cualquiera de las dos poblaciones y cambie los valores de tamaño y numero de muestras hasta que obtenga un histograma similar a una distribucion normal.
tamaño.m<-100
Nm<-40
P2 <- rexp(tamaño.m)
P2
mean(P2)
hist(P2)
Matr.P2<-matrix(NA,Nm,ncol = tamaño.m)
Matr.P2
for (i in 1:Nm){
  muestra <- runif(tamaño.m)
  Matr.P2[i,]=muestra
}
Matr.P2
mediaMatr.P2<- apply(Matr.P2,2,mean)
mediaMatr.P2
media<- cbind(Matr.P2,mediaMatr.P2)
media
par(mfrow=c(2,2))
hist(Matr.P2[1,], main = "Muestra 1", xlab = "Valores", col = "yellow")
hist(Matr.P2[2,], main = "Muestra 2", xlab = "Valores", col = "orange")
hist(Matr.P2[3,], main = "Muestra 3", xlab = "Valores", col = "green")
hist(media[,tamaño.m+1], main = "Medias", xlab= "Valores", col = "blue")

##EJ2: Simule una poblacion con distribucion normal de media 15 y sd 1. ¿Cual es el tamaño y numero de muestral, donde obtenemos una distribucion similar a la normal? Pruebe con Nmuestras 30 y tamaño 5 o 3, tambien podria reducir la SD. Haga lo mismo con una distribucion lognormal.
tamaño.p<-5
Nmp<-rnorm(30, mean = 15, sd = 1)
Matr.P<-matrix(NA,Nmp,ncol = tamaño.p)
Matr.P
for (i in 1:Nmp){
  muestra2 <- runif(tamaño.p)
  Matr.P[i,]=muestra2
}
Matr.P
mediaMatr.P<- apply(Matr.P,2,mean)
mediaMatr.P
mediap<- cbind(Matr.P,mediaMatr.P)
mediap
par(mfrow=c(2,2))
hist(Matr.P[1,], main = "Muestra 1", xlab= "Valores", col = "yellow")
hist(Matr.P[2,], main = "Muestra 2", xlab= "Valores", col = "orange")
hist(Matr.P[3,], main = "Muestra 3", xlab= "Valores", col = "green")
hist(mediap[,tamaño.p+1], main = "Medias", xlab= "Valores", col = "blue")

tamaño.p2<-10
Nmp2<-rlnorm(100, meanlog = 15, sdlog = 1)
Matr.po2<-matrix(NA,Nmp2,ncol = tamaño.p2)
Matr.po2
for (i in 1:Nmp2){
  muestra3 <- runif(tamaño.p2)
  Matr.po2[i,]=muestra3
}
mediaMatr.po2<- apply(Matr.po2,2,mean)
mediaMatr.po2
mediapo2<- cbind(Matr.po2,mediaMatr.po2)
mediapo2
par(mfrow=c(2,2))
hist(Matr.po2[1,], main = "Muestra 1", xlab= "Valores", col = "yellow")
hist(Matr.po2[2,], main = "Muestra 2", xlab= "Valores", col = "orange")
hist(Matr.po2[3,], main = "Muestra 3", xlab= "Valores", col = "green")
hist(mediapo2[,tamaño.p2+1], main = "Medias", xlab= "Valores", col = "blue")

##EJ3: Aumente la SD = 10 y disminuya el numero de individuos de la poblacion, ejemplo 1000. ¿Cuanto es el minimo de muestras a tomar, para que la media obtenida se acerque a la media de la poblacion?
poblacion<-abs(rnorm(1000, mean = 10, sd = 10))
mepob<-mean(poblacion)
mepob
mismuestras <- c(1,5,10,20,150,250,350,450,550,850,930)
mimedia<- NULL
for(i in 1:length(mismuestras)){
  muestra<-sample(poblacion,mismuestras[i],replace = F)
  mimedia<-c(mimedia,mean(muestra))
}
plot(cbind(mismuestras,mimedia),type = "l", xlab = "Muestras", ylab = "Media")
abline(h = mepob,col="red")