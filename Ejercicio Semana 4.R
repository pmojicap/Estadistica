library(psych)
getwd()
datos<-read.csv("2007_EAA_Magud_et_al_data.csv")
View(datos)
?datos

##MEDIA DE LOS DATOS##
mean(datos$A)
mean(datos$P)
mean(datos$W)

##MEDIANA DE LOS DATOS##
median(datos$A)
median(datos$P)
median(datos$W)

##FUNCION DE MODA##
moda<-function(n){
  x<-table (n)
  mode<-x[which.max(x)]
  return(mode)
}##pero no es necesario por ser variable numerica  continua

##RESUMEN ESTADISTICO DE LOS DATOS##
tapply(datos$A,INDEX = datos$Traits,FUN = summary)
tapply(datos$P,INDEX = datos$Traits,FUN = summary)
tapply(datos$W,INDEX = datos$Traits,FUN = summary)

##VARIANZA DE LOS DATOS##
var(datos$A)
var(datos$P)
var(datos$W)
var(datos$A[which(datos$Traits=='I BK')])
var(datos$A[which(datos$Traits=='II BK')])
var(datos$A[which(datos$Traits=='I IV')])
var(datos$A[which(datos$Traits=='II IV')])
var(datos$P[which(datos$Traits=='I BK')])
var(datos$P[which(datos$Traits=='II BK')])
var(datos$P[which(datos$Traits=='I IV')])
var(datos$P[which(datos$Traits=='II IV')])
var(datos$W[which(datos$Traits=='I BK')])
var(datos$W[which(datos$Traits=='II BK')])
var(datos$W[which(datos$Traits=='I IV')])
var(datos$W[which(datos$Traits=='II IV')])

#DESVIACION ESTANDAR DE LOS DATOS##
sd(datos$A)
sd(datos$P)
sd(datos$W)
sd(datos$A[which(datos$Traits=='I BK')])
sd(datos$A[which(datos$Traits=='II BK')])
sd(datos$A[which(datos$Traits=='I IV')])
sd(datos$A[which(datos$Traits=='II IV')])
sd(datos$P[which(datos$Traits=='I BK')])
sd(datos$P[which(datos$Traits=='II BK')])
sd(datos$P[which(datos$Traits=='I IV')])
sd(datos$P[which(datos$Traits=='II IV')])
sd(datos$W[which(datos$Traits=='I BK')])
sd(datos$W[which(datos$Traits=='II BK')])
sd(datos$W[which(datos$Traits=='I IV')])
sd(datos$W[which(datos$Traits=='II IV')])

##DESCRIBEBY, RESUMEN COMPLETO##
describe.by(datos[,c("A","P","W")],group =datos$Traits)

##HISTOGRAMAS##
hist(datos$A[which(datos$Traits==c("I BK","II BK"))],main="Longitud del cuerpo de eriófidos de Belgrado",xlab="Longitud",ylab="Frecuencia", col = "orange")
hist(datos$A[which(datos$Traits==c("I IV","II IV"))],main= "Longitud del cuerpo de eriófidos de Vianca",xlab="Longitud",ylab="Frecuencia", col = "green")
hist(datos$P[which(datos$Traits==c("I BK","II BK"))],main= "Tubercles 1b apart de eriófidos de Belgrado",xlab="Longitud",ylab="Frecuencia", col = "blue")
hist(datos$P[which(datos$Traits==c("I IV","II IV"))],main= "Tubercles 1b apart de eriófidos de Vianca",xlab="Longitud",ylab="Frecuencia", col = "yellow")
hist(datos$W[which(datos$Traits==c("I BK","II BK"))],main= "Tarsus I length de eriófidos de Belgrado",xlab="Longitud",ylab="Frecuencia", col = "purple")
hist(datos$W[which(datos$Traits==c("I IV","II IV"))],main= "Tarsus I length de eriófidos de Vianca",xlab="Longitud",ylab="Frecuencia", col = "red")

##BOXPLOTS##
boxplot(datos$A[which(datos$Traits==c("I BK","II BK"))],notch=T,main="Longitud del cuerpo de eriófidos de Belgrado", col = "orange")
boxplot(datos$A[which(datos$Traits==c("I IV","II IV"))],notch=T,main="Longitud del cuerpo de eriófidos de Vianca", col = "green")
boxplot(datos$P[which(datos$Traits==c("I BK","II BK"))],notch=T,main="Tubercles 1b apart de eriófidos de Belgrado", col = "blue")
boxplot(datos$P[which(datos$Traits==c("I IV","II IV"))],notch=T,main="Tubercles 1b apart de eriófidos de Vianca", col = "yellow")
boxplot(datos$W[which(datos$Traits==c("I BK","II BK"))],notch=T,main="Tarsus I length de eriófidos de Belgrado", col = "purple")
boxplot(datos$W[which(datos$Traits==c("I IV","II IV"))],notch=T,main="Tarsus I length de eriófidos de Vianca", col = "red")

##BOXPLOTS 2##
plot(datos$Traits, datos$A, xlab = "Poblaciones", ylab = "Valores", main = "Longitud del cuerpo", col = rainbow(4))
plot(datos$Traits, datos$P, xlab = "Poblaciones", ylab = "Valores", main = "Tubérculos separados IB", col = rainbow(4))
plot(datos$Traits, datos$W, xlab = "Poblaciones", ylab = "Valores", main = "Longitud del tarso I", col = rainbow(4))

##Al analizar los histogramas y los boxplot de los diferentes rasgos se puede inferir que hay diferencias debido a que los estadisticos difieren significativamente entre las poblaciones##
