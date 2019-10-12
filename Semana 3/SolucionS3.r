#EJERCICIOS SEMANA 3#

#Si la columna iris$Petal.Length tuviera valores 'NA', �C�mo podr�a eliminarlos antes de hacer la estimaci�n de la media?
#Estime la media aritm�tica de la altura, la longitud y el ancho del caparaz�n de las tortugas pintadas [data(tortues), paquete: ade4]
data("iris")
iris
na.rm = TRUE #Para eliminar los 'NA'
data("tortues")
x <- mean(tortues$long)
y <- mean(tortues$larg)
z <- mean(tortues$haut)
tabla_medias <- cbind(x,y,z)
tabla_medias

#Genere una funci�n propia que le permita encontrar la mediana, tal y como lo hizo con la media aritm�tica.
setosa <- iris$Petal.Length[which(iris$Species=='setosa')]
  mi.mediana <- function(x) {
  
  sorted <- sort(x)
  (sorted[25] + sorted[26]) / 2
}
mi.mediana(setosa)

#Genere una funci�n propia que le permita calcular la moda
mi.moda <- function(n) {
  
  x<- table(n)
  moda<-x[which.max(x)]
  return(moda)
}
mi.moda(setosa)
moda <- mi.moda(setosa)
#Despu�s de generar la funci�n del c�lculo de moda, graf�quela sobre el histograma.
hist(setosa, col = "yellow", xlab = "Longitud de P�talo", main = "Histograma LP Setosa", abline(v = 1.4, col = "royalblue",lwd = 2))
     
#Genere los cuantiles en intervalos de 0.5 
quantile(setosa,probs = c(seq(0, 1, 0.5)))

#�Qu� sucede si el n�mero de columnas deseado no se especifica? Obtenga la mediana, la moda y los cuantiles para cada especie de iris y para cada variable morfom�trica.
aggregate(iris[,1:4], list(Especies = iris$Species), mean)
aggregate(iris[,1:4], list(Especies = iris$Species), median)
aggregate(iris[,1:4], list(Especies = iris$Species), quantile)
#si no se especifica columnas toma todo

#Grafique la distribuci�n de frecuencias de cada poblaci�n usando histograma y densidad en el mismo gr�fico. Adiciones la media, la mediana y la moda.
#Estos estad�sticos descriptivos pueden ser resumidos en una tabla usando la funci�n describe(). Para m�s informaci�n podemos ir a ayuda para poder ver los par�metros de la funci�n.
pop1 <-abs(rnorm(200,mean = 6,sd = 1))
pop2 <-abs(rnorm(200,mean = 6,sd = 5))
pops<-cbind(pop1,pop2)
pops
hist(pops[,1], freq = FALSE,
     col = "green",
     border = "black", 
     prob = TRUE, 
     xlab = "Longitud de cola (cm)",
     main = "Frecuencias de longitud de cola en p�jaros P1")
lines(density(pops[,1]),
      lwd = 2,
      col = "orange")
abline(v = mean(pop1),
       col = "blue",
       lwd = 1)
abline(v = 3.41,
       col = "blue",
       lwd = 1)
abline(v = median(pop1),
       col = "red",
       lwd = 1)
legend(x = "topright",
       c("Densidad", "Media", "Mediana", "Moda"),
       col = c("orange", "blue", "red", "green"),
       lwd = c(2, 2, 2))

hist(pops[,2], freq = FALSE,
     col = "orange",
     border = "black", 
     prob = TRUE, 
     xlab = "Longitud de cola (cm)",
     main = "Frecuencias de longitud de cola en p�jaros P2")
lines(density(pops[,2]),
      lwd = 2,
      col = "blue")
abline(v = mean(pop1),
       col = "red",
       lwd = 1)
abline(v = 0.08,
       col = "green",
       lwd = 1)
abline(v = median(pop1),
       col = "yellow",
       lwd = 1)
legend(x = "topright",
       c("Densidad", "Media", "Mediana", "Moda"),
       col = c("blue", "red", "yellow", "green"),
       lwd = c(2, 2, 2))

?describe
help(describe)

#Magund 2007
read.csv("2007M.csv")
magund <- read.csv("2007M.csv")
View(magund)
summary(magund$A[which(magund$Traits=='I BK')])
summary(magund$A[which(magund$Traits=='II BK')])
summary(magund$A[which(magund$Traits=='I IV')])
summary(magund$A[which(magund$Traits=='II IV')])
mi.moda(magund$A[which(magund$Traits=='I BK')])
mi.moda(magund$A[which(magund$Traits=='II BK')])
mi.moda(magund$A[which(magund$Traits=='I IV')])
mi.moda(magund$A[which(magund$Traits=='II IV')])
sd(magund$A[which(magund$Traits=='I BK')])
sd(magund$A[which(magund$Traits=='II BK')])
sd(magund$A[which(magund$Traits=='I IV')])
sd(magund$A[which(magund$Traits=='II IV')])
var(magund$A[which(magund$Traits=='I BK')])
var(magund$A[which(magund$Traits=='II BK')])
var(magund$A[which(magund$Traits=='I IV')])
var(magund$A[which(magund$Traits=='II IV')])

summary(magund$P[which(magund$Traits=='I BK')])
summary(magund$P[which(magund$Traits=='II BK')])
summary(magund$P[which(magund$Traits=='I IV')])
summary(magund$P[which(magund$Traits=='II IV')])
mi.moda(magund$P[which(magund$Traits=='I BK')])
mi.moda(magund$P[which(magund$Traits=='II BK')])
mi.moda(magund$P[which(magund$Traits=='I IV')])
mi.moda(magund$P[which(magund$Traits=='II IV')])
sd(magund$P[which(magund$Traits=='I BK')])
sd(magund$P[which(magund$Traits=='II BK')])
sd(magund$P[which(magund$Traits=='I IV')])
sd(magund$P[which(magund$Traits=='II IV')])
var(magund$P[which(magund$Traits=='I BK')])
var(magund$P[which(magund$Traits=='II BK')])
var(magund$P[which(magund$Traits=='I IV')])
var(magund$P[which(magund$Traits=='II IV')])

summary(magund$W[which(magund$Traits=='I BK')])
summary(magund$W[which(magund$Traits=='II BK')])
summary(magund$W[which(magund$Traits=='I IV')])
summary(magund$W[which(magund$Traits=='II IV')])
mi.moda(magund$W[which(magund$Traits=='I BK')])
mi.moda(magund$W[which(magund$Traits=='II BK')])
mi.moda(magund$W[which(magund$Traits=='I IV')])
mi.moda(magund$W[which(magund$Traits=='II IV')])
sd(magund$W[which(magund$Traits=='I BK')])
sd(magund$W[which(magund$Traits=='II BK')])
sd(magund$W[which(magund$Traits=='I IV')])
sd(magund$W[which(magund$Traits=='II IV')])
var(magund$W[which(magund$Traits=='I BK')])
var(magund$W[which(magund$Traits=='II BK')])
var(magund$W[which(magund$Traits=='I IV')])
var(magund$W[which(magund$Traits=='II IV')])

plot(magund$Traits, magund$A, xlab = "Poblaciones", ylab = "Valores", main = "Longitud del cuerpo", col = rainbow(4))
plot(magund$Traits, magund$P, xlab = "Poblaciones", ylab = "Valores", main = "Tub�rculos separados IB", col = rainbow(4))
plot(magund$Traits, magund$W, xlab = "Poblaciones", ylab = "Valores", main = "Longitud del tarso I", col = rainbow(4))
