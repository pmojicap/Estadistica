## SEMANA 5 - DISTRIBUCIONES Y TEOREMA ##

## INTRO Y AYUDA ## 
help(distributions)
help("dexp")

##E1: ¿Cual es la probabilidad de que 3 sean de fenotipo blanco?
dbinom(3, size = 8, prob =0.25)

##E2: ¿Cual es la probabilidad de encontrar 6 semillas por bloque si el terreno es mas pequeño (2x2m) y el promedio de semillas por bloque es de 10. Grafique todos los eventos de 0 a 50.
dpois(x = 6,lambda = 10)
plot(dpois(x = 0:50,lambda = 10,))

##E3: A partir del tortues del paquete ade4, calcule el promedio y la desviacion estandar de las alturas de caparazones de las tortugas. Haga el analisis por sexo y grafiquelo.
library(ade4)
data("tortues")
View(tortues)
help(tortues)
machos<-tortues$haut[which(tortues$sexe=="M")]
hembras<-tortues$haut[which(tortues$sexe=="F")]
mean(machos)
mean(hembras)
sd(machos)
sd(hembras)
boxplot(tortues$haut~tortues$sexe, main="Altura del caparazón", col = rainbow(2), xlab = "Sexo", ylab = "Altura (mm)")

##E4: Simule 5000 peces de criadero en un tanque, todos son de la misma cohorte. Imagine que los organiza en fila y quiere seleccionar 100 para calcular estadisticos de la población. Haga un muestreo sistematico y calcule los estadisticos y descriptivos vistos anteriormente. Grafique.
peces<-abs(rnorm(5000,5,1))
seleccion<- seq(10,5000,50)
seleccion
muestraf<-peces[seleccion]
mean(muestraf)
sd(muestraf)
boxplot(muestraf, ylab = "Longitud (cm)", col = "yellow", main = "Longitud de mis peces")
pnorm(6, 5, 1,lower.tail = FALSE) ##probabilidad de encontrar peces con longitud mayor a 6cm
pnorm(5, 5, 1) ##probabilidad de encontrar peces con longitud menor a 5cm