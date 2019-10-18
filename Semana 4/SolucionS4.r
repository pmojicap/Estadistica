##SOLUCIÓN EJERCICIOS SEMANA 5##

#E1: ¿Cúal es la probabilidad de que 3 sean de fenotipo blanco?
dbinom(3, size = 8, prob =0.25)

#E2: ¿Cúal es la probabilidad de encontrar 6 semillas por bloque?, si el terreno es más pequeño (2x2m) y el promedio de semillas por bloque es de 10 Grafique todos los eventos de 0 a 20
dpois(x = 6,lambda = 10)
plot(dpois(x = 0:20,lambda = 10), ylab = "Probabilidad", xlab = "Eventos")

#E3: A partir del tortues del paquete ade4, calcule el promedio y la desviación estándar de las alturas de caparazones de las tortugas. Haga el análisis por sexo y grafíquelo.
data("tortues")
aggregate(tortues$larg,by = list(tortues$sexe), FUN = mean)
aggregate(tortues$larg,by = list(tortues$sexe), FUN = sd)
media <- tapply(tortues$larg,tortues$sexe, FUN = mean)
barplot(media, col = rainbow(2))
desviacione <- tapply(tortues$larg,tortues$sexe, FUN = sd)
barplot(desviacione, col = rainbow(2))

#E4: Lea el artículo sobre tipos de crecimientos poblacionales y escriba el código en R que mejor describa el ejemplo de las bacterias en crecimiento exponencial.
Bacterias <- c(1,3,9,27,81)
Horas <-0:4
plot(Horas, Bacterias)

#E5: Simule 5000 peces de criadero en un tanque, todos son de la misma cohorte. Imagine que los organiza en fila y quiere seleccionar 100 para calcular estadísticos de la población. Haga un muestreo sistemático y calcule los estadísticos y descriptivos vistos anteriormente. Grafique.
numero <- c(1:5000)
numero
longitud <- rnorm(n = 5000, mean = 5, sd = 1)
longitud
tabla <- data.frame (cbind(numero,longitud))
tabla
#k es igual a 50 por la división entre 5000 y 100
vec <- 1:50
vec
al <- sample(vec,1)
al
library("devtools")
install_github("DFJL/SamplingUtil")
library(SamplingUtil)
muestreo <- sys.sample(5000,100)
muestra <- tabla[muestreo, ]
muestra
mean(muestra$longitud)
median(muestra$longitud)
mi.moda <- function(n) {
  
  x<- table(n)
  moda<-x[which.max(x)]
  return(moda)
}
mi.moda(muestra$longitud)
sd(muestra$longitud)
var(muestra$longitud)
hist(muestra$longitud, freq = F, xlab = "Longitud (mm)", col = rainbow(12), main = "Longitud de peces")
mtext("sd = 0,99 y var = 0.98")
abline(v = mean(muestra$longitud), col = "red",lwd = 2)
abline(v = median(muestra$longitud), col = "orange", lwd = 2)
lines(density(muestra$longitud))
legend(x = "topright", c("Densidad","Media", "Mediana"), col = c("blue","red", "orange"),lwd = c(2,2,2))
