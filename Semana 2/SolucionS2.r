#EJERCICIOS SEMANA 3#

#Antes que nada se deben installar y cargar las librerias necesairas para la practica#

library(ape)
library(permute)
library(vegan)
library(ade4)
library(geiger)

#Luego debemos cargar los data requeridos para esta practica#
data(carnivora)
data(jackal)
data(jackal)
data(BCI)
data(mite.env)
data(atlas)
data(carniherbi49)
data(corvus)
data(tortues)
data(geospiza)

#¿Que clase de objeto es BIC? ¿Qué clase de objeto es geospiza? use la función 
#*str* para obtener información sobre la estructura.

class(BCI)
class(geospiza)
str(BCI)
str(geospiza)

#¿Qué es geospiza?(informacion sobre los datos: autor,significado de las variables etc.)

help(geospiza)
?geospiza

#¿Qué tipo de variable es tarsusL y qué clase? 
#Abra el objeto *tortues* y mire ¿Qué tipo de variables tiene y qué clase son?, haga lo mismo con el objeto *mite.env* y revise 
#¿Qué tipo de variable es *shurb* y qué clase es?

class(geospiza$tarsusL)

View(tortues)
class(tortues)
class(tortues$long)
class(tortues$larg)
class(tortues$haut)
class(tortues$sexe)

View(mite.env)
class(mite.env)
class(mite.env$SubsDens)
class(mite.env$WatrCont)
class(mite.env$Substrate)
class(mite.env$Shrub)
class(mite.env$Topo)

#Usando el data.frame *tortues* calcule la proporción de hembras y machos de la muestra
#proporcion de machos
sexos <- c('F', 'M')
M <- length(which(tortues == 'M'))
M
F <- length(which(tortues == 'F'))
F
paste(F,'/',M,sep = '')
F/M

#Usando el data.frame *tortues* calcule la proporción entre el ancho y el largo del caparazón, genere una tabla con la información de *tortues* 
#y la nueva variable derivada.
medidas <-cbind(tortues$long,tortues$larg)
colnames(medidas)<-c("Largo","Ancho")
proporcion <- tortues$long/tortues$larg
relacion <-cbind(medidas,proporcion)
relacion

#¿Cuántas especies de *Corvus* fueron muestreadas en la región Paleártica-Africana? Use el data.frame del género 
#*Corvus*. ¿Qué porcentaje de *Corvus* muestreados habitan zonas abiertas
no.muestreos <- table(corvus$phylog)
no.muestreos

#9 especies

zonas <- table(corvus$habitat)
zonas
prop.table(zonas)

#0.61%

#¿Cómo luce el gráfico de frecuencias para los tipos de hábitat del género *Corvus*?
plot(prop.table(zonas), main= "Corvus por habitat", ylab ="Frecuencia", col=c("green","orange"))

#Encuentre el máximo, así como lo hizo para el mínimo
atlas$birds
sp <- apply(atlas$birds,2,sum)
sp
max(sp)
min(sp)

#Repita el ejercicio para las variables continuas de tamaño de pico y tamaño del ala en el género *Corvus*. 
#Busque cómo cambiar colores, ejes y algunas propiedades que le permitan mejorar el diseño del histograma.  

max(corvus$bill)
min(corvus$bill)
range(corvus$bill)

max(corvus$wing)
min(corvus$wing)
range(corvus$wing)

hist(corvus$bill, col= rainbow(8), main = "Histograma picos de Corvus" ,xlab = "Tamaño (mm)", seq(40,90,10))
hist(corvus$wing, col= rainbow(6), main = "Histograma alas de Corvus" ,xlab = "Tamaño (mm)")

#Invente unos datos de medidas, como se hizo en el caso de los craneos, y genere todo el ejercicio hasta el histograma 
ancho <- rnorm(n = 25, mean = 15, sd = 1.5)
largo <- rnorm(n = 25, mean = 20, sd = 1.0)
area <- largo*ancho
area
cuadros <- cbind.data.frame(ancho, largo, area)
cuadros
hist(cuadros$area, main = "Área de los cuadros", xlab = "Área", col= terrain.colors(7))
