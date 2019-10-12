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

#�Que clase de objeto es BIC? �Qu� clase de objeto es geospiza? use la funci�n 
#*str* para obtener informaci�n sobre la estructura.

class(BCI)
class(geospiza)
str(BCI)
str(geospiza)

#�Qu� es geospiza?(informacion sobre los datos: autor,significado de las variables etc.)

help(geospiza)
?geospiza

#�Qu� tipo de variable es tarsusL y qu� clase? 
#Abra el objeto *tortues* y mire �Qu� tipo de variables tiene y qu� clase son?, haga lo mismo con el objeto *mite.env* y revise 
#�Qu� tipo de variable es *shurb* y qu� clase es?

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

#Usando el data.frame *tortues* calcule la proporci�n de hembras y machos de la muestra
#proporcion de machos
sexos <- c('F', 'M')
M <- length(which(tortues == 'M'))
M
F <- length(which(tortues == 'F'))
F
paste(F,'/',M,sep = '')
F/M

#Usando el data.frame *tortues* calcule la proporci�n entre el ancho y el largo del caparaz�n, genere una tabla con la informaci�n de *tortues* 
#y la nueva variable derivada.
medidas <-cbind(tortues$long,tortues$larg)
colnames(medidas)<-c("Largo","Ancho")
proporcion <- tortues$long/tortues$larg
relacion <-cbind(medidas,proporcion)
relacion

#�Cu�ntas especies de *Corvus* fueron muestreadas en la regi�n Pale�rtica-Africana? Use el data.frame del g�nero 
#*Corvus*. �Qu� porcentaje de *Corvus* muestreados habitan zonas abiertas
no.muestreos <- table(corvus$phylog)
no.muestreos

#9 especies

zonas <- table(corvus$habitat)
zonas
prop.table(zonas)

#0.61%

#�C�mo luce el gr�fico de frecuencias para los tipos de h�bitat del g�nero *Corvus*?
plot(prop.table(zonas), main= "Corvus por habitat", ylab ="Frecuencia", col=c("green","orange"))

#Encuentre el m�ximo, as� como lo hizo para el m�nimo
atlas$birds
sp <- apply(atlas$birds,2,sum)
sp
max(sp)
min(sp)

#Repita el ejercicio para las variables continuas de tama�o de pico y tama�o del ala en el g�nero *Corvus*. 
#Busque c�mo cambiar colores, ejes y algunas propiedades que le permitan mejorar el dise�o del histograma.  

max(corvus$bill)
min(corvus$bill)
range(corvus$bill)

max(corvus$wing)
min(corvus$wing)
range(corvus$wing)

hist(corvus$bill, col= rainbow(8), main = "Histograma picos de Corvus" ,xlab = "Tama�o (mm)", seq(40,90,10))
hist(corvus$wing, col= rainbow(6), main = "Histograma alas de Corvus" ,xlab = "Tama�o (mm)")

#Invente unos datos de medidas, como se hizo en el caso de los craneos, y genere todo el ejercicio hasta el histograma 
ancho <- rnorm(n = 25, mean = 15, sd = 1.5)
largo <- rnorm(n = 25, mean = 20, sd = 1.0)
area <- largo*ancho
area
cuadros <- cbind.data.frame(ancho, largo, area)
cuadros
hist(cuadros$area, main = "�rea de los cuadros", xlab = "�rea", col= terrain.colors(7))
