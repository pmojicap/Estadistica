##CARGA, VISTA Y ATTACH DE LOS DATOS##
misdatos<-read.csv("corridaT.csv",sep = ";")
misdatos
attach(misdatos)
View(misdatos)

##INSTALACIÓN Y CARGA DE LIBRERÍAS IMPORTANTES Y NECESARIAS##
if(!require(ape)){install.packages("ape")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(plyr)){install.packages("plyr")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(biotools)){install.packages("biotools")}
if(!require(moments)){install.packages("moments")}
if(!require(nortest)){install.packages("nortest")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(car)){install.packages("car")}
library(ape)
library(ggplot2)
library(ggridges)
library(plyr)
library(Rmisc)
library(dplyr)
library(biotools)
library(moments)
library(nortest)
library(rcompanion)
library(car)

##ESTADÍSTICA DESCRIPTIVA Y MEDIDAS DE RESUMEN##
#Resumen estadístico para las dos variables
summary(Air)
summary(Surf..Water)
#Resumen estadístico para las dos variables según la época del año
summary(Air[Year=="Verano"])
summary(Air[Year=="Otoño"])
summary(Surf..Water[Year=="Verano"])
summary(Surf..Water[Year=="Otoño"])
#Resumen estadístico para las dos variables según el momento del día
summary(Air[Day=="Madrugada"])
summary(Air[Day=="Mañana"])
summary(Air[Day=="Tarde"])
summary(Air[Day=="Noche"])
summary(Surf..Water[Day=="Madrugada"])
summary(Surf..Water[Day=="Mañana"])
summary(Surf..Water[Day=="Tarde"])
summary(Surf..Water[Day=="Noche"])
#Desviación estándar de las dos variables
sd(Air)
sd(Surf..Water)
#Desviación estándar de las dos variables según la época del año
sd(Air[Year=="Verano"])
sd(Air[Year=="Otoño"])
sd(Surf..Water[Year=="Verano"])
sd(Surf..Water[Year=="Otoño"])
#Desviación estándar de las dos variables según el momento del día
sd(Air[Day=="Madrugada"])
sd(Air[Day=="Mañana"])
sd(Air[Day=="Tarde"])
sd(Air[Day=="Noche"])
sd(Surf..Water[Day=="Madrugada"])
sd(Surf..Water[Day=="Mañana"])
sd(Surf..Water[Day=="Tarde"])
sd(Surf..Water[Day=="Noche"])
#Varianza de las dos variables
var(Air)
var(Surf..Water)
#Varianza de las dos variables según la época del año
var(Air[Year=="Verano"])
var(Air[Year=="Otoño"])
var(Surf..Water[Year=="Verano"])
var(Surf..Water[Year=="Otoño"])
#Varianza de las dos variables según el momento del día
var(Air[Day=="Madrugada"])
var(Air[Day=="Mañana"])
var(Air[Day=="Tarde"])
var(Air[Day=="Noche"])
var(Surf..Water[Day=="Madrugada"])
var(Surf..Water[Day=="Mañana"])
var(Surf..Water[Day=="Tarde"])
var(Surf..Water[Day=="Noche"])

##HISTOGRAMAS PARA LAS VARIABLES SEGÚN EL MOMENTO DEL DÍA Y ÉPOCA DEL AÑO##
hist(Air, xlab = "Temperatura del aire (°C)", main = "Histograma de la temperatura del aire", col = "orange",xlim = c(5,30))
hist(Air[Year=="Verano"], xlab = "Temperatura del aire (°C)", main = "Temperatura del aire en verano", col = "orange",xlim = c(5,30))
hist(Air[Year=="Otoño"], xlab = "Temperatura del aire (°C)", main = "Temperatura del aire en otoño", col = "orange",xlim = c(10,25))
hist(Air[Day=="Madrugada"], xlab = "Temperatura del aire (°C)", main = "Temperatura del aire en la madrugada", col = "orange",xlim = c(5,20))
hist(Air[Day=="Mañana"], xlab = "Temperatura del aire (°C)", main = "Temperatura del aire en la mañana", col = "orange",xlim = c(5,25))
hist(Air[Day=="Tarde"], xlab = "Temperatura del aire (°C)", main = "Temperatura del aire en la tarde", col = "orange",xlim = c(5,30))
hist(Air[Day=="Noche"], xlab = "Temperatura del aire (°C)", main = "Temperatura del aire en la noche", col = "orange",xlim = c(5,30))
hist(Surf..Water, xlab = "Temperatura del agua (°C)", main = "Histograma de la temperatura del agua", col = "lightblue",xlim = c(5,35))
hist(Surf..Water[Year=="Verano"], xlab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua en verano", col = "lightblue",xlim = c(5,35))
hist(Surf..Water[Year=="Otoño"], xlab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua en otoño", col = "lightblue",xlim = c(15,28))
hist(Surf..Water[Day=="Madrugada"], xlab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua en la madrugada", col = "lightblue",xlim = c(8,22))
hist(Surf..Water[Day=="Mañana"], xlab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua en la mañana", col = "lightblue",xlim = c(5,30))
hist(Surf..Water[Day=="Tarde"], xlab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua en la tarde", col = "lightblue",xlim = c(10,35))
hist(Surf..Water[Day=="Noche"], xlab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua en la noche", col = "lightblue",xlim = c(5,30))

##BOXPLOTS PARA LAS VARIABLES SEGÚN EL MOMENTO DEL DÍA Y ÉPOCA DEL AÑO##
boxplot(Air~Year, xlab = "Época del año", ylab = "Temperatura del aire (°C)", main = "Temperatura del aire según la época del año", col = c("lightblue","orange")) #Gráfico de cajas y bigotes para la temperatura del aire según la época del año
boxplot(Surf..Water~Year, xlab = "Época del año", ylab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua según la época del año", col = c("lightblue","orange")) #Gráfico de cajas y bigotes para la temperatura de la superficie agua según la época del año
boxplot(Air~Day, xlab = "Momento del día", ylab = "Temperatura del aire (°C)", main = "Temperatura del aire según el momento del día", col = rainbow(4)) #Gráfico de cajas y bigotes para la temperatura del aire según el momento del día
boxplot(Surf..Water~Day, xlab = "Momento del día", ylab = "Temperatura del agua (°C)", main = "Temperatura de la superficie del agua según el momento del día", col = rainbow(4)) #Gráfico de cajas y bigotes para la temperatura de la superficie del agua según el momento del día
boxplot(Air[Year=="Verano"]~Day[Year=="Verano"], xlab = "Momento del día", ylab = "Temperatura del aire (°C)", main = "T. del aire según el momento del día en verano", col = rainbow(4)) #Gráfico de cajas y bigotes para la temperatura del aire según el momento del día en verano
boxplot(Air[Year=="Otoño"]~Day[Year=="Otoño"], xlab = "Momento del día", ylab = "Temperatura del aire (°C)", main = "T. del aire según el momento del día en otoño", col = rainbow(4)) #Gráfico de cajas y bigotes para la temperatura del aire según el momento del día en otoño
boxplot(Surf..Water[Year=="Verano"]~Day[Year=="Verano"], xlab = "Momento del día", ylab = "Temperatura del agua (°C)", main = "T. superficie del agua según el momento del día en verano", col = rainbow(4)) #Gráfico de cajas y bigotes para la temperatura de la superficie del agua según el momento del día en verano
boxplot(Surf..Water[Year=="Otoño"]~Day[Year=="Otoño"], xlab = "Momento del día", ylab = "Temperatura del agua (°C)", main = "T. superficie del agua según el momento del día en otoño", col = rainbow(4)) #Gráfico de cajas y bigotes para la temperatura de la superficie del agua según el momento del día en otoño

##ALGUNOS GRÁFICOS DE DISTRIBUCIÓN##
ggplot(misdatos)+
  geom_point(aes(x=Air, y=Surf..Water,color= Year,shape=Year)) #Relación temperaturas superficie del agua-aire según la época del año
ggplot(misdatos)+
  geom_point(aes(x=Air, y=Surf..Water,color= Day,shape=Day)) #Relación temperaturas superficie del agua-aire según el momento del día
ggplot(misdatos) +
  geom_violin(aes(x=Year, y=Air, fill=Year, color=Year),alpha=0.4,width=1.4) +
  geom_boxplot(aes(x=Year, y=Air),outlier.colour = "red", outlier.shape = 16,width=0.1)+
  geom_jitter(aes(x=Year, y=Air),alpha=0.4) #Distribución de los datos de temperatura del aire según la época del año
ggplot(misdatos) +
  geom_violin(aes(x=Year, y=Surf..Water, fill=Year, color=Year),alpha=0.4,width=1.4) +
  geom_boxplot(aes(x=Year, y=Surf..Water),outlier.colour = "red", outlier.shape = 16,width=0.1)+
  geom_jitter(aes(x=Year, y=Surf..Water),alpha=0.4) #Distribución de los datos de temperatura de la superficie del agua según la época del año
ggplot(misdatos, aes(sample = Air)) +
  stat_qq() +
  stat_qq_line() #Curva de distribución de los datos de temperatura del aire
ggplot(misdatos, aes(sample = Surf..Water)) +
  stat_qq() +
  stat_qq_line() #Curva de distribución de los datos de temperatura de la superficie del agua

##ASIMETRÍA Y CURTOSIS
skewness(Air) #Existe un poco de sesgo
skewness(Surf..Water) #Existe un poco de sesgo
by(Air, Year, skewness) #Existe un poco de sesgo
by(Air, Day, skewness) #Existe un poco de sesgo
by(Surf..Water, Year, skewness) #Existe sesgo en ambas épocas del año
by(Surf..Water, Day, skewness) #Existe sesgo en todos los momentos del día y en la madrugada es extremadamente sesgada la distribución
kurtosis(Air) #Leptocurtica
kurtosis(Surf..Water) #Leptocurtica
by(Air, Year, kurtosis) #Leptocurticas
by(Air, Day, kurtosis) #Leptocurticas
by(Surf..Water, Year, kurtosis) #Leptocurticas
by(Surf..Water, Day, kurtosis) #Leptocurticas

##TESTS DE NORMALIDAD - CONTRASTE DE HIPÓTESIS##
lillie.test(Air) #No hay normalidad para la temperatura del aire
lillie.test(Surf..Water) #No hay normalidad para la temperatura de la superficie del agua
by(Air, Year, lillie.test) #No hay normalidad para la temperatura del aire en ninguna de las dos épocas del año
by(Surf..Water, Year, lillie.test) #No hay normalidad para la temperatura de la superficie del agua en ninguna de las dos épocas del año
by(Air, Day, lillie.test) #No hay normalidad para la temperatura del aire en ninguno de los momentos del día
by(Surf..Water, Day, lillie.test) #No hay normalidad para la temperatura de la superficie del agua en ninguno de los momentos del día

##HOMOGENEIDAD DE VARIANZAS - HOMOCEDASTICIDAD##
fligner.test(Air ~ Year, data = misdatos) #Las varianzas para la temperatura del aire son diferentes según la época del año
fligner.test(Air ~ Day, data = misdatos) #Las varianzas para la temperatura del aire son diferentes según el momento del día
fligner.test(Surf..Water ~ Year, data = misdatos) #Las varianzas para la temperatura de la superficie del agua son diferentes según la época del año
fligner.test(Surf..Water ~ Day, data = misdatos) #Las varianzas para la temperatura de la superficie del agua son diferentes según el momento del día

##TESTS DE DIFERENCIA DE MEDIAS - ALTERNATIVAS NO PARAMÉTRICAS##
kruskal.test(Air ~ Day) #La temperatura del aire cambia según el momento del día
kruskal.test(Surf..Water ~ Day) #La temperatura de la superficie del agua cambia según el momento del día
wilcox.test(Air ~ Year) #La temperatura del aire cambia según la época del año
wilcox.test(Surf..Water ~ Year) #La temperatura de la superficie del agua cambia según la época del año
pairwise.wilcox.test(Air, Day) #Las diferencias entre las temperaturas del aire se presentan entre todos los momentos del día pero la diferencia es menor entre la noche y la mañana
pairwise.wilcox.test(Surf..Water, Day) #Las diferencias entre las temperaturas de la superficie del agua se presentan entre todos los momentos del día pero la diferencia es menor entre la noche y la mañana

##PLOTS DE RELACIÓN (Y VARIACIÓN) ENTRE LAS VARIABLES Y TESTS DE CORRELACIÓN##
scatter.smooth(Air, Surf..Water, xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua - Relación") #Plot para ver la relación y variación de la temperatura del aire y la de la superficie del agua
cor.test(Air, Surf..Water, method = "spearman") #Test de correlación de Spearman
scatter.smooth(Air[Year =="Verano"], Surf..Water[Year == "Verano"], xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua en verano - Relación") #Plot para ver la relación y variación de la temperatura del aire y la de la superficie del agua en verano
cor.test(Air[Year =="Verano"], Surf..Water[Year == "Verano"], method = "spearman") #Test de correlación de Spearman para el verano
scatter.smooth(Air[Year =="Otoño"], Surf..Water[Year == "Otoño"], xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua en otoño - Relación") #Plot para ver la relación y variación de la temperatura del aire y la de la superficie del agua en otoño
cor.test(Air[Year =="Otoño"], Surf..Water[Year == "Otoño"], method = "spearman") #Test de correlación de Spearman para el otoño

##REGRESIÓN LINEAL##
rlverano<-lm(Air[Year =="Verano"] ~ Surf..Water[Year == "Verano"]) #Regresión lineal para el verano
summary(rlverano) #Información de la regresión lineal para el verano
rlotoño<-lm(Air[Year =="Otoño"] ~ Surf..Water[Year == "Otoño"]) #Regresión lineal para el otoño
summary(rlotoño) #Información de la regresión lineal para el otoño
general<-lm(Air ~ Surf..Water) #Regresión lineal general
summary(general) #Información de la regresión lineal general

##PLOTS DE RELACIÓN (Y VARIACIÓN) ENTRE LAS VARIABLES SEGÚN EL MOMENTO DEL DÍA Y TESTS DE CORRELACIÓN##
scatter.smooth(Air[Day=="Madrugada"],Surf..Water[Day=="Madrugada"], xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua en la madrugada - Relación")
scatter.smooth(Air[Day=="Mañana"],Surf..Water[Day=="Mañana"], xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua en la mañana - Relación")
scatter.smooth(Air[Day=="Tarde"],Surf..Water[Day=="Tarde"], xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua en la tarde - Relación")
scatter.smooth(Air[Day=="Noche"],Surf..Water[Day=="Noche"], xlab = "Temperatura del aire (°C)", ylab = "Temperatura del agua (°C)", main = "T. del aire vs T. del agua en la noche - Relación")
cor.test(Air[Day=="Madrugada"],Surf..Water[Day=="Madrugada"], method = "spearman")
cor.test(Air[Day=="Mañana"],Surf..Water[Day=="Mañana"], method = "spearman")
cor.test(Air[Day=="Tarde"],Surf..Water[Day=="Tarde"], method = "spearman")
cor.test(Air[Day=="Noche"],Surf..Water[Day=="Noche"], method = "spearman")

##REGRESIÓN LINEAL##
madrugada<-lm(Air[Day =="Madrugada"] ~ Surf..Water[Day == "Madrugada"]) #Regresión lineal para la madrugada
summary(madrugada) #Información de la regresión lineal para la madrugada
mañana<-lm(Air[Day =="Mañana"] ~ Surf..Water[Day == "Mañana"]) #Regresión lineal para la mañana
summary(mañana) #Información de la regresión lineal para la mañana
tarde<-lm(Air[Day =="Tarde"] ~ Surf..Water[Day == "Tarde"]) #Regresión lineal para la tarde
summary(mañana) #Información de la regresión lineal para la tarde
noche<-lm(Air[Day =="Noche"] ~ Surf..Water[Day == "Noche"])  #Regresión lineal para la noche
summary(mañana) #Información de la regresión lineal para la noche