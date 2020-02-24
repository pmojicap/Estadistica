##EJ:  Lea el artículo: http://www.limnetica.com/documentos/limnetica/limnetica-9-1-p-67.pdf Simule las variables que evaluan en el estudio. Aplique los descriptivos estadisticos que hemos visto hasta el momento. Grafique. ¿Que otras preguntas de investigacion podria formular usando esos datos? Tome los diagramas de flujo de test estadisticos y de graficado. Tome una de las preguntas que formulo y diseñe el analisis.

##SIMULANDO VARIABLES##
PesoHem<-c(abs(rnorm(12,0.7,0.1)), rnorm(12,0.8,0.1))
PesoHem
PesoHemH<-(PesoHem+PesoPues)
PesoHemH
PesoPues<-c(abs(rnorm(12,0.2,0.1)), rnorm(12,0.2,0.1))
PesoPues
NumHuev<-c(250,262,196,214,303,247,208,277,298,400,215,280,222,371,261,414,175,288,244,289,311,172,230,260)
NumHuev ##ESTOS NUMEROS FUERON GENERADOS EN EXCEL##
LongiCo<-c(abs(rnorm(12,3.5,0.2)), rnorm(12,3.8,0.2))
LongiCo
TamaHuev<-c(abs(rnorm(12,1.0,0.2)), rnorm(12,1.0,0.1))
TamaHuev
Pob<-c("A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B")
Pob
Temperatura<-c(abs(rnorm(8,23,0.1)), rnorm(8,25,0.1), rnorm(8,30,0.1))
Temperatura

##CREANDO MI DATA FRAME##
Huevos <- data.frame(
  "Peso Hembra" = PesoHem,
  "Peso Hembra Huevos" = PesoHemH,
  "Peso Puesta" = PesoPues, 
  "Numero de Huevos" = NumHuev,
  "Longitud corporal" = LongiCo,
  "Tamaño Huevos" = TamaHuev,
  "Poblacion" = Pob,
  "Temperatura" = Temperatura)
View(Huevos)

##RESUMEN ESTADISTICO##
summary(Huevos)

##RESUMEN ESTADISTICO POR POBLACIONES##
summary(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")])
summary(Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")])
summary(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")])
summary(Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")])
summary(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")])
summary(Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")])
summary(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")])
summary(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")])
summary(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")])
summary(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")])
summary(Huevos$Temperatura[which(Huevos$Poblacion=="A")])
summary(Huevos$Temperatura[which(Huevos$Poblacion=="B")])

##DESVIACION ESTANDAR POR POBLACIONES##
sd(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")])
sd(Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")])
sd(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")])
sd(Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")])
sd(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")])
sd(Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")])
sd(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")])
sd(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")])
sd(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")])
sd(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")])
sd(Huevos$Temperatura[which(Huevos$Poblacion=="A")])
sd(Huevos$Temperatura[which(Huevos$Poblacion=="B")])

##VARIANZA POR POBLACIONES##
var(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")])
var(Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")])
var(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")])
var(Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")])
var(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")])
var(Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")])
var(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")])
var(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")])
var(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")])
var(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")])
var(Huevos$Temperatura[which(Huevos$Poblacion=="A")])
var(Huevos$Temperatura[which(Huevos$Poblacion=="B")])

##RANGO O AMPLITUD POR POBLACIONES##
range(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")])
range(Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")])
range(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")])
range(Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")])
range(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")])
range(Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")])
range(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")])
range(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")])
range(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")])
range(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")])
range(Huevos$Temperatura[which(Huevos$Poblacion=="A")])
range(Huevos$Temperatura[which(Huevos$Poblacion=="B")])

##ERROR ESTANDAR PESO HEMBRA##
sd(c(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")],Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")]))/sqrt(length(c(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")],Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")])))

##ERROR ESTANDAR PESO PUESTA##
sd(c(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")],Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")]))/sqrt(length(c(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")],Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")])))

##ERROR ESTANDAR LONGITUD CORPORAL##
sd(c(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")],Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")]))/sqrt(length(c(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")],Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")])))

##ERROR ESTANDAR TAMAÑO HUEVOS##
sd(c(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")],Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")]))/sqrt(length(c(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")],Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")])))

##ERROR ESTANDAR PESO HEMBRA CON HUEVOS##
sd(c(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")],Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")]))/sqrt(length(c(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")],Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")])))

##ERROR ESTANDAR TEMPERATURA##
sd(c(Huevos$Temperatura[which(Huevos$Poblacion=="A")],Huevos$Temperatura[which(Huevos$Poblacion=="B")]))/sqrt(length(c(Huevos$Temperatura[which(Huevos$Poblacion=="A")],Huevos$Temperatura[which(Huevos$Poblacion=="B")])))

##HISTOGRAMAS##
hist(Huevos$Peso.Hembra, col = "lightgreen", main = "Peso hembra (mg)")
hist(Huevos$Peso.Puesta, col = "lightblue", main = "Peso puesta (mg)")
hist(Huevos$Longitud.corporal, col = "lightyellow", main = "Longitud corporal (mm)")
hist(Huevos$Tamaño.Huevos, col = "orange", main = "Tamaño huevos (mm)")
hist(Huevos$Peso.Hembra.Huevos, col = "red", main = "Peso hembra con huevos (mg)")
hist(Huevos$Temperatura, col= "yellow", main = "Temperatura (°C)")

##BARPLOT PARA REPRESTENTAR EL NUMERO DE HUEVOS##
barplot(Huevos$Numero.de.Huevos, col= rainbow(24))

##BOXPLOTS PARA COMPARAR VARIABLES POR POBLACIONES##
boxplot(cbind(Huevos$Peso.Hembra[which(Huevos$Poblacion=="A")],Huevos$Peso.Hembra[which(Huevos$Poblacion=="B")]), col = "lightgreen", main="Peso hembra (mg)")
boxplot(cbind(Huevos$Peso.Puesta[which(Huevos$Poblacion=="A")],Huevos$Peso.Puesta[which(Huevos$Poblacion=="B")]), col = "lightblue", main="Peso puesta (mg)")
boxplot(cbind(Huevos$Longitud.corporal[which(Huevos$Poblacion=="A")],Huevos$Longitud.corporal[which(Huevos$Poblacion=="B")]), col = "lightyellow", main="Longitud corporal (mm)")
boxplot(cbind(Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="A")],Huevos$Tamaño.Huevos[which(Huevos$Poblacion=="B")]), col = "orange", main="Tamaño huevos (mm)")
boxplot(cbind(Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="A")],Huevos$Peso.Hembra.Huevos[which(Huevos$Poblacion=="B")]), col = "red", main="Peso hembra con huevos (mg)")
boxplot(cbind(Huevos$Temperatura[which(Huevos$Poblacion=="A")],Huevos$Temperatura[which(Huevos$Poblacion=="B")]), col = "yellow", main="Temperatura (°C)")

##RELACION ENTRE VARIABLES##
plot(Huevos$Peso.Hembra~Huevos$Peso.Puesta, xlab= "Peso de la puesta (mg)", ylab="Peso hembra (mg)")
plot(Huevos$Peso.Hembra~Huevos$Numero.de.Huevos, xlab="Numero de Huevos", ylab="Peso hembra (mg)")
plot(Huevos$Peso.Hembra~Huevos$Longitud.corporal, xlab="Longitud corporal (mm)", ylab="Peso hembra (mg)")
plot(Huevos$Peso.Hembra~Huevos$Tamaño.Huevos, xlab="Tamaño huevos (mm)",ylab="Peso hembra (mg)")
plot(Huevos$Peso.Hembra~Huevos$Peso.Hembra.Huevos, xlab="Peso hembra con huevos (mg)", ylab="Peso hembra (mg)")
plot(Huevos$Tamaño.Huevos~Huevos$Temperatura, xlab="Temperatura (°C)", ylab="Tamaño huevos (mm)")

##En base a los datos y las variables del articulo se pueden formular varias preguntas de investigacion, tales como:
#1. ¿Como afecta el tamaño corporal al peso de las hembras?
#2. ¿Como afecta el numero de huevos al peso de la puesta?
#3. ¿Como afecta el peso corporal al peso de la puesta?
#4. ¿Como afecta el tamaño corporal al numero de huevos?
#5. ¿Como afecta el numero de huevos al peso corporal?
#6. ¿Cómo afecta el tamaño corporal al peso de la puesta?
#7. ¿Cómo afecta el peso de la puesta al peso de las hembras?
#Basicamente se pueden plantear muchas preguntas en cuanto a la relacion entre variables y se pueden comparar todas ellas entre las dos poblaciones. Si escojo, por ejemplo, la pregunta numero 1, podria diseñar una metodologia similar a la del experimento pero pues con las dos variables, realizando distintos calculos y test estadisticos, ademas de representaciones graficas.

plot(Huevos$Peso.Hembra~Huevos$Longitud.corporal, xlab="Longitud corporal (mm)", ylab="Peso hembra (mg)")

#Para el caso de mi simulacion se puede ver que no hay una relacion clara entre las dos variables, sin embargo, por lo general (o puede decirse que) hay un mayor peso de las hembras a medida que la longitud o tamaño corporal es mayor, lo que indica que una hembra de talla grande tiene mas probabilidad de pesar mas que una de talla pequeña.
#En cuanto a las diferencias entre poblaciones se puede decir que, en promedio, las hembras de la poblacion B tienen un mayor peso que las de la poblacion A. Ademas, siguiendo la tendencia, las hembras de la poblacion B tienen en promedio una longitud o talla corporal mayor que las de la poblacion A, por lo que puede decirse que son organismos mas grandes, algo que puede estar relacionado/afectando las variables estudiadas para los huevos en estas poblaciones.