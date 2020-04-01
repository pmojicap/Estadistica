#EJERCICIOS SEMANAS 15 y 16
#Cargamos las librerías importantes
library(tidyverse)
library(ggpubr)
library(rstatix)
library(multcomp)
library(vegan)
library(factoextra)

#EJERCICIO 1 - MANOVA:
#Queremos ver si existen diferencias en los tratimientos para el crecimiento de dientes en ratones según el complemento que se utilice y la dosis del mismo
data("ToothGrowth") #Cargamos los datos
ToothGrowth$dose <- factor(ToothGrowth$dose, 
                           levels = c(0.5, 1, 2),
                           labels = c("D0.5", "D1", "D2")) #Convertimos la variable en factor
ATG <- aov(len ~ supp + dose, data = ToothGrowth) #ANOVA 1
summary(ATG)
BTG <- aov(len ~ supp * dose, data = ToothGrowth) #ANOVA 2
summary(BTG)
TukeyHSD(ATG) #Prueba de pares 1
TukeyHSD(BTG) #Prueba de pares 2

#EJERCICIO 2 - MATRIZ DE CORRELACIÓN:
#Queremos ver la correlación que existe entre la temperatura del aire, la temperatura del suelo y la temperatura del agua en un lago de Colombia
aire<-c(25,22,24,37,31,28,32,35,40,20) #Creamos la primera variable
agua<-c(24,24,21,30,28,24,26,28,34,22) #Creamos la segunda variable
tierra1<-c(27,24,35,30,26,29,33,39,16,17) #Creamos la tercera variable
datos1<-data.frame(aire,agua,tierra1) #Creamos el dataframe
lago1<-cor(datos1,use="pairwise.complete.obs") #Matriz de correlación

#EJERCICIO 3 - TEST DE MANTEL:
#Con ayuda de los datos del punto anterior y los de un nuevo lago se requiere saber el nivel de correlación entre las matrices
aire2<-c(20,22,25,21,27,28,18,22,23,19)
agua2<-c(15,15,17,16,20,22,14,16,16,15)
tierra2<-c(18,20,22,21,25,24,15,18,22,15)
datos2<-data.frame(aire2,agua2,tierra2)
lago2<-cor(datos2,use="pairwise.complete.obs") #Repetir proceso del ejercicio 2 pero con nuevos datos para otro lago
testmantel<-mantel(lago1,lago2)
testmantel #Resultados test de Mantel

#EJERCICIO 4 - CORRELACIÓN:
#Queremos saber el tipo y nivel de correlación que existe entre el viento y la temperatura en una ciudad muy poblada de Estados Unidos
data("airquality") #Cargamos base de datos
View(airquality) #Visualizamos los datos
plot(airquality$Temp~airquality$Wind) #Generamos el gráfico
cor.test(airquality$Temp,airquality$Wind) #Test de correlación

#EJERCICIO 5 - COMPONENTES PRINCIPALES:
#Queremos ver cómo es la relación y el efecto en la variación de distintas variables que afectan  en los páramos de las tres cordilleras de Colombia
CORDI<-read.csv("COREnv_Paramos_Colombia_sel.csv", sep=";",header = T) #Cargamos el archivo de datos
View(CORDI)
COR<-as.factor(CORDI$coordillera) #Creamos vec1
pcaparamosZ<-prcomp(CORDI[,3:10],scale=T) #Creamos vec2
fviz_pca_biplot(pcaparamosZ,geom=c("point"),pointsize = 2.0,legend.title = "coordilleras",title = "PCA", col.ind =COR,col.var = "purple",repel = TRUE) #Realizamos el gráfico de PCA
