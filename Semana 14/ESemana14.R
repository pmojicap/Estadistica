#### EEJRCICIOS SEMANA 14
#Autor 1: Mar�a Alejandra Navarro Corredor - 2161578
#Autor 2: Pablo Emilio Mojica Pradilla - 2160008

## Instalar paquetes
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(car)){install.packages("car")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(ggplot2)){install.packages("ggplot2")}

## ANOVA se llama an�lisis de varianzas aunque mida la diferencia entre las medias pues utiliza precisamente las varianzaas para determinar si hay diferencia entre las medias y lo calcula comparando la "varianza" entre las medias de los grupos y dentro de los grupos.

#Tres diferentes tratamientos se realizaron sobre el crecimiento del ma�z. Luego de aplicar los tratamientos, se calcul� el peso seco de cada planta.

data("PlantGrowth")
head(PlantGrowth)

summarise(.data = group_by(PlantGrowth, group),
          count = n(),
          mean = mean(weight, na.rm = TRUE),
          sd = sd(weight, na.rm = TRUE)
)
glimpse(PlantGrowth)

#EJER1 - ANOVA DE UNA V�A
PlantGrowth2 <- data.frame(ctrl=PlantGrowth[which(PlantGrowth$group==levels(PlantGrowth$group)[1]),1],
                           trt1=PlantGrowth[which(PlantGrowth$group==levels(PlantGrowth$group)[2]),1],
                           trt2=PlantGrowth[which(PlantGrowth$group==levels(PlantGrowth$group)[3]),1])
head(PlantGrowth2 )

OutVals = boxplot(PlantGrowth2)$out
OutVals

x <- which(PlantGrowth$weight %in% OutVals)
PlantGrowth3 <- PlantGrowth[-x, ]

anov2 <- aov(PlantGrowth3$weight ~ PlantGrowth3$group)
summary(anov2) #El p-valor nos indica que hay diferencias significativas y que por ende existen diferencias  para el crecimiento de las plantas entre los tratamientos, sin embargo, no sabemos cu�l/es lo son.

TukeyHSD(anov2) #Aqu� podemos ver que la diferencia est� entre el tratamiento 2 y el tratamiento 1 pues es donde obtenemos un p-valor significativo. Estos dos tratamientos presentan diferencias para el crecimiento de las plantas.

#EJER2: Vuelva a realizar el estimaci�n sin eliminar outliers. Eval�e los supuesto de la ANOVA en la variable peso. �Qu� encuentra? �Qu� supuesto no se cumple? Si no se cumple alg�n supuesto �Qu� deber�a probar antes de pensar en una prueba no param�trica?
ggplot(PlantGrowth, aes(sample = weight, colour = group)) +
  stat_qq() +
  stat_qq_line()

#Primer supuesto- variables independientes --> Son independientes.
levels(PlantGrowth$group)

#Segundo supuesto - No outliers --> Hay outliers, no se cumple.
PlantGrowth2 <- data.frame(ctrl=PlantGrowth[which(PlantGrowth$group==levels(PlantGrowth$group)[1]),1],
                           trt1=PlantGrowth[which(PlantGrowth$group==levels(PlantGrowth$group)[2]),1],
                           trt2=PlantGrowth[which(PlantGrowth$group==levels(PlantGrowth$group)[3]),1])
head(PlantGrowth2 )
OutVals = boxplot(PlantGrowth2)$out #Outlier en tratamiento 1.

#Tercer supuesto - Distribuci�n normal o cercana a la normal
ctrl <-  PlantGrowth[which (PlantGrowth$group == "ctrl"),]
ctrl
shapiro.test(x = ctrl$weight) #Normal

trt1 <- PlantGrowth[which(PlantGrowth$group == "trt1"),]
trt1
shapiro.test(x = trt1$weight) #Normal

trt2 <- PlantGrowth[which(PlantGrowth$group == "trt2"),]
trt2
shapiro.test(x = trt2$weight) #Normal

#Cuarto supuesto - Homogeneidad de varianzas
leveneTest(PlantGrowth$weight ~ PlantGrowth$group) #Hay homogeneidad de varianzas, las varianzas son iguales para el crecimiento de las plantas entre los tratamientos.

#El �nico supuesto que no se cumple es el de no outliers. Dado que los datos tienen una distribuci�n normal y las varianzas son iguales, no se requiere aplicar una prueba no param�trica y podemos proceder a realizar el proceso con el ejercicio anterior.

#### ANOVA
anov <- aov(PlantGrowth$weight ~ PlantGrowth$group)
summary(anov) #Existen diferencias para el crecimiento de las plantas entre los tratamientos.

####  Test post-hoc de Tukey --> Cu�l o cu�les son diferentes
TukeyHSD(anov) #La diferencia se presenta entre el tratamiento 2 y el tratamiento 1 al igual que cuando se elimin� el outlier.



#EJER3: Lea los datos de morphological_data.csv. Haga una an�lisis de varianza para la primera variable de medida 'BL'. Recuerde que para realizar un an�lisis de varianza debe evaluar los supuesto de la prueba.
getwd()
Data <- read.csv("Morphological_data.csv")
View(Data)
head(Data)

DATA <- data.frame(Data$COUNTRY, Data$BL)
DATA

levels(DATA$Data.COUNTRY) #Primer supuesto- variables independientes --> Son independientes, por pa�ses, por g�neros.

#Segundo supuesto - No outliers --> Hay outliers, no se cumple.
boxplot(DATA$Data.BL~DATA$Data.COUNTRY, xlab = "COUNTRY", ylab = "BL") #outliers en mx, ni, y sv
OutVals = boxplot(DATA$Data.BL~DATA$Data.COUNTRY, xlab = "COUNTRY", ylab = "BL")$out
OutVals #Outliers

#Tercer supuesto - Distribuci�n normal o cercana a la normal
gu <- DATA[which(DATA$Data.COUNTRY== "gu"),] 
shapiro.test(x = gu$Data.BL) #Normal

ho <- DATA[which(DATA$Data.COUNTRY== "ho"),]
shapiro.test(x = ho$Data.BL) #No normal

mx <- DATA[which(DATA$Data.COUNTRY== "mx"),]
shapiro.test(x = mx$Data.BL) #No normal

ni <- DATA[which(DATA$Data.COUNTRY== "ni"),]
shapiro.test(ni$Data.BL) #Normal

sv <- DATA[which(DATA$Data.COUNTRY== "sv"),]
shapiro.test(sv$Data.BL) #Normal

us <- DATA[which(DATA$Data.COUNTRY== "us"),]
shapiro.test(us$Data.BL) #Normal

lillie.test(DATA$Data.BL)

#Cuarto supuesto - Homogeneidad de varianzas
leveneTest(DATA$Data.BL ~ DATA$Data.COUNTRY) #No hay homogeneidad de varianzas, la varianza para la variable morfol�gica "BL" no es igual entre los pa�ses.

#El �nico supuesto que se cumple es el de variables independientes, por esto se recurre a una prueba no param�tica. Dado que solo se cumple el primero, no se puede ni siquiera aplicar la prueba alternativa Welch one-way test.

#Alternativa para ANOVA de una v�a
kruskal.test(Data.BL ~ Data.COUNTRY, data = DATA) #Existen diferencias para la variable morfol�gica "BL" entre los pa�ses.

#Test de Wilcoxon entre pares
pairwise.wilcox.test(DATA$Data.BL, DATA$Data.COUNTRY) #Las diferencias que existen para la variable morfol�gica "BL" se presentan entre varios pa�ses.

#�Qu� otro tipo de Anovas existen? �Es lo mismo dos factores que anidado? �Cu�l es el caso de los niveles de az�car en los nectarios de gulupa y la maracuya? Haga el an�lisis de varianza para este caso.
#Tambi�n est�n las ANOVAS mixtas, anidadas.
#No necesariamente dos factores es lo mismo que anidado. Pueden haber dos factores que est�n cruzados as� como tambi�n los puede haber anidados. Dos factores est�n cruzados cuando cada nivel de x factor ocurre en combinaci�n con cada nivel del otro factor z. Por otro lado, dos factores est�n anidados cuando los niveles de x factor son similares pero no id�nticos y cada uno ocurre en combinaci�n con diferentes niveles del otro factor z. En s�, puede decirse que dos factores est�n cruzados cuando todos los niveles de ambos factores "interact�an" entre s�; cuando est�n anidados, los niveles de un factor "interact�an" de manera espec�fica (propia) con los niveles del otro factor sin repetir interacciones.
Input = ("
Var Fase  Azucarmg
Gulupa  F1  0.97
Gulupa  F2  8.25
Gulupa  F3  35.34
Gulupa  F4  32.14
Gulupa  F1  0.97
Gulupa  F2  8.25
Gulupa  F2  12.56
Gulupa  F2  6.25
Gulupa  F2  8.36
Gulupa  F2  4.852
Gulupa  F2  11.2
Gulupa  F2  8.56
Gulupa  F1  1.2
Gulupa  F1  0.5
Gulupa  F1  0.35
Gulupa  F1  0.89
Gulupa  F1  0.96
Gulupa  F3  25.34
Gulupa  F3  39.35
Gulupa  F3  30.23
Gulupa  F3  34.2
Gulupa  F3  30.25
Gulupa  F3  38.26
Gulupa  F3  33.5
Gulupa  F3  33.5
Gulupa  F4  31.25
Gulupa  F4  37.25
Gulupa  F4  28.36
Maracuya    F1  0.5
Maracuya    F1  0.3
Maracuya    F1  0.2
Maracuya    F2  5.6
Maracuya    F2  8.3
Maracuya    F2  6.5
Maracuya    F2  4.2
Maracuya    F2  6.32
Maracuya    F2  5.25
Maracuya    F3  19.8
Maracuya    F3  14.2
Maracuya    F3  21.3
Maracuya    F3  22.5
Maracuya    F3  22.65
Maracuya    F4  35.2
Maracuya    F4  32.54
Maracuya    F4  35.25
Maracuya    F4  30.25
")
Datos = read.table(textConnection(Input),header=TRUE)
Datos
str(Datos)

anova2 <- aov(Datos$Azucarmg ~ Datos$Fase + Datos$Var)
summary(anova2) #Existen diferencias entre las fases y tambi�n entre las variantes de fruta.
anova3 <- aov(Datos$Azucarmg ~ Datos$Fase * Datos$Var)
summary(anova3) #Existen diferencias entre las fases, entre las variantes de fruta y entre las fases en conjunto con las variantes de fruta.

TukeyHSD(anova2) #Las diferencias se presentan entre todas las fases menos entre la F4 y la F3, las cuales presentan medias iguales para la concentraci�n de az�car. En cuanto a las variantes, hay una diferencia significativa entre la maracuy� y la gulupa, por lo que puede decirse que son diferentes para la concentraci�n de az�car.
TukeyHSD(anova3) #Las diferencias se presentan entre casi todas las combinaciones de fase y variantes, exceptuando algunas pocas que no presentan diferencias significativas y puede afirmarse que sus medias son iguales para la concentraci�n de az�car.