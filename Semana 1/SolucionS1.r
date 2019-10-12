# EJERCICIOS SEMANA 2 #

# Reasigne 5 a la letra z, usando el operador <-, ahora, pregunte quien es z?

z <- 5
z

# Revisemos quien es rnorm(1). Empecemos buscando la funcion en help o ?rnorm

?rnorm
help(rnorm)

# Multiplique x por 2 y sumele 1 y dividalo por 1.5, ese resultado elevelo al cuadrado 


x <- 1:10
(((x*2)+1)/1.5)**2 ´´

# Tome cualquier objeto anterior y preguntele la clase

logicos <- c(T,F,TRUE,FALSE)
class(logicos)

# Que clase tiene el vector?

prueba <- c(1, "perro", FALSE, "gato", 1.5)

class(prueba)

# Hasta el momento hemnos generado varios vectores/objetos/variables, 
#podemos listarlas usando la funcion ls() o ls.str(), la cual nos permite, 
#no solo ver las variables, sino el tipo de cada una

ls()
ls.str()


# Cree un una secuencia que repita los numeros de 1 a 4, 3 veces y 
# asignela a un objeto llamado vector1. Ahora, cree una secuencia de numeros 
# pares del 2 al 24 y asignela a un objeto llamado vector2. Genere un arreglo 
# con ambos vectores y busque la columna 2, fila 1 de la segunda matriz y para la primera.

vector1<-rep(1:4,3)
vector2<- seq(2,24,2)
array(data = c(vector1,vector2),dim = c(3,4,2))

# Tome la primera columna y la segunda, concatenelas con c() Que encuentra? 
# Ahora, tome ambas columnas y use la funcion cbind(), asignelo a un objeto 
# llamado minuevatabla. Haga lo mismo, con las filas del 1:5 y del 10:30, use 
# primero c() y luego rbind() Que encuentra?

data(iris)


# Concatenando las dos primeras columnas
c(iris[,1],iris[,2])

# Usando la funcion cbind para unir dos columnas

cbind(iris[,1],iris[,2])

# Asignando las dos primeras columnas a  un vector llamado minuevatabla

minuevatabla<- cbind(iris[,1],iris[,2])
minuevatabla

# Concatenando las filas del 1:5 y del 10:30

c(iris[1:5,],iris[10:30,])
rbind(iris[1:5,],iris[10:30,])

# Busque el archivo suministrado por el profesor, carguelo a R y revise que contiene. 
# Extraiga las 5 primeras columnas con las 10 primeras filas, escriba esa nueva matriz 
# como csv en su computador.


#Descargar Araceae https://datadryad.org/stash/dataset/doi:10.5061/dryad.68n40


# Leer archivo

data <- read.csv('Matrix_Multivariate_Mega.csv')


# Extraer las 5 primeras columnas con las 10 primeras filas

data2 <- data[1:5,1:10]

# Escribir en el archivo final

write.csv(x = data2,file = 'Minimatriz.csv')
