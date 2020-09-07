# Coronavirus con R
setwd("C:/Users/vAsmad/OneDrive/Documentos/Facultad/Probabilidad y estadistica/Talller - RStudio/Coronavirus")

datos<-read.table("datos_coronavirus.txt", header = TRUE, check.names = FALSE)

# En la consola -> datos
# A veces nos alcanza con ver algunas filas, las primeras
# head(datos) <- Primera fila
# tail(datos) <- Ultima fila
datos
View(datos)
head(datos)
tail(datos)

#Traemos de datos nombres de columnas/filas
colnames(datos)
rownames(datos)

#Extraemos las filas 15 y 50
datos[c(15,50)]
datos[1:3]

#Extraer columnas
#Con numero
datos[,4]
#Con Nombre
datos$"4/12/20"

#¿Cuantos paises tienen mas de 5000 casos al dia de hoy?
#La ultima columna numero
ncol(datos)
## [1] 82
#La ultima columna total
sum(datos[ , ncol(datos)]>5000)

#¿Que paises tienen mas de 150000 casos?
datos[,ncol(datos)]>150000
which(datos[,ncol(datos)]>150000)
rownames(datos)[which(datos[,ncol(datos)]>150000)]

#¿Cuantos casos confirmados hay hoy en el mundo?
sum(datos[,ncol(datos)])

i_arg <- which(rownames(datos)=="Argentina")
n_dias <- ncol(datos)
casos_argentina <- datos[i_arg,]
plot(1:n_dias, casos_argentina)

# Graficamos la cantidad de casos confirmados en Argentina por dia hasta el 29 de marzo
i_arg <- which(rownames(datos)=="Argentina");
colnames(datos)
casos_argentina <- datos[i_arg, 42:68]
plot(42:68, casos_argentina)

# Graficamos la cantidad de casos confirmados en Argentina por dia desde el 29 de marzo
i_arg <- which(rownames(datos)=="Argentina");
casos_argentina <- datos[i_arg, 68:ncol(datos)]
plot(68:ncol(datos), casos_argentina)

#Graficamos el logaritmo de la cantidad de casos confirmados en Argentina por dia
i_arg <- which(rownames(datos)=="Argentina")
n_dias <- ncol(datos)
casos_argentina <- datos[i_arg, 40:n_dias]
plot(40:n_dias, log(casos_argentina))

# Graficamos los casos totales en el mundo desde el 22 de enero
x <- 1:ncol(datos)
y <- colSums(datos)
plot(x,y)

# Graficamos el logaritmo d los casos totales en el mundo desde el 22 de enero
x <- 1:ncol(datos)
y <- colSums(datos)
plot(x, log(y))