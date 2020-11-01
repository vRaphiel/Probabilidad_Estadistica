# 29 - 10 - 2020
setwd ("Practica 6/Estadistica Descriptiva/")

library("readxl")
score <- read.table("./scoring.txt", header=TRUE)
score

media_1 <- mean(score[,1])
media_1

# Podemos elegir tambien segun los nombres de las columnas
media_1 <- mean(score$func1)
media_1

media_2 <- mean(score$func1)
media_2

media_3 <- mean(score$func1)
media_3

#colMeans calcula todo el promedio de uuna
colMeans(score)

mediaFunc <- apply(score, 2, FUN="mean")
medianaFunc <- apply(score, 2,FUN="median")

# La media podada quita el 100alpha% de los valores de cada extremo, hacemos la media alpha-podada 
# para alpha = 0, 1, el caso 0,2 queda para luego

media_01 <- rep(0,3)
media_1_01 <- mean(score$func1, 0.1)
media_1_01

#El valor mediano toma el medio, no importan los extremos
#repetir y tenes los valores 

#Chusmear luego el notebook


#Cuartiles
quantile(score$func1, c(0.1,0.25,0.5,0.75,0.9))
quantile(score$func2, c(0.1,0.25,0.5,0.75,0.9))
quantile(score$func3, c(0.1,0.25,0.5,0.75,0.9))


#Desvio estandar
desvio_1 <- sd(score$func1)
desvio_2 <- sd(score$func2)
desvio_3 <- sd(score$func3)
# IQR
IQR(score$func2)

# Distribucion cercana a la normal
hist(score$func1, prob=TRUE)
curve(dnorm(x, mean = media_1, sd=desvio_1), add=TRUE)

# Distribucion bi-modal
hist(score$func2, prob=TRUE)
curve(dnorm(x, mean = media_2, sd = desvio_2), add=TRUE)

# Antisimetrico, la mediana esta mas cerca del 0.75
# outlier en 1.2, valor atipico, pueden aparecer en la vida
# asi que hay que ver si hay una falla en el modelo o en la hipotesis
# podria excluirse de los datos y ver lo que pasa con el resto
hist(score$func3, prob=TRUE)
curve(dnorm(x, mean = media_3, sd = desvio_3), add=TRUE)

with(data=score, boxplot(func1, func2, func3))
