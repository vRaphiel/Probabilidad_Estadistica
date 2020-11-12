# PARTE 1
datos_10 <- read.csv("Ejercicio_Entrega/datos_id76019_n_10.csv")
mean(datos_10$datos)*2
datos_50 <- read.csv("Ejercicio_Entrega/datos_id76019_n_50.csv")
mean(datos_50$datos)*2
max(datos_50$datos)
datos_100 <- read.csv("Ejercicio_Entrega/datos_id76019_n_100.csv")
mean(datos_100$datos)*2
max(datos_100$datos)
datos_1000 <- read.csv("Ejercicio_Entrega/datos_id76019_n_1000.csv")
mean(datos_1000$datos)*2
max(datos_1000$datos)

# Ejercicio para entregar 07/11/2020
require(stats); require(graphics)
or <- Orange
sinOr <- or[c(1,2,4,5,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35),]

mean(sinOr$age)

sd(sinOr$circumference)

median(sinOr$age[sinOr$Tree==2])

ajuste2 <- lm(formula = age~circumference, data=sinOr)
ajuste2

y = -1.849 + 8.025*51
y
