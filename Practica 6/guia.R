#Ejercicio 1
alfajores <- read.table("alfajores.txt", header = TRUE)
table(alfajores)

#Ejercicio 2
datos <- scan("lamparas.txt")
datos

# a) Comienzo por visualizar los datos
#datos[12] <- median(datos)
par( mfrow = c(2,2) )

# Plot eje cartesiano
plot(datos,
     main="Duración de las lámparas",
     xlab="Nro. de dato",
     ylab="Duración (en hs)",
     col="blue", pch=19, cex=0.5)
grid()

# Frecuencia de los datos
hist(datos,
     main="Frecuencia de los datos",
     xlab="Rango de duración (en horas)",
     ylab="Cantidad de lámparas en rango",
     col="lightblue")

# Boxplot de datos
boxplot(datos,
        main="Duración de las lámparas",
        col="lightblue")

# QQ-plot de datos (contra D. Normal)
qqnorm(datos,
       col="blue", pch=19, cex=0.5)
grid

