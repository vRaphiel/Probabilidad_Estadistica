# Cambio de variable

# Genero la uniforme
u <- runif(10000)
u
#Genero histograma
hist(u)

# Le aplico qnorm (Inversa)
x <- qnorm(u)
# Histograma frecuencia relativa
hist(x, freq = F)


# Ahora quiero ver la densidad, para ver la relacion entre histogramas
xx <- seq(-4,4,0.1)
yy <- dnorm(xx)

# Superpongo la cuurva de la normal al histograma
lines(xx,yy)


# Con la exponencial
x <- qexp(u, 2)
hist(x, freq = F)
xx <- seq(0,5,0.1)
yy <- dexp(xx, 2)
lines(xx,yy)

# Graficar 3D
# persp()

# A demas de hacer un histograma, cuando uno tiene una muestra
# como de una exponencial, se puede hacer una aprox de la densidad
# Le doy a R una lista de puntos, puedo pedir una densidad estimada

# hace una densidad aproximada, estima de donde provienen los numeros
plot(density(x))

# Lo superpongo con la exponencial para ver la diferencia
xx <- seq(0,5,0.1)
yy <- dexp(xx, 2)
lines(xx,yy, col=2)

