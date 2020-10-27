# Hallar la mediana usando R
qexp(0.5, 1)

round(qexp(0.5, 1), 2)


#Grafiquito
y <- rexp(100,1)
plot(y)
abline(h=1)
abline(h=0.69)
text(95, 1.1, "y=1", col="blue")
text(95, 0.8, "y=0.69", col="blue")

# ITF
mediana <- qgamma(0.5, shape = 1.33, rate=1/30901)
mediana

media <- 1.33*30901
media

# Predecir estatura
predecir_estatura <- function(estatura_padre){
  0.53*estatura_padre + 33.045
}

estatura_padre <- seq(60,80)
plot(estatura_padre, predecir_estatura(estatura_padre), type="l", xlab = "estatura_padre", ylab="estatura_hijo")