# ) Ejercicio de la Ruleta

mu <- 1/4

datos <- c(rep(1,40), rep(0,68))
n <- length(datos)

xraya <- mean(datos)
s <- sqrt(var(datos))  #sd(datos) o a mano sqrt(sum((datos - xraya)^2)/(n-1))

# Test: H0: mu = 1/4 vs H1: mu > 1/4
Tobs <- sqrt(n)*(xraya - mu)/s

# Region de rechazo: T > zalpha
alpha <- 0.06
zalpha <- qnorm(alpha, 0, 1, lower.tail = FALSE)

Tobs > zalpha

# Rechazo H0 en favor de H1: tengo evidencia para suponer que la ruleta estaba trucada

# Miramos la potencia

mues <- seq(0,1, length.out = 1001)
pies <- c()
PI <- function(mues, mu0, muestra, alpha){
  s <- sd(muestra)
  n <- length(muestra)
  zalpha <- qnorm(alpha, 0, 1, lower.tail = FALSE)
  for( i in 1:length(mues)){
    pies[i] <- 1-pnorm(zalpha+(mu0-mues[i])/(s/sqrt(n)))
  }
  pies
}

potencia <- PI(mues, mu, datos, alpha)

# Como espero que sea el grafico de esta funcion
plot(mues, potencia, type="l", lwd=2, col="red", main="Funcion de potencia (estimada)")

