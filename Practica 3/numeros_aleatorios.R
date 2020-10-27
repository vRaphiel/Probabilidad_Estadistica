#Variable aleatoria exponencial en R
#Run if -> 10 aleatorios entre 0 y 1
u <- runif(10)
lambda <- 2
x <- -log(1-u)/lambda
x

#Genero la esperanza de la exponencial
u <- runif(1000)
lambda <- 2
x <- -log(1-u)/lambda
mean(x)


# Metodo para simular la v.a. exponencial
x <- rexp(10,2)
x
# Simulo la esperanza
x <- rexp(1000,2)
mean(x)


# Variable aleatoria discreta
u <- runif(10)
x <- 1*(u>=0 & u<0.5) + 2*(u >= 0.5 & u < 0.8) + 5*(u >=0.8 & u<1)
x

#Verifico
u <- runif(10000)
x <- 1*(u>=0 & u<0.5) + 2*(u >= 0.5 & u < 0.8) + 5*(u >=0.8 & u<1)
c(mean(x==1), mean(x==2),mean(x==5))


# Monto de transferencias
# a
# Funcion densidad
f_X <- function(x){
  x/18 * (x>0 & x < 6)
}
xs <- seq(-1,7,0.001)
plot(xs, f_X(xs), cex=0.1)

#Calculo y grafico la acumulada
F_X <- function(x){
  (x^2)/36 * (x>=0 & x<6) + (x>= 6)
}
xs<-seq(-1, 7, 0.1)
plot(xs, F_X(xs), type = "l")

# Ahora busco con U
u <- runif(100)
x <- 6*sqrt(u)
x

#Prom
mean(x)

#COmparo con el valor verdadero
faux <- function(x) x*f_X(x)
integrate(faux, 0, 6)


