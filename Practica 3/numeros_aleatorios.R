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
