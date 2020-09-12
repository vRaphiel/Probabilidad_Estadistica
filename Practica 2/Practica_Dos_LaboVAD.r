# Dos equivalencias bien piola
#choose -> combinatorio
choose(10,3)*(1/6)^3 * (5/6)^7

#dbinom -> distribucion binomial
dbinom(3, size = 10, prob=1/6)

#prob de tener entre 3 y 8 unos
# Si una funcionse hace asi con (n:m) aplica esa funcion uno a uno
dbinom(3:8, size = 10, prob=1/6)

# Suma de todo (probabilidad de obtener entre 3 y 8 unos)
sum(dbinom(3:8, size = 10, prob=1/6))

dbinom(8, 10, 1/6) - dbinom(2,10, 1/6)



# Clase practica 2
# Variables aleatorias discretas
# Bolitas y proba de tener azules en 8 totales sin reposiciones
p_X <- function(x)
    {
    if (x %in% 0:3)
        ans <-(choose(5, 3-x)*choose(3,x))/choose(8,3)
    else
        ans <- 0
    ans
    }

R_X = 0:3

plot(R_X, sapply(R_X, p_X))

# Funcion de distribucion
F_X <- function(x){
    acum = 0
    if (x>= 0 & x<=3) 
        acum <- sum(sapply(0:x, p_X))
    if (x>3)
        acum <-1
    acum
}

F_X(2.4)

x <- seq(-3,6, length = 1000)
acumulada <- sapply(x, F_X)

# Graphic function
plot(x, acumulada, ylim = c(-0.05,1.05), cex=0.1)

for (x in R_X)
    {
    text(x,F_X(x), "[")
    text(x,F_X(x-0.01), ")")
}

sample(R_X, 1, prob = sapply(R_X, p_X))

set.seed(1234)

n_rep = seq(10,100000, length = 100)

simulacion <- rep(NA, 100)


for (i in 1:100)
{
    simulacion[i] <- mean(replicate(n_rep[i], sample(R_X, 1, prob = sapply(R_X, p_X))))
}


plot(n_rep, simulacion)
abline(h=63/56,col="red")


