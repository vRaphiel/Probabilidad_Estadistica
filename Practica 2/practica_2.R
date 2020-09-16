x <- 0:15
y <- dbinom(x, size=4, prob=.25)
plot(x, y, type = "h")

litterpmf <- c(0.36, 0.48,0.14,0.008)
custom <- sample(0:3, 30, replace = TRUE, prob=litterpmf)

# probabilidad_puntual = c((11/15)^3, (4/15))
# EJERCICIO 2

ejer_tres <- function(vec_x, vec_p, num)

vecx <- c(1,3,6,12)
vecp <- c(0.3, 0.1, 0.2 ,0.4)

prob <- sample((vecx), 100, replace = TRUE, prob=vecp)


X <- replicate(10000, {
  coin_toss <- sample(c("H", "T"), 3, replace = TRUE)
  sum(coin_toss == "H")
})

mean(X >= 1)

# Ejercicios simulados
# Ejercicio 1
p_X <- function(x)
{
  if (x %in% 0:3)
    ans <-(choose(11, 3-x)*choose(4,x))/choose(15,3)
  else
    ans <- 0
  ans
}

R_X = 0:3

# Item b)
p_X(2)

plot(R_X, sapply(R_X, p_X))

F_X <- function(x){
  acum = 0
  if (x>= 0 & x<=3) 
    acum <- sum(sapply(0:x, p_X))
  if (x>3)
    acum <-1
  acum
}

F_X(1)


# Ejercicio 3
p_X_v <- function(t,x, y){
  position <- which(vector1 < 12)
  sum_of_values = sum(vector2[position])
  sum_of_values
}

vector1 <- c(1, 3, 6, 12, 13)
vector2 <- c(0,0.3,0.4,0.6,1)
p_X_v(4, vector1, vector2)


# Ejercicio 5
experanzap1 <- function(x){
  x*p_X(x)
}

sum(sapply(R_X, experanzap1))

# Funcion de distribucion
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