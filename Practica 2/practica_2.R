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