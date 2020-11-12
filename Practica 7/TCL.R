# TCL: Binomial -> Normal
n = 5
p = 0.3
x <- 0:n
y <- seq(from=0, to=n, length.out = 100)
ppbi <- dbinom(x,size = n, prob = p)
dfn <- dnorm(y, mean = n*p, sd=sqrt(n*p*(1-p)))
plot(x, ppbi, col="blue")
lines(y, dfn, col="red")

#Mientras mas crece el n entonces eso es mas preciso

# Ejercicio 1
103000-260*sum(rbinom(10000, size = 1, prob=1/26))