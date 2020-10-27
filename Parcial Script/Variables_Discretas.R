# Discrete Distributions

# Binomial X -> Bi(n,p) / P(x) = binom(n,p)
x = 1
n = 10
p = 0.23
binom <- function(x,n,p){dbinom(x, size = n, prob = p)}
# proba de un punto
binom(x,n,p)
# proba acumulada P(X<=x)
x = 0:10
n = 100
p = 0.23
sum(binom(x,n,p))

# Geometrica X -> Ge(p)
x = 3
p = 0.23
geom <- function(x,p){dgeom(x,p)}
# proba puntual
geom(x,p)
# proba acumulada P(X<=x)
x = 0:10
p = 0.23
sum(geom(x,p))

# Poisson X -> P(lambda)
lambda = 4
x = 8
poisson <- function(x, l){ppois(x, lambda = l)}
# proba puntual
poisson(x, lambda)

# Negative Binomial X -> BN()
x = 1
n = 10
p = 0.23
binN <- function(x,n,p){dnbinom(x,n,p)}

# HyperGeometric X -> HP()
x = 0:2
m = 15
n = 20
k = 10
hyper <- function(x,m,n,k){dhyper(x, m, n, k)}



