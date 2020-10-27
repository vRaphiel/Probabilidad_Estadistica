# Continuous Distributions

# Normal X -> N(0, 1)
uma = 0.4
sigma_quad = 1.3
normal <- function(x,u,s){if(x > 0){pnorm(x, mean = uma, sd = sqrt(s))}else{1 - pnorm(x, mean = uma, sd = sqrt(s))}}

# proba de un punto
x = 1
uma = 0
sigma_quad = 1
normal(x, uma, sigma_quad)

# Exponential X -> E(lambda)
x = 3
lambda = 5
exp <- function(x,l){pexp(x,l)}

# Proba de X
exp(x,lambda)

# Uniform X -> U[a,b]
x = 7
a = 0
b = 10
uniform <- function(x, a, b){punif(x, a, b)}

#proba de un punto
uniform(x,a,b)