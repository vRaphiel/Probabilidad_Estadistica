medidas <- c(176.4,191.5,186.9,181.1,195.7,
             188.1,187.4,185.1,176.9,191.2,
             193.8,187.0,179.0,173.0,184.4,
             199.6,190.4,206.8,193.0,177.1,
             201.0,192.5,176.6,180.1,186.4)

medidasCuadradas <- medidas
for( i in 1:25){
  medidasCuadradas[i] <- medidas[i]^2
}

prom <- mean(medidas)

desvio <- sd(medidas)

sum(medidasCuadradas) - sum(medidas)*185*2 + 25*185

24*desvio^2 + 25*((prom - 185)^2)


# teorica 26/11
x <- c(6.9,7.1, 7.2, 7.07, 7.15, 7.04, 7.18, 6.95)

n <- 8
sigma <- 0.15
alpha <- 0.05

zobs <- (mean(x) - 7)/(sigma/sqrt(n))
zobs

zcrit <- qnorm(1-alpha/2)
zcrit

abs(zobs) > zcrit
# Este valor no cae en la region de rechazo -> No rechazo H0

# C)
pnorm((7-7.1)/(0.15/sqrt(8)) + 1.96) - pnorm((7-7.1)/(0.15/sqrt(8)) - 1.96)

# d)
ns <- 9:100
  
probs <- pnorm((7-7.1)/(0.15/sqrt(ns)) + 1.96) - pnorm((7-7.1)/(0.15/sqrt(ns)) - 1.96)

plot(ns, probs)
abline

n <- 18
pnorm((7-7.1)/(0.15/sqrt(n)) + 1.96) - pnorm

# e)
# p valor  = 2 P(Z >= Zobs) = 2 * (1 - P(Z < Zobs))
n <- 8

2 * (1-pnorm(zobs))

