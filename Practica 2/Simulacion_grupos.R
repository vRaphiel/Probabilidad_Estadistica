#Grafico de prob de hacer N+1 analisis en funcion de N
N <- 1:50
probs <- 1-0.9^N
plot(N, probs)

#Graficar Esperanza de X en funcion de N
esp <- 0.9^N+(N+1)*(1-0.9^N)
plot(N, esp);
abline(0,1)

# Grafico de esparanza de X/N en funcion de N
plot(N, esp/N)
abline(h=1)


# Ejercicio con urnas

experimento <- function(){
  urna1 <- c(rep("a",5), rep("r",6))
  urna2 <- c(rep("a", 10), rep("r",1))
  extraccion1 <- sample(urna1, 1)
  # c <- concat arrays
  urna2 <- c(extraccion1, urna2)
  extraccion2 <- sample(urna2, 1)
  # concateno bolita1 y bolita2
  bolitas <- c(extraccion1, extraccion2)
  sum(bolitas=="r")
}
# Para tercer experimento, agrego urna3 siguiendo el mismo objetivo

experimento2 <- function(){
  urna1 <- c(rep("a",5), rep("r",6))
  urna2 <- c(rep("a", 10), rep("r",1))
  urna3 <- c(rep("a", 3), rep("r",5))
  extraccion1 <- sample(urna1, 1)
  # c <- concat arrays
  urna2 <- c(extraccion1, urna2)
  extraccion2 <- sample(urna2, 1)
  urna3 <- c(extraccion2, urna3)
  extraccion3 <- sample(urna3, 1)
  # concateno bolita1 y bolita2
  bolitas <- c(extraccion1, extraccion2, extraccion3)
  sum(bolitas=="r")
}

experimento()
experimento2()

muchos_experimentos <- replicate(10000, experimento())
muchos_experimentos2 <- replicate(10000, experimento2())
muchos_experimentos

mean(muchos_experimentos)
mean(muchos_experimentos==0)
5/12
mean(muchos_experimentos==1)
65/132
mean(muchos_experimentos==2)
1/11

esperanza <- function(){
  0*mean(muchos_experimentos==0)+
  1*mean(muchos_experimentos==1)+
  2*mean(muchos_experimentos==2)
}

esperanza()

0*mean(muchos_experimentos2==0)+
1*mean(muchos_experimentos2==1)+
2*mean(muchos_experimentos2==2)+
3*mean(muchos_experimentos2==3)