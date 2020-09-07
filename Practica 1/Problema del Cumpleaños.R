# Para cada m calcula la probabilidad de que al menos dos alumnos cumplan el mismo dia


calcular_proba_mismo_cumple <- function(m){
  1 - prod(365:(365 -m + 1)) / 365^m
}

calcular_proba_mismo_cumple(20)

calcular_proba_mismo_cumple(30)

emes <-seq(20,100)

# Aplicar al vector, la funcion
proba_mismo_cumple <- sapply(emes, calcular_proba_mismo_cumple)

proba_mismo_cumple

plot(emes, proba_mismo_cumple)

# type="l" Formato del grafico de circulos a linea
plot(emes, proba_mismo_cumple, type = "l")
