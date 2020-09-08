# Ejercicio 1
sample(1:6, 2, replace=TRUE)

# L a suma es por lo menos 5
mean(replicate(1000, sum(sample(1:6, 2, replace=TRUE)))>=5)

mean(replicate(1000, sample(1:6, 2, replace=TRUE)[1])>=4)

mean(replicate(1000, tiro_uno_tiro_dos(sample(1:6, 2, replace=TRUE))))

tiro_uno_tiro_dos<-function(x){
  result <- TRUE
  for(i in x){
    if(x[1]<x[2]){
      result <- FALSE
    }
  }
  return(result)
}


# Hay un grupo de 30 personas que juegan al amigo invisible
# Se colocan los nombres en papel, se ponen en una bolsa, se saca un papel y este tiene el nombre






