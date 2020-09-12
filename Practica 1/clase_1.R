# Ejercicio 1
sample(1:6, 2, replace=TRUE)

# L a suma es por lo menos 5
mean(replicate(1000, sum(sample(1:6, 2, replace=TRUE)))>=5)

mean(replicate(1000, sample(1:6, 2, replace=TRUE)[1])>=4)


mean(replicate(1000, tiro_uno_tiro_dos()));

tiro_uno_tiro_dos <- function(){
  casos_totales = 0;
  casos_efectivos = 0;
  
  for(dado1 in (1:6)){
    
    for(dado2 in (1:6)){
      casos_totales = casos_totales + 1;
      if(dado1 > dado2){
        casos_efectivos = casos_efectivos + 1;
      }
    }
  }
  
  return(casos_efectivos/casos_totales);
}

# ejercicio 9
# item i
bailan_parejas_iguales <- function(n){
  result = factorial(n-1)/factorial(n)
  return (result)
}

# item ii
una_mujer_su_esposo <- function(n){
  
}

# ejercicio 11
mean(replicate(1000, sum(sample(1:6, 2, replace =FALSE))==7))

suma_impar <- function(){
  
  casos_suma_siete = 0;
  casos_totales = 0;
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      if((dado1 + dado2)==7 && ((dado1 + dado2)%%2 == 1)){
        casos_suma_siete = casos_suma_siete + 1
      }
    }
  }
  probabilidad_condicionada((casos_suma_siete/casos_totales), 1/2)

}

suma_mayor_que_seis <- function(){
  casos_suma_siete = 0;
  casos_suma_mayor_seis = 0;
  casos_totales = 0;
  casos_siete_seis = 0;
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      
      #Suma es 7 y mayor a 6
      if((dado1 + dado2) == 7 && (dado1 + dado2) > 6){
        casos_siete_seis = casos_siete_seis + 1;
      }
      if((dado1 + dado2) == 7){
        casos_suma_siete = casos_suma_siete + 1;
      }
      if((dado1 + dado2) > 6){
        casos_suma_mayor_seis = casos_suma_mayor_seis + 1;
      }
    }
  }
  
  probabilidad_condicionada((casos_siete_seis/casos_totales), (casos_suma_mayor_seis/casos_totales))
}

segundo_dado_par <- function(){
  casos_totales = 0
  casos_siete_segundo_par = 0
  casos_segundo_par = 0

  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      if(dado2%%2 == 0){
        casos_segundo_par = casos_segundo_par + 1
        if(((dado1+dado2) == 7)){
          casos_siete_segundo_par = casos_siete_segundo_par + 1
        }
      }
    }
  }
  probabilidad_condicionada((casos_siete_segundo_par/casos_totales), (casos_segundo_par/casos_totales))
}

alguno_impar <- function(){
  casos_totales = 0
  casos_siete_impar = 0
  casos_un_impar = 0
    }
  }
  
  return(casos_efectivos/casos_totales);
}

# ejercicio 9
# item i
bailan_parejas_iguales <- function(n){
  result = factorial(n-1)/factorial(n)
  return (result)
}

# item ii
una_mujer_su_esposo <- function(n){
  
}

# ejercicio 11
mean(replicate(1000, sum(sample(1:6, 2, replace =FALSE))==7))

suma_impar <- function(){
  
  casos_suma_siete = 0;
  casos_totales = 0;
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      if((dado1 + dado2)==7 && ((dado1 + dado2)%%2 == 1)){
        casos_suma_siete = casos_suma_siete + 1
      }
    }
  }
  probabilidad_condicionada((casos_suma_siete/casos_totales), 1/2)

}

suma_mayor_que_seis <- function(){
  casos_suma_siete = 0;
  casos_suma_mayor_seis = 0;
  casos_totales = 0;
  casos_siete_seis = 0;
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      
      #Suma es 7 y mayor a 6
      if((dado1 + dado2) == 7 && (dado1 + dado2) > 6){
        casos_siete_seis = casos_siete_seis + 1;
      }
      if((dado1 + dado2) == 7){
        casos_suma_siete = casos_suma_siete + 1;
      }
      if((dado1 + dado2) > 6){
        casos_suma_mayor_seis = casos_suma_mayor_seis + 1;
      }
    }
  }
  
  probabilidad_condicionada((casos_siete_seis/casos_totales), (casos_suma_mayor_seis/casos_totales))
}

segundo_dado_par <- function(){
  casos_totales = 0
  casos_siete_segundo_par = 0
  casos_segundo_par = 0
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
<<<<<<< Updated upstream
      if((dado1%%2 == 1) || (dado2%%2 == 1)){
        casos_un_impar = casos_un_impar + 1
        if(((dado1+dado2) == 7)){
          casos_siete_impar = casos_siete_impar + 1
=======
      if(dado2%%2 == 0){
        casos_segundo_par = casos_segundo_par + 1
        if(((dado1+dado2) == 7)){
          casos_siete_segundo_par = casos_siete_segundo_par + 1
>>>>>>> Stashed changes
        }
      }
    }
  }
  probabilidad_condicionada((casos_siete_segundo_par/casos_totales), (casos_segundo_par/casos_totales))
}

alguno_impar <- function(){
  casos_totales = 0
  casos_siete_impar = 0
  casos_un_impar = 0
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      if((dado1%%2 == 1) || (dado2%%2 == 1)){
        casos_un_impar = casos_un_impar + 1
        if(((dado1+dado2) == 7)){
          casos_siete_impar = casos_siete_impar + 1
        }
      }
    }
  }
  probabilidad_condicionada((casos_siete_impar/casos_totales), (casos_un_impar/casos_totales))
}

dados_iguales <- function(){
  casos_totales = 0
  casos_siete_igual = 0
  dados_iguales = 0
  
  for(dado1 in 1:6){
    for(dado2 in 1:6){
      casos_totales = casos_totales + 1
      if(dado1==dado2){
        dados_iguales = dados_iguales + 1
        if(((dado1+dado2) == 7)){
          casos_siete_igual = casos_siete_igual + 1
        }
      }
    }
  }
  probabilidad_condicionada((casos_siete_igual/casos_totales), (dados_iguales/casos_totales))
}


probabilidad_condicionada <- function(interseccion, elemento){
  return (interseccion/elemento)
}
