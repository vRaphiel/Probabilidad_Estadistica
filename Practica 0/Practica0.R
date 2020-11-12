# Ejercicio 8
library('PASWR2')

t <- TITANIC3

survivor_first <- mean(t$survived[t$pclass == "1st"])
survivor_second <- mean(t$survived[t$pclass == "2nd"])
survivor_third <- mean(t$survived[t$pclass == "3rd"])

s_m_f <- mean(t$survived[t$pclass == "1st" && t$sex == "male"])
s_f_f <- mean(t$survived[t$pclass == "1st" && t$sex == "female"])