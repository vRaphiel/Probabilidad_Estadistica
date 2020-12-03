riego <- c(27,41,22,27,23,35,30,25,27,28,22)

mean(riego)
sd(riego)

1.64 + ((0.1*sqrt(200))/sqrt(0.1275)) - ((0.15*sqrt(200))/sqrt(0.1275)) = 0.6700

1.64 + (0.1/sqrt(9/20000)) - (0.15/sqrt(51/80000))



botellas <- c(201, 212, 207, 199, 197, 207, 211, 195, 197, 204)
botellasC2 <- c()
for(i in 1:10){
  botellasC2[i] = (botellas[i] - mean(botellas))^2
}

sqrt(sum(botellas)/10)

sum(botellas)/sqrt(10)