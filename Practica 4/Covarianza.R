install.packages("HSAUR")
install.packages("ggplot2")
library(HSAUR)
data("water", package = "HSAUR")
mort<-data.frame(water)
head(mort)

library(ggplot2)
ggplot()+
  geom_point(data=mort,aes(x=hardness, y=mortality, colour=location))

cov(mort$hardness, mort$mortality)
cor(mort$hardness, mort$mortality)


linea<-lm(hardness~ mortality, data = mort)
linea

ggplot(data=mort,
       aes(x=hardness,y=mortality),color="red",size=3)+
  geom_point(color="red", size=3)+
  geom_smooth(method = "lm", se=FALSE)


# 100 puntos desde -1 a 1
X=seq(0,1,length=100)
# ecuación de una semi-circunferencia
Y=sqrt(1-X^2)
Y=Y
xy=data.frame(X,Y)
ggplot(data=xy, aes(x=X,y=Y))+
  geom_point(color="darkorange", size=3)

cor(X,Y)
