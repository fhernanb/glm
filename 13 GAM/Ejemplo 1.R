# Ejemplo parecido al mostrado en el video #1 de Noam Roos

n <- 100
x <- runif(n=n, min=0, max=5)
ruido <- rnorm(n=n, mean=0, sd=0.2)
y <- 10 + sin(x) * cos(x) + ruido     # Crazy

datos <- data.frame(y, x)

# Diagrama de dispersion
library(ggplot2)

ggplot(datos, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(colour="purple")

# Ajustando GAM

library(mgcv)

mod <- gam(y ~ s(x, bs="tp", k=5), data=datos,
           family = gaussian(link = "identity"))

summary(mod)

coef(mod)

