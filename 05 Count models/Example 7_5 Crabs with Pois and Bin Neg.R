# -------------------------------------------------------------------------
# Example 7.5 from Agresti (2015)
# -------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/fhernanb/glm/refs/heads/master/data_Agresti_2015/Crabs.dat"
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Histogram for the response variable y
barplot(table(datos$y), col="tomato")

# Vamos a convertir las variables cualitativas en factores
library(dplyr)
datos <- datos %>% mutate(color = as.factor(color),
                          spine = as.factor(spine))

datos |> summary()

# Models with Poisson distribution ----------------------------------------

# Modelo base
mod_poi_1 <- glm(y ~ 1, family=poisson(link="log"), data=datos)

summary(mod_poi_1)

# Aplicando seleccion de variables
library(MASS)
horizonte <- formula(y ~ weight + width + color + spine + 
                       weight*color + weight*spine +
                       width*color + weight*spine)
mod_poi_2 <- stepAIC(object=mod_poi_1, scope=horizonte, 
                     direction="both", k=2)
summary(mod_poi_2)

# Que tan bien acompanan las estimaciones los valores reales Y?
mu_hat <- predict(mod_poi_2, type="response")
cor(datos$y, mu_hat)
plot(x=datos$y, y=mu_hat, las=1,
     xlab="Num cangrejos pegados (Y)", ylab="Media estimada (mu^)")
abline(a=0, b=1, lty="dashed", col="blue3")
text(x=10, y=7, expression(rho(Y, hat(Y))==0.39))

# Envelopes
library(glmtoolbox)
envelope(mod_poi_2)

# Models with Negative Binomial distribution ---------------------------------

# Modelo base
library(MASS)
mod_bin_1 <- glm.nb(y ~ 1, data=datos)

summary(mod_bin_1)

# Aplicando seleccion de variables
library(MASS)
horizonte <- formula(y ~ weight + width + color + spine + 
                       weight*color + weight*spine +
                       width*color + weight*spine)
mod_bin_2 <- stepAIC(object=mod_bin_1, scope=horizonte, direction="both", k=2)
summary(mod_bin_2)

# El parametro k (dispersion) se obtiene del summary y es 0.9311
k <- 0.9311
k

# Que tan bien acompanan las estimaciones los valores reales Y?
mu_hat <- predict(mod_bin_2, type="response")
cor(datos$y, mu_hat)
plot(x=datos$y, y=mu_hat, las=1,
     xlab="Precio obs", ylab="Precio est")
abline(a=0, b=1, lty="dashed", col="blue3")
text(x=10, y=7, expression(rho(Y, hat(Y))==0.29))

