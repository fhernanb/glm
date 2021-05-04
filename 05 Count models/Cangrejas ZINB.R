# -------------------------------------------------------------------------
# Ejemplo sobre cangrejas del libro de Foundations of linear and GLM
# de Agresti
# -------------------------------------------------------------------------

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
datos <- read.table(url, header=T)
head(datos)

# Un poco de estadistica descriptiva
library(ggplot2)
ggplot(datos, aes(x=y)) +
  geom_bar(fill="lightgreen") +
  labs(title="Diagrama de barras para el numero de satelites") +
  xlab("Y") + ylab("Frecuencia absoluta")

# Explorando la relacion entre la media y la varianza
mean(datos$y)
var(datos$y)

# Creando los modelos -----------------------------------------------------
library(gamlss)

mod_zinb <- gamlss(formula = y ~ weight, 
                   sigma.fo= ~ 1,
                   nu.fo= ~ weight + color,
                   data=datos, family=ZINBI)
summary(mod_zinb)

# Extrayendo los coeficientes
coef(mod_zinb, what="mu")
coef(mod_zinb, what="sigma")
coef(mod_zinb, what="nu")

# Residuales
plot(mod_zinb)
wp(mod_zinb)
