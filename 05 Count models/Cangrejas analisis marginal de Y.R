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

mod_pois <- gamlss(y ~ 1, data=datos, family=PO)
summary(mod_pois)
logLik(mod_pois)

mod_negb <- gamlss(y ~ 1, data=datos, family=NBI)
summary(mod_negb)
logLik(mod_negb)

mod_zip <- gamlss(y ~ 1, data=datos, family=ZIP)
summary(mod_zip)
logLik(mod_zip)

mod_zinb <- gamlss(y ~ 1, data=datos, family=ZINBI)
summary(mod_zinb)
logLik(mod_zinb)

# Comparando con nuestros ojos --------------------------------------------
y <- datos$y
histDist(y=y, family=PO)
histDist(y=y, family=NBI)
histDist(y=y, family=ZIP)
histDist(y=y, family=ZINBI)

# Comparando con el AIC = - 2 * logLik + k * npar -------------------------
AIC(mod_pois, mod_negb, mod_zip, mod_zinb, k=2)

# Vamos a explorar las distribuciones de forma automatica
# sin nosotros mencionar las distribuciones

prueba <- fitDist(y=y, type="counts")
prueba$fits  # Casos exitosos
prueba$fails # Casos fallidos


