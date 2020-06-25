# Ejemplo sobre cangrejas del libro de Foundations of linear and GLM
# de Agresti

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
datos <- read.table(url, header=T)
head(datos)

# Un poco de estadistica descriptiva
barplot(table(datos$y), las=1, col='lightgreen',
        xlab='Numero de satelites', ylab='Frecuencia')

# Explorando la relacion entre la media y la varianza
mean(datos$y)
var(datos$y)


# Creando los modelos -----------------------------------------------------
library(gamlss)

mod_pois <- gamlss(y ~ 1, data=datos, family=PO)
summary(mod_pois)
logLik(mod_pois)

mod_negb <- gamlss(y ~ 1, data=datos, family=NBII)
summary(mod_negb)
logLik(mod_negb)

mod_zip <- gamlss(y ~ 1, data=datos, family=ZIP)
summary(mod_zip)
logLik(mod_zip)

mod_zinb <- gamlss(y ~ 1, data=datos, family=ZINBI)
summary(mod_zinb)
logLik(mod_zinb)

# Comparando con el AIC ---------------------------------------------------
AIC(mod_pois, mod_negb, mod_zip, mod_zinb, k=2)

