# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo

# Modelo: Y~BN(mu, k) con mu y k fijos
# Generando los datos
mu <- 5
k <- 3
n <- 10000
y <- rnbinom(n=n, mu=mu, size=k)

# Explorando los datos
barplot(table(y), las=1, xlab='y', ylab='Frecuencia')
mean(y)
var(y)

# Ajustado el modelo con glm.nb de MASS
library(MASS)
mod <- glm.nb(y ~ 1)
summary(mod)

# Las estimaciones
exp(coef(mod)) # para mu
mod$theta      # para k
1 / mod$theta  # para phi

# Ajustando el modelo con gamlss
library(gamlss)

fit <- gamlss(y ~ 1, family=NBI)
summary(fit)

# Las estimaciones
exp(coef(object=fit, what="mu"))        # para mu
1/exp(coef(object=fit, what="sigma"))   # para k

# Reto --------------------------------------------------------------------

# Explorar el efecto del tamano de muestra n en las estimaciones.
# Para cada n usted debe simular 100 conjuntos de datos, estimar
# los parametros y almacenarlos.

# Aproveche el codigo de abajo y cambie xxx por la expresion correcta.

n <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
nrep <- 100
results <- matrix(NA, ncol=2, nrow=nrep*length(n))
colnames(results) <- c('xxx', 'k')
dim(results) # para ver la dimension de la matriz donde se almacenaran

for (i in n) {
  for (j in 1:nrep) {
    y <- rnbinom(n=xxx, mu=mu, size=k)
    mod <- xxx(y ~ 1)
    results[xxx] <- xxx
  }
}

# Crear un diagrama de dispersion del promedio de mu versus n.
# Crear un diagrama de dispersion del ECM de k versus n.
