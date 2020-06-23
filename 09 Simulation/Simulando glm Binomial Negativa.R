# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo

# Modelo: Y~BN(mu, k) con log(mu) = b0 + b1 * x
# donde x ~ U(0, 1)
# con b0=-1 y b1=2

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, k) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- rnbinom(n=n, mu=mu, size=k)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 50
datos <- gen_dat(n=n, b0=-1, b1=2, k=3)
head(datos)

# Ajustado el modelo
library(MASS)
mod <- glm.nb(y ~ x, data=datos)
summary(mod)
coef(mod)     
mod$theta  # Los valores estimados son cercanos a los verdaderos

# Envelopes
fit.model <- mod
source("https://www.ime.usp.br/~giapaula/envel_nbin")

# Ajustando un modelo Poisson
bad <- glm(y ~ x, data=datos, family=poisson)
summary(bad)
coef(bad)
fit.model <- bad
source("https://www.ime.usp.br/~giapaula/envel_pois")

# Comparando con el AIC
AIC(mod, bad, k=2)

# Reto --------------------------------------------------------------------

# Explorar el efecto del tamano de muestra n en las estimaciones.
# Para cada n usted debe simular 100 conjuntos de datos, estimar
# los parametros y almacenarlos.

# Aproveche el codigo de abajo y cambie xxx por la expresion correcta.

n <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
nrep <- 100
results <- matrix(NA, ncol=xxx, nrow=nrep*length(n))
colnames(results) <- c('xxx', 'b1', xxx)
dim(results) # para ver la dimension de la matriz donde se almacenaran

for (i in n) {
  for (j in 1:nrep) {
    xxx <- gen_dat(n=xxx, b0=-1, b1=1, k=xxx)
    mod <- xxx(y ~ x, data=datos)
    results[xxx] <- coef(xxx)
  }
}

# Crear un diagrama de dispersion del promedio de theta_i versus n.
# Crear un diagrama de dispersion del ECM de theta_i versus n.

