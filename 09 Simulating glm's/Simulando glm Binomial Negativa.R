# Comprobando que k es el parametro de precision ---------------------

n <- 10000
mu <- 5
k <- 0.5
y <- rnbinom(n=n, mu=mu, size=k)

mean(y)         # Media muestral
var(y)          # Varianza muestral
mu + mu^2 / k   # Varianza teorica

# -------------------------------------------------------------------------
# En este ejemplo se simulan datos de un glm y se ---------------------
# estiman los parametros del modelo
# -------------------------------------------------------------------------

# Modelo: Y~BN(mu, k), mu es la media y k es el param precision
# con log(mu) = b0 + b1 * x
# donde x ~ U(0, 1)
# con b0=-1 y b1=2

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, k) {
  x <- runif(n=n, min=0, max=1)
  mu <- exp(b0 + b1 * x)
  y <- rnbinom(n=n, mu=mu, size=k)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 80
datos <- gen_dat(n=n, b0=-1, b1=2, k=3)
head(datos)
barplot(table(datos$y))

# Exploremos los datos
library(ggplot2)

ggplot(datos, aes(x=x, y=y)) + 
  geom_point()

# Ajustado el modelo
library(MASS)
mod <- glm.nb(y ~ x, data=datos)

coef(mod)  # Betas estimados    
mod$theta  # This is the value of k (called theta)

# summary basico
summary(mod)

# glm.convert function convertes to the style of output from glm
mod <- glm.convert(mod)
summary(mod)
printCoefmat(coef(summary(mod, dispersion=1)))

# Note that we have to specify explicitly that the dispersion parameter 
# is φ = 1, because after using glm.convert(), r does not know 
# automatically that the resulting glm family should have 
# dispersion equal to one.

# Analisis de residuales
rp <- boot::glm.diag(mod)$rp  # Pearson residuals
rd <- boot::glm.diag(mod)$rd  # Deviance residuals
qr <- statmod::qresid(mod)    # Quantile residuals

library(car) # Para construir un qqPlot especial
qqPlot(x=rp, dist="norm", mean=0, sd=1)
qqPlot(x=rd, dist="norm", mean=0, sd=1)
qqPlot(x=qr, dist="norm", mean=0, sd=1)

# Envelope
library(glmtoolbox)
envelope(mod)

# -------------------------------------------------------------------------
# Ajustando un modelo Poisson que es incorrecto, vamos a ver 
# si este modelo "bad" se puede identificar como pesimo.
# -------------------------------------------------------------------------

bad_mod <- glm(y ~ x, data=datos, family=poisson)
summary(bad_mod)
coef(bad_mod)
fit.model <- bad_mod
source("https://www.ime.usp.br/~giapaula/envel_pois")

# Usando los Quantile residuals de Dunn & Smith (1996)
qr <- statmod::qresiduals(bad_mod)
car::qqPlot(qr, distribution="norm", pch=21, col="tomato")

# Comparando con el AIC
AIC(mod, bad_mod, k=2)

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

