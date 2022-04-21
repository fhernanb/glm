# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo

# Modelo: Y ~ Binomial(m=variable, pi) con logit(pi) = b0 + b1 * x
# con b0=-1 y b1=1

# where y are the observed successes in m trials.

# Funcion para generar los datos
gen_dat <- function(n, b0, b1) {
  x <- runif(n=n, min=0, max=1)
  eta <- b0 + b1 * x
  pi <- exp(eta) / (1 + exp(eta))
  m <- sample(x=5:50, size=n, replace=TRUE)
  y <- rbinom(n=n, size=m, prob=pi)
  data.frame(y=y, m=m, x=x)
}

# Generando los datos
n <- 50
datos <- gen_dat(n=n, b0=-1, b1=1)
head(datos)

# Curioseando los valores de y
hist(datos$y)

# Ajustado el modelo
mod <- glm(y/m ~ x, weights=m, data=datos, family=binomial)
summary(mod)
coef(mod)     # Los valores estimados son cercanos a los verdaderos

# Analisis de residuales
rp <- boot::glm.diag(mod)$rp  # Pearson residuals
rd <- boot::glm.diag(mod)$rd  # Deviance residuals
qr <- statmod::qresid(mod)    # Quantile residuals

library(car) # Para construir un qqPlot especial
qqPlot(x=rp, dist="norm", mean=0, sd=1)
qqPlot(x=rd, dist="norm", mean=0, sd=1)
qqPlot(x=qr, dist="norm", mean=0, sd=1)

# Envelope
source("08 Envelopes/envelope.R")
envelope(mod)

