# -------------------------------------------------------------------------
# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo
# -------------------------------------------------------------------------

# Formas de la densidad ---------------------------------------------------

library(statmod) # Para acceder a la distribucion inv gaussiana

curve(dinvgauss(x, mean=2, dispersion=1), to=3, las=1,
      ylim=c(0, 4), ylab="Densidad")
curve(dinvgauss(x, mean=2, dispersion=2), add=TRUE, col='blue')
curve(dinvgauss(x, mean=2, dispersion=4), add=TRUE, col='red')
curve(dinvgauss(x, mean=2, dispersion=8), add=TRUE, col='purple')
legend("topright", legend=c("mu=2, disp=1", "mu=2, disp=2", 
                            "mu=2, disp=4", "mu=2, disp=8"),
       col=c("black", "blue", "red", "purple"), lty=1, bty="n")

media <- 2
disper <- c(1, 2, 4, 8)
cbind(media=2, disper=disper, varianza=disper * media^3)
# Parece extrano que la distribucion morada tiene mayor varianza

# Vamos a explorar lo que sucede entre 12 y 33
curve(dinvgauss(x, mean=2, dispersion=1), from=12, to=33, las=1,
      ylab="Densidad", main="En la mitad")
curve(dinvgauss(x, mean=2, dispersion=2), add=TRUE, col='blue')
curve(dinvgauss(x, mean=2, dispersion=4), add=TRUE, col='red')
curve(dinvgauss(x, mean=2, dispersion=8), add=TRUE, col='purple')
legend("topright", legend=c("mu=2, disp=1", "mu=2, disp=2", 
                            "mu=2, disp=4", "mu=2, disp=8"),
       col=c("black", "blue", "red", "purple"), lty=1, bty="n")

# Lo que sucede lejos del cero
curve(dinvgauss(x, mean=2, dispersion=1), from=27, to=50, las=1,
      ylim=c(0, 10e-4), ylab="Densidad", main="Al final")
curve(dinvgauss(x, mean=2, dispersion=2), add=TRUE, col='blue')
curve(dinvgauss(x, mean=2, dispersion=4), add=TRUE, col='red')
curve(dinvgauss(x, mean=2, dispersion=8), add=TRUE, col='purple')
legend("topright", legend=c("mu=2, disp=1", "mu=2, disp=2", 
                            "mu=2, disp=4", "mu=2, disp=8"),
       col=c("black", "blue", "red", "purple"), lty=1, bty="n")


# Estimacion sin covariables ----------------------------------------------

# -------------------------------------------------------------------------
# Modelo: Y ~ IG(mu, phi)
# Parametro de localizacion:  mu = 2
# Parametro de   dispersion: phi = 2
# -------------------------------------------------------------------------

library(statmod) # Para acceder a la distribucion inv gaussiana

mu <- 2
phi <- 2
y <- rinvgauss(n=30, mean=mu, dispersion=phi)

# Densidad empirica
plot(density(y), lwd=2, col='tomato', main='Density')
rug(y)

# Ajustando el modelo con link=log
mod1 <- glm(y ~ 1, family=inverse.gaussian(link="log"))
summary(mod1)

exp(coef(mod1))            # estimacion de mu
summary(mod1)$dispersion   # estimacion de phi

# Ajustando el modelo con link=log
mod2 <- glm(y ~ 1, family=inverse.gaussian(link="inverse"))
summary(mod2)

1 / coef(mod2)             # estimacion de mu
summary(mod2)$dispersion   # estimacion de phi

# Ajustando el modelo con link=1/mu^2
mod3 <- glm(y ~ 1, family=inverse.gaussian(link="1/mu^2"))
summary(mod3)

1 / sqrt(coef(mod3))       # estimacion de mu
summary(mod3)$dispersion   # estimacion de phi


# Estimacion con covariables ----------------------------------------------

# -------------------------------------------------------------------------
# Modelo: Y~IG(mu, phi) con log(mu) = b0 + b1 * x
# con b0 = -1 y b1 = 1
# phi = 2
# la covariable x ~ U(0, 1)
# -------------------------------------------------------------------------

library(statmod) # Para acceder a la distribucion inv gaussiana

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n, min=0, max=1)
  mu <- exp(b0 + b1 * x)
  y <- rinvgauss(n=n, mean=mu, disp=phi)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 300
datos <- gen_dat(n=n, b0=-1, b1=1, phi=2)
head(datos)

# Exploremos los datos
library(ggplot2)

ggplot(datos, aes(x=x, y=y)) + 
  geom_point()


# Ajustado el modelo
mod <- glm(y ~ x, data=datos, family=inverse.gaussian(link='log'))
summary(mod)

coef(mod)                 # estimacion de los betas
summary(mod)$dispersion   # estimacion de phi

# Analisis de residuales
rp <- boot::glm.diag(mod)$rp  # Pearson residuals
rd <- boot::glm.diag(mod)$rd  # Deviance residuals
qr <- statmod::qresid(mod)    # Quantile residuals

library(car) # Para construir un qqPlot especial
qqPlot(x=rp, dist="norm", mean=0, sd=1)
qqPlot(x=rd, dist="norm", mean=0, sd=1)
qqPlot(x=qr, dist="norm", mean=0, sd=1)

# Envelope
library(SuppDists)
envelope(mod)

# Envelope de Alvarenga
fit.model <- mod
attach(datos)
source("https://www.ime.usp.br/~giapaula/envel_ninv")


