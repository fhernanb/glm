# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo

# Formas de la densidad ---------------------------------------------------
library(statmod)
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

# Lo que sucede lejos del cero
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

# Modelo: Y~IG(mu, k)
# Parametro de localizacion: mu=2
# Parametro de   dispersion: dispersion=2

library(statmod)

mu <- 2
disp <- 2
y <- rinvgauss(n=3000, mean=mu, dispersion=disp)

# Densidad empirica
plot(density(y), lwd=2, col='tomato', main='Density')
rug(y)

# Ajustando el modelo
mod <- glm(y ~ 1, family=inverse.gaussian(link="log"))
summary(mod)

exp(coef(mod))            # estimacion de mu
summary(mod)$dispersion   # estimacion de disp

# Modelo: Y~IG(mu, disp) con log(mu) = b0 + b1 * x
# con b0=-1 y b1=1
# disp = 2

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, disp) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- rinvgauss(n=n, mean=mu, disp=disp)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 3000
datos <- gen_dat(n=n, b0=-1, b1=1, disp=2)
head(datos)

# Ajustado el modelo
mod <- glm(y ~ x, data=datos, family=inverse.gaussian(link='log'))
summary(mod)

coef(mod) # para obtener los betas
summary(mod)$dispersion   # estimacion de disp

