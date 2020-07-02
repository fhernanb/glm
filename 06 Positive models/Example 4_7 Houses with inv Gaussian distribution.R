# Example 4.7
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

pairs(datos[c('price','size','taxes')])

# Modelo inv Gaussian -----------------------------------------------------
with(datos, plot(density(price)))

mod1 <- glm(price ~ size + new + beds + size*new + size*beds,
           data=datos, family=inverse.gaussian) # upps

mod1 <- glm(price ~ size + new + beds + size*new + size*beds,
            data=datos, family=inverse.gaussian(link="log"))
summary(mod1)

# Aplicando seleccion de variables
library(MASS)
mod2 <- stepAIC(object=mod1, direction="backward", k=2)
summary(mod2)

coef(mod2)                 # estimacion de los betas
summary(mod2)$dispersion   # estimacion de disp

# Valor esperado del Precio
esp_precio <- function(size) exp(3.8043629667+0.0007034536*size)

# Varianza del Precio
var_precio <- function(size) 0.0009083137 * esp_precio(size)^3

# Dibujando Valor esperado y varianza del Precio
par(mfrow=c(1, 2))
curve(esp_precio(x), from=500, to=4000, lwd=3,
      xlab="Size", ylab="E(Precio)", las=1)

curve(var_precio(x), from=500, to=4000, lwd=3,
      xlab="Size", ylab="Var(Precio)", las=1)

# Que tan bien acompanan las estimaciones los valores reales Y?
precio_hat <- esp_precio(size=datos$size)
cor(datos$price, precio_hat)
plot(x=datos$price, y=precio_hat, las=1,
     xlab="Precio obs", ylab="Precio est")
abline(a=0, b=1, lty="dashed", col="blue3")
text(x=100, y=600, expression(rho(Y, hat(Y))==0.80))

# Hagamos una animacion curiosita para mostrar como cambia
# la distribucion del Precio en funcion de Size

Sizes <- seq(from=500, to=4000, length.out=20)
Sizes <- round(Sizes, digits=0)

library(statmod)
for (i in Sizes) {
  curve(dinvgauss(x, mean=esp_precio(i), dispersion=0.0009083137),
        from=0, to=1600, las=1, lwd=3, col='blue3',
        ylim=c(0, 0.03), xlab="Price", ylab="Densidad", 
        main=paste0("Distribucion Price con size = ", i))
  abline(v=esp_precio(i), col=gray(0.8), lty="dotted")
  Sys.sleep(time=1)
}

# Preguntas interesantes (estadistica elemental)

# 1) Cual es el % de casas con size=4000 cuyo Precio < 500
esp_precio(4000)
pinvgauss(q=500, mean=748.5795, disp=0.0009083137)

# 2) Cual es el % de casas con size=4000 cuyo Precio > 1000
pinvgauss(q=500, mean=748.5795, disp=0.0009083137, lower.tail=FALSE)

# 3) Cual es el precio mediano de las casas con size=4000
qinvgauss(p=0.5, mean=748.5795, disp=0.0009083137)

# Envelopes
fit.model <- mod2
source("https://www.ime.usp.br/~giapaula/envel_ninv")

# lrt test
library(car)
Anova(mod2)
