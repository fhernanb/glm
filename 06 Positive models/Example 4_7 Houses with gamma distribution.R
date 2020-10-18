# -------------------------------------------------------------------------
# Example 4.7 from Agresti (2015)
# -------------------------------------------------------------------------

url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

pairs(datos[c('price','size','taxes')])

# Modelos GAMMA -----------------------------------------------------------
with(datos, plot(density(price)))

fit.gamma <- glm(price ~ size + new + beds + size:new + size:beds,
                 data=datos, family=Gamma(link=identity))
summary(fit.gamma)

# Otros modelos
fit.g1 <- glm(price ~ size+new+baths+beds, data=datos,
              family=Gamma(link=identity))
fit.g2 <- glm(price~(size+new+baths+beds)^2, data=datos,
              family=Gamma(link=identity))

anova(fit.g1, fit.g2)

summary(fit.g1)
MASS::gamma.dispersion(fit.g1) # para obtener phi
MASS::gamma.shape(fit.g1)      # para obtener k

fit.g3 <- glm(price ~ size+new+size:new, data=datos,
              family=Gamma(link=identity))

summary(fit.g3)
MASS::gamma.dispersion(fit.g3) # para obtener phi
MASS::gamma.shape(fit.g3)      # para obtener k

par(mfrow=c(2, 2))
plot(fit.g3)


# Lo que viene NO esta en el libro, es mi propuesta -----------------------
mod1 <- glm(price ~ 1, data=datos,
              family=Gamma(link="log"))
summary(mod1)

# Aplicando seleccion de variables
library(MASS)
horizonte <- formula(price ~ taxes*beds*baths*new*size)
mod2 <- stepAIC(object=mod1, scope=horizonte, direction="both", k=2)
summary(mod2)

coef(mod2)                   # estimacion de los betas
MASS::gamma.dispersion(mod2) # para obtener phi
MASS::gamma.shape(mod2)      # para obtener k

# Que tan bien acompanan las estimaciones los valores reales Y?
precio_hat <- predict(mod2, type="response")
cor(datos$price, precio_hat)
plot(x=datos$price, y=precio_hat, las=1,
     xlab="Precio obs", ylab="Precio est")
abline(a=0, b=1, lty="dashed", col="blue3")
text(x=100, y=400, expression(rho(Y, hat(Y))==0.87))

# Envelopes
fit.model <- mod2
source("https://www.ime.usp.br/~giapaula/envel_gama")

# lrt test
library(car)
Anova(mod2)

