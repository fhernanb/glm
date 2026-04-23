# -------------------------------------------------------------------------
# Example 4.7 from Agresti (2015)
# -------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/fhernanb/glm/refs/heads/master/data_Agresti_2015/Houses.dat"
datos <- read.table(url, header=TRUE)
head(datos, n=5)

pairs(datos[c("price","size","taxes")])

# Modelos GAMMA -----------------------------------------------------------
with(datos, plot(density(price)))

fit_g0<- glm(price ~ size + new + beds + size:new + size:beds,
                 data=datos, family=Gamma(link=identity))
summary(fit_g0)

# Otros modelos
fit_g1 <- glm(price ~ size + new + baths + beds, data=datos,
              family=Gamma(link=identity))
fit_g2 <- glm(price ~ (size + new + baths + beds)^2, data=datos,
              family=Gamma(link=identity))

summary(fit_g1)
summary(fit_g2)

# Comparando los dos modelos
anova(fit_g1, fit_g2)

summary(fit_g1)
MASS::gamma.dispersion(fit_g1) # para obtener phi
MASS::gamma.shape(fit_g1)      # para obtener k

# Otro modelo
fit_g3 <- glm(price ~ size + new + size:new, data=datos,
              family=Gamma(link=identity))

summary(fit_g3)

MASS::gamma.dispersion(fit_g3) # para obtener phi
MASS::gamma.shape(fit_g3)      # para obtener k

par(mfrow=c(2, 2))
plot(fit_g3)

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
library(glmtoolbox)
envelope(mod2)

# lrt test
library(car)
Anova(mod2)

# Checking model assumptions
plot(mod2)
