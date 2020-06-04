# Example 4.7
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

pairs(datos[c('price','size','taxes')])


# Modelos NORMAL ----------------------------------------------------------
fit1 <- lm(price ~ size + new + baths + beds, data=datos)
fit2 <- lm(price ~ (size + new + baths + beds)^2, data=datos)
fit3 <- lm(price ~ (size + new + baths + beds)^3, data=datos)

# Comparando
anova(fit1, fit2, fit3)

# Otros modelos
fit4 <- update(fit2, . ~ . - baths:beds)
summary(fit4)

fit5 <- update(fit4, . ~ . - size:baths)
summary(fit5)

fit6 <- update(fit5, . ~ . - new:beds)
summary(fit6)

cor(datos$price, fitted(fit6))

fit7 <- update(fit6, . ~ . - new:baths)
summary(fit7)

fit8 <- update(fit7, . ~ . - baths)
summary(fit8)

cor(datos$price, fitted(fit8))

fit9 <- update(fit8, . ~ . - beds - size:beds)
summary(fit9)

# calculando BIC
AIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, k=4)

# Seleccion de variables
mod10 <- step(object=fit2, k=4, direction='backward')
summary(mod10)


# Modelos GAMMA -----------------------------------------------------------
with(datos, plot(density(price)))

fit.gamma <- glm(price ~ size + new + beds + size:new + size:beds,
                 data= datos, 
                 family = Gamma(link = identity))
summary(fit.gamma)
summary(fit.gamma)$coef

fit.g1 <- glm(price ~ size+new+baths+beds, data=datos,
              family=Gamma(link=identity))
fit.g2 <- glm(price~(size+new+baths+beds)^2, data=datos,
              family=Gamma(link=identity))

anova(fit.g1, fit.g2)

gamma.dispersion(fit.g1)
