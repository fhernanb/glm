# En este ejemplo se muestra como calcular los
# residuales en un glm BINOMIAL

# Aqui los datos ordenados por la variable weight
y <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 
       0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
weight <- c(1, 1, 1, 1.1, 1.1, 1.2, 1.2, 1.2, 1.2, 1.3, 
            1.4, 1.5, 1.5, 1.5, 1.5, 1.6, 1.7, 1.7, 1.9, 
            1.9, 2, 2, 2.1, 2.1, 2.1, 2.1, 2.1, 2.2, 2.2, 
            2.2, 2.2, 2.3, 2.4, 2.4, 2.5, 2.5, 2.5, 2.6, 
            2.7, 2.7, 2.8, 2.9, 2.9, 2.9, 2.9, 2.9, 2.9, 3, 3, 3)

plot(x=weight, y, yaxt='n', pch=20)
axis(side=2, at=0:1, labels=0:1, las=1)

# Los tres modelos
mod <- glm(y ~ weight, family=binomial)
mod_sat <- glm(y ~ as.factor(1:length(y)), family=binomial)
mod_nul <- glm(y ~ 1, family=binomial)

# Dibujando los datos y la curva con cada modelo
plot(x=weight, y, yaxt='n', pch=20, main='Modelo propuesto')
axis(side=2, at=0:1, labels=0:1, las=1)
points(x=weight, y=mod$fitted.values, type='l', col='blue')

plot(x=weight, y, yaxt='n', pch=20, main='Modelo saturado')
axis(side=2, at=0:1, labels=0:1, las=1)
points(x=weight, y=mod_sat$fitted.values, type='l', col='blue')

plot(x=weight, y, yaxt='n', pch=20, main='Modelo nulo')
axis(side=2, at=0:1, labels=0:1, las=1)
points(x=weight, y=mod_nul$fitted.values, type='l', col='blue')


# Residuals ---------------------------------------------------------------

# leer leer y leer
help(residuals.glm)

# Calculado pi_hat
pi_hat <- predict(mod, type='response')
pi_hat

# Raw residuals ---> manually
ei <- y - pi_hat
ei

# Raw residuals ---> automatically
residuals(mod, type='response')

# Deviance residuals and Deviance ---> manually
ll_mod <- dbinom(x=y, size=1, prob=pi_hat, log=TRUE)
sqrt(-2 * ll_mod) # Deviance residuals
sum(-2 * ll_mod)  # Residual deviance
mod$deviance # Residual deviance

# Deviance residuals and Deviance ---> automatically
residuals(mod, type='deviance')         # Deviance residuals
sum(residuals(mod, type='deviance')^2)  # Residual deviance
mod$deviance # Residual deviance

# Pearson residuals ---> manually
pi <- (y - pi_hat) / sqrt(pi_hat * (1-pi_hat) / 1)
pi

# Pearson residuals ---> automatically
residuals(mod, type='pearson')

# Standardized residual ---> manually
hii <- lm.influence(mod)$hat
ri <- ei * sqrt(mod$weights) / sqrt(1-hii)
ri

# Other Residuals
mis_res <- cbind(deviance=residuals(mod, type='deviance'),
                 pearson=residuals(mod, type='pearson'),
                 working=residuals(mod, type='working'),
                 response=residuals(mod, type='response'),
                 weight=residuals(mod, type='partial'),
                 stand=rstandard(mod),
                 stude=rstudent(mod))

round(mis_res, digits=3)

# Algunos graficos
library(car)
qqPlot(rstudent(mod))


# Envelopes de Gilberto Alvarenga -----------------------------------------
fit.model <- mod
source("https://www.ime.usp.br/~giapaula/envel_bino")
source("https://www.ime.usp.br/~giapaula/diag_bino")
