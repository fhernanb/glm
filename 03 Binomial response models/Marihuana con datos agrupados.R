# Ejemplo 4.3.2 del libro de Analisis de datos Categoricos
# de Agresti

Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat",
                        header=TRUE)

Marijuana

# Modelos
fit0 <- glm(yes/(yes+no) ~ 1, weights = yes + no,
           family=binomial, data=Marijuana)
fit1 <- glm(yes/(yes+no) ~ gender, weights = yes + no,
           family=binomial, data=Marijuana)
fit2 <- glm(yes/(yes+no) ~ race, weights = yes + no,
           family=binomial, data=Marijuana)
fit <- glm(yes/(yes+no) ~ gender + race, weights = yes + no,
           family=binomial, data=Marijuana)

summary(fit)


# Anova
anova(fit)
library(car)
Anova(fit, type=2)
Anova(fit, type=3)

-2*(logLik(fit0) - logLik(fit1))
-2*(logLik(fit1) - logLik(fit2))
-2*(logLik(fit0) - logLik(fit2))

