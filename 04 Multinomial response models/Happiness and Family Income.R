# 6.2.5 Example: Happiness and Family Income
# AN INTRODUCTION TO CATEGORICAL DATA ANALYSIS
# Agresti

# La variable respuesta
happi <- c("Not_too_happy", "Pretty_happy", "Very_happy")
happiness <- rep(rep(happi, times=3),
                 times=c(37, 90, 45,
                         25, 93, 56,
                         6, 18, 13))
happiness <- factor(happiness, ordered=TRUE, levels=happi)

# Una covariable
income <- rep(c("Below", "Average", "Above"), times=c(172, 174, 37))
income <- factor(income, levels=c("Below", "Average", "Above"))

# Vamos a crear el marco de datos
datos <- data.frame(income, happiness)

head(datos)

str(datos)

# Ajustando el modelo con vglm
library(VGAM)

fit <- vglm(happiness ~ income, 
            family=cumulative(parallel=TRUE),
            data=datos)
summary(fit)


# Usando los datos agrupados ----------------------------------------------

Happy <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Happy.dat",
                    header=TRUE)

library(VGAM)

fit0 <- vglm(cbind(y1,y2,y3)~ 1, 
             family=cumulative(parallel=TRUE), 
             data=Happy)

fit1 <- vglm(cbind(y1,y2,y3)~ income,
            family=cumulative(parallel=TRUE),
            data=Happy)

fit2 <- vglm(cbind(y1,y2,y3) ~ factor(income), 
             family=cumulative(parallel=TRUE),
             data=Happy)






