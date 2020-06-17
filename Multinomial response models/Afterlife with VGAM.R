# 6.1.5 Example: Afterlife
# AN INTRODUCTION TO CATEGORICAL DATA ANALYSIS
# Agresti

url <- "http://www.stat.ufl.edu/~aa/cat/data/Afterlife.dat"
Afterlife <- read.table(url, header=TRUE)
Afterlife

# Ajustando el modelo
library(VGAM)
fit <- vglm(cbind(yes, undecided, no) ~ gender + race, 
            family=multinomial, data=Afterlife)
summary(fit)
coef(fit, matrix = TRUE)

