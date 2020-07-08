# Ejemplo 1 - Bernoulli ---------------------------------------------------

# Usando los datos de cangrejos
url <- "http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)

# Se desea modelar la variable Y en funcion de width
# Y=0 si la cangreja NO tiene satelites pegados
# Y=1 si la cangreja SI tiene satelites pegados

# El objetivo es estudiar las siguientes hipotesis
# H0: beta_width  = -15
# H1: beta_width != -15

fit <- glm(y ~ width, family=binomial, data=Crabs)
summary(fit)

# Calculando el estadistico y su valor-P
z <- (-12.3508 - (-15)) / 2.6287
z
2 * pnorm(q=z, lower.tail=FALSE) # valor-P

# Conclusion: como el valor-P es mas grande que un nivel de
# significancia usual, se concluye que no hay evidencias para
# rechazar H0.

# Ilustrando el valor-P
library(usefultools)
shadow.dist(dist='dnorm', param=list(mean=0, sd=1),
            a=-z, b=z, type='two', from=-3, to=3)

# Ejemplo 2 - Poisson -----------------------------------------------------

# Usando los datos de cangrejos
# color: 1, medium light; 2, medium; 3, medium dark; 4, dark 
# spine condition: 1, both good; 2, one worn or broken; 3, both worn or broken
# Width, carapace width (cm); 
# Weight, weight (kg).

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)
Crabs$color <- as.factor(Crabs$color)
Crabs$spine <- as.factor(Crabs$spine)

# Se desea modelar la variable Y que representa el 
# numero de satelites.

# El objetivo es estudiar las siguientes hipotesis
# H0: beta_weight = 0.1
# H1: beta_weight > 0.1

fit <- glm(y ~ weight + color + width + spine, 
           family=poisson(link=log), data=Crabs)
summary(fit)

# Calculando el estadistico y su valor-P
z <- (0.49647 - 0.1) / 0.16626
z
pnorm(q=z, lower.tail=FALSE) # valor-P

# Conclusion: como el valor-P es menor grande que un nivel de
# significancia usual, se concluye que SI hay evidencias para
# rechazar H0, es decir, el beta_weight es ahora mayor que 0.1.

# Ilustrando el valor-P
library(usefultools)
shadow.dist(dist='dnorm', param=list(mean=0, sd=1),
            a=z, type='upper', from=1, to=4)
