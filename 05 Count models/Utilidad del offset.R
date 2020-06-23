# En este ejemplo vamos a crear unos datos artificiales
# y vamos a modificar la tasa media de exitos para simular
# valores de Y que difieran en el intervalo de observacion.

# Y representa el numero de pacientes contagiados por intervalo.
# El intervalo sera: dia (1), semanana (7), quincena (15), mes (30)

# Los parametros ----------------------------------------------------------
b0 <- -1
b1 <- 1
betas <- c(b0, b1)

# Creando los datos -------------------------------------------------------
n <- 100
x <- runif(n=n)
intervalo <- sample(x=c(1, 7, 15, 30), size=n, replace=TRUE)
lambda <- exp(b0 + b1 * x)
lambda_transf <- lambda * intervalo
y <- rpois(n=n, lambda=lambda_transf)

head(cbind(x, intervalo, lambda, lambda_transf, y), n=15)

# Explorando los datos ----------------------------------------------------
plot(y=y, x=x, las=1, pch=20)

# Naive model -------------------------------------------------------------
mod1 <- glm(y ~ x, family=poisson(link="log"))
coef(mod1)
betas
mean((betas - coef(mod1))^2) # Error cuadratico medio

# Correct model using offset ----------------------------------------------
mod2 <- glm(y ~ x, family=poisson(link="log"), 
            offset=log(intervalo))
coef(mod2)
betas
mean((betas - coef(mod2))^2) # Error cuadratico medio

# Tentacion ---------------------------------------------------------------
mod3 <- glm(y/intervalo ~ x, family=poisson(link="log"))
coef(mod3)
betas
mean((betas - coef(mod3))^2) # Error cuadratico medio

