# This is an auxiliar function to obtain the phi
# value of the dispersion parameter
phi_hat_glm <- function(fit) {
  if (substr(fit$family$family, start=1, stop=3) == "Neg")
    fit$family$family <- "neg_bin"
  switch(fit$family$family,
         gaussian = summary(fit)$dispersion,
         Gamma = MASS::gamma.dispersion(fit),
         inverse.gaussian = summary(fit)$dispersion,
         poisson = 1,
         neg_bin = 1 / fit$theta,
         binomial = 1)
}


# Normal case
y <- rnorm(n=800, mean=170, sd=5)
mod <- glm(y ~ 1, family=gaussian)

phi_hat_glm(mod)

# Poisson case
y <- rpois(n=800, lambda=25)
mod <- glm(y ~ 1, family=poisson)

phi_hat_glm(mod)

# Gamma case
rgamma_glm <- function(n, mu, phi) {
  x <- rgamma(n=n, shape=1/phi, scale=mu*phi)
  return(x)
}

y <- rgamma_glm(n=800, mu=170, phi=5)
mod <- glm(y ~ 1, family=Gamma)

phi_hat_glm(mod)

# Negative binomial case
phi <- 2
y <- rnbinom(n=800, mu=7, size=1/phi)
library(MASS)
mod <- glm.nb(y ~ 1)

phi_hat_glm(mod)

