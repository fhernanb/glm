

phi_hat_glm <- function(mod) {
  if (substr(mod$family$family, start=1, stop=3) == "Neg")
    mod$family$family <- "neg_bin"
  switch(mod$family$family,
         gaussian = summary(mod)$dispersion,
         Gamma = MASS::gamma.dispersion(mod),
         inverse.gaussian = summary(mod)$dispersion,
         poisson = 1,
         neg_bin = 1/mod$theta,
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

y <- rgamma_glm(n=8000, mu=170, phi=5)
mod <- glm(y ~ 1, family=Gamma)

phi_hat_glm(mod)

# Inverse gaussian case
phi <- 2
y <- rnbinom(n=800, mu=7, size=1/phi)
library(MASS)
mod <- glm.nb(y ~ 1)

phi_hat_glm(mod)

