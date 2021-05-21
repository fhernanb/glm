
stand_dev_res <- function(mod) {
  
  # To obtain some useful quantities
  X <- model.matrix(mod)
  w <- mod$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  
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
  
  phi <- phi_hat_glm(mod)
  
  # To obtain the standardised deviance residual
  tDi <- resid(mod, type="deviance") / sqrt(phi*(1-h))
  
  return(tDi)
}
