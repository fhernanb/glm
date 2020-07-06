# Para estimar el parametro de dispesion phi en un glm gamma
# se puede usar los siguientes metodos:
# 1) Modified Profile Log-Likelihood Estimator
# 2) Mean Deviance Estimator
# 3) Pearson Estimator
# que estan en seccion 6.8 de Dunn & Smyth (2018) pagina 252


# Funciones para Modified Profile Log-Likelihood Estimator ----------------
ll_beta_glm_gamma <- function(beta, phi, y, X, mod) {
  mu <- X %*% matrix(beta, ncol=1)
  mu <- mod$family$linkinv(mu)
  sum(dgamma(x=y, shape=1/phi, scale=mu*phi, log=TRUE))
}

ll_phi_glm_gamma <- function(phi, beta, y, X, mod) {
  mu <- X %*% matrix(beta, ncol=1)
  mu <- mod$family$linkinv(mu)
  sum(dgamma(x=y, shape=1/phi, scale=mu*phi, log=TRUE))
}

mod_ll_prof_phi_glm_gamma <- function(mod, niter=10) {
  mod <- update(mod, y=TRUE, x=TRUE)
  y <- mod$y
  X <- mod$x
  beta_i <- coef(mod)
  phi_i <- MASS::gamma.dispersion(mod)
  thetas <- matrix(NA, ncol=length(beta_i)+1, nrow=niter)
  thetas[1, ] <- c(beta_i, phi_i)
  for (i in 2:niter) {
    res1 <- optimize(f=ll_phi_glm_gamma, interval=c(0, 50), 
                     maximum=TRUE, beta=beta_i, y=y, X=X, mod=mod)
    phi_i <- res1$maximum
    
    #res1 <- optim(par=phi_i, fn=ll_phi_glm_gamma, 
    #              control=list(fnscale=-1), method="Brent",
    #              lower=0, upper=50,
    #              beta=beta_i, y=y, X=X, mod=mod)
    #phi_i <- res1$par
    
    res2 <- optim(par=beta_i, fn=ll_beta_glm_gamma, 
                  control=list(fnscale=-1),
                  phi=phi_i, y=y, X=X, mod=mod)
    beta_i <- res2$par
    thetas[i, ] <- c(beta_i, phi_i)
  }
  phi_final <- thetas[niter, length(beta_i)+1]
  thetas
  #return(phi_final)
}


# Ejemplo -----------------------------------------------------------------
library(GLMsData)
data(trees)
mod <- glm(Volume ~ log(Height) + log(Girth), data=trees,
           family=Gamma(link=identity))

# Para obtener Mean Deviance Estimator se usa:
mod$deviance / mod$df.residual

# Para obtener Pearson Estimator se usa:
w <- weights(mod, type="working")
e <- residuals(mod, type="working")
sum(w * e^2) / df.residual(mod)

# Para obtener Modified Profile Log-Likelihood Estimator se usa:
mod_ll_prof_phi_glm_gamma(mod)

# Automaticamente
MASS::gamma.dispersion(mod)
1 / MASS:::gamma.shape.glm(mod)$alpha
