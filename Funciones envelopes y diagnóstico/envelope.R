
envelope <- function(mod, rep=100, conf=0.95) {
  
  X <- model.matrix(mod)
  n <- nrow(X)
  p <- ncol(X)
  
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
  # To obtain the residual deviance
  dev_res <- resid(mod, type="deviance") * sqrt(phi/(1-h))
  
  weights <- mod$prior.weights
  if(is.null(mod$offset)) offs <- rep(0, n) else offs <- mod$offset
  
  rep <- max(1, floor(abs(rep))) # To ensure an appropiate rep
  e <- matrix(0, n, rep) # To store the residuals
  
  i <- 1
  
  while (i <= rep) {
    y <- simulate(object=mod, nsim=1)$sim_1
    fit <- try(glm(y ~ X[, -1], family=mod$family,
                  offset=offs, start=coef(mod),
                  weights=weights),
               silent=TRUE)
    
    if (class(fit)[1] != "try-error") {
      w <- fit$weights
      W <- diag(w)
      H <- solve(t(X)%*%W%*%X)
      H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
      h <- diag(H)
      phi <- phi_hat_glm(fit)
      ei  <- resid(fit, type="deviance")*sqrt(phi/(1-h))
      e[, i] <- sort(ei)
      i <- i + 1
    }
  }
  
  # With this instruction we obtain the envelopes
  e1 <- apply(X=e, MARGIN=1, FUN=quantile, probs=(1-conf)/2)
  e2 <- apply(X=e, MARGIN=1, FUN=quantile, probs=1-(1-conf)/2)

  med <- apply(e, 1, mean)
  faixa <- range(dev_res, e1, e2)
  list(e1, e2)
  
  par(pty="s")
  qqnorm(dev_res, xlab="Quantiles of N(0,1)", las=1,
         ylab="Deviance", ylim=faixa, pch=16, main="")
  par(new=TRUE)
  #
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
  par(new=TRUE)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
  par(new=TRUE)
  qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")

}

