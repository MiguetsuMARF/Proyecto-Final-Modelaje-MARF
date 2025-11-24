
library(deSolve)

SIRinicial <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_efectivas <- S + I
    dS <- gamma * I - beta_p * S - beta_l * S * I/N_efectivas
    dI <- beta_p * S + beta_l * S * I/N_efectivas - gamma * I - delta * I
    dR <- delta * I
    list(c(dS, dI, dR))
  })
}

  SIQR_erlang <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      N_eff <- S + I1 + I2 + I3 + Q
      if (N_eff <= 0) N_eff <- 1
      lambda <- mu * beta_p + beta_l1 * I1 / N_eff + beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
      dS  <- -lambda * S + gamma1 * I1 + gamma2 * I2 + gamma3 * I3
      dI1 <-  lambda * S - (sigma1 + omega1 + gamma1) * I1
      dI2 <-  omega1 * I1 - (sigma2 + omega2 + gamma2) * I2
      dI3 <-  omega2 * I2 - (sigma3 + gamma3 + delta) * I3
      dQ  <-  sigma1 * I1 + sigma2 * I2 + sigma3 * I3
      dR  <-  delta * I3
      list(c(dS, dI1, dI2, dI3, dQ, dR))
    })
  }
  