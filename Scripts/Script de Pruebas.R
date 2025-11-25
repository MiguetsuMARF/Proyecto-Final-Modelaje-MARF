
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

pars <- c(mu = 1,
          beta_p = 0.03,
          beta_l = 4,
          gamma = 0.4,
          delta = 1/6)

state <- c(S = 400000, I = 0, R = 0)
times <- seq(0, 30, by = 0.001)
out <- ode(y = state, times = times, func = SIRinicial, parms = pars)

par(mfrow = c(1, 1))
options(scipen = 999)
matplot(out[,1], out[,2:4], type = "l", xlab = "tiempo (Dias)", ylab = "Computadoras", main = "Modelo inicial de WannaCry tipo SID", lwd = 2)
legend("topright", c("Susceptible", "Infectado", "Computadoras removidas"), col = 1:4, lty = 1:4, cex = 0.5)


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
  
  pars <- c(mu = 1,
            beta_p = 0.03,
            beta_l1 = 4,
            beta_l2 = 0.3,
            beta_l3 = 0.15,
            gamma1 = 0.2,
            gamma2 = 0.6,
            gamma3 = 0.4,
            sigma1 = 0.02,
            sigma2 = 0.06,
            sigma3 = 0.04,
            omega1 = 0.5,
            omega2 = 0.5,
            delta = 1/6)
  
state <- c(S = 400000, I1 = 0, I2 = 0, I3 = 0, Q = 0, R = 0)
times <- seq(0, 30, by = 0.001)
out <- ode(y = state, times = times, func = SIQR_erlang, parms = pars)
plot(out)

str(out)
par(mfrow = c(1, 1))
options(scipen = 999)
matplot(out[,1], out[,2:7], type = "l", xlab = "tiempo (Dias)", ylab = "Computadoras", main = "Modelo inicial de WannaCry a partir de teoria", lwd = 2)
legend("topright", c("Susceptible", "Infectado inicial", "Periodo de desarrollo de la infeccion", "Periodo final de infeccion", 
                     "Computadoras en cuarentena", "Computadoras removidas"), col = 1:6, lty = 1:6, cex = 0.5)

## Ahora podemos graficar con datos del outbrake y simular la dinamica que ahi sucedio

SIQR_erlang2 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_eff <- S + I1 + I2 + I3 + Q
    if (N_eff <= 0) N_eff <- 1
    if (t >= 1){
      mu <- 1
      beta_p <- 0.01
      I1 <- 2000
      } else if (t < 1){
        }## Liberacion del brote
    if (t >= 1.2){
      beta_p <- 0.2
      I1 <- I1 + 2000
      beta_l1 <- 10
    } else if (t < 1){
    }
    if (t >= 1.5){
      beta_l1 <- 0.001
      delta <- 1
    } else if (t < 1.5){
    }## Kill switch
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

pars <- c(mu = 0, ## Inicialmente no habia diseminacion online
          beta_p = 0.0001, ## Una beta p muy baja ya que no hay mucha transmision online del virus.
          beta_l1 = 5, 
          beta_l2 = 0.6,
          beta_l3 = 0.10,
          gamma1 = 0.02,
          gamma2 = 0.25,
          gamma3 = 0.1666,
          sigma1 = 0.02,
          sigma2 = 0.25,
          sigma3 = 0.15,
          omega1 = 0.5,
          omega2 = 0.5,
          delta = 1/6)

times <- seq(0, 5, by = 0.00001)
state <- c(S = 400000, I1 = 0, I2 = 0, I3 = 0, Q = 0, R = 0)
out <- ode(y = state, times = times, func = SIQR_erlang2, parms = pars)
plot(out)

par(mfrow = c(1, 1))
matplot(out[,1], out[,2:7], type = "l", xlab = "tiempo (Dias)", ylab = "Computadoras", main = "Modelo de WannaCry con condiciones tipo outbrake 2017", lwd = 2)
legend("topright", c("Susceptible", "Infectado inicial", "Periodo de desarrollo de la infeccion", "Periodo final de infeccion", 
                     "Computadoras en cuarentena", "Computadoras removidas"), col = 1:6, lty = 1:6, cex = 0.5)

options(scipen = 999)

## Mejorando el modelo

SIQR_erlang3 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_eff <- S1 + S2 + I11 + I12 + I2 + I3 + Q
    if (N_eff <= 0) N_eff <- 1
    
    lambda1 <- mu * beta_p1 +
      beta_l1_11 * I11 / N_eff + beta_l1_12 * I12 / N_eff +
      beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
    
    lambda2 <- mu * beta_p2 +
      beta_l2_11 * I11 / N_eff + beta_l2_12 * I12 / N_eff +
      beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
    
    dS1 <- -lambda1 * S1 + p11 * gamma11 * I11 + p12 * gamma12 * I12 +
      p2 * gamma2 * I2 + p3 * gamma3 * I3 + alfa * p * R
    
    dS2 <- -lambda2 * S2 + (1 - p11) * gamma11 * I11 + (1 - p12) * gamma12 * I12 +
      (1 - p2) * gamma2 * I2 + (1 - p3) * gamma3 * I3 + alfa * (1 - p) * R
    
    dI11 <- lambda1 * S1 - (sigma11 + omega11 + gamma11) * I11
    dI12 <- lambda2 * S2 - (sigma12 + omega12 + gamma12) * I12
    dI2  <- omega11 * I11 + omega12 * I12 - (sigma2 + omega2 + gamma2) * I2
    dI3  <- omega2 * I2 - (sigma3 + gamma3 + delta) * I3
    dQ   <- sigma11 * I11 + sigma12 * I12 + sigma2 * I2 + sigma3 * I3 - delta * Q
    dR   <- delta * I3 + delta * Q - alfa * R
    list(c(dS1, dS2, dI11, dI12, dI2, dI3, dQ, dR))
  })
}

pars <- c(mu = 1, p = 0.8, p11 = 0.8, p12 = 0.99, p2 = 0.8, p3 = 0.999,
          beta_p1 = 0.003, beta_p2 = 0.03,
          beta_l1_11 = 3, beta_l1_12 = 10, beta_l2_11 = 5, beta_l2_12 = 15,
          beta_l2 = 0.3, alfa = 0.2,
          beta_l3 = 0.15,
          gamma11 = 0.2, gamma12 = 0.3,
          gamma2 = 0.6,
          gamma3 = 0.4,
          sigma11 = 0.02,sigma12 = 0.5,
          sigma2 = 0.06,
          sigma3 = 0.04,
          omega11 = 0.5, omega12 = 0.5,
          omega2 = 0.5,
          delta = 1/6)

state <- c(S1 = 50000, S2 = 1000, I11 = 0, I12 = 0, I2 = 0, I3 = 0, Q = 0, R = 0)
times <- seq(0, 10, by = 0.001)
out <- ode(y = state, times = times, func = SIQR_erlang3, parms = pars)

par(mfrow = c(1, 1))
options(scipen = 999)
matplot(out[,1], out[,2:9], type = "l", xlab = "tiempo (Dias)", 
        ylab = "Computadoras", main = "Modelo avanzado de WannaCry a partir de teoria", 
        col = c("blue", "darkgreen", "red", "darkred", "orange", "yellow", "brown", "black"), lwd = 2)
legend("topright", c("Susceptible de baja conectividad", "Susceptible de alta conectividad", "Infectado inicial de baja conectividad", "Infectado inicial de alta conectividad", "Periodo de desarrollo de la infeccion", "Periodo final de infeccion", 
                     "Computadoras en cuarentena", "Computadoras removidas"), 
       col = c("blue", "darkgreen", "red", "darkred", "orange", "yellow", "brown", "black"), 
       lty = 1:8, cex = 0.5)

SIQR_erlang4 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_eff <- S1 + S2 + I11 + I12 + I2 + I3 + Q
    if (N_eff <= 0) N_eff <- 1
    lambda1 <- mu * beta_p1 +
      beta_l1_11 * I11 / N_eff + beta_l1_12 * I12 / N_eff +
      beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
    
    lambda2 <- mu * beta_p2 +
      beta_l2_11 * I11 / N_eff + beta_l2_12 * I12 / N_eff +
      beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
    
    ## Factores de la simulacion
    
    # 1. El brote empieza en el dia 10.
    
    if (t >= 10){
      mu <- 1
    } else {
    }
    
    # 2. El inicio del brote es explosivo, alta tasa de infeccion, ademas la introduccion de infectados es de muchos con alta conectividad
    
    if (t >= 10){
      lambda2 <- lambda2 * 4
      lambda1 <- lambda1 * 4
      I12 <- I12 + 1000
    } else {
    }
    
    # 3. Al inicio del brote por emergencia la gente paga el tratamiento.
    
    if (t >= 10){
      gamma11 <- gamma11 * 2
      gamma12 <- gamma12 * 2
      gamma2 <- gamma2 * 1.5
    } else {
    }
    
    # 4. Pasados 10 dias del brote inicial la gente comienza a recuperar sus dispositivos por copias externas y por apoyo de la comunidad.
    
    if (t >= 20){
      alfa <- alfa * 4
    } else {
    }
    
    # 5. Pasados 10 dias del brote se promueve mas la cuarentena de los dispositivos una vez se detecta la infeccion.
    
    if (t >= 20){
      sigma11 <- sigma11 * 3
      sigma12 <- sigma12 * 3
      sigma2 <- sigma2 * 2
      sigma3 <- sigma3 * 1.5
    } else {
    }
    
    # 6. Pasados 15 dias se detiene totalmente el flujo de archivos transmisores de la infeccion.
    
    if (t >= 25){
      mu <- 0.001
    } else {
    }
    
    # 7. Pasados 20 dias del brote inicial se promueve la actualizacion de los dispositivos, por lo que la infeccion deja de tener el exito inicial.
    
    if (t >= 30){
  lambda1 <- lambda1 * 
      lambda2 <- lambda2 * 0.4
    } else {
    }
    
    # 8. Adicionalmente comienzan a promover aun mas la restauracion de datos.
    
    if (t >= 40){
      alfa <- alfa * 6
      delta <- delta * 2
    } else {
    }
    
    dS1 <- -lambda1 * S1 + p11 * gamma11 * I11 + p12 * gamma12 * I12 +
      p2 * gamma2 * I2 + p3 * gamma3 * I3 + alfa * p * R
    
    dS2 <- -lambda2 * S2 + (1 - p11) * gamma11 * I11 + (1 - p12) * gamma12 * I12 +
      (1 - p2) * gamma2 * I2 + (1 - p3) * gamma3 * I3 + alfa * (1 - p) * R
    
    dI11 <- lambda1 * S1 - (sigma11 + omega11 + gamma11) * I11
    dI12 <- lambda2 * S2 - (sigma12 + omega12 + gamma12) * I12
    dI2  <- omega11 * I11 + omega12 * I12 - (sigma2 + omega2 + gamma2) * I2
    dI3  <- omega2 * I2 - (sigma3 + gamma3 + delta) * I3
    dQ   <- sigma11 * I11 + sigma12 * I12 + sigma2 * I2 + sigma3 * I3 - delta * Q
    dR   <- delta * I3 + delta * Q - alfa * R
    list(c(dS1, dS2, dI11, dI12, dI2, dI3, dQ, dR))
  })
}

pars <- c(mu = 0.0000001, p = 0.8, p11 = 0.8, p12 = 0.99, p2 = 0.8, p3 = 0.999,
          beta_p1 = 0.003, beta_p2 = 0.03,
          beta_l1_11 = 3, beta_l1_12 = 10, beta_l2_11 = 5, beta_l2_12 = 15,
          beta_l2 = 0.3, alfa = 0.2,
          beta_l3 = 0.15,
          gamma11 = 0.2, gamma12 = 0.3,
          gamma2 = 0.6,
          gamma3 = 0.4,
          sigma11 = 0.02,sigma12 = 0.5,
          sigma2 = 0.06,
          sigma3 = 0.04,
          omega11 = 0.5, omega12 = 0.5,
          omega2 = 0.5,
          delta = 1/6)

state <- c(S1 = 50000, S2 = 5000, I11 = 0, I12 = 0, I2 = 0, I3 = 0, Q = 0, R = 0)
times <- seq(0, 40, by = 0.001)
out <- ode(y = state, times = times, func = SIQR_erlang4, parms = pars)

par(mfrow = c(1, 1))
options(scipen = 999)
matplot(out[,1], out[,2:9], type = "l", xlab = "tiempo (Dias)", 
        ylab = "Computadoras", main = "Simulacion de outbrake de Wannacry y respuesta dinamica en el tiempo", 
        col = c("blue", "darkgreen", "red", "darkred", "orange", "yellow", "purple", "black"), lwd = 2)
legend("topright", c("Susceptible de baja conectividad", "Susceptible de alta conectividad", "Infectado inicial de baja conectividad", "Infectado inicial de alta conectividad", "Periodo de desarrollo de la infeccion", "Periodo final de infeccion", 
                     "Computadoras en cuarentena", "Computadoras removidas"), 
       col = c("blue", "darkgreen", "red", "darkred", "orange", "yellow", "purple", "black"), 
       lty = 1:8, cex = 0.5)


## Ahora vamos a simular el outbrake del 2017

SIQR_erlang5 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_eff <- S1 + S2 + I11 + I12 + I2 + I3 + Q
    if (N_eff <= 0) N_eff <- 1
    lambda1 <- mu * beta_p1 +
      beta_l1_11 * I11 / N_eff + beta_l1_12 * I12 / N_eff +
      beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
    
    lambda2 <- mu * beta_p2 +
      beta_l2_11 * I11 / N_eff + beta_l2_12 * I12 / N_eff +
      beta_l2 * I2 / N_eff + beta_l3 * I3 / N_eff
    
    ## Factores de la simulacion
    
    # 1. El brote empieza en el dia 1.
    
    if (t >= 10){
      mu <- 1
      beta_p1 <- 0.3
      beta_p2 <- 0.5
    } else {
    }
    
    # 2. El inicio del brote es explosivo, alta tasa de infeccion, ademas la introduccion de infectados es de muchos con alta conectividad
    
    if (t >= 10){
      lambda2 <- lambda2 * 6
      lambda1 <- lambda1 * 16
    } else {
    }
    
    # 4. Pasado medio dia se publica el kill switch y se acaba la infeccion.
    
    if (t >= 11){
      lambda2 <- lambda2 * 0.001
      lambda1 <- lambda1 * 0.001
      omega11 <- omega11 * 4
      omega12 <- omega12 * 4
      omega2 <- omega2 * 4
      delta <- delta * 3
      alfa <- alfa * 4
    } else {
    }
    
    dS1 <- -lambda1 * S1 + p11 * gamma11 * I11 + p12 * gamma12 * I12 +
      p2 * gamma2 * I2 + p3 * gamma3 * I3 + alfa * p * R
    
    dS2 <- -lambda2 * S2 + (1 - p11) * gamma11 * I11 + (1 - p12) * gamma12 * I12 +
      (1 - p2) * gamma2 * I2 + (1 - p3) * gamma3 * I3 + alfa * (1 - p) * R
    
    dI11 <- lambda1 * S1 - (sigma11 + omega11 + gamma11) * I11
    dI12 <- lambda2 * S2 - (sigma12 + omega12 + gamma12) * I12
    dI2  <- omega11 * I11 + omega12 * I12 - (sigma2 + omega2 + gamma2) * I2
    dI3  <- omega2 * I2 - (sigma3 + gamma3 + delta) * I3
    dQ   <- sigma11 * I11 + sigma12 * I12 + sigma2 * I2 + sigma3 * I3 - delta * Q
    dR   <- delta * I3 + delta * Q - alfa * R
    
    list(c(dS1, dS2, dI11, dI12, dI2, dI3, dQ, dR))
  })
}

pars <- c(mu = 0.00001, p = 0.8, p11 = 0.8, p12 = 0.99, p2 = 0.8, p3 = 0.999,
          beta_p1 = 0.00000000001, beta_p2 = 0.000000000001,
          beta_l1_11 = 3, beta_l1_12 = 10, beta_l2_11 = 5, beta_l2_12 = 15,
          beta_l2 = 0.3, alfa = 0.2,
          beta_l3 = 0.15,
          gamma11 = 0.2, gamma12 = 0.3,
          gamma2 = 0.6,
          gamma3 = 0.4,
          sigma11 = 0.02,sigma12 = 0.5,
          sigma2 = 0.06,
          sigma3 = 0.04,
          omega11 = 0.5, omega12 = 0.5,
          omega2 = 0.5,
          delta = 1/6)

state <- c(S1 = 45000, S2 = 5000, I11 = 0, I12 = 0, I2 = 0, I3 = 0, Q = 0, R = 0)
times <- seq(0, 20, by = 0.0001)
out <- ode(y = state, times = times, func = SIQR_erlang5, parms = pars)

par(mfrow = c(1, 1))
options(scipen = 999)
matplot(out[,1], out[,2:9], type = "l", xlab = "tiempo (Dias)", 
        ylab = "Computadoras", main = "Simulacion de outbrake de Wannacry en 2017", 
        col = c("blue", "darkgreen", "red", "darkred", "orange", "yellow", "purple", "black"), lwd = 2, xlim = c(9,12))
legend("topright", c("Susceptible de baja conectividad", "Susceptible de alta conectividad", "Infectado inicial de baja conectividad", "Infectado inicial de alta conectividad", "Periodo de desarrollo de la infeccion", "Periodo final de infeccion", 
                     "Computadoras en cuarentena", "Computadoras removidas"), 
       col = c("blue", "darkgreen", "red", "darkred", "orange", "yellow", "purple", "black"), 
       lty = 1:8, cex = 0.5)


