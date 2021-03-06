u_exact <- function(x, t){
  x * (x - 1) * (1i + t) * cos(pi * (t^2))
}

# u_exact2 <- function(x, t){
#   (abs(x * (x - 1) * (1i + t) * cos(pi * (t^2)))^2) *
#     (x * (x - 1) * (1i + t) * cos(pi * (t^2)))
# }
# 
# u_mod_sq <- function(x, t){
#   (abs(u_exact(x, t))^2) * u_exact(x, t)
# }


# u_exact_d_x <- Deriv::Deriv(u_exact, "x")
# round((0+1i + t) * (2 * x - 1) * cos(pi * t^2),14)==round((2*x*t*cos(pi*t^2) - t*cos(pi*t^2)) + 1i*(-cos(pi*t^2) + 2*x*cos(pi*t^2)),14)
# 15 skaicius po kablelio nesutampa
# u_exact_d_x <- function(x, t){
#   (2*x*t*cos(pi*t^2) - t*cos(pi*t^2)) + 1i*(-cos(pi*t^2) + 2*x*cos(pi*t^2))
# }
u_exact_d_x <- function(x, t){
  # isplesta forma
  # 3*((t+1)^3) * ((cos(pi*(t^2)))^3) * ((x-1)^2) * (x^3) +
  #   3*((t+1)^3) * ((cos(pi*(t^2)))^3) * ((x-1)^3) * (x^2)
  # supaprastina
  3 * (t + 1)^3 * ((cos(pi * (t^2)))^3) * ((x - 1)^2) * (x^2) * (2 * x - 1)
  # abi apskaiciuotos naudojantis wolfram alpha, rezultatas toks pat
}

# u_exact_d_d_x <- Deriv::Deriv(Deriv::Deriv(u_exact, "x"),"x")
u_exact_d_d_x <- function(x, t){
  2*t*cos(pi*t^2) + 2*1i*cos(pi*t^2)
}

# u_exact_d_t <- Deriv::Deriv(u_exact, "t")
# .e1 <- pi * t^2
# round(x * (cos(.e1) - 2 * (pi * t * (0+1i + t) * sin(.e1))) * (x - 1),15)==round((x-1)*x*cos(pi*t^2) - 2*pi*(x-1)*x*t*(t+1i)*sin(pi*t^2),15)
# 16 skaicius po kablelio nesutampa
u_exact_d_t <- function(x, t){
  (x-1)*x*cos(pi*t^2) - 2*pi*(x-1)*x*t*(t+1i)*sin(pi*t^2)
}

# U zero
u_exact_0 <- function(x){
  x * (x - 1) * 1i
}

# f(x, t)
f_x_t <- function(x, t, a, beta){
  p1 <- (u_exact_d_t(x, t))
  p2 <- ((a^2 + 1i) * u_exact_d_d_x(x, t))
  p3 <- (beta * u_exact_d_x(x, t)) # * (((abs(u_exact(x, t)))^2) * u_exact(x, t)))
  
  out <- p1 - p2 - p3
  return(out)
}

# C
C <- function(h, a, tau){
  2 + ((2 * h^2) / ((a^2 + 1i) * tau))
}

# F_j(...)
F_j <- function(u_now0, u_now1, u_now2,
                u_next0, u_next1, u_next2,
                f_now, f_next,
                h, tau, a, beta){
  
  p1 <- (u_now2 - 2 * u_now1 + u_now0 + (((2 * (h^2)) / ((a^2 + 1i) * tau)) * u_now1))
  p2 <- (((h * beta) / (a^2 + 1i)) * (1/2) * (((abs(u_next2))^2) * u_next2 - ((abs(u_next0))^2) * u_next0 + 
                                                ((abs(u_now2))^2) * u_now2 - ((abs(u_now0))^2) * u_now0))
  p3 <- (((h^2) / (a^2 + 1i)) * (f_next + f_now))
  
  out <- p1 + p2 + p3
  return(out)
}

# F_j(...) values
F_j_val <- function(u_now, u_next, f_now, f_next, h, tau, a, beta){
  
  out <- c() # rep(NA, length(u_now))
  for (i in 2:(length(u_now)-1)) {
    p1 <- (u_now[i+1] - 2 * u_now[i] + u_now[i-1] + (((2 * (h^2)) / ((a^2 + 1i) * tau)) * u_now[i]))
    p2 <- (((h * beta) / (a^2 + 1i)) * (1/2) * (((abs(u_next[i+1]))^2) * u_next[i+1] - ((abs(u_next[i-1]))^2) * u_next[i-1] + 
                                                  ((abs(u_now[i+1]))^2) * u_now[i+1] - ((abs(u_now[i-1]))^2) * u_now[i-1]))
    p3 <- (((h^2) / (a^2 + 1i)) * (f_next[i] + f_now[i]))
    out[i-1] <- p1 + p2 + p3
  }
  
  return(out)
}


# function for T3
f_test <- function(j){
  d <- 7.5 * sin(j^2) - 1i * j * cos(j)
  d[1] <- 0i
  d[length(d)] <- 0i
  return(d)
}

# Thomas algorithm
Thomas <- function(N, b_12, F_j, a_v, a_12){
  ### --- Thomas
  # 1. alpha_1 and beta_1
  # a_v <- rep(0, (N))
  b_v <- rep(0, (N))
  # a_v[1] <- a_12[1]
  b_v[1] <- b_12[1]
  
  # 2. calculate alpha, beta vectors
  for (i in 1:(N-1)){
    # a_v[i+1] <- 1 / (C_t3 - a_v[i])
    b_v[i+1] <- (b_v[i] + F_j[i]) * a_v[i+1]
  }
  
  # 3. y_N value
  y <- rep(0i, N+1)
  y[N+1] = (a_12[2] * b_v[N] + b_12[2]) / (1 - a_12[2] * a_v[N])
  
  # 4. calculate y_n vector
  for (i in (N):1){
    y[i] <- a_v[i] * y[i+1] + b_v[i] # alpha, beta vektoriu ilgis trumpesnis, del to naudojama ju ***[i] reiksmes, o ne ***[i+1]
  }
  
  return(y)
}


# ------------------------------------------------
# U_dest <- function(x,t){ (1+1i*t)*(sin(pi*x))^2}
# U_test <- function(x,t){(exp(1)^t - 1i * t^2 * x)*(x^2)*(1-x)*(t^2 - 1i*sqrt(1i))*sin(pi*x)}
