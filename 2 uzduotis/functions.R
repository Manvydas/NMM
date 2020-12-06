u_exact <- function(x, t){
  x * (x - 1) * (1i + t) * cos(pi * (t^2))
}

# u_exact_d_x <- Deriv::Deriv(u_exact, "x")
# round((0+1i + t) * (2 * x - 1) * cos(pi * t^2),14)==round((2*x*t*cos(pi*t^2) - t*cos(pi*t^2)) + 1i*(-cos(pi*t^2) + 2*x*cos(pi*t^2)),14)
# 15 skaicius po kablelio nesutampa
u_exact_d_x <- function(x, t){
  (2*x*t*cos(pi*t^2) - t*cos(pi*t^2)) + 1i*(-cos(pi*t^2) + 2*x*cos(pi*t^2))
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
  p3 <- (beta * u_exact_d_x(x, t) * (((abs(u_exact(x, t)))^2) * u_exact(x, t)))
  
  out <- p1 - p2 - p3
  return(out)
}

# C
C <- function(h, tau){
  2 - ((1i * (2 * h^2)) / tau)
  }

# F_j(...)
F_j <- function(u_now0, u_now1, u_now2,
                u_next0, u_next1, u_next2,
                f_now, f_next,
                h, tau, a, beta){
  p1 <- (u_now2 - 2 * u_now1 + u_now0 - ((1i * 2 * (h^2)) / tau) * u_now1)
  p2 <- (1i * a^2 * ((u_next2 - 2 * u_next1 + u_next0) + (u_now2 - 2 * u_now1 + u_now0)))
  p3 <- (1i * h * beta * (1/2) * (((abs(u_next2))^2) * u_next2 - ((abs(u_next0))^2) * u_next0 + 
                               ((abs(u_now2))^2) * u_now2 - ((abs(u_now0))^2) * u_now0))
  p4 <- (1i * (h^2) * (f_next + f_now))
  
  out <- p1 - p2 - p3 - p4
  return(out)
}



# ------------------------------------------------
U_dest <- function(x,t){ (1+1i*t)*(sin(pi*x))^2}
U_test <- function(x,t){(exp(1)^t - 1i * t^2 * x)*(x^2)*(1-x)*(t^2 - 1i*sqrt(1i))*sin(pi*x)}
