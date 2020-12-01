u_exact <- function(x, t){
  x*(x-1)*(1i+t)*cos(pi*t^2)
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

u_exact_0 <- function(x){
  x*(x-1)*(1i)
}

f_x_t <- function(x, t, a, beta){
  u_exact_d_t(x, t) -
    (a^2 + 1i) * u_exact_d_d_x(x, t) -
    beta * u_exact_d_x(x, t) * (((abs(u_exact(x, t)))^2) * u_exact(x, t))
}


# ------------------------------------------------
U_dest <- function(x,t){ (1+1i*t)*(sin(pi*x))^2}
U_test <- function(x,t){(exp(1)^t - 1i * t^2 * x)*(x^2)*(1-x)*(t^2 - 1i*sqrt(1i))*sin(pi*x)}
