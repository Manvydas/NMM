# --- Backward Error (T1) ---
# _t1 --> Test 1
x_t1 <- 0.79
t_t1 <- 1.58
h_t1 <- h
tau_t1 <- tau
back_err <- c() # empty string
step_t1 <- 5 # times to lower h and tau

for (i in 1:100) {
  
  # setting up the values
  u_now0 <- u_exact(x_t1 - h_t1, t_t1)
  u_now1 <- u_exact(x_t1, t_t1)
  u_now2 <- u_exact(x_t1 + h_t1, t_t1)
  u_next0 <- u_exact(x_t1 - h_t1, t_t1 + tau_t1)
  u_next1 <- u_exact(x_t1, t_t1 + tau_t1)
  u_next2 <- u_exact(x_t1 + h_t1, t_t1 + tau_t1)
  f_now <- u_exact(x_t1, t_t1)
  f_next <- u_exact(x_t1, t_t1 + tau_t1)
  
  # left part of alghorithm
  left <- (u_next1 - u_now1) / tau_t1
  
  # right part of alghorithm
  right11 <- (u_next2 - 2 * u_next1 + u_next0) / h^2
  right12 <- (u_now2 - 2 * u_now1 + u_now0) / h^2
  right1 <- (a^2 + 1i) * (1/2) * (right11 + right12)
  right21 <- (((abs(u_next2))^2) * u_next2 - ((abs(u_next0))^2) * u_next0) / (2 * h_t1)
  right22 <- (((abs(u_now2))^2) * u_now2 - ((abs(u_now0))^2) * u_now0) / (2 * h_t1)
  right2 <- beta * (1/2) * (right21 + right22)
  right3 <- (f_next + f_now) / 2
  right <- right1 + right2 + right3
  
  back_err[i] <- abs(left - right)
  
  # Backward Error
  cat(paste0("\n - h: ", h_t1, ", tau: ", tau_t1, "\n",
             " - Backward Error: ", back_err[i], "\n"))
  if(i>1) cat(paste0(" - Ratio: ", back_err[i-1] / back_err[i], "\n"))
  
  # parameters prepartion for next cycle
  h_t1 <- h_t1 / step_t1
  tau_t1 <- tau_t1 / step_t1
}