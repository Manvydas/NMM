# --- Backward Error (T2) ---
# --- C, F_j = ?? ---
# _t2 --> Test 2
a_t2 <- 0 # a
beta_t2 <- 5 # beta
x_t2 <- 0.79
t_t2 <- 1.58
h_t2 <- 0.1 # h
tau_t2 <- 0.1 # tau
back_err_t2 <- c() # empty string
step_t2 <- 10 # times to lower h and tau

C_t2 <- 2 - ((2 * h_t2^2 * 1i) / tau_t2)

for (i in 1:10) {
  
  # setting up the values
  u_now0 <- u_exact(x_t2 - h_t2, t_t2)
  u_now1 <- u_exact(x_t2, t_t2)
  u_now2 <- u_exact(x_t2 + h_t2, t_t2)
  u_next0 <- u_exact(x_t2 - h_t2, t_t2 + tau_t2)
  u_next1 <- u_exact(x_t2, t_t2 + tau_t2)
  u_next2 <- u_exact(x_t2 + h_t2, t_t2 + tau_t2)
  f_now <- f_x_t(x_t2, t_t2, a_t2, beta_t2)
  f_next <- f_x_t(x_t2, t_t2 + tau_t2, a_t2, beta_t2)
  
  F_val <- F_j(u_now0, u_now1, u_now2,
               u_next0, u_next1, u_next2,
               f_now, f_next,
               h_t2, tau_t2, a_t2, beta_t2)
  
  back_err_t2[i] <- abs(u_next2 - C_t2 * u_next1 + u_next0 - F_val)
  # print(back_err_t2)
  
  # Backward Error
  cat(paste0("\n - h: ", h_t2, ", tau: ", tau_t2, "\n",
             " - Backward Error: ", back_err_t2[i], "\n"))
  if(i>1) cat(paste0(" - Ratio: ", back_err_t2[i-1] / back_err_t2[i], "\n"))
  
  # parameters prepartion for next cycle
  h_t2 <- h_t2 / step_t2
  tau_t2 <- tau_t2 / step_t2
}
