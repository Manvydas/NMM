if (save.txt) {
  nm <- file("log_test_2.txt")
  sink(nm)
}

# --- Backward Error (T2) -----
# --- C, F_j = ?? ---
# _t2 --> Test 2
#|- prepare values -----
x_t2 <- 0.79
t_t2 <- 1.58
a_t2 <- 17 # a
beta_t2 <- 7 # beta
h_t2 <- 0.1 # h
tau_t2 <- 0.1 # tau
step_t2 <- 3 # times to lower h and tau

back_err_t2 <- c() # empty string

for (i in 1:10) {
  if (i==1) cat(" --- TEST 2 ---", sep = "\n")

  #|- setting up the values -----
  u_now0 <- u_exact(x_t2 - h_t2, t_t2)
  u_now1 <- u_exact(x_t2, t_t2)
  u_now2 <- u_exact(x_t2 + h_t2, t_t2)
  u_next0 <- u_exact(x_t2 - h_t2, t_t2 + tau_t2)
  u_next1 <- u_exact(x_t2, t_t2 + tau_t2)
  u_next2 <- u_exact(x_t2 + h_t2, t_t2 + tau_t2)
  f_now <- f_x_t(x_t2, t_t2, a_t2, beta_t2)
  f_next <- f_x_t(x_t2, t_t2 + tau_t2, a_t2, beta_t2)
  C_t2 <- C(h_t2, a_t2, tau_t2)
  
  F_t2 <- F_j(u_now0, u_now1, u_now2,
               u_next0, u_next1, u_next2,
               f_now, f_next,
               h_t2, tau_t2, a_t2, beta_t2)
  
  #|- calculate backward error -----
  back_err_t2[i] <- abs(u_next2 - (C_t2 * u_next1) + u_next0 + F_t2)
  # print(back_err_t2)
  
  #|- print results to console -----
  cat(paste0("\n - h: ", h_t2, ", tau: ", tau_t2, "\n",
             " - Backward Error: ", back_err_t2[i], "\n"))
  if(i>1) cat(paste0(" - Ratio: ", back_err_t2[i-1] / back_err_t2[i], "\n"))
  
  #|- prepare parameters for next cycle -----
  h_t2 <- h_t2 / step_t2
  tau_t2 <- tau_t2 / step_t2
}

if (save.txt) sink()