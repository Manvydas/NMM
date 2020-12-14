if (save.txt) {
nm <- file("log_test_1.txt")
sink(nm)
}

# --- Backward Error (T1) -----
# _t1 --> Test 1
#|- prepare values -----
x_t1 <- 0.79
t_t1 <- 1.58
a_t1 <- 7 # a
beta_t1 <- 7 # beta
h_t1 <- 0.1 # h
tau_t1 <- 0.1 # tau
step_t1 <- 3 # times to lower h and tau

back_err_t1 <- c() # empty string

for (i in 1:10) {
  if (i==1) cat(" --- TEST 1 ---", sep = "\n")
  
  #|- setting up the values -----
  u_now0 <- u_exact(x_t1 - h_t1, t_t1)
  u_now1 <- u_exact(x_t1, t_t1)
  u_now2 <- u_exact(x_t1 + h_t1, t_t1)
  u_next0 <- u_exact(x_t1 - h_t1, t_t1 + tau_t1)
  u_next1 <- u_exact(x_t1, t_t1 + tau_t1)
  u_next2 <- u_exact(x_t1 + h_t1, t_t1 + tau_t1)
  f_now <- f_x_t(x_t1, t_t1, a_t1, beta_t1)
  f_next <- f_x_t(x_t1, t_t1 + tau_t1, a_t1, beta_t1)
  
  #|- left part of alghorithm -----
  left <- (u_next1 - u_now1) / tau_t1
  
  #|- right part of alghorithm -----
  right11 <- (u_next2 - 2 * u_next1 + u_next0) / h^2
  right12 <- (u_now2 - 2 * u_now1 + u_now0) / h^2
  right1 <- (a_t1^2 + 1i) * (1/2) * (right11 + right12)
  right21 <- (((abs(u_next2))^2) * u_next2 - ((abs(u_next0))^2) * u_next0) / (2 * h_t1)
  right22 <- (((abs(u_now2))^2) * u_now2 - ((abs(u_now0))^2) * u_now0) / (2 * h_t1)
  right2 <- beta_t1 * (1/2) * (right21 + right22)
  right3 <- (f_next + f_now) / 2
  right <- right1 + right2 + right3
  
  #|- calculate backward error -----
  back_err_t1[i] <- abs(left - right)
  
  #|- print results to console -----
  cat(paste0("\n - h: ", h_t1, ", tau: ", tau_t1, "\n",
             " - Backward Error: ", back_err_t1[i], "\n"))
  if(i>1) cat(paste0(" - Ratio: ", back_err_t1[i-1] / back_err_t1[i], "\n"))
  
  #|- prepare parameters for next cycle -----
  h_t1 <- h_t1 / step_t1
  tau_t1 <- tau_t1 / step_t1
}

if (save.txt) sink()