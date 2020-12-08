if (save.txt) {
  nm <- file("log_test_3.txt")
  sink(nm)
}

# --- (T3) ---
# _t3 --> Test 3
N_t3 <- N # 100
h_t3 <- h # (1/N_t3)
j_t3 <- j # seq(from = 0, to = 1, by = h_t3)
tau_t3 <- tau
a_12_t3 <- a_12
b_12_t3 <- b_12
delta <- 10^(-8)

# x_t3 <- 0.79
t_t3 <- 1.58


f_now <- f_x_t(j_t3, t_t3, a, beta)
f_next <- f_x_t(j_t3, t_t2 + tau_t3, a, beta)

#
# F_t3 <- F_j_val(j_t3, h_t3, t_t3, tau_t3, a, beta)
C_t3 <- C(h_t3, tau_t3)

# 1. alpha_1 and beta_1
a_t3 <- rep(0, (N_t3-1))
b_t3 <- rep(0, (N_t3-1))
a_t3[1] <- a_12_t3[1]
b_t3[1] <- b_12_t3[1]

# 2. calculate alpha, beta vectors
for (i in 1:(N_t3-1)){
  a_t3[i+1] <- 1 / (C_t3 - a_t3[i])
  b_t3[i+1] <- (b_t3[i] + F_t3[i]) * a_t3[i+1]
}

# 3. y_N value
y <- rep(0, (N_t3))
y[N_t3] = (a_12_t3[2] * b_t3[N_t3] + b_12_t3[2]) / (1 - a_12_t3[2] * a_t3[N_t3])

# 4. calculate y_n vector
for (i in (N_t3-1):1){
  y[i] <- a_t3[i+1] * y[i+1] + b_t3[i+1]
}

#############################################

u_j_new <- NULL
stop_t3 <- FALSE
u_now_val <- u_exact(j_t3, t_t3)
u_next_val <- u_now_val
## while (t<T)
while (!stop_t3) {
  # reikia perdaryti F reiksmiu vektoriu, kad galima butu pateikti u_j_new, u_j_old vektorius
  
  F_j_vall <- F_j_val2(u_now_val, u_next_val, f_now, f_next,
                       h_t3, tau_t3, a_t3, beta_t3)
  C_t3 <- C(h_t3, tau_t3)
  
  # 1. alpha_1 and beta_1
  a_t3 <- rep(0, (N_t3-1))
  b_t3 <- rep(0, (N_t3-1))
  a_t3[1] <- a_12_t3[1]
  b_t3[1] <- b_12_t3[1]
  
  # 2. calculate alpha, beta vectors
  for (i in 1:(N_t3-1)){
    a_t3[i+1] <- 1 / (C_t3 - a_t3[i])
    b_t3[i+1] <- (b_t3[i] + F_j_vall[i]) * a_t3[i+1]
  }
  
  # 3. y_N value
  y <- rep(0, (N_t3))
  y[N_t3] = (a_12_t3[2] * b_t3[N_t3] + b_12_t3[2]) / (1 - a_12_t3[2] * a_t3[N_t3])
  
  # 4. calculate y_n vector
  for (i in (N_t3-1):1){
    y[i] <- a_t3[i+1] * y[i+1] + b_t3[i+1]
  }
  
  u_next_val_new <- y
  
  if (max(abs(u_next_val_new - y)) < delta/10000) {
    stop_t3 <- TRUE
  }
  u_next_val <- u_next_val_new
  
  # u_j_old <- u_j_new # ir perskaiciuojame visus F_j
  
  print(u_j_new)
} 









sink()