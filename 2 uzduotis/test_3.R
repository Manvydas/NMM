if (save.txt) {
  nm <- file("log_test_3.txt")
  sink(nm)
}


j_t3 <- j
t_t3 <- 1.58
a_t3 <- a
beta_t3 <- beta
tau_t3 <- tau
delta_t3 <- delta
N_t3 <- N
h_t3 <- 1/N_t3
C_t3 <- C(h_t3, a_t3, tau_t3)

a_12_t3 <- a_12
b_12_t3 <- b_12

u_now_t3 <- u_exact(j_t3, t_t3)

u_next_old_t3 <- u_now_t3
u_next_new_t3 <- c()
f_now_t3 <- f_x_t(j_t3, t_t3, a_t3, beta_t3)
f_next_t3 <- f_x_t(j_t3, t_t3 + tau_t3, a_t3, beta_t3)

# in order to save some resources alpha calculated before while cycle
### --- Thomas
# 1. alpha_1 and beta_1
a_v_t3 <- rep(0, (N_t3))
# b_v_t3 <- rep(0, (N_t3))
a_v_t3[1] <- a_12_t3[1]
# b_v_t3[1] <- b_12_t3[1]

# 2. calculate alpha, beta vectors
for (i in 1:(N_t3-1)){
  a_v_t3[i+1] <- 1 / (C_t3 - a_v_t3[i])
  # b_v_t3[i+1] <- (b_v_t3[i] + F_j_t3[i]) * a_v_t3[i+1]
}

stop_t3 <- FALSE

while(!stop_t3){
  F_j_t3 <- F_j_val(u_now_t3, u_next_old_t3, f_now_t3, f_next_t3, h_t3, tau_t3, a_t3, beta_t3)
  
  ### --- Thomas
  # 1. alpha_1 and beta_1
  # a_v_t3 <- rep(0, (N_t3))
  b_v_t3 <- rep(0, (N_t3))
  # a_v_t3[1] <- a_12_t3[1]
  b_v_t3[1] <- b_12_t3[1]
  
  # 2. calculate alpha, beta vectors
  for (i in 1:(N_t3-1)){
    # a_v_t3[i+1] <- 1 / (C_t3 - a_v_t3[i])
    b_v_t3[i+1] <- (b_v_t3[i] + F_j_t3[i]) * a_v_t3[i+1]
  }
  
  # 3. y_N value
  y <- rep(0i, N_t3+1)
  y[N_t3+1] = (a_12_t3[2] * b_v_t3[N_t3] + b_12_t3[2]) / (1 - a_12_t3[2] * a_v_t3[N_t3])
  
  # 4. calculate y_n vector
  for (i in (N_t3):1){
    y[i] <- a_v_t3[i] * y[i+1] + b_v_t3[i] # alpha, beta vektoriu ilgis trumpesnis, del to naudojama ju ***[i] reiksmes, o ne ***[i+1]
  }
  
  u_next_new_t3 <- y
  
  print(max(abs(u_next_old_t3 - u_next_new_t3)) )
  
  if (max(abs(u_next_old_t3 - u_next_new_t3)) < delta) {
    stop_t3 <- TRUE
  }
  
  u_next_old_t3 <- y
  
}

###### T3
# 1.
y_dat <- f_test(j)

# 2.
F_dat <- c()
for (i in 2:(N_t3)) {
  F_dat[i-1] <- C_t3 * y_dat[i] - y_dat[i+1] - y_dat[i-1]
}

# 3.
stop_t3 <- FALSE

while(!stop_t3){
  # F_j_t3 <- F_j_val(u_now_t3, u_next_old_t3, f_now_t3, f_next_t3, h_t3, tau_t3, a_t3, beta_t3)
  
  ### --- Thomas
  # 1. alpha_1 and beta_1
  a_v_t3 <- rep(0, (N_t3))
  b_v_t3 <- rep(0, (N_t3))
  a_v_t3[1] <- a_12_t3[1]
  b_v_t3[1] <- b_12_t3[1]
  
  # 2. calculate alpha, beta vectors
  for (i in 1:(N_t3-1)){
    a_v_t3[i+1] <- 1 / (C_t3 - a_v_t3[i])
    b_v_t3[i+1] <- (b_v_t3[i] + F_dat[i]) * a_v_t3[i+1]
  }
  
  # 3. y_N value
  y <- rep(0i, N_t3+1)
  y[N_t3+1] = (a_12_t3[2] * b_v_t3[N_t3] + b_12_t3[2]) / (1 - a_12_t3[2] * a_v_t3[N_t3])
  
  # 4. calculate y_n vector
  for (i in (N_t3):1){
    y[i] <- a_v_t3[i] * y[i+1] + b_v_t3[i] # alpha, beta vektoriu ilgis trumpesnis, del to naudojama ju ***[i] reiksmes, o ne ***[i+1]
  }
  
  u_next_new_t3 <- y
  
  print(max(abs(y_dat - u_next_new_t3)) )
  
  if (max(abs(y_dat - u_next_new_t3)) < delta) {
    stop_t3 <- TRUE
  }
  
  u_next_old_t3 <- y
  
}








sink()