if (save.txt) {
  nm <- file("log_test_3.txt")
  sink(nm)
  cat(" --- TEST 3 ---", sep = "\n")
}

# --- (T3) -----
# _t3 --> Test 3
#|-- prepare values -----
j_t3 <- j
t_t3 <- 1.58
a_t3 <- a
beta_t3 <- beta
tau_t3 <- tau
delta_t3 <- delta
N_t3 <- N
h_t3 <- 1/N_t3
C_t3 <- C(h_t3, a_t3, tau_t3)

# Dirichlet boundary conditions (krastines salygos) (I)
a_12_t3 <- a_12
b_12_t3 <- b_12

##|-- T3 u_exact ---------------------------------------------------------------
# u_exact vector for fixed t
u_now_t3 <- u_exact(j_t3, t_t3)

# starting values for while cycle
u_next_old_t3 <- u_now_t3
u_next_new_t3 <- NULL

# f(x, t) vectors for fixed t
f_now_t3 <- f_x_t(j_t3, t_t3, a_t3, beta_t3)
f_next_t3 <- f_x_t(j_t3, t_t3 + tau_t3, a_t3, beta_t3)

# in order to save some resources alpha calculated before while cycle
# 1. alpha_1
a_v_t3 <- rep(0, (N_t3))
a_v_t3[1] <- a_12_t3[1]

# 2. calculate alpha vector
for (i in 1:(N_t3-1)){
  a_v_t3[i+1] <- 1 / (C_t3 - a_v_t3[i])
}

# Starting T3 while cycle
stop_t3 <- FALSE
while(!stop_t3){
  F_j_t3 <- F_j_val(u_now_t3, u_next_old_t3, f_now_t3, f_next_t3, h_t3, tau_t3, a_t3, beta_t3)
  
  # Thomas algorithm
  y <- Thomas(N_t3, b_12_t3, F_j_t3, a_v_t3, a_12_t3)
  u_next_new_t3 <- y
  
  # print(max(abs(u_next_old_t3 - u_next_new_t3)) )
  
  # error check
  if (max(abs(u_next_old_t3 - u_next_new_t3)) < delta_t3) {
    stop_t3 <- TRUE
  }
  
  # set up the values for the next cycle
  u_next_old_t3 <- y
}

##|-- T3 real ------------------------------------------------------------------
# create y vector
y_dat_t3 <- f_test(j_t3)

# create F_j vector
F_dat_t3 <- c()
for (i in 2:(N_t3)) {
  F_dat_t3[i-1] <- C_t3 * y_dat_t3[i] - y_dat_t3[i+1] - y_dat_t3[i-1]
}

# 1. alpha_1
a_v_t3 <- rep(0, (N_t3))
a_v_t3[1] <- a_12_t3[1]

# 2. calculate alpha vector
for (i in 1:(N_t3-1)){
  a_v_t3[i+1] <- 1 / (C_t3 - a_v_t3[i])
}

# Starting T3 while cycle
stop_t3 <- FALSE
while(!stop_t3){
  # Thomas algorithm
  y <- Thomas(N_t3, b_12_t3, F_dat_t3, a_v_t3, a_12_t3)
  u_next_new_t3 <- y
  
  # print error value
  cat(paste0("\n", "T3: ", max(abs(y_dat_t3 - u_next_new_t3)), "\n"))
  
  # error check
  if (max(abs(y_dat_t3 - u_next_new_t3)) < delta_t3) {
    stop_t3 <- TRUE
  }
  
  # set up the values for the next cycle
  u_next_old_t3 <- y
}

if (save.txt) sink()

