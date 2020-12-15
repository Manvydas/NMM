# start log file
if (save.txt) {
  nm <- file("logs/log_test_4.txt")
  sink(nm)
  Sys.time()
}

# --- Global test -----
# _t4 --> Test Global
#|-- prepare values -----
N_t4 <- 100
h_t4 <- 1/N_t4
j_t4 <- seq(from = 0, to = 1, by = h_t4)
T_max_t4 <- 0.5
a_t4 <- 0
beta_t4 <- 0
tau_t4 <- 0.01
delta_t4 <- delta

# C_t4 <- C(h_t4, a_t4, tau_t4)
step_t4 <- 2 # times to lower h and tau

# Dirichlet boundary conditions (krastines salygos) (I)
a_12_t4 <- a_12
b_12_t4 <- b_12

error_max <- NULL
for (ie in 1:7) {
  if (ie==1) cat(" --- GLOBAL TEST ---", sep = "\n")
  
  # set up h, j values for the new cycle
  h_t4 <- 1/N_t4
  j_t4 <- seq(from = 0, to = 1, by = h_t4)
  
  C_t4 <- C(h_t4, a_t4, tau_t4)
  
  # start main while cycle from 0 to T_max
  error_t4 <- NULL
  ind_t4 <- 1
  t_t4 <- 0 # starting time point
  u_now_t4 <- u_exact(j_t4, t_t4)
  
  while (t_t4 < T_max_t4) {
    # u_exact vector for fixed t
    # u_now_t4 <- u_exact(j_t4, t_t4)
    
    # starting values for while cycle
    u_next_old_t4 <- u_now_t4
    
    # f(x, t) vectors for fixed t
    f_now_t4 <- f_x_t(j_t4, t_t4, a_t4, beta_t4)
    f_next_t4 <- f_x_t(j_t4, t_t4 + tau_t4, a_t4, beta_t4)
    
    # in order to save some resources alpha calculated before while cycle
    # 1. alpha_1
    a_v_t4 <- rep(0, (N_t4))
    a_v_t4[1] <- a_12_t4[1]
    
    # 2. calculate alpha
    for (i in 1:(N_t4-1)){
      a_v_t4[i+1] <- 1 / (C_t4 - a_v_t4[i])
    }
    
    # Starting T3 while cycle
    stop_t4 <- FALSE
    while(!stop_t4){
      F_j_t4 <- F_j_val(u_now_t4, u_next_old_t4, f_now_t4, f_next_t4, h_t4, tau_t4, a_t4, beta_t4)
      
      # Thomas algorithm
      y <- Thomas(N_t4, b_12_t4, F_j_t4, a_v_t4, a_12_t4)
      u_next_new_t4 <- y
      
      # print(max(abs(u_next_old_t4 - u_next_new_t4)) )
      
      # error check for fixed t
      if (max(abs(u_next_old_t4 - u_next_new_t4)) < delta_t4) {
        stop_t4 <- TRUE
      }
      
      # set up the values for the next cycle
      u_next_old_t4 <- y
    }
    u_now_t4 <- u_next_old_t4
    # saving maximum error for each t
    error_t4[ind_t4] <- max(abs(u_next_old_t4 - u_exact(j_t4, t_t4+tau_t4)))
    # cat(paste0(ind_t4, ": ", max(abs(u_now_t4 - u_next_old_t4)), "\n\n"))
    
    # prepare values for next cycle
    ind_t4 <- ind_t4 + 1
    t_t4 <- t_t4 + tau_t4
  }
  
  error_max[ie] <- max(error_t4)
  
  #|- print results to console -----
  cat(paste0("\n - h: ", h_t4, ", tau: ", tau_t4, "\n",
             " - Global maximum Error: ", error_max[ie], "\n"))
  if(ie>1) cat(paste0(" - Ratio: ", error_max[ie-1] / error_max[ie], "\n"))
  
  # prepare parameters for the next main cycle
  N_t4 <- N_t4 * step_t4
  tau_t4 <- tau_t4 / step_t4
}


# close log file
if (save.txt) sink()