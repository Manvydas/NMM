# start log file
if (save.txt) {
  nm <- file("logs/log_test_3.txt")
  sink(nm)
  Sys.time()
}
cat(" --- TEST 3 ---", sep = "\n")

# --- (T3) -----
# _t3 --> Test 3
#|-- prepare values -----
a_t3 <- a
tau_t3 <- tau
N_t3 <- N
h_t3 <- 1/N_t3
j_t3 <- seq(from = 0, to = 1, by = h_t3)
C_t3 <- C(h_t3, a_t3, tau_t3)

#|-- Dirichlet boundary conditions (krastines salygos) (I) -----
a_12_t3 <- a_12
b_12_t3 <- b_12

#|-- create y vector -----
y_dat_t3 <- f_test(j_t3)

#|-- calculate F_j vector -----
F_dat_t3 <- NULL
for (i in 2:(N_t3)) {
  F_dat_t3[i-1] <- C_t3 * y_dat_t3[i] - y_dat_t3[i+1] - y_dat_t3[i-1]
}

#|-- 1. alpha_1 -----
a_v_t3 <- rep(0, (N_t3))
a_v_t3[1] <- a_12_t3[1]

#|-- 2. calculate alpha vector -----
for (i in 1:(N_t3-1)){
  a_v_t3[i+1] <- 1 / (C_t3 - a_v_t3[i])
}

#|-- Thomas algorithm -----
y <- Thomas(N_t3, b_12_t3, F_dat_t3, a_v_t3, a_12_t3)

#|-- print results (error value) -----
cat(paste0("\n", "T3: ", max(abs(y_dat_t3 - y)), "\n"))



# close log file
if (save.txt) sink()

