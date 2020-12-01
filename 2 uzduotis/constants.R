a <- 45 # konstanta
beta <- 45 # konstanta, naudojama

N <- 100 # erdves rezoliucija
h <- (1/N) # zingsnis erdveje
j <- seq(from = 0, to = 1, by = h)

tau <- 0.01 # zingsnis laike
T_ <- 2 #3 sek pvz
t <- seq(from = 0, to = T_, by = tau)


# N <- 100 # rezoliucija
# h <- (1/N) # zingsnis erdveje
# j <- seq(from = x_start-0.1, to = x_start+0.1, by = h)
h_10 <- (1/N)/10 # zingsnis erdveje
j_10 <- seq(from = x_start-h_10, to = x_start+h_10, by = h_10)

# tau <- 0.01 # zingsnis laike
# T_ <- 2 #3 sek pvz
# t <- seq(from = t_start-0.1, to = t_start+0.1, by = tau)
tau_10 <- tau/10 # zingsnis laike
t_10 <- seq(from = t_start-tau_10, to = t_start+tau_10, by = tau_10)
