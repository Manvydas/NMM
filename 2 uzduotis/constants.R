a <- 7 # konstanta
beta <- 7 # konstanta, naudojama
a_12 <- c(0, 0)
b_12 <- c(0, 0)

N <- 10 # erdves rezoliucija
h <- (1/N) # zingsnis erdveje
j <- seq(from = 0, to = 1, by = h)

tau <- 0.1 # zingsnis laike
T_max <- 3 #3 sek pvz
t <- seq(from = 0, to = T_max, by = tau)

delta <- 10^(-8) # constant for T3 