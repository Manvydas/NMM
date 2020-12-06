library(Deriv)
library(data.table)

source("constants.R")
source("functions.R")

# save console's output to .txt
save.txt <- FALSE

# TEST 1
source("test_1.R")

# TEST 2
source("test_2.R")



# Deriv::Deriv(U_exact, "x")
# Deriv::Deriv(U_exact, "t")
# 
# Deriv::Deriv(Deriv::Deriv(U_exact, "x"), "x")

# |U_exact|^2
plot((abs(U_exact(x = seq(0,1,0.01), t = seq(0,1,0.01))))^2, type="l")