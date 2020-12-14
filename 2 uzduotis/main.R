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

# TEST 3
source("test_3.R")

# Global TEST
source("global.R")

# |U_exact|^2
plot(seq(0,1,0.01),
     (abs(u_exact(x = seq(0, 1, 0.01), t = 1.58)))^2,
     type= "l", ylab = "|U_exact|^2", xlab = "x value", main = "|U_exact|^2, t = 1.58")
