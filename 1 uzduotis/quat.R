library(data.table)
library(ggplot2)
# load("nion_functions.R")

# z_n+1 = (z_n)^2 + r
f1 <- function(z, r){
  na.omit(quat_mult(z, z) + r)
}

# Parameters
x_min <- -2
x_max <- 4
y_min <- -1.5
y_max <- 1.5
rez_x <- 1024
rez_y <- 768
# begalybe <- 100
max_it_sk <- 256
z_0 <- c(0.1, 0, 0, 0)

# Color codes calculation
r_13 <- c(T,F,T,F)
r_23 <- c(F,T,T,F)

t1 <- Sys.time()
col_r13 <- nion_col(z_0 = z_0, r_ne_0 = r_13,
                    x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max,
                    rez_x = rez_x, rez_y = rez_y, max_it_sk = max_it_sk,
                    fz = f1)
t2 <- Sys.time()
col_r23 <- nion_col(z_0 = z_0, r_ne_0 = r_23,
                    x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max,
                    rez_x = rez_x, rez_y = rez_y, max_it_sk = max_it_sk,
                    fz = f1)
t3 <- Sys.time()
# r_1, r_3 != 0
# Time difference of 3.768826 hours
# Time difference of 3.032261 hours
t2-t1
# r_2, r_3 != 0
# Time difference of 2.43924 hours
# Time difference of 2.054715 hours
t3-t2

# Plotting fractals
plot_r13 <- nion_plot(col_r13, x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max)
plot_r13

plot_r23 <- nion_plot(col_r23, x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max)
plot_r23

