# Octonions multiplication
oct_mult <- function(x, y){
  out <- NULL
  out[1] = +x[1]*y[1] -x[2]*y[2] -x[3]*y[3] -x[4]*y[4] -x[5]*y[5] -x[6]*y[6] -x[7]*y[7] -x[8]*y[8]
  out[2] = +x[2]*y[1] +x[1]*y[2] -x[4]*y[3] +x[3]*y[4] -x[6]*y[5] +x[5]*y[6] +x[8]*y[7] -x[7]*y[8]
  out[3] = +x[3]*y[1] +x[4]*y[2] +x[1]*y[3] -x[2]*y[4] -x[7]*y[5] -x[8]*y[6] +x[5]*y[7] +x[6]*y[8]
  out[4] = +x[4]*y[1] -x[3]*y[2] +x[2]*y[3] +x[1]*y[4] -x[8]*y[5] +x[7]*y[6] -x[6]*y[7] +x[5]*y[8]
  out[5] = +x[5]*y[1] +x[6]*y[2] +x[7]*y[3] +x[8]*y[4] +x[1]*y[5] -x[2]*y[6] -x[3]*y[7] -x[4]*y[8]
  out[6] = +x[6]*y[1] -x[5]*y[2] +x[8]*y[3] -x[7]*y[4] +x[2]*y[5] +x[1]*y[6] +x[4]*y[7] -x[3]*y[8]
  out[7] = +x[7]*y[1] -x[8]*y[2] -x[5]*y[3] +x[6]*y[4] +x[3]*y[5] -x[4]*y[6] +x[1]*y[7] +x[2]*y[8]
  out[8] = +x[8]*y[1] +x[7]*y[2] -x[6]*y[3] -x[5]*y[4] +x[4]*y[5] +x[3]*y[6] -x[2]*y[7] +x[1]*y[8]
  return(out)
}

# Quaternions multiplication
quat_mult <- function(x, y){
  out <- NULL
  out[1] = +x[1]*y[1] -x[2]*y[2] -x[3]*y[3] -x[4]*y[4]
  out[2] = +x[2]*y[1] +x[1]*y[2] -x[4]*y[3] +x[3]*y[4]
  out[3] = +x[3]*y[1] +x[4]*y[2] +x[1]*y[3] -x[2]*y[4]
  out[4] = +x[4]*y[1] -x[3]*y[2] +x[2]*y[3] +x[1]*y[4]
  return(out)
}

# Module of octonion/quaternion
nion_mod <- function(x){
  out <- sqrt(sum(x^2))
  return(out)
}

# Function to run all Z_n iterations and calculate color code
nion_col <- function(z_0=c(0.1,0,0,0),
                     r_ne_0 = c(T,T,F,F),
                     x_min = -2,
                     x_max = 4,
                     y_min = -1.5,
                     y_max = 1.5,
                     rez_x = 1024,
                     rez_y = 768,
                     max_it_sk = 256,
                     begalybe = Inf,
                     fz = function(z,r){na.omit(quat_mult(z, z) + r)}
){
  
  n_iter <- rez_x * rez_y
  counter <- 0
  
  dat <- data.table()
  rez_x_sk <- seq(x_min, x_max, length.out = rez_x)
  rez_y_sk <- seq(y_min, y_max, length.out = rez_y)
  r <- rep(0, 4)
  
  for (iter1 in rez_x_sk) {
    for (iter2 in rez_y_sk) {
      
      r[r_ne_0] <- c(iter1, iter2)
      
      z <- list()
      z[[1]] <- z_0
      it_nr <- 1
      col_code <- 0
      
      while (it_nr <= 256 & nion_mod(z[[it_nr]]) < begalybe) {
        z[[it_nr+1]] <- fz(z[[it_nr]], r)
        if (nion_mod(z[[it_nr]]) <= nion_mod(z[[it_nr+1]])){
          col_code <- col_code + 1
        }
        it_nr <- it_nr + 1
      }
      
      # creating data frame with coordinates and color code ant iteration number
      counter <- counter + 1
      dat_temp <- data.table(x = iter1, y = iter2, col_val = col_code, iteration = counter)
      dat <- rbind(dat, dat_temp)
      
      # Percentage of code already done
      # print(paste0(counter, " iteration of ", n_iter, ". ", round(counter*100/(n_iter), 2), "% done."))
      
    }
  }
  return(dat)
}

# Plot fractal
nion_plot <- function(dat,
                      x_min = -2,
                      x_max = 4,
                      y_min = -1.5,
                      y_max = 1.5){
  
  axis_x <- round(seq(x_min, x_max, by = 0.5),1)
  axis_y <- round(seq(y_min, y_max, by = 0.5),1)
  
  gg_nion <- ggplot(dat, aes(x=dat$x, y=dat$y)) + geom_point(color = grey.colors(256)[dat$col_val]) # geom_point(color = colorRampPalette(coul)(256)[dat$col_val])
  gg_nion <- gg_nion + theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())  +
    
    # grid
    geom_vline(xintercept = 0, linetype="dashed", color = "red", size = 0.3) +
    geom_hline(yintercept = 0, linetype="dashed", color = "red", size = 0.3) +
    
    geom_vline(xintercept = axis_x[axis_x != 0], size = 0.1) +
    geom_hline(yintercept = axis_y[axis_y != 0], size = 0.1) +
    scale_size_manual(values = c(0.001, 0.01)) +
    
    # names
    ggtitle("Plot of quaternions iterations") +
    xlab("x scale") + ylab("y scale") +
    
    # ticks
    scale_x_continuous(breaks = axis_x) +
    scale_y_continuous(breaks = axis_y)
  
  return(gg_nion)
}

