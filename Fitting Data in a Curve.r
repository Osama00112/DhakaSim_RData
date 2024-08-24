#plot(rnorm(50), rnorm(50))
#dev.off()
data = read.csv(file = "data.csv", header=TRUE)
library(ggplot2)
library(mclust)
library(mixtools)
library(flexmix)

freq_tab_stndng_ped = data.frame(value=data[1][!is.na(data[1])], freq=data[2][!is.na(data[2])])
freq_tab_parked_car = data.frame(value=data[3][!is.na(data[3])], freq=data[4][!is.na(data[4])])
freq_tab_parked_rck = data.frame(value=data[5][!is.na(data[5])], freq=data[6][!is.na(data[6])])
freq_tab_parked_cng = data.frame(value=data[7][!is.na(data[7])], freq=data[8][!is.na(data[8])])
freq_tab_walk_ped = data.frame(value=data[9][!is.na(data[9])], freq=data[10][!is.na(data[10])])

dist_stndng_ped = rep(freq_tab_stndng_ped$value, freq_tab_stndng_ped$freq)
dist_parked_car = rep(freq_tab_parked_car$value, freq_tab_parked_car$freq)
dist_parked_rck = rep(freq_tab_parked_rck$value, freq_tab_parked_rck$freq)
dist_parked_cng = rep(freq_tab_parked_cng$value, freq_tab_parked_cng$freq)
dist_walk_ped = rep(freq_tab_walk_ped$value, freq_tab_walk_ped$freq)


curve_function <- function(x, dist, G=NULL) {
  if (is.null(G)) {
    gmm_model <- Mclust(dist)  
  } else {
    gmm_model <- Mclust(dist, G = G) 
  }
  
  num_gaussians <- gmm_model$G
  mixing_proportions <- gmm_model$parameters$pro    # Mixing proportions (factors)
  means <- gmm_model$parameters$mean                # Means of each component
  sds <- sqrt(gmm_model$parameters$variance$sigmasq) # Standard deviations of each component
  
  # Create a summary table
  summary_table <- data.frame(
    Factor = round(mixing_proportions, 3),
    Mean = round(means, 2),
    `Standard Deviation` = round(sds, 3)
  )
  

  summary_table <- cbind(No. = 1:nrow(summary_table), summary_table)
  print(summary_table)
  
  result <- 0
  
  for (i in 1:num_gaussians) {
    if (length(sds) == 1) {
      # Use the same standard deviation for all Gaussians
      result <- result + mixing_proportions[[i]] * dnorm(x, mean = means[[i]], sd = sds)
    } else {
      # Use the corresponding standard deviation for each Gaussian
      result <- result + mixing_proportions[[i]] * dnorm(x, mean = means[[i]], sd = sds[[i]])
    }
  }
  
  return(result)
}

plot_gmm_mixtools <- function(dist, G = NULL) {
  # Fit a Gaussian Mixture Model to the provided distribution data
  if (is.null(G)) {
    gmm_model <- normalmixEM(dist)  # Let mixtools determine the optimal number of Gaussians
  } else {
    gmm_model <- normalmixEM(dist, k = G)  # Use the specified number of Gaussians
  }
  
  # Extract parameters
  num_gaussians <- length(gmm_model$lambda)
  mixing_proportions <- gmm_model$lambda
  means <- gmm_model$mu
  sds <- sqrt(gmm_model$sigma2)
  
  # Create a summary table
  summary_table <- data.frame(
    Factor = round(mixing_proportions, 3),
    Mean = round(means, 2),
    `Standard Deviation` = round(sds, 3)
  )
  
  # Add row numbers
  summary_table <- cbind(No. = 1:nrow(summary_table), summary_table)
  
  # Print the summary table
  print(summary_table)
  
  # Generate a sequence of x values for the curve
  x_values <- seq(min(dist), max(dist), length.out = 1000)
  
  # Initialize a data frame for the Gaussian mixture model
  gmm_df <- data.frame(x = x_values)
  
  # Add columns for each Gaussian component and the total sum
  for (i in 1:num_gaussians) {
    gmm_df[[paste0("Gaussian_", i)]] <- mixing_proportions[i] * dnorm(x_values, mean = means[i], sd = sds[i])
  }
  
  # Add a column for the total mixture density
  gmm_df$total_density <- rowSums(gmm_df[2:(num_gaussians + 1)])
  
  # Create the ggplot
  p <- ggplot() +
    geom_histogram(aes(x = dist, y = ..density..), bins = 30, fill = "grey", alpha = 0.5) +  # Plot the histogram of the data
    geom_line(data = gmm_df, aes(x = x, y = total_density), color = "red", size = 1) +       # Plot the sum of Gaussians
    geom_density(aes(x = dist), color = "blue", linetype = "dotted", size = 1) +             # Plot the KDE as a dotted line
    theme_minimal() +
    labs(title = "Gaussian Mixture Model vs Kernel Density Estimate",
         x = "Value",
         y = "Density")
  
  return(p)
}

plot_gmm_flexmix <- function(dist, G = 3) {
  # Fit a Gaussian Mixture Model to the provided distribution data
  gmm_model <- flexmix(dist ~ 1, k = G)  # Specify the number of components
  
  # Extract parameters
  num_gaussians <- length(parameters(gmm_model)[[1]]$pro)
  mixing_proportions <- parameters(gmm_model)[[1]]$pro
  means <- parameters(gmm_model)[[1]]$mean
  sds <- sqrt(parameters(gmm_model)[[1]]$variance$sigmasq)
  
  # Create a summary table
  summary_table <- data.frame(
    Factor = round(mixing_proportions, 3),
    Mean = round(means, 2),
    `Standard Deviation` = round(sds, 3)
  )
  
  # Add row numbers
  summary_table <- cbind(No. = 1:nrow(summary_table), summary_table)
  
  # Print the summary table
  print(summary_table)
  
  # Generate a sequence of x values for the curve
  x_values <- seq(min(dist), max(dist), length.out = 1000)
  
  # Initialize a data frame for the Gaussian mixture model
  gmm_df <- data.frame(x = x_values)
  
  # Add columns for each Gaussian component and the total sum
  for (i in 1:num_gaussians) {
    gmm_df[[paste0("Gaussian_", i)]] <- mixing_proportions[i] * dnorm(x_values, mean = means[i], sd = sds[i])
  }
  
  # Add a column for the total mixture density
  gmm_df$total_density <- rowSums(gmm_df[2:(num_gaussians + 1)])
  
  # Create the ggplot
  p <- ggplot() +
    geom_histogram(aes(x = dist, y = ..density..), bins = 30, fill = "grey", alpha = 0.5) +  # Plot the histogram of the data
    geom_line(data = gmm_df, aes(x = x, y = total_density), color = "red", size = 1) +       # Plot the sum of Gaussians
    geom_density(aes(x = dist), color = "blue", linetype = "dotted", size = 1) +             # Plot the KDE as a dotted line
    theme_minimal() +
    labs(title = "Gaussian Mixture Model vs Kernel Density Estimate",
         x = "Value",
         y = "Density")
  
  return(p)
}

plot_gmm <- function(dist, G = NULL) {
  # Fit a Gaussian Mixture Model to the provided distribution data
  if (is.null(G)) {
    gmm_model <- Mclust(dist)  # Let Mclust determine the optimal number of Gaussians
  } else {
    gmm_model <- Mclust(dist, G = G)  # Use the specified number of Gaussians
  }
  
  # Extract parameters from the fitted GMM model
  num_gaussians <- gmm_model$G
  mixing_proportions <- gmm_model$parameters$pro    # Mixing proportions (factors)
  means <- gmm_model$parameters$mean                # Means of each component
  sds <- sqrt(gmm_model$parameters$variance$sigmasq) # Standard deviations of each component
  
  # Create a summary table
  summary_table <- data.frame(
    Factor = round(mixing_proportions, 3),
    Mean = round(means, 2),
    `Standard Deviation` = round(sds, 3)
  )
  
  # Add row numbers
  summary_table <- cbind(No. = 1:nrow(summary_table), summary_table)
  
  # Print the summary table
  print(summary_table)
  
  # Generate a sequence of x values for the curve
  x_values <- seq(min(dist), max(dist), length.out = 1000)
  
  # Initialize a data frame for the Gaussian mixture model
  gmm_df <- data.frame(x = x_values)
  
  # Add columns for each Gaussian component and the total sum
  for (i in 1:num_gaussians) {
    if (length(sds) == 1) {
      # Use the same standard deviation for all Gaussians
      gmm_df[[paste0("Gaussian_", i)]] <- mixing_proportions[[i]] * dnorm(x_values, mean = means[[i]], sd = sds)
    } else {
      # Use the corresponding standard deviation for each Gaussian
      gmm_df[[paste0("Gaussian_", i)]] <- mixing_proportions[[i]] * dnorm(x_values, mean = means[[i]], sd = sds[[i]])
    }
  }
  
  # Add a column for the total mixture density
  gmm_df$total_density <- rowSums(gmm_df[2:(num_gaussians + 1)])
  
  # Create the ggplot
  p <- ggplot() +
    geom_histogram(aes(x = dist, y = ..density..), bins = 30, fill = "grey", alpha = 0.5) +  # Plot the histogram of the data
    geom_line(data = gmm_df, aes(x = x, y = total_density), color = "red", size = 1) +       # Plot the sum of Gaussians
    geom_density(aes(x = dist), color = "blue", linetype = "dotted", size = 1) +             # Plot the KDE as a dotted line
    theme_minimal() +
    labs(title = "Gaussian Mixture Model vs Kernel Density Estimate",
         x = "Value",
         y = "Density")
  
  return(p)
}



windowsFonts(A = windowsFont("Times New Roman"))
df <- freq_tab_stndng_ped
ggplot(df , aes(x=freq)) + xlim(0,8) + stat_density(geom = "line", position = "identity")


x <- freq_tab_walk_ped
y <- density(dist_walk_ped)
#smoothingSpline = smooth.spline(x,y, spar=0.35)


par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(y, lwd=2, family = "A",font = 1, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")        # xaxs and yaxs are set to limit the 2 axis strictly between "zero" lines
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3, family = "A")
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3, family = "A")
abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)

# Distribution of standing pedestrians

par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(y, lwd=5, family = "A",font = 1, lty = 5, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)
# curve(
#   +0.4635*dnorm(x,mean=0.825,sd=0.40)
#   +0.3250*dnorm(x,mean=1.925,sd=0.45)
#   +0.1350*dnorm(x,mean=2.900,sd=0.42)
#   +0.0640*dnorm(x,mean=3.710,sd=0.35)
#   +0.0070*dnorm(x,mean=5.750,sd=0.40),
#   col="red", lwd=2, add=TRUE)

curve(curve_function(x, dist_walk_ped, 4), col = "blue", lwd = 2, add = TRUE)
curve(curve_function(x, dist_walk_ped, 2), col = "red", lwd = 2, add = TRUE)
curve(curve_function(x, dist_stndng_ped, 5), col = "red", lwd = 2, add = TRUE)
plot_gmm(dist_stndng_ped, 5)
plot_gmm(dist_parked_car, 6)
plot_gmm(dist_parked_rck, 5)
plot_gmm(dist_parked_cng, 4)
plot_gmm(dist_walk_ped, 5)




par(mar=c(1,3,2,1)+.1, xpd = TRUE)
legend(x = "topright",          # Position
       legend = c("Probability density", "Fitted curve"),
       inset = c(0, 0),# Legend texts
       lty = c(2, 1),           # Line types
       col = c(1,2),           # Line colors
       lwd = 2, text.font = 6, cex = 2.3)                 # Line width


# sspline <- smooth.spline(x, 
#                         y[1:length(x)], 
#                         cv=TRUE) 



#density_anim(
 # kernel = c("gaussian", "epanechnikov", "rectangular", "triangular","biweight", "cosine", "optcosine"),
#  duration = 7
#)
#density_anim(n = 2:50)
#ggplot() + geom_density(mapping = aes(dist_stndng_ped), beta = .6) + labs(x = "x",y="y",title = "title")


#x <- freq_tab_stndng_ped$freq

#y <- density(dist_stndng_ped)
#values <- loess(y ~ x)
#

#plot(x, y)
#lines(predict(values), col = 'red', lwd = 2)



# standing pedestrian
par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_parked_car), lwd=2, family = "A",font = 1, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)

# Distribution of Parked car
par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_parked_car), lwd=4, family = "A",font = 1, lty = 5, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)
# Distribution of parked cars
curve(
  +0.050*dnorm(x,mean=1.12,sd=0.390)
  +0.515*dnorm(x,mean=2.07,sd=0.322)
  +0.200*dnorm(x,mean=2.73,sd=0.310)
  +0.172*dnorm(x,mean=3.64,sd=0.334)
  +0.042*dnorm(x,mean=4.48,sd=0.300)
  +0.010*dnorm(x,mean=5.92,sd=0.310),
  col="red", lwd=2, add=TRUE)

par(mar=c(1,3,2,1)+.1, xpd = TRUE)
legend(x = "topright",          # Position
       legend = c("Probability density", "Fitted curve"),
       inset = c(0, 0),# Legend texts
       lty = c(2, 1),           # Line types
       col = c(1,2),           # Line colors
       lwd = 2, text.font = 6, cex = 2.3)                # Line width


# plot(density(dist_parked_car),main="", lwd=2,
#      xlab="Blockage of Road (m)",
#      ylab="Probability Density of Parked Car")
# # Distribution of parked cars
# curve(
#   +0.050*dnorm(x,mean=1.12,sd=0.390)
#   +0.515*dnorm(x,mean=2.07,sd=0.322)
#   +0.200*dnorm(x,mean=2.73,sd=0.310)
#   +0.172*dnorm(x,mean=3.64,sd=0.334)
#   +0.042*dnorm(x,mean=4.48,sd=0.300)
#   +0.010*dnorm(x,mean=5.92,sd=0.310),
#   col="red", lwd=2, add=TRUE)



par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_parked_rck), lwd=2, family = "A",font = 1, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)


# Distribution of parked rickshaw
par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_parked_rck), lwd=4, family = "A",font = 1, lty = 5, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)
# Distribution of parked rickshaws
curve(
  +0.582*dnorm(x,mean=1.57,sd=0.500)
  +0.258*dnorm(x,mean=2.76,sd=0.440)
  +0.085*dnorm(x,mean=3.67,sd=0.320)
  +0.046*dnorm(x,mean=4.65,sd=0.380)
  +0.009*dnorm(x,mean=5.88,sd=0.380),
  col="red", lwd=2, add=TRUE)

par(mar=c(1,3,2,1)+.1, xpd = TRUE)
legend(x = "topright",          # Position
       legend = c("Probability density", "Fitted curve"),
       inset = c(0, 0),# Legend texts
       lty = c(2, 1),           # Line types
       col = c(1,2),           # Line colors
       lwd = 2, text.font = 6, cex = 2.3)  

# plot(density(dist_parked_rck),main="", lwd=2,
#      xlab="Blockage of Road (m)",
#      ylab="Probability Density of Parked Rickshaw")
# # Distribution of parked rickshaws
# curve(
#   +0.582*dnorm(x,mean=1.57,sd=0.500)
#   +0.258*dnorm(x,mean=2.76,sd=0.440)
#   +0.085*dnorm(x,mean=3.67,sd=0.320)
#   +0.046*dnorm(x,mean=4.65,sd=0.380)
#   +0.009*dnorm(x,mean=5.88,sd=0.380),
#   col="red", lwd=2, add=TRUE)


par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_parked_cng), lwd=2, family = "A",font = 1, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)


# Distribution of parked cng
par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_parked_cng), lwd=4, family = "A",font = 1, lty = 5, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)

# Distribution of parked CNGs
curve(
  +0.119*dnorm(x,mean=1.13,sd=0.29)
  +0.730*dnorm(x,mean=2.30,sd=0.55)
  +0.065*dnorm(x,mean=3.81,sd=0.27)
  +0.015*dnorm(x,mean=4.65,sd=0.32),
  col="red", lwd=2, add=TRUE)

par(mar=c(1,3,2,1)+.1, xpd = TRUE)
legend(x = "topright",          # Position
       legend = c("Probability density", "Fitted curve"),
       inset = c(0, 0),# Legend texts
       lty = c(2, 1),           # Line types
       col = c(1,2),           # Line colors
       lwd = 2, text.font = 6, cex = 2.3)  




# Distribution of walk
par(mar=c(6,7,1,1)+.1, xpd = FALSE)
plot(density(dist_walk_ped), lwd=4, family = "A",font = 1, lty = 5, cex.lab = 3,cex.main = 3,cex.axis = 2.5,
     main = "",
     xlab="",
     ylab="",
     xlim = c(0,6),
     ylim = c(0, 0.7),
     xaxs = "i",
     yaxs = "i")
title(xlab = "Blockage of road (m)", family = "A", line = 4.5 , cex.lab=3)
title(ylab="Probability density", family = "A", line = 4.5,cex.lab=3)

abline(h = 0)           # without this line, x axis appear gridded (density func is spooky. using ggplot is recommended)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "black", # Grid line color
     lwd = 1.5)


# Distribution of walk
# curve(
#   +0.35*dnorm(x,mean=1.34,sd=0.393)
#   +0.3*dnorm(x,mean=2.38,sd=0.5)
#   +0.25*dnorm(x,mean=3.77,sd=0.546)
#   +0.1*dnorm(x,mean=5.3,sd=1.412),
#   col="red", lwd=2, add=TRUE)

curve(
  +0.213*dnorm(x,mean=1.13,sd=0.532)
  +0.304*dnorm(x,mean=1.56,sd=0.465)
  +0.215*dnorm(x,mean=2.54,sd=0.396)
  +0.152*dnorm(x,mean=3.67,sd=0.397)
  +0.116*dnorm(x,mean=5.05,sd=1.332),
  col="red", lwd=2, add=TRUE)

par(mar=c(1,3,2,1)+.1, xpd = TRUE)
legend(x = "topright",          # Position
       legend = c("Probability density", "Fitted curve"),
       inset = c(0, 0),# Legend texts
       lty = c(2, 1),           # Line types
       col = c(1,2),           # Line colors
       lwd = 2, text.font = 6, cex = 2.3)  







# plot(density(dist_parked_cng),main="", lwd=2,
#      xlab="Blockage of Road (m)",
#      ylab="Probability Density of Parked CNG")
# # Distribution of parked CNGs
# curve(
#   +0.119*dnorm(x,mean=1.13,sd=0.29)
#   +0.730*dnorm(x,mean=2.30,sd=0.55)
#   +0.065*dnorm(x,mean=3.81,sd=0.27)
#   +0.015*dnorm(x,mean=4.65,sd=0.32),
#   col="red", lwd=2, add=TRUE)

# Test Stuffs
x = seq(-10, 10, by = .01)
y = dnorm(x,mean=3,sd=.8)
plot(x,y)

n = 1000
# aList = rnorm(n/0.3,mean=1,sd=0.3)+rnorm(n/0.7,mean=4,sd=0.5)
aList = rnorm(n,mean=2,sd=0.8)
# aList = rnorm(n/0.7,mean=4,sd=0.5)
aList = sort(aList)
plot(aList)
hist(aList)

