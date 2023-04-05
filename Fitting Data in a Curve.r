data = read.csv(file = "data.csv", header=TRUE)

freq_tab_stndng_ped = data.frame(value=data[1][!is.na(data[1])], freq=data[2][!is.na(data[2])])
freq_tab_parked_car = data.frame(value=data[3][!is.na(data[3])], freq=data[4][!is.na(data[4])])
freq_tab_parked_rck = data.frame(value=data[5][!is.na(data[5])], freq=data[6][!is.na(data[6])])
freq_tab_parked_cng = data.frame(value=data[7][!is.na(data[7])], freq=data[8][!is.na(data[8])])

dist_stndng_ped = rep(freq_tab_stndng_ped$value, freq_tab_stndng_ped$freq)
dist_parked_car = rep(freq_tab_parked_car$value, freq_tab_parked_car$freq)
dist_parked_rck = rep(freq_tab_parked_rck$value, freq_tab_parked_rck$freq)
dist_parked_cng = rep(freq_tab_parked_cng$value, freq_tab_parked_cng$freq)

windowsFonts(A = windowsFont("Times New Roman"))
df <- freq_tab_stndng_ped
ggplot(df , aes(x=freq)) + xlim(0,8) + stat_density(geom = "line", position = "identity")


#x <- freq_tab_stndng_ped$freq
#y <- density(dist_stndng_ped)
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
curve(
  +0.4635*dnorm(x,mean=0.825,sd=0.40)
  +0.3250*dnorm(x,mean=1.925,sd=0.45)
  +0.1350*dnorm(x,mean=2.900,sd=0.42)
  +0.0640*dnorm(x,mean=3.710,sd=0.35)
  +0.0070*dnorm(x,mean=5.750,sd=0.40),
  col="red", lwd=2, add=TRUE)

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


# Distribution of standing pedestrians
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

