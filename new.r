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

curve(
  +0.4635*dnorm(x,mean=0.825,sd=0.40)
  +0.3250*dnorm(x,mean=1.925,sd=0.45)
  +0.1350*dnorm(x,mean=2.900,sd=0.42)
  +0.0640*dnorm(x,mean=3.710,sd=0.35)
  +0.0070*dnorm(x,mean=5.750,sd=0.40),
  col="red", lwd=2, add=TRUE)