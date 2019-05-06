install.packages('readr')

library(lubridate)
library(readr)
library(psymonitor)  # For testting for bubble monitoring
library(ggplot2)     # To handle plots
library(knitr)      

VIX <- read_csv('/Users/yiwenwei/Downloads/vixcurrent.csv',  col_names = TRUE)
colnames(VIX)[colnames(VIX)=="VIX Close"] <- "VIX_Close"
colnames(VIX)[colnames(VIX)=="VIX Open"] <- "VIX_Open"
VIX$Date <- as.Date(VIX$Date, "%m/%d/%y")
VIX_sub <- subset(VIX, Date > as.Date("2016-01-01") )


y        <- VIX_sub$VIX_Close
obs      <- length(y)
r0       <- 0.01 + 1.8/sqrt(obs)
swindow0 <- floor(r0*obs)
dim      <- obs - swindow0 + 1
IC       <- 2
adflag   <- 6
yr      <- 2
Tb       <- 3*yr + swindow0 - 1
nboot    <- 99
nCore <- 2

bsadf_vix_close          <- PSY(y, swindow0, IC, adflag)
quantilesBsadf_vix_close <- cvPSYwmboot(y, swindow0, IC, adflag, Tb, nboot, nCores = 2) #Note that the number of cores is arbitrarily set to 2.


monitorDates <- VIX_sub$Date[swindow0:obs]
quantile95   <- quantilesBsadf_vix_close %*% matrix(1, nrow = 1, ncol = dim)
ind95        <- (bsadf_vix_close > t(quantile95[1, ])) * 1
periods      <- locate(ind95, monitorDates)

bubbleDates_vix_close <- disp(periods, obs)
kable(bubbleDates_vix_close, caption = "Bubble and Crisis Periods VIX Close")


ggplot() + 
  geom_line(data = VIX_sub, aes(Date, VIX_Close)) +
  geom_rect(data = bubbleDates_vix_close, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), alpha = 0.5) +
  labs(title = "VIX Close",x = "time", y = "Price") 

VIX_sub$Diff <- VIX_sub$VIX_Close - VIX_sub$VIX_Open


y        <- VIX_sub$Diff
obs      <- length(y)
r0       <- 0.01 + 1.8/sqrt(obs)
swindow0 <- floor(r0*obs)
dim      <- obs - swindow0 + 1
IC       <- 2
adflag   <- 6
yr      <- 2
Tb       <- 3*yr + swindow0 - 1
nboot    <- 99
nCore <- 2

bsadf_vix_diff          <- PSY(y, swindow0, IC, adflag)
quantilesBsadf_vix_diff <- cvPSYwmboot(y, swindow0, IC, adflag, Tb, nboot, nCores = 2) #Note that the number of cores is arbitrarily set to 2.


monitorDates <- VIX_sub$Date[swindow0:obs]
quantile95   <- quantilesBsadf_vix_diff %*% matrix(1, nrow = 1, ncol = dim)
ind95        <- (bsadf_vix_diff > t(quantile95[1, ])) * 1
periods      <- locate(ind95, monitorDates)

bubbleDates_vix_diff <- disp(periods, obs)
kable(bubbleDates_vix_close, caption = "Bubble and Crisis Periods VIX Diff")


ggplot() + 
  geom_line(data = VIX_sub, aes(Date, Diff)) +
  geom_rect(data = bubbleDates_vix_diff, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), alpha = 0.5) +
  labs(title = "VIX Close",x = "time", y = "Price") 

