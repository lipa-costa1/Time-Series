#### Times series project part 2 ############

library("lubridate")
library(stlplus)
library(readxl)
library(latex2exp)
library(forecast)
library(fpp2)
library(ggplot2)
library(Metrics)
library(fGarch)
library(rugarch)
library(ggpubr)

# Load the datasets

data_NOVA <- read_excel("C:\\Users\\macac\\OneDrive\\Ambiente de Trabalho\\IST_e_afins\\ST\\Projecto\\NOVABASESGPSprice.xls")
data_NOVA <- data_NOVA[,c(1,5)]
data_NOVA$Date <- as.Date(data_NOVA$Date)

# Checking for missing values
sum(is.na(data_NOVA))

# Original data plot
plot(data_NOVA$Date,data_NOVA$Close, type = 'l', xlab="Time", ylab="Closing values for NOVA Stock")

# log-returns associated to the daily closing values
log_return <- as.ts(diff(log(data_NOVA$Close)))

# Plot Log-return plot
plot(data_NOVA$Date[-1],log_return, type = 'l',xlab="Time", ylab="Log-returns associated to the closing values")

# Analysis log-return
mean(log_return)
median(log_return)
var(log_return)
kurtosis(log_return)
boxplot(log_return,col="#FFA726",ylab=(TeX('Boxplot of the log-returns for NOVA stock')))

# ACF plot of log-returns
par(mfrow=c(1,3), mai=c(0.7,0.6,0.1,0.1))
acf(log_return, lag.max = 200, ylab=TeX("ACF of Log-returns"))
acf(log_return^2, ylab=TeX("ACF of Squared Log-returns"))
acf(abs(log_return), ylab=TeX("ACF of Absolute Log-returns"))
par(mfrow=c(1,1))

select.GARCH <- function(model, data, p, q, dist){ 
  
  final <- matrix(NA,nrow=p*q,ncol=4)
  colnames(final) <- c("p","q","AIC","BIC")
  if(model == "GARCH"){
    k <- 1
    for(i in 1:p){
      for(j in 1:q){
        garch <- ugarchspec(variance.model=list(model="fGARCH", submodel="GARCH",
                                                garchOrder=c(i,j)), 
                            mean.model=list(armaOrder=c(0,0),
                                            include.mean=TRUE), 
                            distribution.model=dist)
        fit <- ugarchfit(garch, data)
        final[k,] <- c(i, j, infocriteria(fit)[1], infocriteria(fit)[2])
        k <- k+1
      }
    }
  }else{
    k <- 1
    for(i in 1:p){
      for(j in 1:q){
        garch <- ugarchspec(variance.model=list(model = model, garchOrder=c(i,j)), 
                            mean.model=list(armaOrder=c(0,0),
                                            include.mean=TRUE), 
                            distribution.model=dist)
        fit <- ugarchfit(garch, data)
        final[k,] <- c(i, j, infocriteria(fit)[1], infocriteria(fit)[2])
        k <- k+1
      }
    }
  }
  return(as.data.frame(final))
}

# run all GARCH-type models with norm
GARCH_norm_NOVA <- select.GARCH("GARCH", log_return, 5,5, "norm")
iGARCH_norm_NOVA <- select.GARCH("iGARCH", log_return, 5,5, "norm")
mGARCH_norm_NOVA <- select.GARCH("sGARCH", log_return, 5,5, "norm")
#apARCH_norm_NOVA <- select.GARCH("apARCH", log_return, 5,5, "norm")

model <- c(rep("GARCH",25), rep("iGARCH",25), rep("mGARCH",25))

norm_models_NOVA <- cbind(model, rbind(GARCH_norm_NOVA, iGARCH_norm_NOVA, mGARCH_norm_NOVA))

order(norm_models_NOVA[,4], decreasing = FALSE)
order(norm_models_NOVA[,5], decreasing = FALSE) #model 30

igarch15_norm_NOVA <- ugarchspec(variance.model=list(model="iGARCH",
                                                    garchOrder=c(1,5)), 
                                mean.model=list(armaOrder=c(0,0),
                                                include.mean=TRUE), 
                                distribution.model="norm")
igarch15_norm_NOVA_fit <- ugarchfit(igarch15_norm_NOVA, log_return)
igarch15_norm_NOVA_fit

# run all GARCH-type models with std
GARCH_std_NOVA <- select.GARCH("GARCH", log_return, 5,5, "std")
iGARCH_std_NOVA <- select.GARCH("iGARCH", log_return, 5,5, "std")
mGARCH_std_NOVA <- select.GARCH("sGARCH", log_return, 5,5, "std")
#apARCH_std_NOVA <- select.GARCH("apARCH", log_return, 5,5, "std")

model <- c(rep("GARCH",25), rep("iGARCH",25), rep("mGARCH",25))

std_models_NOVA <- cbind(model, rbind(GARCH_std_NOVA, iGARCH_std_NOVA, mGARCH_std_NOVA))

order(std_models_NOVA[,4], decreasing = FALSE)
order(std_models_NOVA[,5], decreasing = FALSE) # 26 is the best performer

igarch11_std_NOVA <- ugarchspec(variance.model=list(model="iGARCH",
                                                   garchOrder=c(1,1)), 
                               mean.model=list(armaOrder=c(0,0),
                                               include.mean=TRUE), 
                               distribution.model="std")
igarch11_std_NOVA_fit <- ugarchfit(igarch11_std_NOVA, log_return)
igarch11_std_NOVA_fit

# run all GARCH-type models with ged
GARCH_ged_NOVA <- select.GARCH("GARCH", log_return, 5,5, "ged")
iGARCH_ged_NOVA <- select.GARCH("iGARCH", log_return, 5,5, "ged")
mGARCH_ged_NOVA <- select.GARCH("sGARCH", log_return, 5,5, "ged")
#apARCH_ged_NOVA <- select.GARCH("apARCH", log_return, 5,5, "ged")

model <- c(rep("GARCH",25), rep("iGARCH",25), rep("mGARCH",25))

ged_models_NOVA <- cbind(model, rbind(GARCH_ged_NOVA, iGARCH_ged_NOVA, mGARCH_ged_NOVA))

order(ged_models_NOVA[,4], decreasing = FALSE)
order(ged_models_NOVA[,5], decreasing = FALSE) # 26 is the best in both

igarch11_ged_NOVA <- ugarchspec(variance.model=list(model="iGARCH",
                                                   garchOrder=c(1,1)), 
                               mean.model=list(armaOrder=c(0,0),
                                               include.mean=TRUE), 
                               distribution.model="ged")
igarch11_ged_NOVA_fit <- ugarchfit(igarch11_ged_NOVA, log_return)
igarch11_ged_NOVA_fit #we see that this is the model that gets the best AIC and BIC out of the 3
igarch11_ged_NOVA_fit@fit$coef
modelo_NOVA <- igarch11_ged_NOVA_fit
# Residuals 
residuals <- modelo_NOVA@fit$residuals
mean(residuals)
var(residuals)

# Standardized residuals
standard_residuals <- residuals(modelo_NOVA, standardize = TRUE)
mean(standard_residuals)
var(standard_residuals)

## Residuals diagnostics ########

# Ljung-Box Test
Box.test(standard_residuals, lag=10,type = "Ljung-Box")
Box.test(standard_residuals, lag=20,type = "Ljung-Box")
Box.test(standard_residuals, lag=30,type = "Ljung-Box")

Box.test(standard_residuals^2, lag=10,type = "Ljung-Box")
Box.test(standard_residuals^2, lag=20,type = "Ljung-Box")
Box.test(standard_residuals^2, lag=30,type = "Ljung-Box")

standard_residuals <- as.vector(standard_residuals)

# Plot residuals
par(mfrow=c(2,2), mai=c(0.7,0.7,0.1,0.1))
plot(residuals)
plot(residuals^2)
plot(standard_residuals)
plot(standard_residuals^2)

# Plot ACF residuals
par(mfrow=c(1,3), mai=c(0.7,0.7,0.1,0.1))
acf(standard_residuals,ylab=TeX("ACF of Standardized Residuals"))
acf(standard_residuals^2, ylab=TeX("ACF of Squared Standardized Residuals"))
acf(abs(standard_residuals), ylab=TeX("ACF of Absolute Standardized Residuals"))
par(mfrow=c(1,1))

qqnorm(standard_residuals, pch = 1, frame = FALSE)
qqline(standard_residuals, col = "steelblue", lwd = 2)
shapiro.test(standard_residuals)
