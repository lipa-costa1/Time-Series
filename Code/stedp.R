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

data_EDP <- read_excel("C:\\Users\\macac\\OneDrive\\Ambiente de Trabalho\\IST_e_afins\\ST\\Projecto\\EDP RENOVAVEISprice.xls")
data_EDP <- data_EDP[,c(1,5)]
data_EDP$Date <- as.Date(data_EDP$Date)

# Checking for missing values
sum(is.na(data_EDP))

# Original data plot
plot(data_EDP$Date,data_EDP$Close, type = 'l', xlab="Time", ylab="Closing values for EDP Stock")

# log-returns associated to the daily closing values
log_return <- as.ts(diff(log(data_EDP$Close)))

# Plot Log-return plot
plot(data_EDP$Date[-1],log_return, type = 'l',xlab="Time", ylab="Log-returns associated to the closing values")

# Analysis log-return
mean(log_return)
median(log_return)
var(log_return)
kurtosis(log_return)
boxplot(log_return,col="#FFA726",ylab=(TeX('Boxplot of the log-returns for EDP stock')))

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
GARCH_norm_EDP <- select.GARCH("GARCH", log_return, 5,5, "norm")
iGARCH_norm_EDP <- select.GARCH("iGARCH", log_return, 5,5, "norm")
mGARCH_norm_EDP <- select.GARCH("sGARCH", log_return, 5,5, "norm")
apARCH_norm_EDP <- select.GARCH("apARCH", log_return, 5,5, "norm")

model <- c(rep("GARCH",25), rep("iGARCH",25), rep("mGARCH",25), rep("apARCH",25))

norm_models_EDP <- cbind(model, rbind(GARCH_norm_EDP, iGARCH_norm_EDP, mGARCH_norm_EDP, apARCH_norm_EDP))

order(norm_models_EDP[,4], decreasing = FALSE)
order(norm_models_EDP[,5], decreasing = FALSE) #model 51 is the best performer in both criteria

mgarch11_norm_EDP <- ugarchspec(variance.model=list(model="sGARCH",
                                                 garchOrder=c(1,1)), 
                             mean.model=list(armaOrder=c(0,0),
                                             include.mean=TRUE), 
                             distribution.model="norm")
mgarch11_norm_EDP_fit <- ugarchfit(mgarch11_norm_EDP, log_return)
mgarch11_norm_EDP_fit

# run all GARCH-type models with std
GARCH_std_EDP <- select.GARCH("GARCH", log_return, 5,5, "std")
iGARCH_std_EDP <- select.GARCH("iGARCH", log_return, 5,5, "std")
mGARCH_std_EDP <- select.GARCH("sGARCH", log_return, 5,5, "std")
apARCH_std_EDP <- select.GARCH("apARCH", log_return, 5,5, "std")

model <- c(rep("GARCH",25), rep("iGARCH",25), rep("mGARCH",25), rep("apARCH",25))

std_models_EDP <- cbind(model, rbind(GARCH_std_EDP, iGARCH_std_EDP, mGARCH_std_EDP, apARCH_std_EDP))

order(std_models_EDP[,4], decreasing = FALSE)
order(std_models_EDP[,5], decreasing = FALSE) # 51 is the best performer in AIC and 2nd in BIC

mgarch11_std_EDP <- ugarchspec(variance.model=list(model="sGARCH",
                                                garchOrder=c(1,1)), 
                            mean.model=list(armaOrder=c(0,0),
                                            include.mean=TRUE), 
                            distribution.model="std")
mgarch11_std_EDP_fit <- ugarchfit(mgarch11_std_EDP, log_return)
mgarch11_std_EDP_fit

# run all GARCH-type models with ged
GARCH_ged_EDP <- select.GARCH("GARCH", log_return, 5,5, "ged")
iGARCH_ged_EDP <- select.GARCH("iGARCH", log_return, 5,5, "ged")
mGARCH_ged_EDP <- select.GARCH("sGARCH", log_return, 5,5, "ged")
apARCH_ged_EDP <- select.GARCH("apARCH", log_return, 5,5, "ged")

model <- c(rep("GARCH",25), rep("iGARCH",25), rep("mGARCH",25), rep("apARCH",25))

ged_models_EDP <- cbind(model, rbind(GARCH_ged_EDP, iGARCH_ged_EDP, mGARCH_ged_EDP, apARCH_ged_EDP))

order(ged_models_EDP[,4], decreasing = FALSE)
order(ged_models_EDP[,5], decreasing = FALSE) # 51 is the best performer in AIC and 2nd in BIC

mgarch11_ged_EDP <- ugarchspec(variance.model=list(model="sGARCH",
                                               garchOrder=c(1,1)), 
                           mean.model=list(armaOrder=c(0,0),
                                           include.mean=TRUE), 
                           distribution.model="ged")
mgarch11_ged_EDP_fit <- ugarchfit(mgarch11_ged_EDP, log_return)
mgarch11_ged_EDP_fit #we see that this is the model that gets the best AIC out of the 3
mgarch11_ged_EDP_fit@fit$coef
modelo_EDP <- mgarch11_ged_EDP_fit
# Residuals 
residuals <- modelo_EDP@fit$residuals
mean(residuals)
var(residuals)

# Standardized residuals
standard_residuals <- residuals(modelo_EDP, standardize = TRUE)
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
