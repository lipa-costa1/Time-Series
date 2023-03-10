library("lubridate")
library(stlplus)
library(readxl)
library(latex2exp)
library(forecast)
library(fpp2)
library(ggplot2)
library(Metrics)

#meter s? um dataset com tempo e Paio-Pires
dataa<-cbind(data[10], data$`Sobreiras-Porto`)
names(dataa) <- c('time', 'o3')
View(dataa)

data_ts <- ts(dataa$o3, frequency = 24)
plot(data_ts, col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))

#boxplots para ver variancia

O3 <- as.numeric(data$`Paio-Pires`)
data_hour_extended <- as.data.frame(cbind(Year,Month,Day,Weekday, Hour, O3))

par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(O3~Month,data=data_hour_extended,col="#FFA726",ylab=(TeX('$PM_{10}$ particles ($\\mu g /m^3 $)')))
boxplot(O3~Weekday,data=data_hour_extended,col="#FFA726",ylab=(TeX('$PM_{10}$ particles ($\\mu g /m^3 $)')))
boxplot(O3~Day,data=data_hour_extended,col="#FFA726",ylab=(TeX('$PM_{10}$ particles ($\\mu g /m^3 $)')))


#m?dia no verao ? alta e as variancias no mes 7 e 9 tamb?m.


##### vamos aplicar a box-cox transformation porque as variancias nao aparentam ser constantes e isso faz com que a serie nao seja estacion?ria. 

#### Log-transformation #########################################################

lambda <- BoxCox.lambda(data_ts)
print(lambda)
data_ts.box.cox <- (data_ts^(lambda) - 1)/lambda

#ver se a s?rie has a stationary behavior.

plot(data_ts.box.cox, col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
plot(diff(data_ts.box.cox), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))


#I will differentiate the series until I obtain stationary.

#(1 ??? B)^d (1 ??? B^S)^D y_T, onde S remove a seasonal e d remove a trend

acf(data_ts.box.cox[1:8784], lag=100) #sazonal de 24h em 24h; 24
pacf(data_ts.box.cox[1:8784], lag=100)
#como ? sazonal de 24 em 24 horas, vou diferenciar de 24 em 24 (seasonal differencing)
plot(diff(data_ts.box.cox,lag = 24), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
acf(diff(data_ts.box.cox[1:8784],lag = 24), lag=100)
pacf(diff(data_ts.box.cox[1:8784],lag = 24), lag=100) #repete se de 24 em 24 hrs
#na net diz para diferenciar novamente https://campus.datacamp.com/courses/forecasting-in-r/forecasting-with-arima-models?ex=4
plot(diff(diff(data_ts.box.cox,lag = 24)), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
acf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=100)
pacf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=100) #repete se de 24 em 24 hrs

#Logo, eu concluo que I can then conclude that d=D=1 and T=24.


#mais bonitos
ggAcf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=100)
ggPacf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=100) #repete se de 24 em 24 hrs

#Regarding its autocorrelation and partial autorrelation functions (ACF and PACF, respectively) showed in the
#figure below, we notice that the ACF decreases almost exponentially and they both have long tails and so these are
#good indicators that we are in presence of a stationary time series

#Two approaches were taken to determine the ideal SARIMA parameters: 
#ACF and PACF plots, and a grid search. 
#The ACF and PACF plots were used as a starting point to narrow down
#to a few potential parameters, and then a grid search was used to
#identify the best parameters.

#1 semana e meia
par(mfrow=c(1,2))
ggAcf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=168)
ggPacf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=168)
par(mfrow=c(1,1))

#auto-arima, complexidade, tempo computacional 

####dividir treino e teste
train_set=ts(data_ts.box.cox[1:8779],frequency = 24)
test_set=data_ts.box.cox[8780:8784]

#vou fazer auto arima para ter ideia dos parametros
model_auto<-auto.arima(train_set,max.p=5, max.q=5) #ARIMA(4,1,4)(2,0,0)[24], AIC=42774.83, BIC=42852.71


####modelo 


##treinar modelos
models_sobreiro=vector("list", 180)
for(i in 1:180){
  try(models_sobreiro[[i]]<-Arima(train_set, order = dt_params[i,1:3], seasonal = list(order=dt_params[i,4:6],period=24),lambda = NULL))
}

models<-models_sobreiro


###remover modelos cujos residuos nao sao white noise - teste ljung box

dt_params2=data.frame(dt_params)
dt_params2$residuals=residuos(models)  #y-> a escolha de par?metros levou a res?duos white noite

#agora vamos calcular o aic de cada modelo
aic=rep(NA,180)
model_names=rep(NA,180)
for(i in 1:180){
  if(length(models[[i]]$aic)>0){
    aic[i]=models[[i]]$aic
    model_names[i]=as.character(models[[i]])
  }
}
dt_params2$aic=aic
dt_params2$model=model_names


#We will then select the models with AIC smaller than the one of auto.arima (37761.41) and we display them all.

i=as.numeric(rownames(dt_params2)[which(dt_params2$aic<42774.83)])
res=sapply(i, function(x)as.character(models[[x]]))
res


dt_params2=na.omit(dt_params2)

#agora vamos manter os modelos cujo residuo era white noise (residuals=y)
dt_params2=dt_params2[dt_params2[,8]=="y",]

tabela_final <- dt_params2

cc=rep(NA,nrow(dt_params2))
for(j in 1:nrow(dt_params2)){
  cc[j]<-models[[as.numeric(rownames(dt_params2))[j]]]$aicc
}

dd=rep(NA,nrow(dt_params2))
for(j in 1:nrow(dt_params2)){
  dd[j]<-models[[as.numeric(rownames(dt_params2))[j]]]$bic
}

tabela_final$aicc <- cc
tabela_final$bic <- dd

#Between the values of AIC and BIC, the
#BIC penalizes the introduction of new parameter,
#so it is more important for us than AIC. 
#Like the Occam's razor the simplest explanation is usually the best one.
#In this case, it is important to choose one model with fewer parameters
#as much as possible. All together we will try to select optimal model,
#with low AIC and BIC at the same time.

View(tabela_final)
order(tabela_final$aic, decreasing = FALSE)
order(tabela_final$aicc, decreasing = FALSE)
order(tabela_final$bic, decreasing = FALSE)

#Vamos escolher o n?mero 30 (porque ? o 3 melhor aic e bic) e o auto.arima, para comparar
#nota que todos estes t?m bic < bic(auto.arima) e aic<aic(auto.arima)

modelo1<-models[[as.numeric(rownames(tabela_final))[30]]]
modelo2<-model_auto


##ver se os coeficientes sao significativos (se valor-p<0.05)

#I will now reject the models that contain non significant coefficient.
#To do so, since arima uses maximum likelihood for estimation,
#the coefficients are assymptoticaly normal.
#Hence dividing the coefficients by their standard errors 
#to get the z-statistics and then calculate p-values.

#se <- sqrt(diag(models[[15]]$var.coef))
#valorp<- 2*(1-pnorm(abs(models[[15]]$coef)/se))

#H0:coef=0 vs H1:coef???0

valorp <- function(modelo) {
  se <- sqrt(diag(modelo$var.coef))
  valorpp <- 2*(1-pnorm(abs(modelo$coef)/se))
  return(valorpp)
}

valorp(modelo1)
valorp(modelo2)


####### Residual diagnostics ########################################################

#check if the residuals are independently, identically distributed and driven
#by a normal distribution


#plot acf residuos - s?o independentes, j? tinhamos visto pelo ljun box 

ggAcf(residuals(modelo1), lag=168, main="Residuals ACF")
ggAcf(residuals(modelo2), lag=168, main="Residuals ACF")

# density function to observe normality of the residuals
plot(density(residuals(modelo1)),main=NA,xlab=NA, ylab=TeX('Density of the residuals'))
plot(density(residuals(modelo2)),main=NA,xlab=NA, ylab=TeX('Density of the residuals'))

# QQ-plot to observe normality of the residuals
qqnorm(residuals(modelo1), pch = 1, frame = FALSE, xlab=TeX("Theoretical Quantiles"),ylab=TeX("Sample Quantiles"))
qqline(residuals(modelo1), col = "steelblue", lwd = 2)

qqnorm(residuals(modelo2), pch = 1, frame = FALSE, xlab=TeX("Theoretical Quantiles"),ylab=TeX("Sample Quantiles"))
qqline(residuals(modelo2), col = "steelblue", lwd = 2)


# histogram to observe normality of the residuals
hist(residuals(modelo1), breaks=15, xlab="Residuals", main="")
hist(residuals(modelo2), breaks=15, xlab="Residuals", main="")


#### Forecasting ################################################################

##Vamos escolher o modelo1 pois apresenta melhores resultados

#we forecasted the 5 time periods ahead and calculate 95% prediction intervals
#for each of the 5 forecasts.

#forecasting
predict(modelo1,n.ahead=5) #este d? os se
f_fit<-forecast::forecast(modelo1, h=5) #d?o igual #este d? os ic

#plot
x_tr <- window(data_ts.box.cox)
autoplot(x_tr, series="Data") + 
  autolayer(modelo1$fitted, series="SARIMA(3,1,1)(0,1,2)[24]") +
  autolayer(f_fit, series="Prediction") +
  xlab("Day") + ylab("O_3 Level") + ggtitle("Restelo") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")


#predicted vs real 

valores_real <- test_set
valores_predicted <- f_fit$mean[c(1:5)]

#diferen?a entre valores
valores_predicted-valores_real # = -3.770027 -6.127267 -7.149668 -6.886244 -7.053849
mape(valores_real, valores_predicted) #-> 30%


#This tells us that the mean absolute percent error between
#the 03 level predicted by the model and the actual 03 level is 30%.