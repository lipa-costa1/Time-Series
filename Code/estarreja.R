library("lubridate")
library(stlplus)
library(readxl)
library(latex2exp)
library(forecast)
library(fpp2)
library(ggplot2)
library(Metrics)

#meter só um dataset com tempo e data$Estarreja
dataa<-cbind(data[10], data$Estarreja)
names(dataa) <- c('time', 'o3')
View(dataa)

data_ts <- ts(dataa$o3, frequency = 24)
plot(data_ts, col="limegreen", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))

#boxplots para ver variancia

O3 <- as.numeric(data$Estarreja)
data_hour_extended <- as.data.frame(cbind(Year,Month,Day,Weekday, Hour, O3))

par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(O3~Month,data=data_hour_extended,col="limegreen",ylab=(TeX('$0_{3}$ particles ($\\mu g /m^3 $)')))
boxplot(O3~Weekday,data=data_hour_extended,col="seagreen3",ylab=(TeX('$0_{3}$ particles ($\\mu g /m^3 $)')))
boxplot(O3~Day,data=data_hour_extended,col="seagreen",ylab=(TeX('$0_{3}$ particles ($\\mu g /m^3 $)')))


#média no verao é alta e as variancias no mes 7 e 9 também.

#tentar ir pelo remainder do stl é merdoso

y.stl<-stl(data_ts.box.cox,s.window="period")
plot(y.stl)

y_residuals <- y.stl$data[,4]
par(mfrow=c(2,2))
acf(y_residuals, lag=168, col="limegreen")
pacf(y_residuals, lag=100)
#por isso vamos pelas diferenças

##### vamos aplicar a box-cox transformation porque as variancias nao aparentam ser constantes e isso faz com que a serie nao seja estacionária. 

#### Log-transformation #########################################################

lambda <- BoxCox.lambda(data_ts)
print(lambda)
data_ts.box.cox <- (data_ts^(lambda) - 1)/lambda


#ver se a série has a stationary behavior.

plot(data_ts.box.cox, col="limegreen", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
plot(diff(data_ts.box.cox), col="limegreen", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))


#I will differentiate the series until I obtain stationary.

#(1 ??? B)^d (1 ??? B^S)^D y_T, onde S remove a seasonal e d remove a trend

acf(data_ts.box.cox[1:8784], col="limegreen", lag=100) #sazonal de 24h em 24h; 24
pacf(data_ts.box.cox[1:8784],col="limegreen", lag=100)
#como é sazonal de 24 em 24 horas, vou diferenciar de 24 em 24 (seasonal differencing)
plot(diff(data_ts.box.cox,lag = 24), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
acf(diff(data_ts.box.cox[1:8784],lag = 24), lag=100)
pacf(diff(data_ts.box.cox[1:8784],lag = 24), lag=100) #repete se de 24 em 24 hrs
#na net diz para diferenciar novamente https://campus.datacamp.com/courses/forecasting-in-r/forecasting-with-arima-models?ex=4
plot(diff(diff(data_ts.box.cox,lag = 24)), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
acf(diff(diff(data_ts.box.cox[1:8784], lag = 24)), lag=100, col="limegreen")
pacf(diff(diff(data_ts.box.cox[1:8784], lag = 24)), col="limegreen", lag=100) #repete se de 24 em 24 hrs

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
model_auto<-auto.arima(train_set,max.p=5, max.q=5) #ARIMA(3,0,1)(2,1,0)[24], AIC=37761.41, BIC=37810.95


####modelo 


##treinar modelos
models_estarreja=vector("list", 180)
for(i in 1:180){
  try(models_estarreja[[i]]<-Arima(train_set, order = dt_params[i,1:3], seasonal = list(order=dt_params[i,4:6],period=24),lambda = NULL))
}

models<-models_estarreja


###remover modelos cujos residuos nao sao white noise - teste ljung box

dt_params2=data.frame(dt_params)
dt_params2$residuals=residuos(models)  #y-> a escolha de parâmetros levou a resíduos white noite

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

i=as.numeric(rownames(dt_params2)[which(dt_params2$aic<33764.33)])
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

#Vamos escolher o número 9 (porque é o 3 melhor aic e bic) e o auto.arima, para comparar
#nota que todos estes têm bic < bic(auto.arima) e aic<aic(auto.arima)

modelo1<-models[[as.numeric(rownames(tabela_final))[36]]]
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


#plot acf residuos - são independentes, já tinhamos visto pelo ljun box 

ggAcf(residuals(modelo1), lag=168, col= "limegreen", main="Residuals ACF")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )
ggAcf(residuals(modelo2), lag=168, main="Residuals ACF")

par(mfrow=c(2,2))

# density function to observe normality of the residuals
plot(density(residuals(modelo1)),main=NA,xlab=NA,col= "limegreen", ylab=TeX('Density of the residuals'))
plot(density(residuals(modelo2)),main=NA,xlab=NA, ylab=TeX('Density of the residuals'))

# QQ-plot to observe normality of the residuals
qqnorm(residuals(modelo1), pch = 1, frame = FALSE, xlab=TeX("Theoretical Quantiles"),ylab=TeX("Sample Quantiles"))
qqline(residuals(modelo1), col = "limegreen", lwd = 2)

qqnorm(residuals(modelo2), pch = 1, frame = FALSE, xlab=TeX("Theoretical Quantiles"),ylab=TeX("Sample Quantiles"))
qqline(residuals(modelo2), col= "limegreen", lwd = 2)


# histogram to observe normality of the residuals
hist(residuals(modelo1), col="limegreen", breaks=15, xlab="Residuals", main="")
hist(residuals(modelo2), breaks=15, xlab="Residuals", main="")

#plot residuos

plot(residuals(modelo1), col="limegreen", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))

#### Forecasting ################################################################

##Vamos escolher o modelo1 pois apresenta melhores resultados

#we forecasted the 5 time periods ahead and calculate 95% prediction intervals
#for each of the 5 forecasts.

#forecasting
predict(modelo1,n.ahead=5) #este dá os se
f_fit<-forecast::forecast(modelo1, h=5) #dão igual #este dá os ic

#plot
x_tr <- window(data_ts.box.cox)
autoplot(x_tr, series="Data") + 
  autolayer(modelo1$fitted, series="SARIMA(3,1,1)(0,1,2)[24]") +
  autolayer(f_fit, series="Prediction") +
  xlab("Day") + ylab("O_3 Level") + ggtitle("Paio Pires") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

#plot mais perto
x_tr <- window(data_ts.box.cox)
autoplot(x_tr, series="Data") + 
  autolayer(modelo1$fitted, series="SARIMA(3,1,1)(0,1,2)[24]") +
  autolayer(f_fit, series="Prediction") +
  xlab("Day") + ylab("O_3 Level") + ggtitle("Paio Pires") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom") + xlim(366,368)

#predicted vs real 

valores_real <- test_set
valores_predicted <- f_fit$mean[c(1:5)]

#diferença entre valores
valores_predicted-valores_real # = -3.770027 -6.127267 -7.149668 -6.886244 -7.053849
mape(valores_real, valores_predicted) #-> 30%


#This tells us that the mean absolute percent error between
#the 03 level predicted by the model and the actual 03 level is 30%.





