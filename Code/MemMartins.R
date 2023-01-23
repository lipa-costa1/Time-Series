library("lubridate")
library(stlplus)
library(readxl)
library(latex2exp)
library(forecast)
library(fpp2)
library(ggplot2)
library(Metrics)

QualidadeARO3 <- read_excel("C:/Users/Filipa/Downloads/QualidadeARO3.xlsx")

# deleting ílhavo for consistency with what was asked in the handout
data <- QualidadeARO3[,-4]

#no missing data
which(is.na(data))

#Add collumn with dates and times
dates_hour <- seq.POSIXt(from = as.POSIXct("2020-01-01 00:00:00",format="%Y-%m-%d %H",tz="Europe/Lisbon"), length.out = 8784, by = "60 mins")
data$time <- dates_hour
View(data)

#### Initial examination of the data ##############################################

time <- seq.POSIXt(from = as.POSIXct("2020-01-01 00:00:00",format="%Y-%m-%d %H",tz="Europe/Lisbon"), length.out = 8784, by = "60 mins")
Year <- as.numeric(format(time, '%Y'));
Month <- as.numeric(format(time, '%m'));
Day <- as.numeric(format(time, '%d'));
Weekday <- as.numeric(format(time, '%u'));
Hour <- as.numeric(format(time, '%H'));

#################################Para analisar por ano, mês etc....

#meter só um dataset com tempo e Mem-Martins
dataa<-cbind(data[10], data$`Mem-Martins`)
names(dataa) <- c('time', 'o3')
View(dataa)

data_ts <- ts(dataa$o3, frequency = 24)
plot(data_ts, col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))

#stl decomposition
y<-stlplus(data_ts,s.window="period")
plot(y)

#boxplots para ver variancia

O3 <- as.numeric(data)
data_hour_extended <- as.data.frame(cbind(Year,Month,Day,Weekday, Hour, O3))

par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(O3~Month,data=data_hour_extended,col="#FFA726",ylab=(TeX('$PM_{10}$ particles ($\\mu g /m^3 $)')))
boxplot(O3~Weekday,data=data_hour_extended,col="#FFA726",ylab=(TeX('$PM_{10}$ particles ($\\mu g /m^3 $)')))
boxplot(O3~Day,data=data_hour_extended,col="#FFA726",ylab=(TeX('$PM_{10}$ particles ($\\mu g /m^3 $)')))


#média no verao é alta e as variancias no mes 7 e 9 também.


##### vamos aplicar a box-cox transformation porque as variancias nao aparentam ser constantes e isso faz com que a serie nao seja estacionária. 

#### Log-transformation #########################################################

lambda <- BoxCox.lambda(data_ts)
print(lambda)
data_ts.box.cox <- (data_ts^(lambda) - 1)/lambda

#ver se a série has a stationary behavior.

plot(data_ts.box.cox, col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
plot(diff(data_ts.box.cox), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))

#I will confirm this using ADF test and KPSS test too. 

library(aTSA)
adf.test(data_ts.box.cox, alternative = "stationary") #p-value 1%
library(tseries)
kpss.test(data_ts.box.cox, null = c("Trend")) #1%

#o valor do teste pode estar mal porque tenho um grande numero de observações.
#e o valor p tende a diminuir quando tenho um numero muito grande de observações


#I will differentiate the series until I obtain stationary.

#(1 ??? B)^d (1 ??? B^S)^D y_T, onde S remove a seasonal e d remove a trend

acf(data_ts.box.cox[1:8784], lag=100) #sazonal de 24h em 24h; 24
pacf(data_ts.box.cox[1:8784], lag=100)
#como é sazonal de 24 em 24 horas, vou diferenciar de 24 em 24 (seasonal differencing)
plot(diff(data_ts.box.cox,lag = 24), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
acf(diff(data_ts.box.cox[1:8784],lag = 24), lag=100)
pacf(diff(data_ts.box.cox[1:8784],lag = 24), lag=100) #repete se de 24 em 24 hrs
#na net diz para diferenciar novamente https://campus.datacamp.com/courses/forecasting-in-r/forecasting-with-arima-models?ex=4
plot(diff(diff(data_ts.box.cox,lag = 24)), col="#FFA726", xlab=TeX("Time"), ylab=TeX('$0_{3}$ particles'))
acf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=100)
pacf(diff(diff(data_ts.box.cox[1:8784],lag = 24)), lag=100) #repete se de 24 em 24 hrs

#Logo, eu concluo que I can then conclude that d=D=1 (pode ser verificado pelo comando abaixo)
#and T=24.
ndiffs(data_ts.box.cox)
#Let's notice that T=24 is expected since we're dealing with daily data.


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
model_auto<-auto.arima(train_set,max.p=5, max.q=5) #ARIMA(4,1,3)(2,0,0)[24] with drift, AIC=53404.25, BIC=53482.13


#limitar p e q a 5 e p+q<=5
pq<-c()
for(i in 0:5){
  for(j in 0:5){
    if(i==0&j==0 || i+j>5){next}
    pq <- c(pq,c(i,j))
  }
}

#limitar Q e P a c(0,1,2)
Q<-c(0,1,2)
P<-c(0,1,2)

dt_params=c()
for(i in 1:20){
  for(j in 1:3){
    for (k in 1:3){
      temp=c(pq[2*i-1],1,pq[i*2],P[j],1,Q[k],24)
      dt_params=rbind(temp,dt_params)
    }
  }
}

colnames(dt_params)=c("p","d","q","P","D","Q","T")
rownames(dt_params)=1:180

####modelo 


##treinar modelos
models_martins=vector("list", 180)
for(i in 1:180){
  try(models_martins[[i]]<-Arima(train_set, order = dt_params[i,1:3], seasonal = list(order=dt_params[i,4:6],period=24),lambda = NULL))
}

#You can notice that I have added the command try in the loop.
#Indeed I didn't want that the loops stops when the estimation algorithm doesn't converge. 
#It will just provide a void object.

###remover modelos cujos residuos nao sao white noise - teste ljung box

#These tests are sometimes applied to the residuals from an ARMA(p, q) fit,
#in which case the references suggest a better approximation to the
#null-hypothesis distribution is obtained by setting fitdf = p+q,
#provided of course that lag > fitdf.

models<-models_martins
dt_params2=data.frame(dt_params)
dt_params2$residuals=residuos(models)


##########################mudar 


library(stats)
aa=rep(NA,180)
for(i in 1:180){
  if(length(models[[i]]$residuals)>1){
    a=Box.test(x = models[[i]]$residuals,lag = 10,type="Ljung-Box") #lag > p+q=5
    z=prod((a$p.value>0.05))
    if(z==1) aa[i]="y"
    else aa[i]="n"
  }
  
}
dt_params2=data.frame(dt_params)
dt_params2$residuals=aa  #y-> a escolha de parâmetros levou a resíduos white noite

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

#resultados todos nesta tabela
library(DT)
dt_params2$aic=round(dt_params2$aic,4)
dt_params2=na.omit(dt_params2)
datatable(dt_params2,rownames = F)


#We will then select the models with AIC smaller than the one of auto.arima (39072.81) and we display them all.

i=as.numeric(rownames(dt_params2)[which(dt_params2$aic<53404.25)])
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

#Vamos escolher o número 6 (porque é o que tem menor aic e o terceiro com menor bic) e o auto.arima, para comparar
#nota que todos estes têm bic < bic(auto.arima) e aic<aic(auto.arima)

modelo1<-models[[as.numeric(rownames(tabela_final))[20]]]
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
predict(modelo1,n.ahead=5) #este dá os se
f_fit<-forecast(modelo1, h=5) #dão igual #este dá os ic

#plot
x_tr <- window(data_ts.box.cox)
autoplot(x_tr, series="Data") + 
  autolayer(modelo1$fitted, series="SARIMA(5,1,0)(1,1,1)[24]") +
  autolayer(f_fit, series="Prediction") +
  xlab("Day") + ylab("O_3 Level") + ggtitle("Restelo") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")


#predicted vs real 

valores_real <- test_set
valores_predicted <- f_fit$mean[c(1:5)]

#diferença entre valores
valores_predicted-valores_real # = -2.980720 -5.254126 -6.331542 -4.413158 -3.282945
mape(valores_real, valores_predicted) #-> 18%


#This tells us that the mean absolute percent error between
#the 03 level predicted by the model and the actual 03 level is 18%.