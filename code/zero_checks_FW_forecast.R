#notes----
#author: Sara E Miller 
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: July 2018

# load ----
source("code/helper.R")
source("code/functions.R")

#data----
read.csv('data/non_zero_Chilkat_FW.csv') -> data

#analysis----
output <- lm(formula = (esc) ~ sw23_25,data=data)
#summary(Model1)#outputs P-value and R^2
newdata<-data.frame(sw23_25 = 10.8)#2018 value
predictions<-predict(output, newdata, se.fit=TRUE)
pred.w.plim <- predict(output, newdata, interval = "prediction", level=0.80) #same as Zar (1999) 17.29
pred.w.clim <- predict(output, newdata, interval = "confidence", level=0.80)
citation("stats")


Diagnostics<-autoplot(Model1, label.size=3)
par(mfrow=c(2,2)) 
plot(Model1, which=1, main="Figure A")
plot(Model1, which=2, main="Figure B")
plot(Model1, which=3, main="Figure C")
plot(Model1, which=4, main="Figure D")
par(mfrow=c(1,2)) 
plot(Model1, which=5, main="Figure E")
plot(Model1, which=6, main="Figure F")

png(file='figs/forecast_plot.png', res=200, width=12, height=7, units ="in") 
fit = lm(esc ~ sw23_25, data = data)
d = data.frame(data, predict(fit, interval="prediction"))
d$residuals = residuals(fit)
p1 = ggplot(d,aes(x=sw23_25,y=esc)) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method="lm",aes(fill='confidence'),alpha=0.3) +
  geom_smooth(method="lm",se=FALSE,color='blue') +
  geom_point() +
  scale_fill_manual('interval', values = c('grey', 'yellow')) +
  labs(title="SW23-25",x="Fish wheel catch non-zero checks (cumulative)", y="Escapement")+
  theme(legend.position="none")

fit = lm(esc ~ sw26, data = data)
d = data.frame(data, predict(fit, interval="prediction"))
d$residuals = residuals(fit)
p2 = ggplot(d,aes(x=sw26,y=esc)) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method="lm",aes(fill='confidence'),alpha=0.3) +
  geom_smooth(method="lm",se=FALSE,color='blue') +
  geom_point() +
  scale_fill_manual('interval', values = c('grey', 'yellow')) +
  labs(title="SW26",x="Fish wheel catch non-zero checks (cumulative)", y="Escapement")+
  theme(legend.position="none")

fit = lm(esc ~ sw27, data = data)
d = data.frame(data, predict(fit, interval="prediction"))
d$residuals = residuals(fit)
p3 = ggplot(d,aes(x=sw27,y=esc)) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method="lm",aes(fill='confidence'),alpha=0.3) +
  geom_smooth(method="lm",se=FALSE,color='blue') +
  geom_point() +
  scale_fill_manual('interval', values = c('grey', 'yellow')) +
  labs(title="SW27",x="Fish wheel catch non-zero checks (cumulative)", y="Escapement")+
  theme(legend.position="none")

fit = lm(esc ~ sw28, data = data)
d = data.frame(data, predict(fit, interval="prediction"))
d$residuals = residuals(fit)
p4 = ggplot(d,aes(x=sw28,y=esc)) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method="lm",aes(fill='confidence'),alpha=0.3) +
  geom_smooth(method="lm",se=FALSE,color='blue') +
  geom_point() +
  scale_fill_manual('interval', values = c('grey', 'yellow')) +
  labs(title="SW28",x="Fish wheel catch non-zero checks (cumulative)", y="Escapement")+
  theme(legend.position="none")


g4<-arrange_ggplot2(p1,p2,p3,p4, ncol=2)
dev.off() 


png(file='Diagnostics.png', res=200, width=6, height=4, units ="in")  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
vplayout<-function(x,y) viewport (layout.pos.row=x, layout.pos.col=y)
print(Diagnostics,vp=vplayout(1,1:1)) 
dev.off()