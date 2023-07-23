#Veriler import dataset ile yuklenmistir.
#Tanimlayici istatistikler
names(regresyon_veriler)
names(regresyon_veriler)<-c("y","x1","x2","x3","x4")
attach(regresyon_veriler)
nitel<-as.factor(x4)

summary(regresyon_veriler)
library(moments)
skewness(regresyon_veriler)
kurtosis(regresyon_veriler)

#Normallik testi 
library(nortest)
shapiro.test(y)
ad.test(y)
qqnorm(y)
qqline(y,col="red")
hist(y, col="lightblue")

#ln Donusumu
library(lmtest)
lny<-log(y)
shapiro.test(lny)
ad.test(lny)
qqnorm(lny,col="red")
qqline(lny,col="purple")
boxplot(y, col="lightblue")

#Verilerin cikartilmasi ve yeniden normallik testi 
which(y%in%boxplot(y)$out)
yeni_data<-regresyon_veriler[-c(38,43,67,90,109),]
attach(yeni_data)
names(yeni_data)<-c("?cret","uzakl?k","s?cakl?k","b?y?kl?k","oda tipi")
nitel<-as.factor(x4)
boxplot(y,col="lightblue")
library(nortest)
ad.test(y)
shapiro.test(y)
qqnorm(y,col="blue")
qqline(y)


#Model kurulmasi
my_colors<-c("red","purple","green","blue")
pairs(yeni_data, pch = 15,  cex = 0.7,main="ikili Korelasyon Grafikleri",
      col = my_colors)

pairs(yeni_data, col=my_colors)
sonuc<-lm(y~x1+x2+x3+nitel)
sonuc
summary(sonuc)
n<-125
k<-5
inf<-ls.diag(sonuc)
inf

#Gozlem uzakligi 
hii<-which(inf$hat>0.096)
hii
library(zoo)
hat<-inf$hat
plot(hat, pch= "*", cex=2, main="Hat")
abline(h = 2*6/125 , col="red")
text(x=1:length(hat)+1,y=hat,labels=ifelse(hat>2*6/125,index(hat)," "), col="red")

#Aykiri deger
std_count <- sum(inf$stud.res < -2 | inf$stud.res > 2)
std_count
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")

stud_count <- sum(inf$stud.res < -3 | inf$stud.res > 3)
stud_count
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")

#Etkin Deger(cook uzakligi)
cook<-which(inf$cooks>4/n)
cook
n<-125
k<-5
cooksd <- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="red")

#verileri ayiklama
veri<-yeni_data[-c(33,34,35,36,37,38,39,48,61),]
names(veri)
names(veri)<-c("y","x1","x2","x3","x4")
detach(yeni_data)
attach(veri)
nitel<-as.factor(x4)
sonuc2<-lm(y~x1+x2+x3+nitel)
sonuc2
summary(sonuc2)
inf2<-ls.diag(sonuc2)
inf2


#Gozlem uzakligi 2
hii2<-which(inf2$hat>0.103)
hii2
hat2<-inf2$hat
plot(hat2, pch= "*", cex=2, main="Hat")
abline(h = 2*6/116 , col="purple")
text(x=1:length(hat2)+1,y=hat2,labels=ifelse(hat2>2*6/116,index(hat2)," "), col="blue")

#Aykiri deger 2
n<-116
k<-5
std2<-inf2$std.res
plot(std2, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="purple")
text(x=1:length(std2)+1, y=std2, labels=ifelse(std2<-2 & std2>2,index(std2),""), col="blue")


stud2<-inf2$stud.res
plot(stud2, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="green")
text(x=1:length(stud2)+1, y=stud2, labels=ifelse(stud2<-3 & stud2>3,index(stud2),""), col="blue")

#Etkin Deger2(cook uzakl???)
n<-116
k<-5
cook2<-which(inf2$cooks>4/n)
cook2
cooksd <- cooks.distance(sonuc2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="purple")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="blue")

#Yeniden model kurulmasi
model<-lm(formula=y~x1+x2+x3+nitel)
summary(model)
confint(model,level = .99)

#Degisen Varyanslilik
summary(lm(abs(residuals(model)) ~ fitted(model)))
library(lmtest)
bptest(model)

#Degisen Varyanslilik Grafigi
par(mfrow=c(1,1))
plot(predict(model), inf2$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")
abline(h = c(-3,3) ,col="purple")
text(x=1:length(stud2)+1, y=stud2, labels=ifelse(stud2<-3 & stud2>3,index(stud2),""), col="blue")

#Oziiliski sorununun incelenmesi 
dwtest(model)

#Coklu BAglantinin incelenmesi
cor(veri)
install.packages("olsrr")
library(olsrr)
ols_vif_tol(model)
ols_eigen_cindex(model)


#Ozdeger ve Ozvektor
names(veri)
names(veri)<-c("y","x1","x2","x3","x4")
attach(veri)
x4<-as.factor(x4)
library(fastDummies)
dummy<-dummy_cols(x4)
x41<-dummy$.data_1
x42<-dummy$.data_2
x43<-dummy$.data_3

ort1<-mean(x1)
kt1<-sum((x1-ort1)^2)
skx1<-(x1-ort1)/(kt1^0.5)
ort2<-mean(x2)
kt2<-sum((x2-ort2)^2)
skx2<-(x2-ort2)/(kt2^0.5)
ort3<-mean(x3)
kt3<-sum((x3-ort3)^2)
skx3<-(x3-ort3)/(kt3^0.5)
ort42<-mean(x42)
kt42<-sum((x42-ort42)^2)
skx42<-(x42-ort42)/(kt42^0.5)
ort43<-mean(x43)
kt43<-sum((x43-ort43)^2)
skx43<-(x43-ort43)/(kt43^0.5)
x<-cbind(skx1,skx2,skx3,skx42,skx43)
sm<- eigen (t(x)%*%x)
signif(sm$values,3)
signif(sm$vectors,3)
V<-sm$vectors
t(V)%*%V
V %% diag(sm$values) %% t(V)

#Uyum Kestirimi
predict(model,interval = "confidence",level=0.95)[58,]

#on kestirim
onkestirim<-data.frame(x1=3,x2=4,x3=10,x4=x43)
x4<-as.factor(nitel)
predict(model,onkestirim,interval = "prediction",level=0.95)

#ileriye Dogru Secim Yontemi 
library(stats)
lm.null <- lm(y ~ 1)
x4<-as.factor(x4)
forward <- step(lm.null,y~x1+x2+x3+x4, direction = "forward")
forward
summary(forward)

#Geriye Doðru Cýkarma Secim Yontemi
backward<-step(model,direction="backward")
summary(backward)

#Adimsal Secim Yontemi
library(MASS)
step.model <- stepAIC(model, direction = "both", trace = FALSE)
summary(step.model)

#Ridge Regresyon 
ridge <- lm.ridge(y~x1+x2+x3+x4 ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef

select(ridge)
ridge$coef[,ridge$lam == 0.4]

