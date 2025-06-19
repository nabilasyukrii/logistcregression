library(dplyr)
library(caret)
library(caTools)
library(MLmetrics)
library(mice)

hasil = read.csv("C:/Users/Dell/Downloads/hasilaaAfixx.csv")
head(hasil)
str(hasil)

#Mengecek missing value
any(is.na(hasil))
summary(hasil)
md.pattern(hasil)

#Mengatasi missing value
hasil$UN=ifelse(is.na(hasil$UN),
                ave(hasil$UN, FUN=function(x)
                  mean(x,na.rm=T)),hasil$UN)


md.pattern(hasil)
boxplot((hasil$UN),main="nilai UN")


#korelasi
cor(hasil)
library(corrplot)
korelasi= cor(hasil[ ,1:8]) #create an object of the features
corrplot.mixed(korelasi)

#Mengatasi outliers
quantile(hasil$UN)
Q =quantile ( hasil$UN, probs= c ( .25 , .75 ) , na.rm = FALSE )
iqr=IQR (hasil$UN)
iqr
up=Q[ 2 ] + 1.5 *iqr # Upper Range  
up
rendah=Q[ 1 ] -1.5 *iqr # Kisaran Bawah
rendah


hasil=subset( hasil, hasil$UN > ( Q [ 1 ] - 1.5 *iqr ) & hasil$UN < ( Q [ 2 ] + 1.5 *iqr ))
boxplot((hasil$UN),main="Nilai UN New")

#perbaikan data 
hasil$AyahDasar=ifelse(hasil$Ayah == 2, 1, 0)
hasil$AyahMenengah=ifelse(hasil$Ayah == 3, 1, 0)
hasil$AyahTinggi=ifelse(hasil$Ayah == 4,1,0)
hasil$IbuDasar=ifelse(hasil$Ibu == 2, 1, 0)
hasil$IbuMenengah=ifelse(hasil$Ibu == 3, 1, 0)
hasil$IbuTinggi=ifelse(hasil$Ibu == 4,1,0)
hasil$ProdiBiologi=ifelse(hasil$Prodi == 2, 1, 0)
hasil$ProdiMtk=ifelse(hasil$Prodi == 3,1,0)
hasil$ProdiPeternakan=ifelse(hasil$Prodi == 4,1,0)
hasil$ProdiPWK=ifelse(hasil$Prodi ==5,1,0)
hasil$ProdiArsitek=ifelse(hasil$Prodi == 6,1,0)
hasil$ProdiFisika=ifelse(hasil$Prodi == 7,1,0)
hasil$ProdiKimia=ifelse(hasil$Prodi == 8,1,0)
hasil$ProdiSisfo=ifelse(hasil$Prodi == 9,1,0)
str(hasil)
dataq = hasil[, -c(2,4,5,7,12,13,14)]
dataq


#Membagi Data
set.seed(101)
sampel = sample.split(Y=dataq$ket, SplitRatio = 0.7)
data.training = subset(dataq, sampel ==T)
data.testing = subset(dataq, sampel == F)

#Model Awal
reglog = glm(ket~., data.training, family = "binomial")
reglog

#uji kecocokan
library(generalhoslem)
model=model.frame(reglog)
logitgof(model$ket, fitted(reglog))


#uji simultan
pR2(reglog)
qchisq(0.95,15)
qchisq(0.95,8)

#uji parsial
summary(reglog)

#odds ratio
exp(coef(reglog))



#Model Rekomendasi1
step(reglog)

#Validasi Model terbaik
kontrol = trainControl(method = "cv", number = 10)
model1 = train(ket~., data.training, method = "glm",
               family = "binomial", trControl = kontrol)

prediksimodel1=predict(model1, data.testing, type="response")

model2 = train(ket ~ JK + Jurusan + UN + ProdiBiologi + ProdiMtk + ProdiPeternakan + 
                 ProdiPWK + ProdiArsitek + ProdiFisika,
               data.training, method = "glm",
               family = "binomial", trControl = kontrol)
summary(model1)
summary(model2)
model2$resample
model1$finalModel
model2$finalModel


modelfix=model2$finalModel
modelfix
summary(modelfix)

##evaluasi
prediksi=predict(modelfix, data.testing, type="response")


ypred=ifelse(prediksi>0.5,1,0)
ypred


confusionMatrix(factor(ypred), factor(data.testing$ket))
