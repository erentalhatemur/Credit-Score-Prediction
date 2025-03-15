#Using this dataset for predict someone's credit score
#Kullanacağımız sütunlar:Credit Score bağımlı değişken olacak
options(scipen=999)
library(MASS)
library(VGAM)
library(pscl)
library(caret)
#"Age","Num_Bank_Accounts","Num_Credit_Card","Interest_Rate","Num_of_Loan",     
#"Num_of_Delayed_Payment","Changed_Credit_Limit","Num_Credit_Inquiries"
#"Total_EMI_per_month","Amount_invested_monthly" değişkenleri ilk model için kullanılacak.

#ÖNCELİKLE DATAYI DÜZENLEYELİM
data<-as.data.frame(Credit_Score_Clean[c("Age","Num_Bank_Accounts","Num_Credit_Card","Interest_Rate","Num_of_Loan",     
                                         "Num_of_Delayed_Payment","Changed_Credit_Limit","Num_Credit_Inquiries",
                                         "Total_EMI_per_month","Amount_invested_monthly","Credit_Score")])
View(data)
#10 bağımsız 1 bağımlı değişken var ve ordinal lojistik yapılacak.
summary(data) #BAŞLAMADAN ÖNCE VERİ SETİ İNCELENMELİ


#Öncelikle elimizdeki veriyi kullanarak bir ordinal loj.reg. modeli kurup modelimizin gerekli varsayımları
#sağlayıp sağlamamasına göre optimal modeli oluşturacağız.
data$Credit_Score<-as.factor(data$Credit_Score) #bağımlı değişken factor olarak tanımlanmalıdır.
levels(data$Credit_Score) #GOOD REFERANS OLMUŞTUR


#MODELİ KURMAYA GEÇELİM.ORDİNAL LOJİSTİK REGR. YAPILACAK
#EĞİTİM TEST PARÇALANMASI:
table(data$Credit_Score)
poor_rows <- data[data$Credit_Score == "Poor", ]
std_rows <- data[data$Credit_Score == "Standard", ]
good_rows <- data[data$Credit_Score == "Good", ]
set.seed(155)
creditpoor<-sample(1:nrow(poor_rows),size = 0.85*nrow(poor_rows))
creditstd<-sample(1:nrow(std_rows),size = 0.85*nrow(poor_rows))
creditgood<-sample(1:nrow(good_rows),size = 0.85*nrow(poor_rows))

trainpoor<-poor_rows[-creditpoor,]
trainstd<-std_rows[-creditstd,]

traingood<-good_rows[-creditgood,]
egitimset<-rbind(trainpoor,trainstd,traingood)
nrow(egitimset)
nrow(data)

testpoor<-poor_rows[creditpoor,]
teststd<-std_rows[creditstd,]
testgood<-good_rows[creditgood,]
testset<-rbind(testpoor,teststd,testgood)
nrow(testset)
#EĞİTİM TEST PARÇALANMASI DÜZGÜN BİR DAĞILIMLA YAPILDI!
#şimdi egitim verisi ile model kurulup test ile performans degerlendirmeye gecilecektir.
model_1<-polr(Credit_Score~.,data=egitimset) #train model
summary(model_1)#İLK MODEL
#t değeri < -2 VE >2 OLANLAR ANLAMLIDIR!
#MODELDEN ÇIKARILACAKLAR:LOAN,DELAYED,INQUIRIES,EMI çıkarılmalıdır.
data1<-as.data.frame(Credit_Score_Clean[c("Age","Num_Bank_Accounts","Num_Credit_Card","Interest_Rate"    
                                         ,"Changed_Credit_Limit","Amount_invested_monthly","Credit_Score")])
data1$Credit_Score<-as.factor(data1$Credit_Score)
poor_rows1 <- data1[data1$Credit_Score == "Poor", ]
std_rows1 <- data1[data1$Credit_Score == "Standard", ]
good_rows1 <- data1[data1$Credit_Score == "Good", ]
set.seed(155)
creditpoor1<-sample(1:nrow(poor_rows1),size = 0.85*nrow(poor_rows1))
creditstd1<-sample(1:nrow(std_rows1),size = 0.85*nrow(poor_rows1))
creditgood1<-sample(1:nrow(good_rows1),size = 0.85*nrow(poor_rows1))

trainpoor1<-poor_rows1[-creditpoor1,]
trainstd1<-std_rows1[-creditstd1,]
traingood1<-good_rows1[-creditgood1,]
egitimset1<-rbind(trainpoor1,trainstd1,traingood1)
nrow(egitimset1)
nrow(data)

testpoor1<-poor_rows[creditpoor1,]
teststd1<-std_rows[creditstd1,]
testgood1<-good_rows[creditgood1,]
testset1<-rbind(testpoor1,teststd1,testgood1)
nrow(testset1)

model_2<-polr(Credit_Score~.,data=egitimset1)
summary(model_2)

#modellerin karşılaştırılması:
AIC(model_1);AIC(model_2)
BIC(model_1);BIC(model_2)
#iki metrikte de modelin performansının arttığı gözlemlenmiştir.
pR2(model_1);pR2(model_2) #bu metrikte pek fazla fark gözlemlenmedi.

#STEPWISE REG UYGULAYARAK EN UYGUN MODELİ İLK DATA ÜZERİNDEN BULMAYA ÇALIŞALIM.
step(polr(Credit_Score ~ 1, data = egitimset1),
     direction = "both",
     scope = ~ Age + Num_Bank_Accounts + Num_Credit_Card + 
       Interest_Rate + Changed_Credit_Limit + 
       Amount_invested_monthly)
#STEP SONUCU CIKAN MODEL
model_3<-polr(formula = Credit_Score ~ Interest_Rate + Num_Bank_Accounts + 
                Changed_Credit_Limit + Num_Credit_Card + Amount_invested_monthly + 
                Age, data = egitimset1)
pR2(model_3)

#MODELİ TEST ETME AŞAMASI
#model2 ve model3 için tahmin sonuçlarını alalım.
tahmin2<-predict(model_2,testset1)
tahmin3<-predict(model_3,testset1)

#hatalar
hata2<-residuals(model_2)
hata3<-residuals(model_3)
#dataframe hale gelsin
mod2df<-data.frame("gercekdeger"=testset1$Credit_Score,"tahmin"=tahmin2)

#OLMADI.TAHMİN SONUÇLARI FAZLASI İLE DENGESİZ.MODELİ TEKRAR FARKLI BİR YÖNTEMLE KURMAYA GEÇİYORUZ


#AĞIRLIKLAR İLE VERİ SETİNİ DENGELEYİP TEKRAR MODEL KURUP TEKRAR DEĞERLENDİRECEĞİZ.
# Sınıf frekanslarına göre ağırlık hesaplama
class_weights <- table(egitimset1$Credit_Score)
# Ağırlıkların tersini alarak azınlık sınıfları daha fazla ağırlıklandırma
weights <- 1 / class_weights
# Ağırlıkları normalize etme (toplamları 1 olacak şekilde)
weights <- weights / sum(weights)
weights<-as.numeric(weights)
# Ağırlıkları eğitim veri setine ekleme
egitimset1$weights <- weights[as.factor(egitimset1$Credit_Score)]
testset1$weights<-weights[as.factor(testset1$Credit_Score)]
# Ağırlıklı model
model_weighted <- polr(Credit_Score ~ Interest_Rate + Num_Bank_Accounts + 
                         Changed_Credit_Limit + Num_Credit_Card + Amount_invested_monthly + 
                         Age,data = egitimset1,weights = egitimset1$weights)
summary(model_weighted)
#AIC DEĞERİ MÜKEMMEL SEVİYELERE DÜŞTÜ!
AIC(model_1);AIC(model_2);AIC(model_weighted)

#TESTSET ÜZERİNDEN VERİYİ DEĞERLENDİRME
predweig<-predict(model_weighted,testset1)
gercektahmin<-data.frame("gercekdeger"=testset1$Credit_Score,"tahmin"=predweig)
gercektahmin

#MODELİN TUTARLILIĞINI CONFUSION MATRIX İLE DEĞERLENDİRELİM,AUC YAPALIM
library(caret)
confusionMatrix(testset1$Credit_Score,predweig)
"""Modelimizin doğruluğu 0.455 ile düşük, yani model sınıfları doğru tahmin etmede zorlanıyor.
Kappa değeri 0.1825 ile oldukça düşük, bu da modelin çok iyi bir sınıflandırma yapmadığını gösteriyor.
Sensitivity ve Specificity metriklerinde farklı sınıflar arasında ciddi farklar var. Özellikle Poor sınıfındaki duyarlılık düşük (0.3201).
Balanced Accuracy değerleri, modelin her sınıf için sınıflandırma yaparken zorluk yaşadığını gösteriyor."""


#AYKIRI DEĞER TEMİZLEME
remove_outliers_iqr <- function(data, num_cols) {
  for (col in num_cols) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)  # 1. çeyrek
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)  # 3. çeyrek
    IQR_value <- Q3 - Q1  # IQR hesapla
    lower_bound <- Q1 - 1.5 * IQR_value  # Alt sınır
    upper_bound <- Q3 + 1.5 * IQR_value  # Üst sınır
    
    # Sınırlar dışında kalanları temizle
    data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
  }
  return(data)
}
num_cols <- c("Age","Num_Bank_Accounts","Num_Credit_Card","Interest_Rate"    
              ,"Changed_Credit_Limit","Amount_invested_monthly")
temizegitimset1 <- remove_outliers_iqr(egitimset1, num_cols)
nrow(temizegitimset1)

#Z SCORE İLE TEKRAR TEMİZLEME
remove_outliers_zscore <- function(data, num_cols, threshold = 3) {
  for (col in num_cols) {
    z_scores <- (data[[col]] - mean(data[[col]], na.rm = TRUE)) / sd(data[[col]], na.rm = TRUE)
    data <- data[abs(z_scores) <= threshold, ]  # Z-score > 3 olanları temizle
  }
  return(data)
}
# Z-score ile ekstra aykırı değer temizleme
temizegitimset2 <- remove_outliers_zscore(temizegitimset1, num_cols)
nrow(temizegitimset2)  # Son temizlenen veri seti
summary(temizegitimset2)  # Veriyi inceleme

#MODELİ TEKRAR KURMA
class_weights <- table(temizegitimset2$Credit_Score)
# Ağırlıkların tersini alarak azınlık sınıfları daha fazla ağırlıklandırma
weights <- 1 / class_weights
# Ağırlıkları normalize etme (toplamları 1 olacak şekilde)
weights <- weights / sum(weights)
weights<-as.numeric(weights)
# Ağırlıkları eğitim veri setine ekleme
temizegitimset2$weights <- weights[as.factor(temizegitimset2$Credit_Score)]
testset1$weights<-weights[as.factor(testset1$Credit_Score)]
# Ağırlıklı model
model_weighted_2 <- polr(Credit_Score ~ Interest_Rate + Num_Bank_Accounts + 
                         Changed_Credit_Limit + Num_Credit_Card + Amount_invested_monthly + 
                         Age,data = temizegitimset2,weights = temizegitimset2$weights)
#DEĞERLENDİRME
summary(model_weighted_2)
AIC(model_1);AIC(model_2);AIC(model_weighted);AIC(model_weighted_2)
BIC(model_1);BIC(model_2);BIC(model_weighted);BIC(model_weighted_2)

pR2(model_weighted);pR2(model_weighted_2)

confusionMatrix(model_weighted_2)


predweig2<-predict(model_weighted_2,testset1)
gercektahmin<-data.frame("gercekdeger"=testset1$Credit_Score,"tahmin"=predweig2)
gercektahmin

#MODELİN TUTARLILIĞINI CONFUSION MATRIX İLE DEĞERLENDİRELİM
library(caret)
confusionMatrix(testset1$Credit_Score,predweig2)


#SON OLARAK NİHAİ MODEL OLAN modelweighted_2 varsayımları sağlıyor mu? testi
#1.Orantılılık varsayımı(ordinal loj.reg. için)
install.packages("brant")
library(brant)
brant(model_weighted_2)


#2.Multicol test
library(car)
vif(model_weighted_2)
#VIF değerleri genellikle 1 ile 5 arasında olduğunda, 
#değişkenlerin multikollinearlik (çoklu doğrusal ilişki) açısından ciddi bir sorun teşkil etmediğini gösterir.

#3.DEĞİŞEN VARYANS PROBLEMİ
plot(fitted(model_weighted_2), residuals(model_weighted_2), 
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
