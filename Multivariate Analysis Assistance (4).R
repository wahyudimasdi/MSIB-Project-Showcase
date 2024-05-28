### Analisis Diskriminan 2 kelompok ###
library(MASS)
library(caret)
library(MVN)
data(iris)
iris_sub <- iris[iris$Species %in% c("setosa", "versicolor"),] #virginica gadipake

# Model analisis diskriminan
model_discriminant <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris_sub)
model_discriminant

# hanya ada satu fungsi diskriminan yang dihasilkan. 
# karena hanya ada dua kelompok yang dibandingkan, 
# sehingga satu fungsi diskriminan sudah cukup untuk memisahkan kelas-kelas tersebut secara optimal.
# LD itulah yang digunakan untuk membedakan antara kelompok "setosa" dan "versicolor" 
# berdasarkan variabel Sepal.Length dan Sepal.Width.

pred_LDA1<- predict(model_discriminant, iris_sub)
confmatLDA1<-confusionMatrix(pred_LDA1$class,as.factor(Species),positive = "0")
confmatLDA1

# Membuat scatterplot
library(ggplot2)
ggplot(data = iris_sub, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Sepal Width", color = "Species") +
  ggtitle("Scatterplot of Sepal Length vs Sepal Width by Species")

### Analisis Diskriminan Ganda ###
buah <- data.frame(
  Jenis = factor(c("Apel", "Apel", "Apel", "Jeruk", "Jeruk", "Jeruk", "Pisang", "Pisang", "Pisang")),
  Berat = c(150, 160, 155, 200, 190, 180, 120, 130, 125),
  Tinggi = c(15, 16, 14, 10, 9, 11, 20, 18, 19),
  Diameter = c(8, 9, 7, 6, 7, 6.5, 4, 4.5, 5))

library(MASS)
#pengujian asumsi
#cek outlier
library(MVN)
hasildata<-mvn(buah[,2:4], multivariateOutlierMethod = "adj", showNewData = TRUE)
databaru<-hasildata$newData
databaru

## Normal multivariate jika p-value > 0.05=alpha
hasil<- mvn(databaru, mvnTest = '',  alpha = 0.05)
hasil

#cek korelasi dan multikolinearitas
kor<-cor(databaru)
kor

# Model analisis diskriminan ganda
model_discriminant <- lda(Jenis ~ Berat + Tinggi + Diameter, data = buah)
model_discriminant

plot(model_discriminant, col = c("red", "blue", "green"))

pred_LDA1<- predict(model_discriminant, buah)
confmatLDA1<-confusionMatrix(pred_LDA1$class,as.factor(Jenis),positive = "0")
confmatLDA1

### Analisis klasifikasi ###
library(ggplot2)
library(randomForest)
library(datasets)
library(caret)
data<-iris
str(data)
data$Species <- as.factor(data$Species)
table(data$Species)
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]
rf <- randomForest(Species~., data=train, proximity=TRUE) 
rf
p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)D
p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)
