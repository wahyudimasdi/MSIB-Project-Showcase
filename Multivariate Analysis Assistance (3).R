### MANOVA 1 ARAH ###

# Contoh 1
# Sebuah penelitian dilakukan untuk menguji pengaruh perlakuan yang berbeda (A, B, dan C) terhadap dua variabel dependen, yaitu Variabel1 dan Variabel2. 
# Sebanyak 30 subjek dibagi menjadi tiga kelompok perlakuan, yaitu kelompok A, B, dan C, dengan masing-masing 10 subjek.
# Peneliti tertarik untuk mengetahui apakah terdapat perbedaan yang signifikan dalam respons terhadap perlakuan antara kelompok-kelompok perlakuan tersebut.

data <- data.frame(
  Group = factor(c(rep("A", 10), rep("B", 10), rep("C", 10))), # Variabel kelompok
  Variable1 = rnorm(30, mean = c(10, 15, 20), sd = 2), # Variabel dependen
  Variable2 = rnorm(30, mean = c(8, 12, 18), sd = 3) # Variabel dependen lainnya
)

# Melakukan MANOVA
result <- manova(cbind(Variable1, Variable2) ~ Group, data = data)

# Menampilkan hasil
summary(result)

# Nilai P-value lebih dr alpha = 0.05, 
# yang berarti kita tidak dapat menolak hipotesis nol Pillai test
# berarti bahwa tidak ada perbedaan yang signifikan dalam respons 
# terhadap perlakuan antara kelompok-kelompok tersebut.

# Parameter statistik pilai manual
p = 3-1 #jumlah grup-1 #kl dr summary yg df grup 
h = 4 # p*jlh dep var = (3-1)*2 #kl dari summary df model
e = 30 - p - 1 #kl dari summary ini residual
s = min(p,h)
N = (e-p-1)/2
m = (abs(p-h)-1)/2

# Contoh 2 
View(iris)
dependent_vars <- cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
independent_var <- iris$Species

manova_model <- manova(dependent_vars ~ independent_var, data = iris)
summary(manova_model)

# Nilai P-value kurang dr alpha = 0.05, 
# yang berarti kita dapat menolak hipotesis nol Pillai test
# berarti bahwa setidaknya satu vektor rata-rata grup berbeda dari yang lainnya.

# Parameter statistik pilai manual
p = 3-1 #jumlah spesies -1 #kl dr summary yg df grup 
h = 8 # p*jlh dep var = (3-1)*4 #kl dari summary df model
e = 150 - p - 1 #kl dari summary ini residual
s = min(p,h)
N = (e-p-1)/2
m = (abs(p-h)-1)/2

library(effectsize)
eta_squared(manova_model)
# Salah satu metrik yang sering digunakan dengan MANOVA adalah Partial Eta Squared. 
# Metrik ini  mengukur efek yang dimiliki variabel independen terhadap variabel dependen. 
# Jika nilai tersebut adalah 0,14 atau lebih besar, kita dapat mengatakan bahwa ukuran efeknya besar.
# Nilainya adalah 0,6, yang berarti ukuran efeknya besar. 

# Melakukan uji post hoc Tukey untuk masing-masing variabel dependen
library(stats)
tukey_results <- lapply(1:ncol(dependent_vars), function(i) {
  aov_model <- aov(dependent_vars[,i] ~ independent_var, data = iris)
  tukey_result <- TukeyHSD(aov_model)
  return(tukey_result)
})
names(tukey_results) <- colnames(dependent_vars)
tukey_results

# Pada setiap sepal dan petal untuk setiap spesies (kelompok),
# nilai p-adjusted untuk semua perbandingan antara kelompok adalah 0, 
# hal ini menunjukkan bahwa perbedaan tersebut signifikan secara statistik.

# Analisis Diskriminan Linier (LDA), 
# untuk mencari kombinasi linear fitur yang paling baik memisahkan dua atau lebih kelompok.
# Dengan melakukan ini, kita akan dapat memvisualisasikan plot pencar yang menunjukkan dua diskriminan linear pada sumbu X dan Y, 
# dan memberi kode warna pada mereka untuk sesuai dengan variabel independen kita - spesies bunga.

library(MASS)
iris_lda <- lda(independent_var ~ dependent_vars, CV = F)
iris_lda

lda_df <- data.frame(
  species = iris[, "Species"],
  lda = predict(iris_lda)$x
)
lda_df

library(ggplot2)
ggplot(lda_df) +
  geom_point(aes(x = lda.LD1, y = lda.LD2, color = species), size = 4) +
  theme_classic()

### MANOVA 2 ARAH ###
# Contoh data
data2 <- data.frame(
  Treatment = factor(rep(c("A", "B", "C"), each = 6)), # Variabel faktor pertama
  Gender = factor(rep(c("Male", "Female"), times = 9)), # Variabel faktor kedua
  Response1 = c(4, 3, 6, 5, 8, 7, 9, 10, 12, 11, 14, 13, 16, 15, 18, 17, 20, 19), # Variabel dependen 1
  Response2 = c(2, 3, 1, 4, 5, 6, 7, 8, 9, 10, 8, 7, 5, 6, 3, 4, 1, 2) # Variabel dependen 2
)

# Melakukan MANOVA
result2 <- manova(cbind(Response1, Response2) ~ Treatment * Gender, data = data2)

# Menampilkan hasil
summary(result2)

# Pada hasil 2 way MANOVA tersebut terdapat 1 efek yang signifikan.
# Melalui pillai test didapat p-value untuk treatment kurang dr alpha =0.05,
# oleh karena itu hipotesis nol pillai test ditolak sehingga,
# Hal ini berarti bahwa Treatment menunjukkan dampak yang besar yang menunjukkan perbedaan.
# sementara untuk gender dan interaksi kedua faktor tidak berpengarih signifikan terhadap variabel dependen.
# Hal ini menggarisbawahi bahwa pentingya mempertimbangkan efek gabungan dari treatment dan gender 
# untuk melihat beda efek yang amati pada kasus tersebut.


### PCA ###
# Contoh data
data3 <- iris[,1:4] # Menggunakan dataset iris sebagai contoh

# Standarisasi data (opsional, tapi direkomendasikan untuk PCA)
scaled_data <- scale(data3)

# Melakukan PCA
pca_result <- prcomp(scaled_data)

# Menampilkan hasil
summary(pca_result)

# Dari hasil PCA ini, kita dapat menyimpulkan bahwa PC1 memiliki 
# deviasi standar yang tinggi dan menjelaskan sebagian besar varians data, 
# sementara PC2 dan komponen lainnya memiliki deviasi standar yang lebih rendah 
# dan menjelaskan sebagian kecil varians tambahan. 
# Hal ini menunjukkan bahwa PC1 mungkin mengandung informasi yang paling relevan 
# dan signifikan dalam menjelaskan pola atau struktur dalam data.

# Mengakses vektor beban untuk setiap komponen utama
loadings <- pca_result$rotation

# Mencetak persamaan linier untuk setiap komponen utama
for (i in 1:ncol(loadings)) {
  cat("Persamaan untuk PC", i, ":\n")
  cat("PC", i, " = ")
  for (j in 1:nrow(loadings)) {
    cat(loadings[j, i], "* X", j)
    if (j < nrow(loadings)) {
      cat(" + ")
    }
  }
  cat("\n\n")
}

### FACTOR ANALYSIS ###
#install.packages("psych") # Instal paket psych jika belum diinstal
library(psych)

# Contoh data
data4 <- mtcars[, c(1:7, 10, 11)] # Menggunakan dataset mtcars sebagai contoh

# Menghapuskan missing values jika ada
data4 <- na.omit(data4)

# Melakukan Analisis Faktor
factor_analysis_result <- fa(data4, nfactors = 3, rotate = "varimax")
print(factor_analysis_result)

# Tiga faktor diidentifikasi dalam analisis disebut sebagai MR1, MR2, dan MR3. 
# Proporsi varians yang dijelaskan oleh masing-masing faktor adalah 51% untuk MR1, 32% untuk MR2, dan 18% untuk MR3. 

# Model yang diuji memiliki kesesuaian yang lebih baik dengan data daripada model nol. 
# Hal ini terlihat dari nilai yang lebih rendah dari fungsi objektif dan chi-square untuk model yang diuji dibandingkan dengan model nol. 
# Selain itu, karena derajat kebebasan model yang diuji lebih rendah daripada model nol, 
# Hal ini menunjukkan bahwa model yang diuji lebih sederhana dan lebih cocok dengan data dengan baik. 

# Correlation of (regression) scores with factors, nilainya > 70% bahkan mencapai 90an %.
# Multiple R Square of Scores with Factors, menunjukkan seberapa banyak variabilitas dalam skor faktor 
# yang dapat dijelaskan oleh faktor-faktor yang diekstraksijuga nilainya mendekati 1 (sangat besar)
# Minimum Correlation of Possible Factor Scores juga besar berarti bahwa faktor konsisten dan memiliki hubungan yang kuat dengan faktor lainnya. 

### Sekian Responsi 1nya ###