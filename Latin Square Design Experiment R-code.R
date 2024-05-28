rm(list = ls(all = TRUE))
graphics.off()
shell("cls")
.libPaths( "C:/Users/wahyu/OneDrive/Documents/R/win-library/4.1/00LOCK" )
install.packages("shinyBS", lib="C:/Users/wahyu/OneDrive/Documents/R/win-library/4.1/00LOCK")
library(readxl)

UAS
str(UAS)
UAS$ubi <- as.factor(UAS$ubi)
UAS$area <- as.factor(UAS$area)
UAS$pupuk = as.factor(UAS$pupuk)
str(UAS)

attach(UAS)
model <- lm(formula = score ~ ubi + area + pupuk)
anova(model)

library(agricolae)
data(s)
model<-aov(score ~ ubi + area + pupuk,data=UAS)
out <- duncan.test(model,"virus", 
                   main="Yield of sweetpotato. Dealt with different virus")
plot(out,variation="IQR")
duncan.test(model,"virus",alpha=0.05,console=TRUE)
# version old duncan.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(sweetpotato,duncan.test(yield,virus,df,MSerror, group=TRUE))
plot(out,horiz=TRUE,las=1)
print(out$groups)