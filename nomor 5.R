library(readxl)
data <- read_excel("C:/Users/Lenovo/Downloads/data.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
View(data)
summary(data)

par(mfrow=c(2,2))
hist(data$`IPK`,main="Histogram IPK",xlab="IPK", las=1)
hist(data$`Jam belajar`, main = "Histogram Lama Belajar",
                                xlab="Lama Belajar", las=1)
boxplot(data$`Jam belajar`, data$`IPK`,
                                main="Boxplot IPK vs Lama Belajar",
                                names=c("Lama Belajar","IPK"))
plot(data$`Jam belajar`, data$`IPK`, main="Scatterplot IPK vs Lama Belajar",
                                 xlab="Lama Belajar",
                                 ylab="IPK", pch=15,las=1)
abline(lm(data$`IPK`~data$`Jam belajar`),col="red")

waktu_belajar = data$`Jam belajar`
IPK=data$`IPK`
cor.test(waktu_belajar, IPK)

Model_Regresi = summary(lm(IPK~waktu_belajar))
Model_Regresi
