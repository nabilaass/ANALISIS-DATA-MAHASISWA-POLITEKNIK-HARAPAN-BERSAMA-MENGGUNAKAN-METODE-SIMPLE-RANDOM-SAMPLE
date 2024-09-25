library(readxl)
data <- read_excel("C:/Users/Lenovo/Downloads/data.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
View(data)

library(ggplot2)
ggplot(data, aes(x=IPK, y=`Jam belajar`)) + geom_point(size=5)+
  labs(x="NILAI IPK", y="Jam belajar(jam)")

library(dplyr)
data = mutate(data, Jam_belajar= case_when(
  data$`Jam belajar` <= 1 ~ "cepat    ",
  data$`Jam belajar` <= 2 ~ "sedang  ",
  data$`Jam belajar` >= 2 ~ "lama     "
),
nilai = case_when(
  data$IPK <= 3 ~ "grade C            ",
  data$IPK <= 3.5 ~ "grade B          ",
  data$IPK >= 3.5 ~ "grade A          "))


table(data$Jam_belajar)
table(data$nilai)

ggplot(data, aes(x=IPK, y=`Jam belajar`, color = Jam_belajar)) +
  geom_point(size=3)+
  labs(x="Nilai IPK mahasiswa PHB", y="Jam belajar(jam)")

databaru = filter(data, `Jam belajar` < 3)
databaru

cor(databaru$IPK, databaru$`Jam belajar`)

lm(`Jam belajar`~IPK, databaru)

plot(databaru$IPK, databaru$`Jam belajar`, pch=16,
     xlab = "Nilai IPK mahasiswa PHB",
     ylab = "Jam belajar(jam)")
abline(lm(databaru$`Jam belajar` ~ databaru$IPK), col=2, lwd=2)
text(3.4, 40, paste("y = 0.5493+0.2339x"))

model = lm(`Jam belajar` ~ IPK, databaru)
residu = resid(model)
plot(databaru$IPK, residu, pch=16,
     xlab = "Nilai IPK mahasiswa PHB",
     ylab = "residu regresi model")
abline(h=0, col=2, lwd=2)

summary(model)

nilaibaru = data.frame(IPK=3.85)
predict(model, nilaibaru)