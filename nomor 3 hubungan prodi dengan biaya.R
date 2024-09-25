library(readxl)
data <- read_excel("C:/Users/Lenovo/Downloads/data.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
View(data)

library(dplyr)
data = mutate(data, jk =
                if_else(data$`Jenis kelamin` == 0 ,"pria","wanita"),
                biaya = case_when(
                    data$Pembiayaan == 0 ~ "orangtua/wali",
                    data$Pembiayaan == 1 ~ "keduanya",
                    data$Pembiayaan == 2 ~ "mandiri"
                  )
)

table(data$jk)
prop.table(table(data$jk))*100

table(data$biaya)
prop.table(table(data$biaya))*100

addmargins(table(data$jk,data$biaya))

prop.table(table(data$jk,data$biaya), margin = 2)*100

barplot(prop.table(table(data$jk,
                         data$biaya),
                   margin = 2) *100,beside = TRUE,
        legend.text = TRUE, ylab = "Persen", col = c( "yellow", "pink")
)