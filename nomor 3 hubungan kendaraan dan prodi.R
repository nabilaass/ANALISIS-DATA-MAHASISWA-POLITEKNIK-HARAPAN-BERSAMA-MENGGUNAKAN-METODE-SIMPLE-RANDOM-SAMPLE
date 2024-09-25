library(readxl)
data <- read_excel("data.xlsx", col_types = c("text", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric"))
View(data)

library(dplyr)
data = mutate(data, jurusan =
                case_when(
                  data$Prodi == 0  ~ "informatika",
                  data$Prodi == 1  ~ "ASP",
                  data$Prodi == 2  ~ "T.komputer",
                  data$Prodi == 3  ~ "T.mesin",
                  data$Prodi == 4  ~ "T.elektro",
                  data$Prodi == 5  ~ "Akuntansi",
                  data$Prodi == 6  ~ "Farmasi",
                  data$Prodi == 7  ~ "Kebidanan",
                  data$Prodi == 8  ~ "PerhoteLAN",
                  data$Prodi == 9  ~ "DKV"
                ),
              kendaraan = case_when(
                data$Kendaraan == 1 ~ "SEPEDA MOTOR",
                data$Kendaraan == 2 ~ "MOBIL",
                data$Kendaraan == 3 ~ "ANGKUTAN UMUM"
              )
)

table(data$jurusan)
prop.table(table(data$jurusan))*100

table(data$kendaraan)
prop.table(table(data$kendaraan))*100

addmargins(table(data$jurusan,data$kendaraan))

prop.table(table(data$jurusan,data$kendaraan), margin = 2)*100

barplot(prop.table(table(data$kendaraan,
                         data$jurusan),
                   margin = 2) *100,beside = TRUE,
        legend.text = TRUE, ylab = "Persen", col = c("blue", "red", "yellow")
)