# Memuat paket yang diperlukan
library(dplyr)
library(readxl)
library(tidyr)

set.seed(123) # Untuk reproduktibilitas
Data <- read_excel("Keterampilan Sosial_13.01.25.xlsx",sheet = "Input")
Sosiografi <- Data[,1:7]
data <- Data[,-c(1:7)]

# Bobot untuk jawaban (STS = 1, TS = 2, S = 3, SS = 4)
bobot <- c(1, 2, 3, 4)
data <- data %>%
  mutate(across(everything(), ~ recode(.,
                                       "STS" = 1,
                                       "TS" = 2,
                                       "S" = 3,
                                       "SS" = 4)))
#### Analisis Deskriptif ####
# Memilih hanya kolom numerik
data_numerik <- data %>% select(where(is.numeric))

statistik <- data.frame(
  Variabel = names(data_numerik),
  Minimum = apply(data_numerik, 2, min),
  Maksimum = apply(data_numerik, 2, max),
  Median = apply(data_numerik, 2, median),
  Mean = apply(data_numerik, 2, mean),
  StandarDeviasi = apply(data_numerik, 2, sd)
)

# Menampilkan tabel
print(statistik)

# Mengekspor hasil ke file Excel
write_xlsx(statistik, "Tabel Deskriptif.xlsx")
