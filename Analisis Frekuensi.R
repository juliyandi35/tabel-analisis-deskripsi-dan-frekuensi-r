# Memuat paket yang diperlukan
library(dplyr)
library(readxl)
library(tidyr)

set.seed(123) # Untuk reproduktibilitas
Data <- read_excel("Keterampilan Sosial_13.01.25.xlsx",sheet = "Input")
Sosiografi <- Data[,1:7]
data <- Data[,-c(1:7)]

#### Analisis Frekuensi ####
# Bobot untuk jawaban (STS = 1, TS = 2, S = 3, SS = 4)
bobot <- c(1, 2, 3, 4)
data <- data %>%
  mutate(across(everything(), ~ recode(.,
                                       "STS" = 1,
                                       "TS" = 2,
                                       "S" = 3,
                                       "SS" = 4)))


# Menghitung skor total untuk setiap indikator
data <- data %>%
  mutate(
    Peer_Relationship_Total = rowSums(across(starts_with("Peer_Relationship"))),
    Manajemen_Diri_Total = rowSums(across(starts_with("Manajemen_Diri"))),
    Kesuksesan_Akademik_Total = rowSums(across(starts_with("Kesuksesan_Akademik"))),
    Kepatuhan_Total = rowSums(across(starts_with("Kepatuhan"))),
    Asertif_Total = rowSums(across(starts_with("Asertif")))
  )

# Menentukan kategori berdasarkan interval
kategori <- function(nilai, interval) {
  case_when(
    nilai >= interval[1] & nilai <= interval[2] ~ "Sangat Rendah",
    nilai > interval[2] & nilai <= interval[3] ~ "Rendah",
    nilai > interval[3] & nilai <= interval[4] ~ "Tinggi",
    nilai > interval[4] ~ "Sangat Tinggi",
    TRUE ~ NA_character_
  )
}

# Interval untuk setiap indikator
intervals <- list(
  Peer_Relationship = c(9, 15.75, 22.5, 29.25, 36),
  Manajemen_Diri = c(9, 15.75, 22.5, 29.25, 36),
  Kesuksesan_Akademik = c(5, 8.75, 12.5, 16.25, 20),
  Kepatuhan = c(2, 3.5, 5, 6.5, 8),
  Asertif = c(7, 12.25, 17.5, 22.75, 28)
)

# Menambahkan kategori ke data
data <- data %>%
  mutate(
    Peer_Relationship_Kategori = kategori(Peer_Relationship_Total, intervals$Peer_Relationship),
    Manajemen_Diri_Kategori = kategori(Manajemen_Diri_Total, intervals$Manajemen_Diri),
    Kesuksesan_Akademik_Kategori = kategori(Kesuksesan_Akademik_Total, intervals$Kesuksesan_Akademik),
    Kepatuhan_Kategori = kategori(Kepatuhan_Total, intervals$Kepatuhan),
    Asertif_Kategori = kategori(Asertif_Total, intervals$Asertif)
  )

# Kategori lengkap yang akan selalu muncul
kategori_lengkap <- c("Sangat Rendah", "Rendah", "Tinggi", "Sangat Tinggi")

# Fungsi hasil yang menyertakan kategori dengan frekuensi 0
hasil <- function(column) {
  # Pastikan kolom menjadi faktor dengan level kategori lengkap
  column <- factor(column, levels = kategori_lengkap)
  
  freq <- table(column)
  persen <- prop.table(freq) * 100
  data.frame(
    Kategori = names(freq),
    Frekuensi = as.vector(freq),
    Persentase = as.vector(persen)
  )
}

# Membuat tabel hasil untuk setiap indikator
tabel_peer <- hasil(data$Peer_Relationship_Kategori)
tabel_manajemen <- hasil(data$Manajemen_Diri_Kategori)
tabel_akademik <- hasil(data$Kesuksesan_Akademik_Kategori)
tabel_kepatuhan <- hasil(data$Kepatuhan_Kategori)
tabel_asertif <- hasil(data$Asertif_Kategori)

# Menampilkan tabel hasil
list_tabel <- list(
  Peer_Relationship = tabel_peer,
  Manajemen_Diri = tabel_manajemen,
  Kesuksesan_Akademik = tabel_akademik,
  Kepatuhan = tabel_kepatuhan,
  Asertif = tabel_asertif
)

# Menambahkan kolom "Dimensi" dan memindahkannya ke posisi pertama
list_tabel_renamed <- lapply(names(list_tabel), function(name) {
  tabel <- list_tabel[[name]]
  tabel$Dimensi <- name # Menambahkan kolom "Dimensi"
  tabel <- tabel[, c("Dimensi", setdiff(names(tabel), "Dimensi"))] # Menjadikan "Dimensi" kolom pertama
  return(tabel)
})

# Menggabungkan semua tabel menjadi satu data frame
df_final <- do.call(rbind, list_tabel_renamed)

# Mengecek hasil
print(df_final)

# Mengekspor ke file Excel
library(writexl)
write_xlsx(df_final, "Analisis Frekuensi.xlsx")

