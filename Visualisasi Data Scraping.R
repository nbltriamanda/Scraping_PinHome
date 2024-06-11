# ---- Packages ----
message('Loading Packages')
library(rvest)
library(tidyverse)
library(mongolite)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
library(ggplot2)

# --------- MongoDB ---------
message('Ambil data dari MongoDB Atlas')
library(mongolite)
# Nama koleksi
collection <- "Hunian_Full"
# Nama database
db <- "scraping"
# Koneksi ke mongoDB
url <- "mongodb+srv://amandatriamanda:Juni222001@cluster0.tdwaaer.mongodb.net/"
# Membuat koneksi ke MongoDB
Hunian <- mongo(collection = collection, db = db, url = url)
# Ambil data dari MongoDB
Data_Properti <- Hunian$find()
# Cek struktur data yang diambil
str(Data_Properti)

# ------- PREPROCESSING DATA ---------
#Cleaning Data Spesifikasi
message('Preprocessing Data')
Data_Properti <- Data_Properti %>%
  mutate(
    Spesifikasi_KT = str_extract(Spesifikasi, "\\d+(?:-\\d+)? KT"),
    Spesifikasi_LT = str_extract(Spesifikasi, "LT \\d+(?:-\\d+)?m²"),
    Spesifikasi_LB = str_extract(Spesifikasi, "LB \\d+(?:-\\d+)?m²")
  ) %>%
  mutate(
    Spesifikasi_KT = str_replace(Spesifikasi_KT, " KT", ""),
    Spesifikasi_LT = str_replace_all(Spesifikasi_LT, c("LT " = "", "m²" = "")),
    Spesifikasi_LB = str_replace_all(Spesifikasi_LB, c("LB " = "", "m²" = ""))
  )
# Hapus spasi dari kolom Spesifikasi_KT, Spesifikasi_LT, dan Spesifikasi_LB
Data_Properti <- Data_Properti %>%
  mutate(
    Spesifikasi_KT = str_replace_all(Spesifikasi_KT, "\\s+", ""),
    Spesifikasi_LT = str_replace_all(Spesifikasi_LT, "\\s+", ""),
    Spesifikasi_LB = str_replace_all(Spesifikasi_LB, "\\s+", "")
  )
# Convert Skala Harga dalam Ribu Rupiah
# Fungsi untuk mengonversi nilai dan Range menjadi Ribu Rupiah
convert_to_ribu_rupiah <- function(value) {
  value <- gsub("\\s+", "", value)
  value <- gsub("Jt", "*1000", value)
  value <- gsub("Rb", "", value)
  value <- gsub("M", "*1000000", value)
  return(eval(parse(text=value)))
}
convert_range_to_ribu_rupiah <- function(range) {
  parts <- strsplit(range, "-")[[1]]
  min_value <- convert_to_ribu_rupiah(parts[1])
  if (length(parts) == 2) {
    max_value <- convert_to_ribu_rupiah(parts[2])
    return(paste(min_value, max_value, sep = " - "))
  } else {
    return(as.character(min_value))
  }
}
# Convert kolom Harga dan Angsuran
Data_Properti$Harga <- sapply(Data_Properti$Harga, convert_range_to_ribu_rupiah)
Data_Properti$Angsuran <- sapply(Data_Properti$Angsuran, convert_to_ribu_rupiah)
# Convert kolom Harga dan Angsuran ke numerik
Data_Properti$Harga <- as.character(Data_Properti$Harga)
Data_Properti$Angsuran <- as.numeric(Data_Properti$Angsuran)

#---- PREPROCESSING Untuk VISUALISASI 
# Pisahkan rentang harga menjadi dua kolom
Data_Properti <- Data_Properti %>%
  separate(Harga, into = c("Harga_Minimal", "Harga_Maksimal"), sep = " - ", fill = "right", convert = TRUE)
# Pisahkan rentang harga menjadi dua kolom
Data_Properti <- Data_Properti %>%
  mutate(Harga_Med = ifelse(is.na(Harga_Maksimal), Harga_Minimal, (Harga_Minimal + Harga_Maksimal) / 2))
# Memisahkan kolom Spesifikasi_KT 
Data_Properti <- Data_Properti %>%
  separate(Spesifikasi_KT, into = c("KT_Minl", "KT_Max"), sep = "-", fill = "right", extra = "merge", convert = TRUE)
# Memisahkan kolom Spesifikasi_LT 
Data_Properti <- Data_Properti %>%
  separate(Spesifikasi_LT, into = c("LT_Min", "LT_Max"), sep = "-", fill = "right", extra = "merge", convert = TRUE)
# Memisahkan kolom Spesifikasi_LB 
Data_Properti <- Data_Properti %>%
  separate(Spesifikasi_LB, into = c("LB_Min", "LB_Max"), sep = "-", fill = "right", extra = "merge", convert = TRUE)

# ------ VISUALISASI HASIL DATA HUNIAN ------
message('Loading Packages')
library(stringr)
library(GGally)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(wordcloud)
library(tm)
# Set palette colors
palette <- brewer.pal(8, "Set3")

# ----Data Top 5 Developer dengan jumlah hunian terbanyak
top5_pengembang <- Data_Properti %>%  group_by(Pengembang) %>%  summarise(Jumlah_Rumah = n()) %>%
  arrange(desc(Jumlah_Rumah)) %>%  head(5)
# Bar Chart Top 5 Developer dengan jumlah hunian terbanyak
ggplot(top5_pengembang, aes(x = reorder(Pengembang, -Jumlah_Rumah), y = Jumlah_Rumah, fill = Pengembang)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Pengembang dengan Pembangunan Hunian Tertinggi", x = "Pengembang", y = "Jumlah Hunian") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(0, 6))

# ------Data Top 5 kab/kota dengan hunian rumah terbanyak
top5_kab_kota <- Data_Properti %>%  group_by(Kota_Kabupaten) %>%  summarise(Jumlah_Rumah = n()) %>%
  arrange(desc(Jumlah_Rumah)) %>%  head(5)
# Bar Chart Top 5 kab/kota dengan jumlah hunian terbanyak
ggplot(top5_kab_kota, aes(x = reorder(Kota_Kabupaten, -Jumlah_Rumah), y = Jumlah_Rumah, fill = Kota_Kabupaten)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Kab/Kota dengan Jumlah Hunian Terbanyak", x = "Kabupaten/Kota", y = "Jumlah Hunian") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(0, 20))

# ----- Data 10 rumah dengan angsuran terendah dan 10 rumah dengan angsuran tertinggi
top_10_angsur_terendah <- Data_Properti %>% arrange(Angsuran) %>% head(10)
# Barplot untuk 10 rumah dengan angsuran terendah
plot_angsuran_terendah <- ggplot(top_10_angsur_terendah, aes(x = Angsuran, y = reorder(Nama_Hunian, Angsuran))) +
  geom_bar(stat = "identity", fill = "#FFB6C1") +
  labs(title = "Top 10 Hunian dengan Angsuran Terendah", x = "Angsuran (Ribu Rupiah)", y = "Nama Hunian") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 10))+
  scale_x_continuous(limits = c(0, 2000))

# ----- Data 10 rumah dengan angsuran terendah dan 10 rumah dengan angsuran tertinggi
top_10_angsur_tertinggi <- Data_Properti %>% arrange(desc(Angsuran)) %>% head(10)
# Barplot untuk 10 rumah dengan angsuran tertinggi
plot_angsuran_tertinggi <- ggplot(top_10_angsur_tertinggi, aes(x = Angsuran, y = reorder(Nama_Hunian, Angsuran))) +
  geom_bar(stat = "identity", fill = "#B0E0E6") +
  labs(title = "Top 10 Hunian dengan Angsuran Tertinggi", x = "Angsuran (Ribu Rupiah)", y = "Nama Hunian") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 10))+
  scale_x_continuous(limits = c(0, 60000))

plot_angsuran_terendah
plot_angsuran_tertinggi

# ---- WORDCLOUD Nama_Hunian
wordcloud(words = Data_Properti$Nama_Hunian, min.freq = 2, scale = c(5, 0.5),
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Pastel1"))

# ---- TOP 10 Harga Hunian Tertinggi dan Terendah
# Filter top 10 tertinggi dan terendah
top_10_tertinggi <- Data_Properti %>% arrange(desc(Harga_Med)) %>%  slice(1:10)
top_10_terendah <- Data_Properti %>%  arrange(Harga_Med) %>%  slice(1:10)
# Plot untuk 10 hunian dengan harga tengah tertinggi
plot_harga_tertinggi <- ggplot(top_10_tertinggi, aes(x = reorder(Nama_Hunian, Harga_Med), y = Harga_Med, fill = Kategori)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Hunian dengan Harga Tengah Tertinggi",
       x = "Nama Hunian",
       y = "Harga (Ribu Rupiah)",
       fill = "Kategori") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Set3")
# Plot untuk 10 hunian dengan harga tengah terendah
plot_harga_terendah <- ggplot(top_10_terendah, aes(x = reorder(Nama_Hunian, -Harga_Med), y = Harga_Med, fill = Kategori)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Hunian dengan Harga Terendah",
       x = "Nama Hunian",
       y = "Harga (Ribu Rupiah)",
       fill = "Kategori") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Set3")

plot_harga_tertinggi
plot_harga_terendah

# ---- Visualisasi Scatter plot 
# Scatter plot untuk hubungan harga dengan jumlah kamar tidur (KT)
plot_kt <- ggplot(Data_Properti, aes(x = KT_Max, y = Harga_Med)) +
  geom_point() +
  labs(title = "ScatterPlot Harga Tengah dengan Jumlah Kamar Tidur",
       x = "Jumlah Kamar Tidur (KT)",
       y = "Harga Tengah (Ribu Rupiah)") +
  theme_minimal(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        axis.title.x = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 8))

# Scatter plot untuk hubungan harga tengah dengan luas tanah (LT)
plot_lt <- ggplot(Data_Properti, aes(x = LT_Max, y = Harga_Med)) +
  geom_point() +
  labs(title = "ScatterPlot Harga dengan Luas Tanah",
       x = "Luas Tanah (m²)",
       y = "Harga  (Ribu Rupiah)") +
  theme_minimal(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        axis.title.x = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 8))

# Scatter plot untuk hubungan harga tengah dengan luas bangunan (LB)
plot_lb <- ggplot(Data_Properti, aes(x = LB_Max, y = Harga_Med)) +
  geom_point() +
  labs(title = "ScatterPlot Harga  dengan Luas Bangunan",
       x = "Luas Bangunan (m²)",
       y = "Harga  (Ribu Rupiah)") +
  theme_minimal(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        axis.title.x = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 8))

# Scatter plot untuk hubungan harga dan angsuran
plot_hubungan <- ggplot(data = Data_Properti, aes(x = Angsuran, y = Harga_Med)) +
  geom_point() +
  labs(title = "ScatterPlot Harga dengan Angsuran",
       x = "Angsuran (Ribu Rupiah)",
       y = "Harga (Ribu Rupiah)") +
  theme_minimal(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        axis.title.x = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 8))

grid.arrange(plot_kt, plot_lt, plot_lb, plot_hubungan, nrow = 2, ncol = 2)

#----- CORR PLOT
# Subset data beberapa variabel
subset_data <- Data_Properti[, c("Harga_Med", "KT_Max", "LT_Max", "LB_Max", "Angsuran")]
ggcorr(subset_data, label = TRUE, label_size = 3, label_round = 2, var_text_size = 2, label_position = "0", hjust = 0.6)






