# ---- Packages ----
message('Loading Packages')
library(rvest)
library(tidyverse)
library(mongolite)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)

# ---- Scraping PinHome ----
message('Scraping Data')
Data_Properti <- data.frame(
  Nama_Hunian = character(),
  Kategori = character(),
  Harga = character(),
  Angsuran = character(),
  Pengembang = character(),
  Lokasi = character(),
  Kota_Kabupaten = character(),
  Spesifikasi = character(),
  Status = character(),
  stringsAsFactors = FALSE
)
for (i in 1:3) { 
  url <- paste0("https://www.pinhome.id/dijual/rumah-baru?isExclusive=true&page=", i)
  webpage <- read_html(url)
  elemen_properti <- webpage %>%
    html_nodes(".pin-card__ellipsis___xqz18") %>%
    html_text(trim = TRUE)
  nama_properti <- webpage %>%
    html_nodes(".pin-card__link___h5vvm") %>%
    html_text(trim = TRUE)
  elemen_properti <- elemen_properti[elemen_properti != ""]
  for (j in seq(1, length(elemen_properti), by = 6)) {
    if (j + 5 <= length(elemen_properti)) {
      Data_Properti <- rbind(Data_Properti, data.frame(
        Nama_Hunian = nama_properti[(j + 5) / 6],
        Kategori = elemen_properti[j],
        Harga = elemen_properti[j+1],
        Angsuran = elemen_properti[j+2],
        Pengembang = elemen_properti[j+3],
        Lokasi = str_split(elemen_properti[j+4], ", ", simplify = TRUE)[1] %>% str_replace_all("\\s*\\([^\\)]+\\)", "") %>% str_trim(),
        Kota_Kabupaten = str_split(elemen_properti[j+4], ", ", simplify = TRUE)[2],
        Spesifikasi = str_replace(elemen_properti[j+5], "SHM|HGB", "") %>% str_trim(),
        Status = paste(ifelse(!is.na(str_extract(elemen_properti[j+5], "SHM|HGB")), str_extract(elemen_properti[j+5], "SHM|HGB"), ""), ifelse(!is.na(str_extract(elemen_properti[j+5], "\\d+ Unit")), str_extract(elemen_properti[j+5], "\\d+ Unit"), ""), sep = " ")
      ))
      
      Data_Properti$Kategori <- gsub("BaruSisa", "Baru Sisa", Data_Properti$Kategori)
      Data_Properti$Pengembang <- gsub("by", "", Data_Properti$Pengembang)
      Data_Properti$Angsuran <- gsub("Angsuran mulai dari", "", Data_Properti$Angsuran)
    }
  }
}

srape_data <- sample(1:103,2,replace=F)
data_scrape <- Data_Properti[srape_data,]

# ---- MongoDB ----
message('Input Data to MongoDB Atlas')
library(mongolite)
# nama koleksi
collection <- "Hunian"
# nama database
db <- "scraping"
# koneksi ke mongoDB
url <- "mongodb+srv://amandatriamanda:Juni222001@cluster0.tdwaaer.mongodb.net/"
Hunian <- mongo(collection=collection, db=db, url=url)
Hunian$insert(data_scrape)
rm(Hunian)
