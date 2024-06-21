# Scraping Pinhome

<p align="center" width="100%">
    <img width="750" height="400" src="https://github.com/nbltriamanda/Scraping_PinHome/blob/main/PINHOME.png">
</p>


## :blue_book: **Deskripsi**
<p align="justify">
Properti adalah segala sesuatu yang dimiliki oleh seseorang atau badan hukum, baik itu berupa benda bergerak maupun benda tidak bergerak, yang memiliki nilai ekonomi dan dapat digunakan atau diinvestasikan untuk menghasilkan keuntungan. Dalam konteks real estate atau properti fisik, istilah ini mengacu pada tanah dan bangunan serta struktur lain yang ada di atasnya. Project ini terfokus pada Properti Hunian yang dipasarkan di situs https://www.pinhome.id/.
  
</p>

<p align="justify">
Web scraping adalah teknik untuk mengekstraksi informasi dari suatu situs web. Pada Project ini dilakukan web scraping dari situs "Pinhome.com" yang merupakan platform properti yang menyediakan berbagai informasi tentang properti yang dijual. Batasan yang digunakan dalam scraping ini adalah Hunian yang Di "Jual" dengan kondisi Baru dan Eksklusif di Daerah Jabodetabek. Tools yang digunakan dalam Scraping ini adalah Rstudio dengan packages rvest untuk mengambil data seperti jenis rumah, harga, angsuran, pengembang, lokasi, dan spesifikasi rumah, kemudian data disimpan ke MongoDB menggunakan mongolite.
</p>

## :clipboard: **Dokumen**

Berikut contoh dokumen pada Mongo DB :

```
{
"$oid":"66650c2cc0a4dce3d40be001",
"Nama_Hunian":"Bukit Cimanggu City",
"Kategori":"Rumah Baru",
"Harga":"Rp 805,4 Jt - Rp 2,7 M",
"Angsuran":" Rp5,5 Jt/bln",
"Pengembang":" Gapuraprima Group",
"Lokasi":"Tanah Sareal",
"Kota_Kabupaten":"Kota Bogor",
"Spesifikasi":"2-4 KTLT 90-160m²LB 36-133m²",
"Status":"HGB "
}
```

## :woman_with_headscarf: **Pengembang**
**Nabila Tri Amanda (G1501231068)**
