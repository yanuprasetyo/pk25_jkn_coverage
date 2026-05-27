# Analisis Determinan dan Ketimpangan Kepesertaan JKN
## Berbasis Data Pendataan Keluarga 2025 (PK 2025)

**Penulis:** Yanu Endar Prasetyo, Syahmida S. Arsyad, Arga Nugraha, et al.  
**Institusi:** Pusat Riset Kependudukan, BRIN  
**Tahun:** 2026  
**Sitasi:** Prasetyo, Y.E., et al (2026). Determinan dan Ketimpangan Kepesertaan Jaminan Kesehatan Nasional (JKN): Analisis Berbasis Pendataan Keluarga 2025. Policy Brief. Jakarta: Pusat Riset Kependudukan, BRIN.

---

## Deskripsi Proyek

Repository ini berisi seluruh kode R untuk mereproduksi analisis pada Policy Brief "Siapa yang Belum Terlindungi?". Analisis mencakup:

- Estimasi coverage JKN berbobot menurut sub-kelompok sosiodemografi
- Konstruksi Welfare Index menggunakan Principal Component Analysis (PCA)
- Pengukuran ketimpangan distribusi kepesertaan menggunakan Concentration Index (CI)
- Dekomposisi CI menurut sub-kelompok
- Regresi logistik berbobot dengan complex survey design
- Visualisasi seluruh temuan utama

---

## Struktur Repository

```
jkn_pk25/
├── README.md
├── 01_data_preparation.R       # Pembersihan data & rekoding variabel
├── 02_welfare_index_pca.R      # Konstruksi Welfare Index via PCA
├── 03_descriptive_analysis.R   # Coverage JKN per sub-kelompok
├── 04_concentration_index.R    # Hitung CI nasional & dekomposisi
├── 05_regression_analysis.R    # Regresi logistik berbobot (svyglm)
├── 06_visualization.R          # Semua gambar (Gambar 1–4)
└── output/
    ├── figures/                # Output gambar (.png)
    └── tables/                 # Output tabel (.csv / .xlsx)
```

---

## Data

Data yang digunakan adalah **Pendataan Keluarga 2025 (PK 2025)** yang diselenggarakan oleh BKKBN. Data tidak didistribusikan dalam repository ini karena bersifat restricted. Untuk akses data, hubungi BKKBN atau BRIN.

**Asumsi nama variabel** dalam data mentah mengikuti konvensi PK 2025:

| Nama Variabel | Deskripsi |
|---|---|
| `no_kk` | Nomor identitas kepala keluarga |
| `weight` | Bobot survei (sampling weight) |
| `status_jkn` | Status kepesertaan JKN (1=PBI, 2=NonPBI, 3=Swasta, 4=Tidak punya) |
| `sex_kk` | Jenis kelamin KK (1=Laki-laki, 2=Perempuan) |
| `umur_kk` | Usia KK dalam tahun |
| `pendidikan_kk` | Tingkat pendidikan KK (1–5) |
| `pekerjaan_kk` | Jenis pekerjaan KK (1–11) |
| `klasifikasi` | Klasifikasi wilayah (1=Perkotaan, 2=Perdesaan) |
| `pk13` | Kecukupan penghasilan (1=Cukup, 0=Tidak cukup) |
| `aset_tabungan` | Kepemilikan tabungan/rekening aktif |
| `aset_tv` | Kepemilikan televisi layar datar |
| `aset_kulkas` | Kepemilikan lemari es/kulkas |
| `aset_komputer` | Kepemilikan komputer/laptop |
| `aset_motor` | Kepemilikan sepeda motor |
| `aset_mobil` | Kepemilikan mobil |
| `aset_emas` | Kepemilikan emas/perhiasan ≥10 gram |
| `aset_lahan` | Kepemilikan lahan |

---

## Kebutuhan Sistem

- **R** versi 4.6.0 atau lebih baru
- **Packages:**

```r
install.packages(c(
  "survey",      # v4.4 — complex survey analysis
  "srvyr",       # tidy interface untuk survey
  "tidyverse",   # data wrangling & ggplot2
  "ggplot2",     # visualisasi
  "scales",      # format angka di plot
  "ggpubr",      # kombinasi multiple plots
  "broom",       # tidy model output
  "openxlsx"     # ekspor tabel ke Excel
))
```

---

## Cara Menjalankan

Jalankan script secara berurutan:

```r
source("01_data_preparation.R")
source("02_welfare_index_pca.R")
source("03_descriptive_analysis.R")
source("04_concentration_index.R")
source("05_regression_analysis.R")
source("06_visualization.R")
```

Atau gunakan `Rscript` dari terminal:

```bash
Rscript 01_data_preparation.R
Rscript 02_welfare_index_pca.R
# dst.
```

---

## Catatan Metodologis Utama

1. **Complex survey design**: Seluruh estimasi menggunakan `svydesign()` dengan bobot (`weight`) untuk menjamin representativitas nasional.
2. **Welfare Index**: Dikonstruksi dari PC1 analisis PCA atas 8 variabel aset (menjelaskan 36% varians total).
3. **Concentration Index**: Dihitung dengan formula Kakwani: `CI = 2 × cov(y, r) / mean(y)`, di mana `r` adalah fractional rank berdasarkan welfare index yang telah dibobot.
4. **Regresi logistik**: Menggunakan `svyglm()` dengan `family = quasibinomial(link = "logit")`. PNS/TNI/POLRI dan Pejabat Negara dikecualikan karena *perfect separation* (coverage 100%).
5. **Interpretasi usia muda**: Kelompok <25 tahun adalah *kategori referensi* dalam regresi. Tidak signifikannya kelompok 25–34 tahun (ns) berarti kedua kelompok muda ini berada pada kondisi kepesertaan yang setara dan sama-sama rendah dibanding semua kelompok usia 35+ yang secara signifikan lebih tinggi.

---

## Lisensi

© 2026. Dilisensikan di bawah [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/).  
Dilarang diperbanyak untuk tujuan komersial.

---

## Kontak

Yanu Endar Prasetyo | yanu005@brin.go.id  
Pusat Riset Kependudukan, Badan Riset dan Inovasi Nasional (BRIN)
