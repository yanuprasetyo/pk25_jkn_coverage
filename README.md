# Analisis Kepesertaan JKN — Kepala Keluarga Indonesia (PK 2025)

> **"Determinan dan Ketimpangan Kepesertaan JKN: Bukti dari Pendataan Keluarga 2025"**

---

## Gambaran Umum

Repositori ini berisi kode R untuk menganalisis kepesertaan Jaminan Kesehatan Nasional (JKN) pada kepala keluarga Indonesia menggunakan data **Pendataan Keluarga 2025 (PK 2025)** dari BKKBN.

### Temuan Utama

| Indikator | Hasil |
|-----------|-------|
| Coverage JKN nasional | **72.5%** |
| BPJS PBI | 48.7% |
| BPJS Non-PBI | 23.8% |
| Tidak memiliki JKN | **27.5%** |
| Concentration Index | **0.0261 (pro-kaya)** |
| Kelompok paling rentan | Petani/Nelayan (65.3%), KK usia muda <25 th (65.7%) |

---

## Struktur Repositori

```
jkn_pk25/
│
├── analysis_JKN_PK25.R       ← Script analisis utama (R)
├── README.md                  ← Dokumen ini
├── .gitignore                 ← Exclude data & file besar
│
└── output/                    ← Dibuat otomatis saat run
    ├── hasil_analisis_JKN_PK25.xlsx
    ├── figure_final_JKN_PK25.png
    ├── plot_A_kuintil.png
    ├── plot_B_usia_gender.png
    ├── plot_C_pekerjaan.png
    ├── plot_D_forestplot.png
    └── session_info.txt
```

> **Catatan:** File data (`.sav`) tidak di-upload ke GitHub karena ukuran besar dan bersifat rahasia. Lihat bagian *Data* di bawah.

---

## Data

| Item | Detail |
|------|--------|
| Nama | Blok PK Pemutakhiran PK-25 Probability — PD BRIN |
| Sumber | BKKBN — Badan Riset dan Inovasi Nasional (BRIN) |
| Tahun | 2025 |
| Unit analisis | Kepala Keluarga |
| N observasi | 989.610 KK |
| Variabel | 136 |
| Desain sampel | Stratified two-stage cluster sampling |
| Bobot | `Wfinal_kel` |

---

## Metode

### 1. Statistik Deskriptif Berbobot
Coverage JKN diestimasi menggunakan `svymean()` dari package `survey` dengan mempertimbangkan desain sampel kompleks (strata, cluster, bobot).

### 2. Welfare Index (PCA)
Indeks kesejahteraan proxy dikonstruksi menggunakan **Principal Component Analysis (PCA)** dari 8 variabel aset keluarga (tabungan, TV, kulkas, laptop, motor, mobil, emas, lahan). PC1 digunakan sebagai skor kesejahteraan, kemudian dibagi menjadi 5 kuintil.

### 3. Concentration Index (CI)
**CI = 2 × cov(JKN, rank_kesejahteraan) / mean(JKN)**

- CI > 0 → distribusi JKN lebih terkonsentrasi pada kelompok kaya (*pro-kaya*)
- CI < 0 → distribusi JKN lebih terkonsentrasi pada kelompok miskin (*pro-miskin*)
- CI = 0 → distribusi merata

### 4. Regresi Logistik Berbobot
Model `svyglm` dengan `family = quasibinomial(link = "logit")` menggunakan desain sampel kompleks. PNS/TNI/Pejabat dikeluarkan dari model karena *perfect separation* (coverage 100%).

**Variabel independen:**
- Gender KK, Kelompok usia, Pendidikan
- Jenis pekerjaan, Wilayah (urban/rural)
- Welfare Index, Kecukupan penghasilan

---

## Cara Menjalankan

### Prasyarat
- R ≥ 4.3.0
- File data `Blok_PK_Pemutakhiran_PK-25_Probability_PD_BRIN.sav` tersedia di working directory

### Langkah

```r
# 1. Clone repositori
# git clone https://github.com/[username]/jkn_pk25.git

# 2. Set working directory ke folder data
setwd("path/ke/folder/data/")

# 3. Jalankan script
source("analysis_JKN_PK25.R")
```

Semua output akan tersimpan otomatis di folder `output/`.

### Packages yang Dibutuhkan
Script akan menginstall otomatis jika belum tersedia:

```r
c("haven", "tidyverse", "survey", "srvyr",
  "psych", "broom", "patchwork", "writexl")
```

---

## Output yang Dihasilkan

### Tabel (Excel)
| Sheet | Isi |
|-------|-----|
| `00_Coverage_Nasional` | Coverage JKN per tipe (PBI, Non-PBI, Tidak) |
| `01_Coverage_Gender_Wil` | Coverage per gender × wilayah |
| `02_Coverage_Usia` | Coverage per kelompok usia |
| `03_Coverage_Pendidikan` | Coverage per tingkat pendidikan |
| `04_Coverage_Pekerjaan` | Coverage per jenis pekerjaan |
| `05_Coverage_Kuintil` | Coverage per kuintil kesejahteraan |
| `06_Concentration_Index` | CI nasional dan per sub-kelompok |
| `07_Regresi_OddsRatio` | Odds Ratio + 95% CI dari regresi logistik |

### Visualisasi
- **Figure Final** — 4 panel: kuintil, usia-gender, pekerjaan, forest plot
- **Plot A** — Coverage per kuintil kesejahteraan
- **Plot B** — Coverage per usia & gender KK
- **Plot C** — Coverage per jenis pekerjaan
- **Plot D** — Forest plot Odds Ratio

---

## Temuan Kunci

1. **Coverage 72.5%** — 27.5% KK (±17 juta keluarga) belum memiliki JKN
2. **CI = 0.0261 (pro-kaya)** — JKN belum sepenuhnya merata; kelompok kaya lebih terlindungi
3. **Kelompok paling rentan:**
   - Petani/Nelayan: OR = 0.795 (20% lebih rendah vs tidak bekerja)
   - Pedagang: OR = 0.756 (24% lebih rendah — paling rentan)
   - KK muda <25 tahun: coverage hanya 65.7%
   - Perdesaan: 19% lebih rendah odds JKN vs perkotaan
4. **Pendidikan** sebagai prediktor kuat: PT/Akademi 2.1× lebih tinggi odds vs tidak sekolah
5. **Petani/Nelayan** & **SD**: satu-satunya kelompok dengan CI negatif (sedikit pro-miskin dalam kelompoknya)

---

## Sitasi

Jika menggunakan kode ini, mohon sitasi:

```
[Nama Penulis]. (2025). Analisis Kepesertaan JKN Berbasis Data PK 2025.
GitHub: https://github.com/[username]/jkn_pk25
Data: Pendataan Keluarga 2025, BKKBN-BRIN.
```

---

## Lisensi

Kode: MIT License
Data: Hak cipta BKKBN — tidak didistribusikan ulang tanpa izin

---

*Analisis ini merupakan bagian dari paper akademik yang sedang dikembangkan.*
