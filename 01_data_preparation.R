# =============================================================================
# SCRIPT 01: DATA PREPARATION & REKODING VARIABEL
# =============================================================================
# Proyek  : Analisis Determinan dan Ketimpangan Kepesertaan JKN, PK 2025
# Penulis : Yanu Endar Prasetyo et al., Pusat Riset Kependudukan BRIN
# Versi   : 2.0 (Revisi)
# Tanggal : 2026
# -----------------------------------------------------------------------------
# Tujuan  : Membersihkan data mentah PK 2025, melakukan rekoding variabel,
#           dan menghasilkan dataset analitik yang siap digunakan di script
#           selanjutnya.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP: LOAD PACKAGES & KONFIGURASI PATH
# -----------------------------------------------------------------------------

library(tidyverse)

# Path ke data mentah — sesuaikan dengan lokasi file di sistem Anda
PATH_DATA_RAW  <- "data/pk2025_raw.rds"   # atau .csv / .dta
PATH_DATA_OUT  <- "data/pk2025_analytic.rds"

# Buat folder output jika belum ada
dir.create("data",          showWarnings = FALSE)
dir.create("output",        showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)
dir.create("output/tables",  showWarnings = FALSE)


# -----------------------------------------------------------------------------
# 1. LOAD DATA MENTAH
# -----------------------------------------------------------------------------

# Sesuaikan dengan format file yang tersedia:
# df_raw <- read_rds(PATH_DATA_RAW)          # format RDS
# df_raw <- read_csv("data/pk2025_raw.csv")  # format CSV
# df_raw <- haven::read_dta("data/pk2025_raw.dta")  # format Stata

message("Jumlah observasi awal: ", nrow(df_raw))
# Expected: ~989,622 sebelum filtering


# -----------------------------------------------------------------------------
# 2. EKSKLUSI OBSERVASI
# -----------------------------------------------------------------------------
# Mengikuti kriteria inklusi/eksklusi dalam Lampiran Metodologi:
# (a) Keluarkan peserta asuransi swasta (status_jkn == 3)
# (b) Keluarkan KK berusia < 15 tahun
# (c) Keluarkan KK berusia > 100 tahun (nilai ekstrem)

df_clean <- df_raw %>%
  filter(status_jkn != 3) %>%          # (a) ekskl. asuransi swasta
  filter(umur_kk >= 15) %>%            # (b) ekskl. usia < 15
  filter(umur_kk <= 100)               # (c) ekskl. usia > 100

message("Observasi setelah eksklusi asuransi swasta: ",
        nrow(df_raw %>% filter(status_jkn != 3)))
message("Observasi setelah eksklusi usia ekstrem: ", nrow(df_clean))
# Expected: 971,919


# -----------------------------------------------------------------------------
# 3. REKODING VARIABEL DEPENDEN: KEPESERTAAN JKN
# -----------------------------------------------------------------------------
# Variabel biner: 1 = memiliki JKN (PBI atau Non-PBI), 0 = tidak memiliki JKN

df_clean <- df_clean %>%
  mutate(
    # Variabel utama: biner kepesertaan JKN
    jkn = case_when(
      status_jkn %in% c(1, 2) ~ 1L,  # PBI atau Non-PBI = punya JKN
      status_jkn == 4         ~ 0L,  # tidak punya JKN
      TRUE ~ NA_integer_
    ),

    # Variabel tipe kepesertaan (untuk analisis deskriptif Tabel 2)
    tipe_jkn = case_when(
      status_jkn == 1 ~ "BPJS-PBI",
      status_jkn == 2 ~ "BPJS Non-PBI",
      status_jkn == 4 ~ "Tidak Memiliki JKN",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("BPJS-PBI", "BPJS Non-PBI", "Tidak Memiliki JKN"))
  )


# -----------------------------------------------------------------------------
# 4. REKODING VARIABEL INDEPENDEN
# -----------------------------------------------------------------------------

df_clean <- df_clean %>%
  mutate(

    # --- 4.1 Jenis Kelamin KK ---
    gender_kk = factor(
      sex_kk,
      levels = c(1, 2),
      labels = c("Laki-laki", "Perempuan")
    ),

    # --- 4.2 Kelompok Usia ---
    # Referensi regresi: < 25 tahun (kelompok dengan coverage terendah)
    usia_kel = cut(
      umur_kk,
      breaks = c(14, 24, 34, 44, 54, 64, Inf),
      labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+"),
      right  = TRUE
    ) %>%
      factor(levels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+")),

    # --- 4.3 Tingkat Pendidikan ---
    # 5 kategori; referensi regresi: Tidak/Tidak tamat SD
    pendidikan_kel = case_when(
      pendidikan_kk == 1 ~ "Tidak/Tdk tamat SD",
      pendidikan_kk == 2 ~ "SD",
      pendidikan_kk == 3 ~ "SMP",
      pendidikan_kk == 4 ~ "SMA",
      pendidikan_kk == 5 ~ "PT/Akademi",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c(
        "Tidak/Tdk tamat SD", "SD", "SMP", "SMA", "PT/Akademi"
      )),

    # --- 4.4 Jenis Pekerjaan ---
    # 11 kategori asli PK 2025; referensi regresi: Tidak/Belum Bekerja
    # Catatan: PNS/TNI/POLRI & Pejabat Negara dikecualikan dari model regresi
    #          karena perfect separation (coverage 100%)
    pekerjaan_kel = case_when(
      pekerjaan_kk == 1  ~ "Tidak/Belum Bekerja",
      pekerjaan_kk == 2  ~ "Petani",
      pekerjaan_kk == 3  ~ "Nelayan",
      pekerjaan_kk == 4  ~ "Pedagang",
      pekerjaan_kk == 5  ~ "Pejabat Negara",
      pekerjaan_kk == 6  ~ "PNS/TNI/POLRI",
      pekerjaan_kk == 7  ~ "Swasta Pertanian",
      pekerjaan_kk == 8  ~ "Swasta Industri",
      pekerjaan_kk == 9  ~ "Swasta Jasa",
      pekerjaan_kk == 10 ~ "Pensiunan",
      pekerjaan_kk == 11 ~ "Pekerja Lepas",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c(
        "Tidak/Belum Bekerja", "Petani", "Nelayan", "Pedagang",
        "Pejabat Negara", "PNS/TNI/POLRI",
        "Swasta Pertanian", "Swasta Industri", "Swasta Jasa",
        "Pensiunan", "Pekerja Lepas"
      )),

    # --- 4.5 Klasifikasi Wilayah ---
    # Referensi regresi: Perkotaan
    wilayah = factor(
      klasifikasi,
      levels = c(1, 2),
      labels = c("Perkotaan", "Perdesaan")
    ),

    # --- 4.6 Kecukupan Penghasilan (pk13) ---
    # Variabel pk13 dari instrumen PK 2025
    cukup_penghasilan = as.integer(pk13)  # 1 = cukup, 0 = tidak cukup
  )


# -----------------------------------------------------------------------------
# 5. FLAG UNTUK SUBSAMPEL REGRESI
# -----------------------------------------------------------------------------
# Untuk model regresi, PNS/TNI/POLRI dan Pejabat Negara dikecualikan
# karena perfect separation (coverage 100%)

df_clean <- df_clean %>%
  mutate(
    flag_regresi = !(pekerjaan_kel %in% c("PNS/TNI/POLRI", "Pejabat Negara"))
  )

message("N subsampel untuk regresi: ",
        sum(df_clean$flag_regresi, na.rm = TRUE))
# Expected: ~930,400


# -----------------------------------------------------------------------------
# 6. VALIDASI AWAL
# -----------------------------------------------------------------------------

# Cek distribusi variabel dependen
message("\n--- Distribusi Kepesertaan JKN ---")
df_clean %>%
  count(tipe_jkn) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print()

# Cek missing values pada variabel kunci
message("\n--- Missing Values Variabel Kunci ---")
df_clean %>%
  select(jkn, gender_kk, usia_kel, pendidikan_kel,
         pekerjaan_kel, wilayah, pk13) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variabel", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()

# Distribusi kelompok usia
message("\n--- Distribusi Kelompok Usia ---")
df_clean %>%
  count(usia_kel) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# Distribusi jenis pekerjaan
message("\n--- Distribusi Jenis Pekerjaan ---")
df_clean %>%
  count(pekerjaan_kel) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n)) %>%
  print()


# -----------------------------------------------------------------------------
# 7. SIMPAN DATASET ANALITIK
# -----------------------------------------------------------------------------

write_rds(df_clean, PATH_DATA_OUT)
message("\nDataset analitik disimpan ke: ", PATH_DATA_OUT)
message("Jumlah observasi final: ", nrow(df_clean))
message("Jumlah variabel: ", ncol(df_clean))
