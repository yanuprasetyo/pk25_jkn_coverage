# =============================================================================
# SCRIPT 04: CONCENTRATION INDEX — PENGUKURAN KETIMPANGAN KEPESERTAAN JKN
# =============================================================================
# Proyek  : Analisis Determinan dan Ketimpangan Kepesertaan JKN, PK 2025
# Penulis : Yanu Endar Prasetyo et al., Pusat Riset Kependudukan BRIN
# Versi   : 2.0 (Revisi)
# Tanggal : 2026
# -----------------------------------------------------------------------------
# Tujuan  : Menghitung Concentration Index (CI) menggunakan formula standar
#           Kakwani et al. (1997): CI = 2 × cov(y, r) / mean(y)
#           di mana y = kepesertaan JKN, r = fractional rank berdasarkan
#           welfare index yang telah dibobot.
#           Melakukan dekomposisi CI menurut sub-kelompok:
#           kelompok usia, gender, wilayah, dan jenis pekerjaan.
# -----------------------------------------------------------------------------
# Referensi:
#   Kakwani, N., Wagstaff, A., & Van Doorslaer, E. (1997).
#   Socioeconomic inequalities in health. Journal of Econometrics, 77(1).
#   O'Donnell et al. (2008). Analyzing Health Equity. World Bank.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(openxlsx)

PATH_DATA_IN <- "data/pk2025_analytic.rds"


# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

df <- read_rds(PATH_DATA_IN)


# -----------------------------------------------------------------------------
# 2. FUNGSI INTI: HITUNG CONCENTRATION INDEX (KAKWANI)
# -----------------------------------------------------------------------------
# Formula: CI = 2 * cov(y, r) / mean(y)
# - y = variabel kesehatan (kepesertaan JKN, 0/1)
# - r = fractional rank berdasarkan welfare index (berbobot)
# Interpretasi:
#   CI > 0 : distribusi pro-kaya (kelompok kaya lebih terlindungi)
#   CI < 0 : distribusi pro-miskin (kelompok miskin lebih terlindungi)
#   CI = 0 : distribusi merata sempurna

hitung_ci <- function(data, var_y = "jkn",
                       var_welfare = "welfare_index",
                       var_weight  = "weight") {

  # Filter observasi lengkap
  data_clean <- data %>%
    filter(!is.na(.data[[var_y]]),
           !is.na(.data[[var_welfare]]),
           !is.na(.data[[var_weight]]))

  y       <- data_clean[[var_y]]
  welfare <- data_clean[[var_welfare]]
  w       <- data_clean[[var_weight]]

  # Hitung fractional rank berbobot
  # r_i = (1/N_total) * sum_{j<i}(w_j) + (w_i / 2*N_total)
  # dengan N_total = sum(w)
  ord <- order(welfare)
  w_ord <- w[ord]
  y_ord <- y[ord]

  N_total  <- sum(w_ord)
  cumw     <- cumsum(w_ord)
  lagcumw  <- c(0, cumw[-length(cumw)])
  r        <- (lagcumw + w_ord / 2) / N_total

  # Kembalikan ke urutan asli
  r_original      <- numeric(length(r))
  r_original[ord] <- r

  # Hitung weighted mean of y
  mean_y_w <- weighted.mean(y, w)

  # Hitung weighted covariance antara y dan r
  cov_yr_w <- weighted.mean(
    (y - mean_y_w) * (r_original - 0.5),
    w
  )

  # CI = 2 * cov(y, r) / mean(y)
  ci <- 2 * cov_yr_w / mean_y_w

  # Tambahan: standar error CI menggunakan metode bootstrap sederhana
  # (opsional, dapat diaktifkan jika diperlukan)

  return(list(
    ci        = round(ci, 4),
    mean_y    = round(mean_y_w, 4),
    n         = nrow(data_clean)
  ))
}


# -----------------------------------------------------------------------------
# 3. CI NASIONAL
# -----------------------------------------------------------------------------

ci_nasional <- hitung_ci(df)

message("=== CONCENTRATION INDEX NASIONAL ===")
message(sprintf("CI Nasional : %.4f", ci_nasional$ci))
message(sprintf("Mean JKN    : %.4f (%.1f%%)",
                ci_nasional$mean_y, ci_nasional$mean_y * 100))
message(sprintf("N           : %s", format(ci_nasional$n, big.mark = ".")))
message("")
message("Interpretasi: CI > 0 mengindikasikan distribusi JKN pro-kaya.")
message("Kelompok lebih sejahtera cenderung lebih banyak memiliki JKN.")
# Expected CI: ~0.0260


# -----------------------------------------------------------------------------
# 4. DEKOMPOSISI CI MENURUT KELOMPOK USIA
# -----------------------------------------------------------------------------

message("\n=== CI MENURUT KELOMPOK USIA ===")

# Catatan metodologis tentang kelompok usia:
# - <25 tahun memiliki COVERAGE terendah (65.6%)
# - Kelompok 25-34 (CI=0.0361) dan 35-44 (CI=0.0366) memiliki CI TERTINGGI
# - <25 tahun bukan yang tertinggi CI-nya
# - Ini berarti ketimpangan DISTRIBUSI terbesar ada di usia produktif,
#   sedangkan kelompok <25 menderita dari sisi AKSES (coverage rendah)

ci_usia <- df %>%
  filter(!is.na(usia_kel)) %>%
  group_by(usia_kel) %>%
  group_map(~ hitung_ci(.x), .keep = TRUE) %>%
  map2_dfr(
    df %>% filter(!is.na(usia_kel)) %>%
      group_by(usia_kel) %>% group_keys(),
    ~ bind_cols(.y, as_tibble(.x))
  )

print(ci_usia)
# Expected:
# <25   : ~0.0323
# 25-34 : ~0.0361
# 35-44 : ~0.0366 (tertinggi)
# 45-54 : ~0.0xxx
# 55-64 : ~0.0xxx
# 65+   : ~0.0163 (terendah)

message("\nCatatan: CI tertinggi ada di usia produktif (35-44 dan 25-34),")
message("BUKAN di kelompok <25 tahun. Kelompok <25 tahun paling rentan")
message("dari sisi LEVEL coverage (65.6%), bukan dari sisi ketimpangan.")


# -----------------------------------------------------------------------------
# 5. DEKOMPOSISI CI MENURUT GENDER
# -----------------------------------------------------------------------------

message("\n=== CI MENURUT GENDER ===")

ci_gender <- df %>%
  filter(!is.na(gender_kk)) %>%
  group_by(gender_kk) %>%
  group_map(~ hitung_ci(.x), .keep = TRUE) %>%
  map2_dfr(
    df %>% filter(!is.na(gender_kk)) %>%
      group_by(gender_kk) %>% group_keys(),
    ~ bind_cols(.y, as_tibble(.x))
  )

print(ci_gender)


# -----------------------------------------------------------------------------
# 6. DEKOMPOSISI CI MENURUT WILAYAH
# -----------------------------------------------------------------------------

message("\n=== CI MENURUT WILAYAH ===")
# Expected: CI rural (0.0148) < CI urban (0.0273)
# Distribusi di perdesaan lebih merata meski levelnya lebih rendah

ci_wilayah <- df %>%
  filter(!is.na(wilayah)) %>%
  group_by(wilayah) %>%
  group_map(~ hitung_ci(.x), .keep = TRUE) %>%
  map2_dfr(
    df %>% filter(!is.na(wilayah)) %>%
      group_by(wilayah) %>% group_keys(),
    ~ bind_cols(.y, as_tibble(.x))
  )

print(ci_wilayah)
message("\nCatatan: CI perdesaan lebih rendah dari perkotaan,")
message("menunjukkan distribusi JKN lebih merata di desa meski levelnya")
message("lebih rendah (efek PBI menekan ketimpangan di perdesaan).")


# -----------------------------------------------------------------------------
# 7. DEKOMPOSISI CI MENURUT JENIS PEKERJAAN
# -----------------------------------------------------------------------------

message("\n=== CI MENURUT JENIS PEKERJAAN ===")
# Temuan penting: petani CI = -0.0019 (sedikit pro-miskin dalam kelompok)
# Ini BERBEDA dari coverage keseluruhan petani yang terendah (64.6%)

ci_pekerjaan <- df %>%
  filter(!is.na(pekerjaan_kel)) %>%
  group_by(pekerjaan_kel) %>%
  group_map(~ hitung_ci(.x), .keep = TRUE) %>%
  map2_dfr(
    df %>% filter(!is.na(pekerjaan_kel)) %>%
      group_by(pekerjaan_kel) %>% group_keys(),
    ~ bind_cols(.y, as_tibble(.x))
  ) %>%
  arrange(ci)

print(ci_pekerjaan)

message("\nCatatan penting untuk CI petani (~-0.0019):")
message("CI negatif BUKAN berarti petani sedikit terlindungi.")
message("CI negatif berarti: di DALAM kelompok petani, petani MISKIN")
message("secara proporsional lebih terlindungi dari petani KAYA.")
message("Ini kemungkinan efek mekanisme PBI yang menyasar petani miskin.")
message("Meski demikian, coverage TOTAL petani tetap yang terendah (64.6%).")

# Nelayan CI = ~0.0097 (pro-kaya dalam kelompok)
message("\nCatatan nelayan: CI positif meski coverage di atas rata-rata (76%).")
message("Artinya nelayan lebih mampu lebih terlindungi vs nelayan kurang mampu.")


# -----------------------------------------------------------------------------
# 8. KOMPILASI & SIMPAN SEMUA HASIL CI
# -----------------------------------------------------------------------------

ringkasan_ci <- list(
  "CI_Nasional"   = tibble(subkelompok = "Nasional",
                            ci = ci_nasional$ci,
                            mean_y = ci_nasional$mean_y,
                            n = ci_nasional$n),
  "CI_Usia"       = ci_usia,
  "CI_Gender"     = ci_gender,
  "CI_Wilayah"    = ci_wilayah,
  "CI_Pekerjaan"  = ci_pekerjaan
)

write.xlsx(ringkasan_ci, "output/tables/hasil_concentration_index.xlsx",
           overwrite = TRUE)
message("\nSemua hasil CI disimpan ke output/tables/hasil_concentration_index.xlsx")


# -----------------------------------------------------------------------------
# 9. SIMPAN OBJEK UNTUK DIGUNAKAN SCRIPT LAIN
# -----------------------------------------------------------------------------

save(
  ci_nasional, ci_usia, ci_gender, ci_wilayah, ci_pekerjaan,
  file = "data/hasil_ci.RData"
)
message("Objek CI disimpan ke data/hasil_ci.RData")
