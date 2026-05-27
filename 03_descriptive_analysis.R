# =============================================================================
# SCRIPT 03: ANALISIS DESKRIPTIF — COVERAGE JKN PER SUB-KELOMPOK
# =============================================================================
# Proyek  : Analisis Determinan dan Ketimpangan Kepesertaan JKN, PK 2025
# Penulis : Yanu Endar Prasetyo et al., Pusat Riset Kependudukan BRIN
# Versi   : 2.0 (Revisi)
# Tanggal : 2026
# -----------------------------------------------------------------------------
# Tujuan  : Mengestimasi coverage JKN berbobot beserta 95% CI untuk
#           sub-kelompok: nasional, gender, wilayah, usia, pendidikan,
#           pekerjaan, dan kuintil kesejahteraan. Menghasilkan Tabel 2
#           dan data untuk Gambar 1–3.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)
library(openxlsx)

PATH_DATA_IN <- "data/pk2025_analytic.rds"


# -----------------------------------------------------------------------------
# 1. LOAD DATA & DEFINISIKAN SURVEY DESIGN
# -----------------------------------------------------------------------------

df <- read_rds(PATH_DATA_IN)

# Definisi survey design dengan sampling weight
# Sesuaikan dengan struktur desain sampel PK 2025 yang sebenarnya
# (strata, cluster jika tersedia)
svy_design <- df %>%
  as_survey_design(
    weights = weight   # kolom bobot survei
    # Jika ada strata/cluster, tambahkan:
    # strata = nama_strata,
    # ids    = id_cluster,
    # nest   = TRUE
  )

message("Survey design berhasil didefinisikan.")
message("N observasi: ", nrow(df))


# -----------------------------------------------------------------------------
# 2. FUNGSI HELPER: ESTIMASI COVERAGE BERBOBOT
# -----------------------------------------------------------------------------

hitung_coverage <- function(design, grup_var = NULL) {
  if (is.null(grup_var)) {
    # Estimasi nasional
    design %>%
      summarise(
        coverage_pct = survey_mean(jkn, na.rm = TRUE,
                                   vartype = "ci", level = 0.95) * 100
      ) %>%
      mutate(subkelompok = "Nasional", .before = 1)
  } else {
    # Estimasi per sub-kelompok
    design %>%
      group_by(across(all_of(grup_var))) %>%
      summarise(
        coverage_pct = survey_mean(jkn, na.rm = TRUE,
                                   vartype = "ci", level = 0.95) * 100
      )
  }
}


# -----------------------------------------------------------------------------
# 3. COVERAGE NASIONAL (TABEL 2)
# -----------------------------------------------------------------------------

message("\n--- Coverage Nasional ---")

# Coverage total JKN
cov_nasional <- svy_design %>%
  summarise(
    coverage_pct    = survey_mean(jkn, na.rm = TRUE,
                                  vartype = "ci", level = 0.95) * 100
  )

cov_nasional_pct <- round(cov_nasional$coverage_pct, 1)
message(sprintf("Coverage JKN Nasional: %.1f%% (95%% CI: %.1f–%.1f%%)",
                cov_nasional_pct,
                round(cov_nasional$coverage_pct_low, 1),
                round(cov_nasional$coverage_pct_upp, 1)))

# Coverage per tipe kepesertaan (Tabel 2)
tabel2 <- svy_design %>%
  group_by(tipe_jkn) %>%
  summarise(
    n_sampel    = survey_total(vartype = "ci"),
    persentase  = survey_mean(vartype = "ci") * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

message("\nTabel 2: Coverage JKN per Tipe Kepesertaan")
print(tabel2)

write.xlsx(tabel2, "output/tables/tabel2_coverage_nasional.xlsx",
           overwrite = TRUE)


# -----------------------------------------------------------------------------
# 4. COVERAGE MENURUT GENDER & WILAYAH
# -----------------------------------------------------------------------------

cov_gender <- hitung_coverage(svy_design, "gender_kk")
cov_wilayah <- hitung_coverage(svy_design, "wilayah")

message("\n--- Coverage per Gender ---")
print(cov_gender %>% mutate(across(where(is.numeric), ~ round(., 1))))

message("\n--- Coverage per Wilayah ---")
print(cov_wilayah %>% mutate(across(where(is.numeric), ~ round(., 1))))

# Hitung kesenjangan urban-rural
gap_urban_rural <- cov_wilayah %>%
  filter(!is.na(wilayah)) %>%
  summarise(gap = diff(coverage_pct))
message(sprintf("Kesenjangan urban-rural: %.1f poin persentase",
                abs(gap_urban_rural$gap)))


# -----------------------------------------------------------------------------
# 5. COVERAGE MENURUT KELOMPOK USIA & GENDER (GAMBAR 1)
# -----------------------------------------------------------------------------

cov_usia_gender <- svy_design %>%
  filter(!is.na(usia_kel)) %>%
  group_by(usia_kel, gender_kk) %>%
  summarise(
    coverage_pct     = survey_mean(jkn, na.rm = TRUE,
                                   vartype = "ci", level = 0.95) * 100,
    .groups = "drop"
  )

message("\n--- Coverage per Kelompok Usia & Gender ---")
print(cov_usia_gender %>% mutate(across(where(is.numeric), ~ round(., 1))))

write.xlsx(cov_usia_gender, "output/tables/coverage_usia_gender.xlsx",
           overwrite = TRUE)


# -----------------------------------------------------------------------------
# 6. COVERAGE MENURUT TINGKAT PENDIDIKAN
# -----------------------------------------------------------------------------

cov_pendidikan <- hitung_coverage(svy_design, "pendidikan_kel") %>%
  filter(!is.na(pendidikan_kel))

message("\n--- Coverage per Tingkat Pendidikan ---")
print(cov_pendidikan %>% mutate(across(where(is.numeric), ~ round(., 1))))

# Hitung selisih pendidikan terendah vs tertinggi
gap_pendidikan <- cov_pendidikan %>%
  filter(pendidikan_kel %in% c("Tidak/Tdk tamat SD", "PT/Akademi")) %>%
  summarise(gap = diff(coverage_pct))
message(sprintf("Gap pendidikan (Tidak tamat SD vs PT): %.1f poin",
                abs(gap_pendidikan$gap)))


# -----------------------------------------------------------------------------
# 7. COVERAGE MENURUT JENIS PEKERJAAN (GAMBAR 2)
# -----------------------------------------------------------------------------

cov_pekerjaan <- hitung_coverage(svy_design, "pekerjaan_kel") %>%
  filter(!is.na(pekerjaan_kel)) %>%
  mutate(
    vs_rata_rata = case_when(
      coverage_pct > cov_nasional_pct ~ "Di atas rata-rata",
      coverage_pct < cov_nasional_pct ~ "Di bawah rata-rata",
      TRUE                            ~ "Setara rata-rata"
    ) %>% factor(levels = c("Di atas rata-rata", "Di bawah rata-rata",
                             "Setara rata-rata"))
  )

message("\n--- Coverage per Jenis Pekerjaan ---")
print(cov_pekerjaan %>%
        select(pekerjaan_kel, coverage_pct, vs_rata_rata) %>%
        arrange(coverage_pct) %>%
        mutate(coverage_pct = round(coverage_pct, 1)))

write.xlsx(cov_pekerjaan, "output/tables/coverage_pekerjaan.xlsx",
           overwrite = TRUE)


# -----------------------------------------------------------------------------
# 8. COVERAGE MENURUT KUINTIL KESEJAHTERAAN (GAMBAR 3)
# -----------------------------------------------------------------------------

cov_kuintil <- svy_design %>%
  filter(!is.na(kuintil_welfare)) %>%
  group_by(kuintil_welfare, kuintil_label) %>%
  summarise(
    coverage_pct = survey_mean(jkn, na.rm = TRUE,
                               vartype = "ci", level = 0.95) * 100,
    .groups = "drop"
  )

message("\n--- Coverage per Kuintil Kesejahteraan ---")
print(cov_kuintil %>% mutate(across(where(is.numeric), ~ round(., 1))))

# Catatan metodologis: Q2 (68.9%) < Q1 (69.5%) — gradien tidak sepenuhnya
# monoton di ujung bawah distribusi. Ini mencerminkan efek PBI pada Q1.
message("\nCatatan: Coverage Q2 lebih rendah dari Q1 — gradien tidak")
message("sepenuhnya monoton. Ini kemungkinan mencerminkan efek mekanisme PBI")
message("yang menyasar rumah tangga termiskin di Q1.")

gap_q1_q5 <- cov_kuintil %>%
  filter(kuintil_welfare %in% c(1, 5)) %>%
  summarise(gap = diff(coverage_pct))
message(sprintf("Gap Q1–Q5: %.1f poin persentase", abs(gap_q1_q5$gap)))

write.xlsx(cov_kuintil, "output/tables/coverage_kuintil.xlsx",
           overwrite = TRUE)


# -----------------------------------------------------------------------------
# 9. SIMPAN SEMUA HASIL COVERAGE KE SATU FILE RINGKASAN
# -----------------------------------------------------------------------------

# Kompilasi semua tabel coverage
ringkasan_coverage <- list(
  "Nasional"        = cov_nasional,
  "Tabel2_Tipe"     = tabel2,
  "Gender"          = cov_gender,
  "Wilayah"         = cov_wilayah,
  "Usia_Gender"     = cov_usia_gender,
  "Pendidikan"      = cov_pendidikan,
  "Pekerjaan"       = cov_pekerjaan,
  "Kuintil"         = cov_kuintil
)

write.xlsx(ringkasan_coverage,
           "output/tables/ringkasan_coverage_semua.xlsx",
           overwrite = TRUE)
message("\nSemua tabel coverage disimpan ke output/tables/")


# -----------------------------------------------------------------------------
# 10. SIMPAN OBJEK UNTUK DIGUNAKAN SCRIPT LAIN
# -----------------------------------------------------------------------------

save(
  svy_design, cov_nasional, cov_nasional_pct,
  cov_usia_gender, cov_pekerjaan, cov_kuintil,
  cov_gender, cov_wilayah, cov_pendidikan,
  file = "data/hasil_deskriptif.RData"
)
message("Objek deskriptif disimpan ke data/hasil_deskriptif.RData")
