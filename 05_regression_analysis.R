# =============================================================================
# SCRIPT 05: REGRESI LOGISTIK BERBOBOT — DETERMINAN KEPESERTAAN JKN
# =============================================================================
# Proyek  : Analisis Determinan dan Ketimpangan Kepesertaan JKN, PK 2025
# Penulis : Yanu Endar Prasetyo et al., Pusat Riset Kependudukan BRIN
# Versi   : 2.0 (Revisi)
# Tanggal : 2026
# -----------------------------------------------------------------------------
# Tujuan  : Mengidentifikasi determinan kepesertaan JKN menggunakan regresi
#           logistik berbobot (svyglm, quasibinomial logit) yang memper-
#           timbangkan complex survey design. Menyajikan Odds Ratio (OR)
#           dan 95% CI untuk seluruh prediktor (Tabel 1 & Gambar 4).
# -----------------------------------------------------------------------------
# Catatan metodologis penting:
#   - PNS/TNI/POLRI dan Pejabat Negara DIKECUALIKAN (perfect separation,
#     coverage 100%)
#   - Kelompok referensi usia: <25 tahun (coverage terendah)
#   - Tidak signifikannya usia 25-34 (ns) berarti kelompok tersebut SETARA
#     dengan <25 tahun, bukan berarti usia muda tidak penting. Justru ini
#     memperkuat argumen bahwa KK muda (<35 tahun) adalah segmen paling rentan.
#   - Semua kelompok usia 35+ secara signifikan lebih tinggi dari referensi.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(broom)
library(openxlsx)

PATH_DATA_IN <- "data/pk2025_analytic.rds"


# -----------------------------------------------------------------------------
# 1. LOAD DATA & SIAPKAN SUBSAMPEL REGRESI
# -----------------------------------------------------------------------------

df <- read_rds(PATH_DATA_IN)

# Subsampel: ekskl. PNS/TNI/POLRI dan Pejabat Negara
df_reg <- df %>%
  filter(flag_regresi == TRUE,
         !is.na(jkn),
         !is.na(gender_kk),
         !is.na(usia_kel),
         !is.na(pendidikan_kel),
         !is.na(pekerjaan_kel),
         !is.na(wilayah),
         !is.na(welfare_index),
         !is.na(cukup_penghasilan))

message("N subsampel regresi: ", nrow(df_reg))
# Expected: ~930,400


# -----------------------------------------------------------------------------
# 2. SET KATEGORI REFERENSI
# -----------------------------------------------------------------------------

# Referensi yang dipilih berdasarkan pertimbangan teoritis dan empiris:
# - Usia: <25 tahun (coverage terendah = referensi alami)
# - Pendidikan: Tidak/Tdk tamat SD (terendah)
# - Pekerjaan: Tidak/Belum Bekerja
# - Wilayah: Perkotaan

df_reg <- df_reg %>%
  mutate(
    usia_kel      = relevel(usia_kel,       ref = "<25"),
    pendidikan_kel = relevel(pendidikan_kel, ref = "Tidak/Tdk tamat SD"),
    pekerjaan_kel  = relevel(pekerjaan_kel,  ref = "Tidak/Belum Bekerja"),
    wilayah        = relevel(wilayah,        ref = "Perkotaan")
  )


# -----------------------------------------------------------------------------
# 3. DEFINISI SURVEY DESIGN UNTUK SUBSAMPEL REGRESI
# -----------------------------------------------------------------------------

svy_reg <- svydesign(
  ids     = ~1,          # tidak ada cluster (atau sesuaikan)
  weights = ~weight,
  data    = df_reg
  # Tambahkan strata jika tersedia: strata = ~nama_strata, nest = TRUE
)

message("Survey design untuk regresi berhasil didefinisikan.")


# -----------------------------------------------------------------------------
# 4. MODEL REGRESI LOGISTIK BERBOBOT
# -----------------------------------------------------------------------------

# svyglm dengan family quasibinomial dan link logit
# quasibinomial digunakan untuk menangani overdispersion pada data survei

model_jkn <- svyglm(
  formula = jkn ~ gender_kk +
    usia_kel +
    pendidikan_kel +
    pekerjaan_kel +
    wilayah +
    welfare_index +
    cukup_penghasilan,
  design  = svy_reg,
  family  = quasibinomial(link = "logit")
)

message("\nModel regresi berhasil diestimasi.")
message("Ringkasan model:")
print(summary(model_jkn))


# -----------------------------------------------------------------------------
# 5. EKSTRAK ODDS RATIO & 95% CI
# -----------------------------------------------------------------------------

# Konversi koefisien log-odds ke Odds Ratio
hasil_or <- tidy(model_jkn, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # Label bersih untuk tiap variabel
    variabel = case_when(
      term == "gender_kkPerempuan"                   ~ "KK Perempuan (ref: Laki-laki)",
      term == "usia_kel25-34"                        ~ "Usia 25-34 (ref: <25 tahun)",
      term == "usia_kel35-44"                        ~ "Usia 35-44",
      term == "usia_kel45-54"                        ~ "Usia 45-54",
      term == "usia_kel55-64"                        ~ "Usia 55-64",
      term == "usia_kel65+"                          ~ "Usia 65+",
      term == "pendidikan_kelSD"                     ~ "Pendidikan SD (ref: Tidak/Tdk tamat SD)",
      term == "pendidikan_kelSMP"                    ~ "Pendidikan SMP",
      term == "pendidikan_kelSMA"                    ~ "Pendidikan SMA",
      term == "pendidikan_kelPT/Akademi"             ~ "Pendidikan PT/Akademi",
      term == "pekerjaan_kelPetani"                  ~ "Petani (ref: Tidak/Belum Bekerja)",
      term == "pekerjaan_kelNelayan"                 ~ "Nelayan",
      term == "pekerjaan_kelPedagang"                ~ "Pedagang",
      term == "pekerjaan_kelSwasta Pertanian"        ~ "Swasta Pertanian",
      term == "pekerjaan_kelSwasta Industri"         ~ "Swasta Industri",
      term == "pekerjaan_kelSwasta Jasa"             ~ "Swasta Jasa",
      term == "pekerjaan_kelPensiunan"               ~ "Pensiunan",
      term == "pekerjaan_kelPekerja Lepas"           ~ "Pekerja Lepas",
      term == "wilayahPerdesaan"                     ~ "Perdesaan (ref: Perkotaan)",
      term == "welfare_index"                        ~ "Welfare Index (PCA Aset)",
      term == "cukup_penghasilan"                    ~ "Kecukupan Penghasilan (pk13)",
      TRUE ~ term
    ),

    # Flag signifikansi
    signifikansi = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),

    # Flag arah efek
    arah = case_when(
      signifikansi == "ns"         ~ "Tidak signifikan",
      estimate > 1                 ~ "Positif",
      estimate < 1                 ~ "Negatif",
      TRUE                         ~ "Netral"
    ) %>% factor(levels = c("Positif", "Negatif", "Tidak signifikan"))
  ) %>%
  select(variabel, OR = estimate,
         ci_low = conf.low, ci_high = conf.high,
         p_value = p.value, signifikansi, arah)

message("\n=== TABEL 1: ODDS RATIO KEPESERTAAN JKN ===")
print(hasil_or %>%
        mutate(across(c(OR, ci_low, ci_high), ~ round(., 3))) %>%
        select(variabel, OR, ci_low, ci_high, signifikansi))


# -----------------------------------------------------------------------------
# 6. VALIDASI TEMUAN KUNCI
# -----------------------------------------------------------------------------

message("\n=== VALIDASI TEMUAN UTAMA ===")

# Temuan 1: KK perempuan (harus ~1.07, ***)
or_perempuan <- hasil_or %>% filter(grepl("Perempuan", variabel))
message(sprintf("KK Perempuan OR: %.2f (%.2f–%.2f) %s",
                or_perempuan$OR, or_perempuan$ci_low,
                or_perempuan$ci_high, or_perempuan$signifikansi))

# Temuan 2: Usia 25-34 harus ns (tidak berbeda dari <25)
or_25_34 <- hasil_or %>% filter(grepl("25-34", variabel))
message(sprintf("Usia 25-34 OR: %.2f (%s) — Jika ns, berarti setara dengan",
                or_25_34$OR, or_25_34$signifikansi))
message("         kelompok referensi <25 tahun. Kedua kelompok muda sama-sama rentan.")

# Temuan 3: Usia 65+ (harus ~1.79, ***)
or_65plus <- hasil_or %>% filter(grepl("65\\+", variabel))
message(sprintf("Usia 65+ OR: %.2f (%.2f–%.2f) %s",
                or_65plus$OR, or_65plus$ci_low,
                or_65plus$ci_high, or_65plus$signifikansi))

# Temuan 4: Petani (harus ~0.773, negatif ***)
or_petani <- hasil_or %>% filter(grepl("^Petani", variabel))
message(sprintf("Petani OR: %.3f (%.3f–%.3f) %s — %.1f%% lebih rendah",
                or_petani$OR, or_petani$ci_low, or_petani$ci_high,
                or_petani$signifikansi, (1 - or_petani$OR) * 100))

# Temuan 5: Nelayan (harus ~1.44, berlawanan arah petani)
or_nelayan <- hasil_or %>% filter(grepl("^Nelayan", variabel))
message(sprintf("Nelayan OR: %.2f (%.2f–%.2f) %s — %.0f%% lebih tinggi",
                or_nelayan$OR, or_nelayan$ci_low, or_nelayan$ci_high,
                or_nelayan$signifikansi, (or_nelayan$OR - 1) * 100))

# Temuan 6: Perdesaan (harus ~0.823, negatif ***)
or_desa <- hasil_or %>% filter(grepl("Perdesaan", variabel))
message(sprintf("Perdesaan OR: %.3f (%.3f–%.3f) %s — %.1f%% lebih rendah",
                or_desa$OR, or_desa$ci_low, or_desa$ci_high,
                or_desa$signifikansi, (1 - or_desa$OR) * 100))

# Temuan 7: PT/Akademi (harus ~2.12 — BUKAN 2.13, sesuaikan jika ada diskrepansi)
or_pt <- hasil_or %>% filter(grepl("PT/Akademi", variabel))
message(sprintf("PT/Akademi OR: %.2f (%.2f–%.2f) %s",
                or_pt$OR, or_pt$ci_low, or_pt$ci_high, or_pt$signifikansi))
message("         Gunakan nilai ini (bukan 2.13) secara konsisten di semua narasi.")


# -----------------------------------------------------------------------------
# 7. SIMPAN TABEL 1
# -----------------------------------------------------------------------------

# Tabel 1 final untuk policy brief
tabel1_final <- hasil_or %>%
  mutate(
    OR_display     = round(OR, 3),
    CI_display     = sprintf("%.3f – %.3f", ci_low, ci_high),
    p_display      = ifelse(p_value < 0.001, "<0.001", round(p_value, 3))
  ) %>%
  select(Variabel = variabel,
         OR = OR_display,
         `95% CI` = CI_display,
         Sig. = signifikansi)

write.xlsx(tabel1_final, "output/tables/tabel1_odds_ratio.xlsx",
           overwrite = TRUE)
message("\nTabel 1 disimpan ke output/tables/tabel1_odds_ratio.xlsx")


# -----------------------------------------------------------------------------
# 8. SIMPAN OBJEK UNTUK VISUALISASI
# -----------------------------------------------------------------------------

save(
  model_jkn, hasil_or, tabel1_final,
  file = "data/hasil_regresi.RData"
)
message("Objek regresi disimpan ke data/hasil_regresi.RData")
