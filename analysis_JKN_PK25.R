# =============================================================================
#  ANALISIS KEPESERTAAN JKN — KEPALA KELUARGA INDONESIA
#  "Determinan dan Ketimpangan Kepesertaan JKN: Bukti dari PK 2025"
#
#  Data   : Pendataan Keluarga 2025 (Blok PK, Probability Sample, PD BRIN)
#  Unit   : Kepala Keluarga (989.610 observasi, 136 variabel)
#  Metode : Complex Survey Analysis + Concentration Index + Logistic Regression
#  Software: R >= 4.3.0
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 00. PACKAGE
# ─────────────────────────────────────────────────────────────────────────────

packages <- c(
  "haven",          # baca .sav
  "tidyverse",      # data wrangling + ggplot2
  "survey",         # complex survey (svyglm)
  "srvyr",          # tidy interface survey
  "psych",          # PCA
  "broom",          # tidy model output
  "patchwork",      # gabung plot
  "writexl"         # export Excel
)

new_pkg <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(new_pkg)) install.packages(new_pkg, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

options(survey.lonely.psu = "adjust")   # handle strata dengan 1 PSU


# ─────────────────────────────────────────────────────────────────────────────
# 01. LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
# Sesuaikan path ke lokasi file .sav di komputer Anda

raw <- haven::read_sav(
  "Blok_PK_Pemutakhiran_PK-25_Probability_PD_BRIN.sav"
)

cat(sprintf("Data loaded: %s baris, %s kolom\n",
            format(nrow(raw), big.mark = "."),
            ncol(raw)))


# ─────────────────────────────────────────────────────────────────────────────
# 02. SELEKSI & RECODE VARIABEL
# ─────────────────────────────────────────────────────────────────────────────

pk25 <- raw |>
  select(
    # Identitas & sampling
    kode_provinsi, nama_provinsi,
    kode_gab_kec, kode_gab_desa, Wfinal_kel,
    # Variabel utama
    jns_asuransi, jenis_kelamin, umur_olah,
    klasifikasi_desa, jenis_pendidikan,
    jns_pekerjaan, status_pekerjaan, pk13, pk14,
    # Aset (untuk welfare index)
    pk14_1, pk14_3, pk14_4, pk14_5,
    pk14_6, pk14_7, pk14_9, pk14_11
  ) |>
  mutate(
    # ── Outcome: JKN biner ──────────────────────────────────────────────────
    # 1 = BPJS PBI atau Non-PBI | 0 = Tidak memiliki | NA = Swasta (eksklusif)
    jkn = case_when(
      jns_asuransi %in% c(1, 2) ~ 1L,
      jns_asuransi == 4         ~ 0L,
      TRUE                      ~ NA_integer_
    ),

    jkn_tipe = case_when(
      jns_asuransi == 1 ~ "PBI",
      jns_asuransi == 2 ~ "Non-PBI",
      jns_asuransi == 3 ~ "Swasta",
      jns_asuransi == 4 ~ "Tidak"
    ),

    # ── Sosiodemografi ───────────────────────────────────────────────────────
    gender = factor(jenis_kelamin,
                    levels = c(1, 2),
                    labels = c("Laki-laki", "Perempuan")),

    kel_usia = cut(umur_olah,
                   breaks = c(0, 24, 34, 44, 54, 64, Inf),
                   labels = c("<25", "25-34", "35-44",
                              "45-54", "55-64", "65+"),
                   right = TRUE),

    wilayah = factor(klasifikasi_desa,
                     levels = c(1, 2),
                     labels = c("Perkotaan", "Perdesaan")),

    # ── Pendidikan (5 kategori) ──────────────────────────────────────────────
    pendidikan = case_when(
      jenis_pendidikan %in% c(1, 2)       ~ "Tidak/Tdk tamat SD",
      jenis_pendidikan %in% c(3, 4)       ~ "SD",
      jenis_pendidikan %in% c(5, 6, 7)    ~ "SMP",
      jenis_pendidikan %in% c(8, 9, 10)   ~ "SMA",
      jenis_pendidikan %in% c(11, 12)     ~ "PT/Akademi",
      TRUE                                ~ NA_character_
    ) |> factor(levels = c("Tidak/Tdk tamat SD", "SD", "SMP",
                            "SMA", "PT/Akademi")),

    # ── Pekerjaan (7 kategori) ───────────────────────────────────────────────
    pekerjaan = case_when(
      jns_pekerjaan == 1            ~ "Tidak bekerja",
      jns_pekerjaan %in% c(2, 3)   ~ "Petani/Nelayan",
      jns_pekerjaan == 4            ~ "Pedagang",
      jns_pekerjaan %in% c(5, 6)   ~ "PNS/TNI/Pejabat",
      jns_pekerjaan %in% c(7,8,9)  ~ "Swasta",
      jns_pekerjaan == 10           ~ "Pensiunan",
      jns_pekerjaan == 11           ~ "Pekerja lepas",
      TRUE                          ~ NA_character_
    ) |> factor(levels = c("Tidak bekerja", "Petani/Nelayan",
                            "Pedagang", "Swasta", "Pekerja lepas",
                            "PNS/TNI/Pejabat", "Pensiunan")),

    # ── Aset biner (1=punya, 0=tidak) ───────────────────────────────────────
    aset_tabungan = if_else(!is.na(pk14_1),  1L, 0L),
    aset_tv       = if_else(!is.na(pk14_3),  1L, 0L),
    aset_kulkas   = if_else(!is.na(pk14_4),  1L, 0L),
    aset_laptop   = if_else(!is.na(pk14_5),  1L, 0L),
    aset_motor    = if_else(!is.na(pk14_6),  1L, 0L),
    aset_mobil    = if_else(!is.na(pk14_7),  1L, 0L),
    aset_emas     = if_else(!is.na(pk14_9),  1L, 0L),
    aset_lahan    = if_else(!is.na(pk14_11), 1L, 0L)
  )

cat(sprintf("pk25 siap: %s baris, %s kolom\n",
            format(nrow(pk25), big.mark = "."), ncol(pk25)))


# ─────────────────────────────────────────────────────────────────────────────
# 03. DATASET ANALITIK
#     Filter ke observasi yang valid untuk analisis utama
# ─────────────────────────────────────────────────────────────────────────────

pk25_analisis <- pk25 |>
  filter(
    !is.na(jkn),           # keluarkan Swasta (kode 3)
    !is.na(pendidikan),
    !is.na(pekerjaan),
    umur_olah >= 15,       # KK minimal 15 tahun
    umur_olah <= 100       # trim outlier usia ekstrem
  )

cat(sprintf("N analitik: %s KK (dari %s)\n",
            format(nrow(pk25_analisis), big.mark = "."),
            format(nrow(pk25), big.mark = ".")))


# ─────────────────────────────────────────────────────────────────────────────
# 04. WELFARE INDEX (PCA dari variabel aset)
# ─────────────────────────────────────────────────────────────────────────────

aset_matrix <- pk25_analisis |>
  select(starts_with("aset_")) |>
  as.matrix()

pca_hasil <- prcomp(aset_matrix, scale. = TRUE, center = TRUE)

cat("\nVariance explained per komponen PCA:\n")
print(summary(pca_hasil)$importance[, 1:4])

# PC1 sebagai welfare index
pk25_analisis <- pk25_analisis |>
  mutate(
    welfare_index = pca_hasil$x[, 1],
    kuintil = ntile(welfare_index, 5) |>
      factor(labels = c("Q1 (Termiskin)", "Q2", "Q3",
                        "Q4", "Q5 (Terkaya)"))
  )

cat("\nDistribusi kuintil:\n")
print(pk25_analisis |> count(kuintil))


# ─────────────────────────────────────────────────────────────────────────────
# 05. SURVEY DESIGN
# ─────────────────────────────────────────────────────────────────────────────

svy_pk25 <- pk25_analisis |>
  as_survey_design(
    ids     = kode_gab_desa,
    strata  = kode_gab_kec,
    weights = Wfinal_kel,
    nest    = TRUE
  )

# Survey design tanpa PNS (untuk model regresi utama)
svy_nopns <- pk25_analisis |>
  filter(pekerjaan != "PNS/TNI/Pejabat") |>
  as_survey_design(
    ids     = kode_gab_desa,
    strata  = kode_gab_kec,
    weights = Wfinal_kel,
    nest    = TRUE
  )

cat(sprintf("\nSurvey design: %s KK | %s KK (tanpa PNS)\n",
            format(nrow(svy_pk25$variables),  big.mark = "."),
            format(nrow(svy_nopns$variables), big.mark = ".")))


# ─────────────────────────────────────────────────────────────────────────────
# 06. STATISTIK DESKRIPTIF BERBOBOT
# ─────────────────────────────────────────────────────────────────────────────

cat("\n── Coverage JKN nasional ──\n")
desc_nasional <- svy_pk25 |>
  group_by(jkn_tipe) |>
  summarise(
    n_sampel = unweighted(n()),
    persen   = survey_mean(vartype = "ci") * 100
  )
print(desc_nasional)

cat("\n── Coverage JKN per gender × wilayah ──\n")
desc_gender_wilayah <- svy_pk25 |>
  group_by(gender, wilayah) |>
  summarise(
    n_sampel = unweighted(n()),
    pct_jkn  = survey_mean(jkn, vartype = "ci") * 100,
    .groups  = "drop"
  )
print(desc_gender_wilayah)

cat("\n── Coverage JKN per kelompok usia ──\n")
desc_usia <- svy_pk25 |>
  group_by(kel_usia) |>
  summarise(
    n_sampel = unweighted(n()),
    pct_jkn  = survey_mean(jkn, vartype = "ci") * 100,
    .groups  = "drop"
  )
print(desc_usia)

cat("\n── Coverage JKN per pendidikan ──\n")
desc_pendidikan <- svy_pk25 |>
  group_by(pendidikan) |>
  summarise(
    n_sampel = unweighted(n()),
    pct_jkn  = survey_mean(jkn, vartype = "ci") * 100,
    .groups  = "drop"
  )
print(desc_pendidikan)

cat("\n── Coverage JKN per pekerjaan ──\n")
desc_pekerjaan <- svy_pk25 |>
  group_by(pekerjaan) |>
  summarise(
    n_sampel = unweighted(n()),
    pct_jkn  = survey_mean(jkn, vartype = "ci") * 100,
    .groups  = "drop"
  ) |>
  arrange(pct_jkn)
print(desc_pekerjaan)

cat("\n── Coverage JKN per kuintil kesejahteraan ──\n")
desc_kuintil <- svy_pk25 |>
  group_by(kuintil) |>
  summarise(
    n_sampel = unweighted(n()),
    pct_jkn  = survey_mean(jkn, vartype = "ci") * 100,
    .groups  = "drop"
  )
print(desc_kuintil)


# ─────────────────────────────────────────────────────────────────────────────
# 07. CONCENTRATION INDEX (CI)
# ─────────────────────────────────────────────────────────────────────────────

# Fungsi hitung CI per sub-kelompok
ci_by_group <- function(data, group_var) {
  data |>
    filter(!is.na(jkn), !is.na(welfare_index)) |>
    group_by({{ group_var }}) |>
    arrange(welfare_index, .by_group = TRUE) |>
    mutate(
      w      = Wfinal_kel / sum(Wfinal_kel),
      cum_w  = cumsum(w),
      rank_w = lag(cum_w, default = 0) + w / 2
    ) |>
    summarise(
      n        = n(),
      mean_jkn = weighted.mean(jkn, Wfinal_kel),
      ci       = 2 * cov.wt(
                   cbind(jkn, rank_w),
                   wt  = Wfinal_kel,
                   cor = FALSE
                 )$cov[1, 2] / weighted.mean(jkn, Wfinal_kel),
      .groups = "drop"
    ) |>
    mutate(
      mean_pct = round(mean_jkn * 100, 2),
      ci       = round(ci, 4)
    )
}

# CI nasional
df_ci_nasional <- pk25_analisis |>
  arrange(welfare_index) |>
  mutate(
    w      = Wfinal_kel / sum(Wfinal_kel),
    cum_w  = cumsum(w),
    rank_w = lag(cum_w, default = 0) + w / 2
  )
mean_jkn  <- weighted.mean(df_ci_nasional$jkn, df_ci_nasional$Wfinal_kel)
ci_nasional <- 2 * cov.wt(
  cbind(df_ci_nasional$jkn, df_ci_nasional$rank_w),
  wt = df_ci_nasional$Wfinal_kel, cor = FALSE
)$cov[1, 2] / mean_jkn

cat(sprintf("\nConcentration Index Nasional: %.4f (%s)\n",
            ci_nasional,
            ifelse(ci_nasional > 0, "Pro-kaya", "Pro-miskin")))

# CI per sub-kelompok
ci_wilayah    <- ci_by_group(pk25_analisis, wilayah)
ci_gender     <- ci_by_group(pk25_analisis, gender)
ci_usia       <- ci_by_group(pk25_analisis, kel_usia)
ci_pendidikan <- ci_by_group(pk25_analisis, pendidikan)
ci_pekerjaan  <- ci_by_group(pk25_analisis, pekerjaan)

# Tabel ringkasan CI
tabel_ci <- bind_rows(
  tibble(kelompok = "Nasional", subgroup = "Semua KK",
         n = nrow(pk25_analisis),
         mean_pct = round(mean_jkn * 100, 2),
         ci = round(ci_nasional, 4)),
  ci_wilayah    |> mutate(kelompok = "Wilayah",    subgroup = as.character(wilayah))    |> select(kelompok, subgroup, n, mean_pct, ci),
  ci_gender     |> mutate(kelompok = "Gender",     subgroup = as.character(gender))     |> select(kelompok, subgroup, n, mean_pct, ci),
  ci_usia       |> mutate(kelompok = "Usia",       subgroup = as.character(kel_usia))   |> select(kelompok, subgroup, n, mean_pct, ci),
  ci_pendidikan |> mutate(kelompok = "Pendidikan", subgroup = as.character(pendidikan)) |> select(kelompok, subgroup, n, mean_pct, ci),
  ci_pekerjaan  |> mutate(kelompok = "Pekerjaan",  subgroup = as.character(pekerjaan))  |> select(kelompok, subgroup, n, mean_pct, ci)
)

cat("\nTabel Concentration Index:\n")
print(tabel_ci, n = 30)


# ─────────────────────────────────────────────────────────────────────────────
# 08. REGRESI LOGISTIK (Complex Survey)
#     Catatan: PNS/TNI/Pejabat dikeluarkan karena perfect separation (100% JKN)
# ─────────────────────────────────────────────────────────────────────────────

model_logit <- svyglm(
  jkn ~ gender + kel_usia + pendidikan +
        pekerjaan + wilayah + welfare_index + pk13,
  design = svy_nopns,
  family = quasibinomial(link = "logit")
)

# Tabel Odds Ratio
tabel_or <- broom::tidy(model_logit,
                        exponentiate = TRUE,
                        conf.int     = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    across(c(estimate, conf.low, conf.high), ~round(., 3)),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    term = recode(term,
      "genderPerempuan"          = "KK Perempuan (ref: Laki-laki)",
      "kel_usia25-34"            = "Usia 25-34 (ref: <25)",
      "kel_usia35-44"            = "Usia 35-44",
      "kel_usia45-54"            = "Usia 45-54",
      "kel_usia55-64"            = "Usia 55-64",
      "kel_usia65+"              = "Usia 65+",
      "pendidikanSD"             = "Pendidikan SD (ref: Tidak/Tdk tamat SD)",
      "pendidikanSMP"            = "Pendidikan SMP",
      "pendidikanSMA"            = "Pendidikan SMA",
      "pendidikanPT/Akademi"     = "Pendidikan PT/Akademi",
      "pekerjaanPetani/Nelayan"  = "Petani/Nelayan (ref: Tidak bekerja)",
      "pekerjaanPedagang"        = "Pedagang",
      "pekerjaanSwasta"          = "Swasta",
      "pekerjaanPekerja lepas"   = "Pekerja lepas",
      "pekerjaanPensiunan"       = "Pensiunan",
      "wilayahPerdesaan"         = "Perdesaan (ref: Perkotaan)",
      "welfare_index"            = "Welfare Index (PCA aset)",
      "pk13"                     = "Kecukupan penghasilan"
    )
  ) |>
  rename(OR = estimate, CI_95_low = conf.low, CI_95_high = conf.high)

cat("\nTabel Odds Ratio:\n")
print(tabel_or, n = 20)


# ─────────────────────────────────────────────────────────────────────────────
# 09. VISUALISASI
# ─────────────────────────────────────────────────────────────────────────────

theme_paper <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "gray40", size = 9),
    plot.caption     = element_text(color = "gray50", size = 8),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# ── Plot A: Coverage per kuintil kesejahteraan ─────────────────────────────
plot_data_kuintil <- desc_kuintil |>
  mutate(kuintil_label = c("Q1\nTermiskin","Q2","Q3","Q4","Q5\nTerkaya"))

p1 <- ggplot(plot_data_kuintil,
             aes(x = kuintil_label, y = pct_jkn)) +
  geom_col(fill = "#2E75B6", alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = pct_jkn_low, ymax = pct_jkn_upp),
                width = 0.2, linewidth = 0.6, color = "gray30") +
  geom_text(aes(label = sprintf("%.1f%%", pct_jkn)),
            vjust = -0.8, fontface = "bold", size = 3.2) +
  geom_hline(yintercept = mean_jkn * 100,
             linetype = "dashed", color = "red", linewidth = 0.5) +
  annotate("text", x = 0.6, y = mean_jkn * 100 + 1.2,
           label = sprintf("Rata-rata\n%.1f%%", mean_jkn * 100),
           color = "red", size = 2.8, hjust = 0) +
  scale_y_continuous(limits = c(0, 85),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "A. Coverage per Kuintil Kesejahteraan",
       x = "Kuintil Kesejahteraan (PCA Aset)",
       y = "Coverage JKN (%)") +
  theme_paper +
  theme(panel.grid.major.x = element_blank())

# ── Plot B: Coverage per usia & gender ────────────────────────────────────
p2 <- ggplot(desc_gender_wilayah |>
               group_by(gender) |>
               summarise(pct_jkn = mean(pct_jkn), .groups = "drop") |>
               left_join(
                 svy_pk25 |>
                   group_by(kel_usia, gender) |>
                   summarise(pct_jkn  = survey_mean(jkn, vartype = "ci") * 100,
                             .groups = "drop"),
                 by = "gender"
               ),
             aes(x = kel_usia, y = pct_jkn.y,
                 color = gender, group = gender)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = pct_jkn_low, ymax = pct_jkn_upp,
                  fill = gender),
              alpha = 0.1, color = NA) +
  scale_color_manual(values = c("Laki-laki" = "#2E75B6",
                                "Perempuan" = "#E85D04"), name = NULL) +
  scale_fill_manual(values  = c("Laki-laki" = "#2E75B6",
                                "Perempuan" = "#E85D04"), name = NULL) +
  scale_y_continuous(limits = c(60, 82),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "B. Coverage per Usia & Gender KK",
       x = "Kelompok Usia KK", y = "Coverage JKN (%)") +
  theme_paper

# ── Plot C: Coverage per pekerjaan ────────────────────────────────────────
plot_data_pkj <- desc_pekerjaan |>
  filter(pekerjaan != "PNS/TNI/Pejabat") |>
  arrange(pct_jkn) |>
  mutate(pekerjaan = factor(pekerjaan, levels = pekerjaan))

p3 <- ggplot(plot_data_pkj,
             aes(x = pct_jkn, y = pekerjaan,
                 color = pct_jkn > mean_jkn * 100)) +
  geom_vline(xintercept = mean_jkn * 100,
             linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = pct_jkn_low, xmax = pct_jkn_upp),
                 height = 0.3, linewidth = 0.6) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.1f%%", pct_jkn)),
            hjust = -0.3, size = 3.2, fontface = "bold") +
  scale_color_manual(
    values = c("TRUE" = "#2E75B6", "FALSE" = "#E85D04"),
    labels = c("TRUE" = "Di atas rata-rata",
               "FALSE"= "Di bawah rata-rata"), name = NULL) +
  scale_x_continuous(limits = c(60, 96),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "C. Coverage per Jenis Pekerjaan KK",
       subtitle = "(PNS/TNI/Pejabat dikecualikan — coverage 100%)",
       x = "Coverage JKN (%)", y = NULL) +
  theme_paper +
  theme(panel.grid.major.y = element_blank())

# ── Plot D: Forest plot Odds Ratio ─────────────────────────────────────────
plot_or <- tabel_or |>
  mutate(
    term       = factor(term, levels = rev(term)),
    signifikan = sig %in% c("*", "**", "***")
  )

p4 <- ggplot(plot_or,
             aes(x = OR, y = term, color = signifikan)) +
  geom_vline(xintercept = 1,
             linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = CI_95_low, xmax = CI_95_high),
                 height = 0.3, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "#2E75B6", "FALSE" = "#AAAAAA"),
    labels = c("TRUE" = "Signifikan (p<0.05)",
               "FALSE"= "Tidak signifikan"), name = NULL) +
  scale_x_continuous(breaks = seq(0.6, 2.4, 0.2)) +
  labs(title    = "D. Forest Plot Odds Ratio",
       subtitle = "Regresi logistik berbobot (complex survey)",
       x = "Odds Ratio (95% CI)", y = NULL) +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8))

# ── Gabungkan semua plot ───────────────────────────────────────────────────
figure_final <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "Analisis Kepesertaan JKN — Kepala Keluarga Indonesia (PK 2025)",
    subtitle = sprintf(
      "Coverage nasional: %.1f%% | CI = %.4f (pro-kaya) | N = %s KK",
      mean_jkn * 100, ci_nasional,
      format(nrow(pk25_analisis), big.mark = ".")),
    caption  = "Sumber: Pendataan Keluarga 2025, BKKBN — BRIN\nAnalisis menggunakan complex survey design (stratified cluster sampling + bobot)",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10),
      plot.caption  = element_text(color = "gray50", size = 8)
    )
  )

ggsave("output/figure_final_JKN_PK25.png",
       plot = figure_final,
       width = 14, height = 11, dpi = 300, bg = "white")

# Plot individu
ggsave("output/plot_A_kuintil.png",    p1, width=7,  height=5, dpi=300, bg="white")
ggsave("output/plot_B_usia_gender.png",p2, width=8,  height=5, dpi=300, bg="white")
ggsave("output/plot_C_pekerjaan.png",  p3, width=8,  height=5, dpi=300, bg="white")
ggsave("output/plot_D_forestplot.png", p4, width=9,  height=7, dpi=300, bg="white")

cat("Semua plot tersimpan di folder output/\n")


# ─────────────────────────────────────────────────────────────────────────────
# 10. EXPORT HASIL KE EXCEL
# ─────────────────────────────────────────────────────────────────────────────

if (!dir.exists("output")) dir.create("output")

write_xlsx(
  list(
    "00_Coverage_Nasional"     = as.data.frame(desc_nasional),
    "01_Coverage_Gender_Wil"   = as.data.frame(desc_gender_wilayah),
    "02_Coverage_Usia"         = as.data.frame(desc_usia),
    "03_Coverage_Pendidikan"   = as.data.frame(desc_pendidikan),
    "04_Coverage_Pekerjaan"    = as.data.frame(desc_pekerjaan),
    "05_Coverage_Kuintil"      = as.data.frame(desc_kuintil),
    "06_Concentration_Index"   = as.data.frame(tabel_ci),
    "07_Regresi_OddsRatio"     = as.data.frame(tabel_or)
  ),
  path = "output/hasil_analisis_JKN_PK25.xlsx"
)

cat("Hasil tersimpan: output/hasil_analisis_JKN_PK25.xlsx\n")


# ─────────────────────────────────────────────────────────────────────────────
# 11. SESSION INFO
# ─────────────────────────────────────────────────────────────────────────────

sink("output/session_info.txt")
cat("Analisis JKN PK 2025\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
sessionInfo()
sink()

cat("\n=== ANALISIS SELESAI ===\n")
cat("Output tersimpan di folder: output/\n")
