# =============================================================================
# SCRIPT 06: VISUALISASI — GAMBAR 1 HINGGA 4
# =============================================================================
# Proyek  : Analisis Determinan dan Ketimpangan Kepesertaan JKN, PK 2025
# Penulis : Yanu Endar Prasetyo et al., Pusat Riset Kependudukan BRIN
# Versi   : 2.0 (Revisi)
# Tanggal : 2026
# -----------------------------------------------------------------------------
# Tujuan  : Mereproduksi seluruh gambar pada Policy Brief:
#   Gambar 1 — Coverage JKN menurut kelompok usia dan gender
#   Gambar 2 — Coverage JKN menurut jenis pekerjaan
#   Gambar 3 — Coverage JKN menurut kuintil kesejahteraan
#   Gambar 4 — Forest plot determinan kepesertaan JKN (OR dari regresi)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)

# Load hasil dari script sebelumnya
load("data/hasil_deskriptif.RData")
load("data/hasil_regresi.RData")

# Palet warna konsisten dengan desain policy brief BRIN
WARNA_UTAMA    <- "#1A5276"   # biru tua (laki-laki / bar utama)
WARNA_KEDUA    <- "#C84B31"   # merah bata (perempuan / aksen)
WARNA_POSITIF  <- "#2C6E49"   # hijau tua (di atas rata-rata)
WARNA_NEGATIF  <- "#922B21"   # merah tua (di bawah rata-rata)
WARNA_NS       <- "#95A5A6"   # abu-abu (tidak signifikan)
WARNA_RATA     <- "#566573"   # garis rata-rata

# Tema ggplot kustom
tema_brin <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle    = element_text(color = "gray40", size = 10, hjust = 0),
    plot.caption     = element_text(color = "gray50", size = 8, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_blank()
  )


# -----------------------------------------------------------------------------
# GAMBAR 1: COVERAGE JKN MENURUT KELOMPOK USIA & GENDER
# -----------------------------------------------------------------------------

gambar1 <- cov_usia_gender %>%
  filter(!is.na(usia_kel), !is.na(gender_kk)) %>%
  ggplot(aes(x = usia_kel, y = coverage_pct,
             color = gender_kk, group = gender_kk)) +

  # Confidence interval ribbon
  geom_ribbon(aes(ymin = coverage_pct_low, ymax = coverage_pct_upp,
                  fill = gender_kk),
              alpha = 0.12, color = NA) +

  # Garis tren
  geom_line(linewidth = 0.9) +

  # Titik data
  geom_point(size = 3.5, shape = 19) +

  # Label angka
  geom_text(aes(label = sprintf("%.1f%%", coverage_pct)),
            vjust = -1.1, size = 3.0, fontface = "bold",
            show.legend = FALSE) +

  # Garis rata-rata nasional
  geom_hline(yintercept = cov_nasional_pct,
             linetype = "dashed", color = WARNA_RATA, linewidth = 0.7) +
  annotate("text", x = 1, y = cov_nasional_pct + 0.6,
           label = sprintf("Rata-rata\n%.1f%%", cov_nasional_pct),
           size = 2.8, color = WARNA_RATA, hjust = 0) +

  scale_color_manual(values = c("Laki-laki" = WARNA_UTAMA,
                                "Perempuan" = WARNA_KEDUA)) +
  scale_fill_manual(values  = c("Laki-laki" = WARNA_UTAMA,
                                "Perempuan" = WARNA_KEDUA)) +
  scale_y_continuous(
    limits = c(60, 90),
    breaks = seq(60, 90, 5),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Coverage JKN Menurut Kelompok Usia dan Gender KK",
    subtitle = "Estimasi berbobot — PK 2025 | Ribbon = 95% CI",
    x        = "Kelompok Usia Kepala Keluarga",
    y        = "Coverage JKN (%)",
    caption  = "Sumber: Pendataan Keluarga 2025, BKKBN-BRIN",
    color    = "Gender KK",
    fill     = "Gender KK"
  ) +
  tema_brin

ggsave("output/figures/gambar1_coverage_usia_gender.png",
       gambar1, width = 9, height = 5.5, dpi = 300)
message("Gambar 1 disimpan.")


# -----------------------------------------------------------------------------
# GAMBAR 2: COVERAGE JKN MENURUT JENIS PEKERJAAN
# -----------------------------------------------------------------------------

# Urutan tampilan: dari terendah ke tertinggi, kecuali PNS (100%) dihilangkan
# dari tampilan utama sesuai policy brief

gambar2 <- cov_pekerjaan %>%
  filter(!pekerjaan_kel %in% c("PNS/TNI/POLRI", "Pejabat Negara")) %>%
  arrange(coverage_pct) %>%
  mutate(
    pekerjaan_kel = fct_inorder(as.character(pekerjaan_kel)),
    warna_bar     = case_when(
      coverage_pct > cov_nasional_pct ~ "Di atas rata-rata",
      TRUE                            ~ "Di bawah rata-rata"
    ) %>% factor(levels = c("Di atas rata-rata", "Di bawah rata-rata"))
  ) %>%
  ggplot(aes(y = pekerjaan_kel, x = coverage_pct, color = warna_bar)) +

  # Garis rata-rata nasional
  geom_vline(xintercept = cov_nasional_pct,
             linetype = "dashed", color = WARNA_RATA, linewidth = 0.9) +
  annotate("label", x = cov_nasional_pct, y = 5,
           label = sprintf("Rata-rata\n%.1f%%", cov_nasional_pct),
           size = 2.8, color = WARNA_RATA, fill = "white", label.size = 0.3) +

  # Error bar (CI)
  geom_errorbarh(aes(xmin = coverage_pct_low, xmax = coverage_pct_upp),
                 height = 0.3, linewidth = 0.8) +

  # Titik estimasi
  geom_point(size = 4, shape = 19) +

  # Label persen
  geom_text(aes(label = sprintf("%.1f%%", coverage_pct)),
            hjust = -0.25, size = 3.2, fontface = "bold",
            show.legend = FALSE) +

  scale_color_manual(values = c("Di atas rata-rata" = WARNA_POSITIF,
                                "Di bawah rata-rata" = WARNA_NEGATIF)) +
  scale_x_continuous(
    limits = c(55, 98),
    breaks = seq(60, 95, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Coverage JKN Menurut Jenis Pekerjaan Kepala Keluarga",
    subtitle = "Estimasi berbobot — PK 2025 | Error bar = 95% CI\nPNS/TNI/POLRI & Pejabat Negara dikecualikan (coverage 100%)",
    x        = "Coverage JKN (%)",
    y        = NULL,
    caption  = "Sumber: Pendataan Keluarga 2025, BKKBN-BRIN"
  ) +
  tema_brin +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"))

ggsave("output/figures/gambar2_coverage_pekerjaan.png",
       gambar2, width = 9, height = 6, dpi = 300)
message("Gambar 2 disimpan.")


# -----------------------------------------------------------------------------
# GAMBAR 3: COVERAGE JKN MENURUT KUINTIL KESEJAHTERAAN
# -----------------------------------------------------------------------------
# Catatan: Q2 (68.9%) < Q1 (69.5%) — label subtitle menjelaskan pola ini

gambar3 <- cov_kuintil %>%
  filter(!is.na(kuintil_welfare)) %>%
  ggplot(aes(x = kuintil_label, y = coverage_pct, group = 1)) +

  # Garis rata-rata
  geom_hline(yintercept = cov_nasional_pct,
             linetype = "dashed", color = WARNA_RATA, linewidth = 0.8) +
  annotate("label", x = 1, y = cov_nasional_pct + 1.2,
           label = sprintf("Rata-rata: %.1f%%", cov_nasional_pct),
           size = 2.8, color = WARNA_RATA, fill = "white", label.size = 0.3) +

  # Ribbon CI
  geom_ribbon(aes(ymin = coverage_pct_low, ymax = coverage_pct_upp),
              alpha = 0.15, fill = WARNA_UTAMA) +

  # Garis & titik
  geom_line(color = WARNA_UTAMA, linewidth = 1.0) +
  geom_point(color = WARNA_UTAMA, size = 4, shape = 19) +

  # Label
  geom_text(aes(label = sprintf("%.1f%%", coverage_pct)),
            vjust = -1.2, size = 3.3, fontface = "bold",
            color = WARNA_UTAMA) +

  scale_y_continuous(
    limits = c(60, 85),
    breaks = seq(60, 85, 5),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Coverage JKN Menurut Kuintil Kesejahteraan",
    subtitle = "Estimasi berbobot — PK 2025 | Ribbon = 95% CI\nCatatan: Q2 < Q1 mencerminkan efek PBI pada rumah tangga termiskin",
    x        = "Kuintil Kesejahteraan (Welfare Index PCA)",
    y        = "Coverage JKN (%)",
    caption  = "Sumber: Pendataan Keluarga 2025, BKKBN-BRIN"
  ) +
  tema_brin

ggsave("output/figures/gambar3_coverage_kuintil.png",
       gambar3, width = 8, height = 5.5, dpi = 300)
message("Gambar 3 disimpan.")


# -----------------------------------------------------------------------------
# GAMBAR 4: FOREST PLOT — DETERMINAN KEPESERTAAN JKN (OR REGRESI)
# -----------------------------------------------------------------------------

# Urutan variabel untuk forest plot (sesuai Gambar 4 policy brief)
urutan_var <- c(
  "KK Perempuan (ref: Laki-laki)",
  "Usia 25-34 (ref: <25 tahun)", "Usia 35-44", "Usia 45-54",
  "Usia 55-64", "Usia 65+",
  "Pendidikan SD (ref: Tidak/Tdk tamat SD)", "Pendidikan SMP",
  "Pendidikan SMA", "Pendidikan PT/Akademi",
  "Petani (ref: Tidak/Belum Bekerja)", "Nelayan", "Pedagang",
  "Swasta Pertanian", "Swasta Industri", "Swasta Jasa",
  "Pensiunan", "Pekerja Lepas",
  "Perdesaan (ref: Perkotaan)",
  "Welfare Index (PCA Aset)",
  "Kecukupan Penghasilan (pk13)"
)

df_forest <- hasil_or %>%
  filter(variabel %in% urutan_var) %>%
  mutate(
    variabel = factor(variabel, levels = rev(urutan_var)),
    warna_plot = case_when(
      signifikansi == "ns"       ~ "Tidak signifikan",
      OR > 1 & signifikansi != "ns" ~ "Signifikan — Positif",
      OR < 1 & signifikansi != "ns" ~ "Signifikan — Negatif"
    ) %>%
      factor(levels = c("Signifikan — Positif",
                        "Signifikan — Negatif",
                        "Tidak signifikan"))
  )

gambar4 <- ggplot(df_forest,
                  aes(y = variabel, x = OR,
                      xmin = ci_low, xmax = ci_high,
                      color = warna_plot, shape = warna_plot)) +

  # Garis referensi OR = 1
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "gray40", linewidth = 0.9) +

  # Error bar (CI)
  geom_errorbarh(height = 0.4, linewidth = 0.7) +

  # Titik OR
  geom_point(size = 3.2) +

  scale_color_manual(
    values = c("Signifikan — Positif"  = WARNA_POSITIF,
               "Signifikan — Negatif"  = WARNA_NEGATIF,
               "Tidak signifikan"       = WARNA_NS)
  ) +
  scale_shape_manual(
    values = c("Signifikan — Positif"  = 19,
               "Signifikan — Negatif"  = 19,
               "Tidak signifikan"       = 1)
  ) +
  scale_x_continuous(
    breaks = c(0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2),
    labels = function(x) as.character(x)
  ) +
  labs(
    title    = "Determinan Kepesertaan JKN — Kepala Keluarga Indonesia",
    subtitle = paste0(
      "Odds Ratio dari Regresi Logistik Berbobot (PK 2025, n = 930.400)\n",
      "Garis putus-putus = OR 1,0 | PNS/TNI/POLRI & Pejabat Negara dikecualikan"
    ),
    x        = "Odds Ratio (95% CI)",
    y        = NULL,
    color    = NULL,
    shape    = NULL,
    caption  = "Sumber: Pendataan Keluarga 2025, BKKBN-BRIN"
  ) +
  tema_brin +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray88"),
    legend.position    = "bottom"
  )

ggsave("output/figures/gambar4_forest_plot_or.png",
       gambar4, width = 10, height = 9, dpi = 300)
message("Gambar 4 disimpan.")


# -----------------------------------------------------------------------------
# KOMPILASI: PANEL SEMUA GAMBAR
# -----------------------------------------------------------------------------

panel_semua <- ggarrange(
  gambar1, gambar3,
  gambar2, gambar4,
  nrow = 2, ncol = 2,
  labels = c("Gambar 1", "Gambar 3", "Gambar 2", "Gambar 4"),
  font.label = list(size = 9, face = "bold")
)

ggsave("output/figures/panel_semua_gambar.png",
       panel_semua, width = 18, height = 14, dpi = 300)
message("\nPanel semua gambar disimpan.")
message("\n=== Semua visualisasi selesai dibuat ===")
message("Lokasi output: output/figures/")
