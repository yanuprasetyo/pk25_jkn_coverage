# =============================================================================
# SCRIPT 02: KONSTRUKSI WELFARE INDEX VIA PRINCIPAL COMPONENT ANALYSIS (PCA)
# =============================================================================
# Proyek  : Analisis Determinan dan Ketimpangan Kepesertaan JKN, PK 2025
# Penulis : Yanu Endar Prasetyo et al., Pusat Riset Kependudukan BRIN
# Versi   : 2.0 (Revisi)
# Tanggal : 2026
# -----------------------------------------------------------------------------
# Tujuan  : Mengonstruksi welfare index sebagai proxy status ekonomi
#           menggunakan PCA dari 8 variabel kepemilikan aset. Komponen utama
#           pertama (PC1) yang menjelaskan 36% varians digunakan sebagai skor.
#           Skor kemudian digunakan untuk ranking dalam perhitungan CI dan
#           dibagi menjadi 5 kuintil untuk analisis distribusional.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------

library(tidyverse)

PATH_DATA_IN  <- "data/pk2025_analytic.rds"
PATH_DATA_OUT <- "data/pk2025_analytic.rds"  # update dataset yang sama


# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

df <- read_rds(PATH_DATA_IN)


# -----------------------------------------------------------------------------
# 2. DEFINISI VARIABEL ASET UNTUK PCA
# -----------------------------------------------------------------------------
# 8 variabel aset biner (0/1) dari instrumen PK 2025

aset_vars <- c(
  "aset_tabungan",   # tabungan/rekening aktif
  "aset_tv",         # televisi layar datar
  "aset_kulkas",     # lemari es/kulkas
  "aset_komputer",   # komputer/laptop
  "aset_motor",      # sepeda motor
  "aset_mobil",      # mobil
  "aset_emas",       # emas/perhiasan minimal 10 gram
  "aset_lahan"       # lahan
)

# Pastikan semua variabel aset tersedia
stopifnot(all(aset_vars %in% names(df)))

# Ekstrak matriks aset (pastikan numerik, bersih dari NA)
df_aset <- df %>%
  select(all_of(aset_vars)) %>%
  mutate(across(everything(), as.numeric))

message("Jumlah missing per variabel aset:")
print(colSums(is.na(df_aset)))


# -----------------------------------------------------------------------------
# 3. ANALISIS PCA
# -----------------------------------------------------------------------------

# Jalankan PCA dengan scaling (center + scale = TRUE)
# Scaling penting karena semua variabel biner dengan mean dan variance berbeda
pca_result <- prcomp(
  df_aset,
  center = TRUE,
  scale. = TRUE,
  na.action = na.omit
)

# Ringkasan variance explained
pca_summary <- summary(pca_result)

message("\n--- Variance Explained per Komponen ---")
var_explained <- pca_summary$importance["Proportion of Variance", ] * 100
print(round(var_explained[1:5], 3))

message("\nPC1 menjelaskan: ",
        round(var_explained["PC1"], 1), "% varians")
# Expected: ~36%

# Loadings PC1 — menunjukkan kontribusi tiap aset
message("\n--- Loadings PC1 ---")
loadings_pc1 <- pca_result$rotation[, "PC1"]
print(round(sort(loadings_pc1, decreasing = TRUE), 3))


# -----------------------------------------------------------------------------
# 4. EKSTRAK SKOR PC1 & TAMBAHKAN KE DATASET
# -----------------------------------------------------------------------------

# Skor PC1 = welfare index (makin tinggi = makin sejahtera)
welfare_scores <- pca_result$x[, "PC1"]

# Periksa dimensi — perlu handle jika ada NA yang di-drop saat PCA
if (length(welfare_scores) == nrow(df)) {
  df$welfare_index <- welfare_scores
} else {
  # Jika ada baris yang di-drop karena NA, isi dengan NA
  rows_complete <- complete.cases(df_aset)
  df$welfare_index <- NA_real_
  df$welfare_index[rows_complete] <- welfare_scores
  message("Peringatan: ", sum(!rows_complete),
          " observasi memiliki NA pada variabel aset.")
}


# -----------------------------------------------------------------------------
# 5. KONSTRUKSI KUINTIL KESEJAHTERAAN
# -----------------------------------------------------------------------------
# Kuintil digunakan untuk analisis distribusional (Gambar 3)
# dan sebagai dasar perhitungan Concentration Index

df <- df %>%
  mutate(
    kuintil_welfare = ntile(welfare_index, 5),
    kuintil_label = factor(
      kuintil_welfare,
      levels = 1:5,
      labels = c("Q1\n(Termiskin)", "Q2", "Q3", "Q4", "Q5\n(Terkaya)")
    )
  )

# Verifikasi distribusi kuintil
message("\n--- Distribusi Kuintil Kesejahteraan ---")
df %>%
  count(kuintil_welfare) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()


# -----------------------------------------------------------------------------
# 6. STATISTIK DESKRIPTIF WELFARE INDEX
# -----------------------------------------------------------------------------

message("\n--- Statistik Welfare Index ---")
summary(df$welfare_index)

# Coverage JKN per kuintil (preview awal)
message("\n--- Coverage JKN per Kuintil (unweighted, preview) ---")
df %>%
  group_by(kuintil_welfare) %>%
  summarise(
    n = n(),
    coverage_pct = round(mean(jkn, na.rm = TRUE) * 100, 1)
  ) %>%
  print()
# Expected gradient: Q1~69.5%, Q2~68.9%, Q3~71.4%, Q4~73.3%, Q5~78.4%


# -----------------------------------------------------------------------------
# 7. SIMPAN SCREE PLOT (DIAGNOSTIK PCA)
# -----------------------------------------------------------------------------

# Data untuk scree plot
scree_df <- data.frame(
  PC        = paste0("PC", 1:length(var_explained)),
  var_pct   = as.numeric(var_explained),
  cumvar    = cumsum(as.numeric(var_explained))
) %>%
  slice(1:8)  # tampilkan 8 PC pertama

scree_plot <- ggplot(scree_df, aes(x = reorder(PC, -var_pct), y = var_pct)) +
  geom_col(fill = "#2C6E49", alpha = 0.85) +
  geom_line(aes(group = 1), color = "#C84B31", linewidth = 0.8) +
  geom_point(color = "#C84B31", size = 2.5) +
  geom_text(aes(label = paste0(round(var_pct, 1), "%")),
            vjust = -0.5, size = 3.2) +
  labs(
    title    = "Scree Plot: Variance Explained per Komponen PCA",
    subtitle = "8 variabel kepemilikan aset, PK 2025",
    x        = "Komponen Utama",
    y        = "Variance Explained (%)",
    caption  = "Sumber: Pendataan Keluarga 2025, BKKBN-BRIN"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/figures/diagnostik_pca_scree.png",
       scree_plot, width = 8, height = 5, dpi = 300)
message("Scree plot disimpan.")


# -----------------------------------------------------------------------------
# 8. SIMPAN DATASET YANG SUDAH DILENGKAPI WELFARE INDEX
# -----------------------------------------------------------------------------

write_rds(df, PATH_DATA_OUT)
message("\nDataset dengan welfare index disimpan ke: ", PATH_DATA_OUT)
message("Variabel baru: welfare_index, kuintil_welfare, kuintil_label")
