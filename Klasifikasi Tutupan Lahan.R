# ==============================================================
# TUGAS BESAR PEMROGRAMAN SPASIAL A
# Analisis Indeks Spektral & Klasifikasi Tutupan Lahan
# Studi Kasus: Kabupaten Semarang, Jawa Tengah
# --------------------------------------------------------------
# Anggota 1 : Icha Ramanda Putri (5016241119)
# Anggota 2 : Katerine Thalia Syahiranadya (5016241126)
# Kelompok  : x
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi tugas:
# 1. Menentukan ROI (Kabupaten Semarang)
# 2. Mengunduh citra Sentinel-2 L2A (Copernicus) dan batas
#    administrasi kabupaten & kecamatan (Ina-Geoportal)
# 3. Memotong (crop) citra sesuai batas kabupaten
# 4. Menghitung NDVI, NDBI, NDWI, NDCI dan mengklasifikasikan
#    tutupan lahan: vegetasi, bangunan, perairan, awan
# 5. Menghitung zonal statistics (mean, min, max, median, sd)
#    tiap indeks per kecamatan
# 6. Membuat peta komposit klasifikasi & tabel zonal statistics
# --------------------------------------------------------------
# Data:
# - Sentinel-2 L2A tile T49MDN & T49MDM, 14 Oktober 2026
# - Batas administrasi kecamatan Kab. Semarang (Ina-Geoportal)
# --------------------------------------------------------------
# Struktur folder project:
# Peta Tutupan Lahan/
# ├── Klasifikasi Tutupan Lahan.R
# ├── Peta Tutupan Lahan.Rproj
# ├── data/
# │   ├── S2B_MSIL2A_..._T49MDN_....SAFE   (citra tile atas)
# │   ├── S2B_MSIL2A_..._T49MDM_....SAFE   (citra tile bawah)
# │   └── Semarang/                        (data batas administrasi)
# └── output/                              (hasil peta, tabel, raster)
# ==============================================================


# ==============================================================
# 0. PERSIAPAN
# ==============================================================

# --- 0.1 Install package (cukup sekali) ---
# Hapus tanda # di baris bawah kalau package belum pernah diinstall,
# jalankan, lalu beri tanda # lagi.
# install.packages(c("terra", "sf", "dplyr", "ggplot2", "ggspatial"))

# --- 0.2 Memanggil package ---
Sys.setenv(PROJ_LIB  = system.file("proj", package = "terra"),
           PROJ_DATA = system.file("proj", package = "terra"))

library(terra)      # olah raster (citra)
library(sf)         # olah vektor (shapefile)
library(dplyr)      # olah tabel
library(ggplot2)    # membuat peta statis
library(ggspatial)  # skala & panah utara di peta

# --- 0.3 Cek folder kerja ---
# Karena memakai R Project, folder kerja otomatis di folder project
getwd()

# --- 0.4 Cek isi folder data ---
list.files("data")                        # harus ada 2 folder .SAFE + folder Semarang
list.files("data", pattern = "\\.SAFE$")  # cek 2 folder citra


# ==============================================================
# 1. PENENTUAN REGION OF INTEREST (ROI)
# ==============================================================
# Area studi : Kabupaten Semarang, Provinsi Jawa Tengah
#
# Alasan pemilihan:
# - Memiliki variasi tutupan lahan yang lengkap dalam satu wilayah:
#   * Vegetasi  : hutan & perkebunan di lereng G. Ungaran dan
#                 G. Merbabu, serta sawah di dataran rendah
#   * Permukiman: kawasan perkotaan Ungaran, Ambarawa, Bawen
#   * Badan air : Danau Rawa Pening (danau alami yang besar)
#   * Awan      : sering muncul di sekitar puncak gunung
# - Terdiri dari 19 kecamatan dengan karakter beragam (perkotaan,
#   sekitar danau, pegunungan) sehingga hasil zonal statistics
#   menarik untuk dibandingkan
# - Luas wilayah sedang sehingga pemrosesan citra 10 m tetap ringan
# - Belum digunakan oleh kelompok lain
# ==============================================================

# --- 1.1 Lokasi data batas administrasi ---
gdb <- "data/Semarang/KAB SEMARANG/2022_RBI25K_KAB_SEMARANG_KUGI50_20221231.gdb"

# --- 1.2 Baca batas kecamatan & filter Kabupaten Semarang ---
kec <- st_read(gdb, layer = "ADMINISTRASI_AR_KECAMATAN") %>%
  filter(WADMKK == "Semarang") %>%      # Kabupaten Semarang,BUKAN Kota Semarang
  st_zm() %>%                           # buang dimensi Z/M bawaan geodatabase
  st_make_valid() %>%                   # perbaiki geometri yang tidak valid
  group_by(WADMKC) %>%                  # 1 baris per kecamatan
  summarise(.groups = "drop") %>%
  st_cast("MULTIPOLYGON") %>%           # samakan tipe geometri
  st_transform(32749)                   # UTM zona 49S

nrow(kec)        # jumlah kecamatan 
kec$WADMKC       # daftar nama kecamatan

# --- 1.3 Batas kabupaten  ---
kab <- kec %>%
  st_buffer(1) %>%      # tutup celah kecil antar kecamatan (1 m)
  st_union() %>%
  st_buffer(-1) %>%     # kembalikan ke ukuran semula
  st_as_sf()

# Luas wilayah
kec$luas_km2 <- round(as.numeric(st_area(kec)) / 1e6, 2)
sum(kec$luas_km2)   

# --- 1.4 Peta ROI ---
peta_roi <- ggplot() +
  geom_sf(data = kec, fill = "lightyellow", color = "grey40") +
  geom_sf(data = kab, fill = NA, color = "red", linewidth = 0.8) +
  geom_sf_text(data = kec, aes(label = WADMKC), size = 2.3) +
  coord_sf(datum = NA) +                       
  labs(title    = "Region of Interest: Kabupaten Semarang",
       subtitle = "Batas administrasi kecamatan") +
  theme_void() +                              
  theme(plot.background = element_rect(fill = "white", color = NA))

peta_roi
ggsave("output/peta_roi_semarang.png", peta_roi,
       width = 8, height = 8, dpi = 300)

# --- 1.5 Simpan batas administrasi ---
st_write(kec, "output/kecamatan_semarang.gpkg", delete_dsn = TRUE)
st_write(kab, "output/batas_kab_semarang.shp",  delete_dsn = TRUE)
st_write(kec, "output/batas_kec_semarang.shp",  delete_dsn = TRUE)

# ==============================================================
# 2. DATA SATELIT DAN DATA ADMINISTRASI
# ==============================================================

# --- 2.1 Citra Sentinel-2 Level-2A ---
# Sumber   : Copernicus Data Space Ecosystem (pengganti Copernicus
#            Open Access Hub), https://dataspace.copernicus.eu
# Produk   : S2B_MSIL2A_20261014T023639_N0513_R089
# Tile     : T49MDN (bagian utara) & T49MDM (bagian selatan)
# Tanggal  : 14 Oktober 2026
# Filter   : tutupan awan maksimal 10%
# Kab. Semarang terbagi di 2 tile, sehingga keduanya digunakan
# dan nanti digabung (mosaic) di tahap 3.

dir_mdn <- list.files("data", pattern = "T49MDN.*\\.SAFE$", full.names = TRUE)
dir_mdm <- list.files("data", pattern = "T49MDM.*\\.SAFE$", full.names = TRUE)
dir_mdn
dir_mdm

# Fungsi untuk mencari lokasi file band di dalam folder .SAFE
cari_band <- function(dir_safe, band, res) {
  f <- list.files(dir_safe,
                  pattern = paste0("_", band, "_", res, "\\.jp2$"),
                  recursive = TRUE, full.names = TRUE)
  if (length(f) != 1) stop("Band ", band, "_", res, " tidak ditemukan di ", dir_safe)
  f
}

# Band yang dibutuhkan untuk perhitungan indeks:
# - 10 m : B03 (hijau), B04 (merah), B08 (NIR)
# - 20 m : B05 (red edge), B11 (SWIR), SCL (scene classification)
band10 <- c("B03", "B04", "B08")
band20 <- c("B05", "B11", "SCL")

mdn10 <- rast(sapply(band10, function(b) cari_band(dir_mdn, b, "10m")))
mdm10 <- rast(sapply(band10, function(b) cari_band(dir_mdm, b, "10m")))
mdn20 <- rast(sapply(band20, function(b) cari_band(dir_mdn, b, "20m")))
mdm20 <- rast(sapply(band20, function(b) cari_band(dir_mdm, b, "20m")))

names(mdn10) <- names(mdm10) <- band10
names(mdn20) <- names(mdm20) <- band20

# Tetapkan sistem koordinat citra (EPSG:32749,WGS 84 / UTM 49S) karena PROJ 
utm49s <- st_crs(32749)$wkt
crs(mdn10) <- utm49s
crs(mdm10) <- utm49s
crs(mdn20) <- utm49s
crs(mdm20) <- utm49s
# Cek informasi citra (sistem koordinat, resolusi, jangkauan)
mdn10
mdn20
crs(mdn10, proj = TRUE)       
same.crs(mdn10, vect(kab))    

# Tampilan awal: false color (NIR-Merah-Hijau), vegetasi tampak merah
par(mfrow = c(1, 2))
plotRGB(mdn10, r = 3, g = 2, b = 1, stretch = "lin", main = "T49MDN")
plot(st_geometry(kab), add = TRUE, border = "yellow", lwd = 2)
plotRGB(mdm10, r = 3, g = 2, b = 1, stretch = "lin", main = "T49MDM")
plot(st_geometry(kab), add = TRUE, border = "yellow", lwd = 2)
par(mfrow = c(1, 1))

# --- 2.2 Batas Administrasi ---
# Sumber : Ina-Geoportal (BIG), RBI 25K Kab. Semarang (2022)
# Layer  : ADMINISTRASI_AR_KECAMATAN (sudah dibaca di Tahap 1)
# Objek  : kec (19 kecamatan) dan kab (batas kabupaten)

# ==============================================================
# 3. PEMOTONGAN CITRA (CROPPING)
# ==============================================================
# Citra dipotong menggunakan batas Kabupaten Semarang. Karena wilayah
# kabupaten berada di 2 tile, tiap tile dipotong dulu lalu digabung
# (mosaic), kemudian di-clip mengikuti batas kabupaten.

# --- 3.1 Batas administrasi dalam format terra ---
v_kab <- vect(kab)
v_kec <- vect(kec)
e_kab <- ext(buffer(v_kab, 200))   # kotak pembatas kabupaten + 200 m

# --- 3.2 Potong tiap tile & ubah nilai 0 menjadi NA ---
potong <- function(r) {
  r <- crop(r, e_kab)
  classify(r, cbind(0, NA))
}
mdn10_c <- potong(mdn10)
mdm10_c <- potong(mdm10)
mdn20_c <- potong(mdn20)
mdm20_c <- potong(mdm20)

# --- 3.3 Gabungkan 2 tile (mosaic) ---
s10 <- mosaic(mdn10_c, mdm10_c, fun = "first")
s20 <- mosaic(mdn20_c, mdm20_c, fun = "first")
names(s10) <- band10
names(s20) <- band20

# --- 3.4 Samakan resolusi band 20 m menjadi 10 m ---
b5  <- resample(s20$B05, s10, method = "bilinear")
b11 <- resample(s20$B11, s10, method = "bilinear")
scl <- resample(s20$SCL, s10, method = "near")   

s2 <- c(s10$B03, s10$B04, b5, s10$B08, b11)
names(s2) <- c("B3", "B4", "B5", "B8", "B11")

# --- 3.5 Clip sesuai batas Kabupaten Semarang ---
s2  <- crop(s2,  v_kab, mask = TRUE)
scl <- crop(scl, v_kab, mask = TRUE)

# --- 3.6 Konversi nilai digital (DN) ke reflektan ---
# Cek nilai offset & faktor skala di metadata produk
f_mtd <- list.files(dir_mdn, pattern = "MTD_MSIL2A\\.xml$",
                    recursive = TRUE, full.names = TRUE)
f_mtd   
grep("BOA_ADD_OFFSET|QUANTIFICATION_VALUE", readLines(f_mtd[1]), value = TRUE)

# Reflektan = (DN + offset) / 10000, dengan offset = -1000
s2 <- clamp((s2 - 1000) / 10000, lower = 0, upper = 1)

# --- 3.7 Cek hasil crop ---
s2          # cek dimensi, resolusi (10 m), CRS, nilai min-max (0-1)
names(s2)   # semua band untuk indeks harus ada: B3, B4, B5, B8, B11

plotRGB(s2, r = 4, g = 2, b = 1, stretch = "lin")   # false color B8-B4-B3
plot(v_kec, add = TRUE, border = "yellow")

# --- 3.8 Simpan hasil crop ---
writeRaster(s2,  "output/s2_crop_semarang.tif",  overwrite = TRUE)
writeRaster(scl, "output/scl_crop_semarang.tif", overwrite = TRUE)

# ==============================================================
# 4. KLASIFIKASI TUTUPAN LAHAN
# ==============================================================

# --- 4.1 Menghitung indeks spektral dari citra hasil crop ---
# NDVI (vegetasi)      = (NIR - Red) / (NIR + Red)          = (B8 - B4) / (B8 + B4)
# NDBI (terbangun)     = (SWIR - NIR) / (SWIR + NIR)        = (B11 - B8) / (B11 + B8)
# NDWI (perairan)      = (Green - NIR) / (Green + NIR)      = (B3 - B8) / (B3 + B8)
# NDCI (awan)          = (RedEdge - Red) / (RedEdge + Red)  = (B5 - B4) / (B5 + B4)
ndvi <- (s2$B8  - s2$B4) / (s2$B8  + s2$B4)
ndbi <- (s2$B11 - s2$B8) / (s2$B11 + s2$B8)
ndwi <- (s2$B3  - s2$B8) / (s2$B3  + s2$B8)
ndci <- (s2$B5  - s2$B4) / (s2$B5  + s2$B4)

idx <- c(ndvi, ndbi, ndwi, ndci)
names(idx) <- c("NDVI", "NDBI", "NDWI", "NDCI")

idx          # nilai min-max tiap indeks (rentang -1 sampai 1)
plot(idx)    

writeRaster(idx, "output/indeks_semarang.tif", overwrite = TRUE)

# --- 4.2 Bahan penentuan threshold ---
# (a) Histogram sebaran nilai tiap indeks
par(mfrow = c(2, 2))
for (n in names(idx)) {
  hist(idx[[n]], maxcell = 1e6, breaks = 100,
       main = n, xlab = "Nilai indeks", col = "grey70")
}
par(mfrow = c(1, 1))

# (b) Rata-rata nilai indeks & reflektan per kelas SCL (acuan objektif)
#     SCL: 4 = vegetasi, 5 = tidak bervegetasi, 6 = air, 7 = tidak terklasifikasi
acuan <- zonal(c(idx, s2$B3, s2$B11), scl, fun = "mean", na.rm = TRUE)
round(acuan, 3)

# --- 4.3 Penentuan threshold ---
# Berdasarkan rata-rata indeks per kelas SCL (tabel acuan) dan uji
# beberapa nilai threshold:
# - Air (SCL 6)       : NDWI = 0.189, vegetasi = -0.605,
#                       tidak bervegetasi = -0.384           -> NDWI > 0
# - Bangunan (SCL 5)  : NDBI = 0.089, vegetasi = -0.131      -> NDBI > 0
#                       NDBI saja juga menangkap ladang kering/lahan terbuka
#                       (bangunan menjadi 455 km2 atau 45% wilayah), sehingga
#                       ditambah syarat NDVI < 0.3 (NDVI rata-rata SCL 5 =
#                       0.314)                                -> +/- 207 km2
# - Vegetasi (SCL 4)  : NDVI = 0.640 (tertinggi)             -> sisa piksel
# - Awan              : SCL tidak mendeteksi awan. NDCI vegetasi justru
#                       tertinggi (0.285), sehingga awan dicirikan dari
#                       reflektan hijau sangat tinggi (B3 > 0.3) dan NDCI
#                       rendah (< 0.1). Uji B3 > 0.2 / 0.25 / 0.3 / 0.35
#                       menghasilkan 4.64 / 1.66 / 0.72 / 0.36 km2.
T_awan_ndci <- 0.1
T_awan_b3   <- 0.3
T_air       <- 0
T_bang      <- 0
T_bang_ndvi <- 0.3

# --- 4.4 Klasifikasi 4 kelas (peta komposit) ---
# Kode kelas: 1 = vegetasi, 2 = bangunan, 3 = perairan, 4 = awan
kelas <- ifel(ndci < T_awan_ndci & s2$B3 > T_awan_b3, 4,
              ifel(ndwi > T_air, 3,
                   ifel(ndbi > T_bang & ndvi < T_bang_ndvi, 2, 1)))
kelas <- as.int(mask(kelas, v_kab))
names(kelas) <- "kelas"

# Luas tiap kelas (1 piksel = 10 m x 10 m = 100 m2)
luas <- freq(kelas)
luas$kelas    <- c("Vegetasi", "Bangunan", "Perairan", "Awan")[luas$value]
luas$luas_km2 <- round(luas$count * 100 / 1e6, 2)
luas$persen   <- round(100 * luas$count / sum(luas$count), 2)
luas[, c("kelas", "luas_km2", "persen")]

# Cek kesesuaian dengan klasifikasi bawaan Sentinel-2 (SCL)
crosstab(c(kelas, scl))
warna <- c("#2E8B3E",   # 1 vegetasi : hijau
           "#D7301F",   # 2 bangunan : merah
           "#2171B5",   # 3 perairan : biru
           "#BDBDBD")   # 4 awan     : abu-abu
levels(kelas) <- data.frame(value = 1:4,
                            kelas = c("Vegetasi", "Bangunan", "Perairan", "Awan"))
coltab(kelas) <- data.frame(value = 1:4, col = warna)

# Peta komposit: 4 kelas + legenda, skala, panah utara, judul, sumber data
buat_peta <- function() {
  plot(kelas, axes = FALSE, mar = c(4, 1, 3, 8),
       main = "Peta Klasifikasi Tutupan Lahan Kabupaten Semarang",
       plg = list(title = "Kelas Tutupan Lahan"))         
  plot(v_kec, add = TRUE, border = "grey30", lwd = 0.5)  
  plot(v_kab, add = TRUE, border = "black",  lwd = 1.5)  
  sbar(10000, xy = "bottomleft", type = "bar", divs = 4,
       below = "m", labels = c("0", "5000", "10000"))           
  north(xy = "topright", type = 2)                         
  mtext(paste("Sumber: Citra Sentinel-2 L2A 14 Oktober 2026 (ESA Copernicus);",
              "Batas administrasi RBI 25K (Ina-Geoportal, BIG)"),
        side = 1, line = 2, adj = 0, cex = 0.7)           
}

buat_peta()

png("output/peta_komposit_klasifikasi.png", width = 10, height = 9,
    units = "in", res = 300)
buat_peta()
dev.off()

writeRaster(kelas, "output/klasifikasi_semarang.tif",
            datatype = "INT1U", overwrite = TRUE)
write.csv(luas[, c("kelas", "luas_km2", "persen")],
          "output/luas_kelas_semarang.csv", row.names = FALSE)

# ==============================================================
# 5. ZONAL STATISTICS PER KECAMATAN
# ==============================================================
# Statistik (mean, min, max, median, sd) dihitung untuk NDVI, NDBI,
# NDWI, dan NDCI pada setiap kecamatan (batas kecamatan Ina-Geoportal).

# --- 5.1 Raster zona kecamatan (sama grid dengan citra) ---
zona <- rasterize(v_kec, idx, field = "WADMKC")

# --- 5.2 Hitung mean, min, max, sd ---
hasil <- data.frame(Kecamatan = v_kec$WADMKC)
for (s in c("mean", "min", "max", "sd")) {
  z <- zonal(idx, zona, fun = s, na.rm = TRUE)
  names(z) <- c("Kecamatan", paste0(names(idx), "_", s))
  hasil <- merge(hasil, z, by = "Kecamatan")
}

# --- 5.3 Hitung median ---
med <- extract(idx, v_kec, fun = median, na.rm = TRUE, ID = FALSE)
names(med) <- paste0(names(idx), "_median")
med$Kecamatan <- v_kec$WADMKC
hasil <- merge(hasil, med, by = "Kecamatan")

# --- 5.4 urutan kolom per indeks (mean, min, max, median, sd) ---
stat <- c("mean", "min", "max", "median", "sd")
urut <- c("Kecamatan", paste0(rep(names(idx), each = 5), "_", rep(stat, 4)))
hasil <- hasil[, urut]
hasil[, -1] <- round(hasil[, -1], 4)

nrow(hasil)   
hasil

write.csv(hasil, "output/zonal_stats_semarang.csv", row.names = FALSE)

# Gabungkan ke batas kecamatan (dipakai di script Leaflet)
kec_zs <- merge(kec, hasil, by.x = "WADMKC", by.y = "Kecamatan")
st_write(kec_zs, "output/kecamatan_zonal_semarang.gpkg", delete_dsn = TRUE)

# --- 6.2 Tabel Zonal Statistics ---
# Tabel lengkap (21 kolom) dipecah per indeks agar rapi saat dicetak:
# Kecamatan | Mean | Min | Max | Median | Std. Deviasi
for (i in names(idx)) {
  tb <- hasil[, c("Kecamatan", paste0(i, "_", stat))]
  names(tb) <- c("Kecamatan", "Mean", "Min", "Max", "Median", "Std. Deviasi")
  print(tb)
  write.csv(tb, paste0("output/tabel_zonal_", i, ".csv"), row.names = FALSE)
}

# Tabel luas tiap kelas tutupan lahan
luas[, c("kelas", "luas_km2", "persen")]

# ==============================================================
# 6. LAPORAN HASIL (PRINTOUT)
# ==============================================================

# --- 6.1 Ekspor data peta untuk layout di QGIS ---
# Raster klasifikasi diperkecil ke 30 m (nilai modus) agar ringan di QGIS
k <- kelas
levels(k) <- NULL                                  
k30 <- aggregate(k, fact = 3, fun = "modal", na.rm = TRUE)

# Raster klasifikasi 
writeRaster(k30, "output/peta_klasifikasi.tif",
            datatype = "INT1U", overwrite = TRUE)

writeLines(c(
  "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>",
  "<qgis version=\"3.28\">",
  "  <pipe>",
  "    <rasterrenderer type=\"paletted\" band=\"1\" opacity=\"1\" alphaBand=\"-1\">",
  "      <colorPalette>",
  "        <paletteEntry value=\"1\" color=\"#2e8b3e\" label=\"Vegetasi\" alpha=\"255\"/>",
  "        <paletteEntry value=\"2\" color=\"#d7301f\" label=\"Bangunan\" alpha=\"255\"/>",
  "        <paletteEntry value=\"3\" color=\"#2171b5\" label=\"Perairan\" alpha=\"255\"/>",
  "        <paletteEntry value=\"4\" color=\"#bdbdbd\" label=\"Awan\" alpha=\"255\"/>",
  "      </colorPalette>",
  "    </rasterrenderer>",
  "  </pipe>",
  "</qgis>"),
  "output/peta_klasifikasi.qml")

# Batas kecamatan & kabupaten
st_write(kec, "output/peta_batas_kecamatan.shp", delete_dsn = TRUE)
st_write(kab, "output/peta_batas_kabupaten.shp", delete_dsn = TRUE)

# Cek hasil
crs(rast("output/peta_klasifikasi.tif"), proj = TRUE)   
list.files("output", pattern = "peta")