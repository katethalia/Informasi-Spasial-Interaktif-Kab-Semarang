# ==============================================================
# TUGAS BESAR PEMROGRAMAN SPASIAL A
# Script 2: Peta Interaktif Tutupan Lahan (Shiny Dashboard + Leaflet)
# Studi Kasus: Kabupaten Semarang, Jawa Tengah
# --------------------------------------------------------------
# Anggota 1 : Icha Ramanda Putri (5016241119)
# Anggota 2 : Katerine Thalia Syahiranadya (5016241126)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Input (hasil Script 1, folder output):
# - peta_polos_klasifikasi.tif     : raster klasifikasi 30 m
# - klasifikasi_semarang.tif       : raster klasifikasi 10 m
# - indeks_semarang.tif            : raster NDVI, NDBI, NDWI, NDCI
# - kecamatan_zonal_semarang.gpkg  : batas kecamatan + zonal statistics
# - batas_kab_semarang.shp         : batas kabupaten
# - luas_kelas_semarang.csv        : luas tiap kelas
# - zonal_stats_semarang.csv       : tabel zonal statistics
# ==============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)      # pengganti rgdal & sp (rgdal sudah tidak tersedia di CRAN)
library(terra)   # pengganti raster


# ==============================================================
# 1. MENYIAPKAN DATA
# ==============================================================

# --- 1.1 Kelas tutupan lahan, indeks, dan warna ---
nama_kelas   <- c("Vegetasi", "Bangunan", "Perairan", "Awan")   # kode 1-4
warna        <- c("#2E8B3E", "#D7301F", "#2171B5", "#BDBDBD")
indeks       <- c("NDVI", "NDBI", "NDWI", "NDCI")
palet        <- c(NDVI = "Greens", NDBI = "Reds", NDWI = "Blues", NDCI = "Purples")
warna_indeks <- c(NDVI = "#2E8B3E", NDBI = "#D7301F", NDWI = "#2171B5", NDCI = "#6A51A3")
stat         <- c("mean", "min", "max", "median", "sd")

# --- 1.2 Batas administrasi (diproyeksikan ke WGS 84 untuk Leaflet) ---
kec_utm <- st_read("output/kecamatan_zonal_semarang.gpkg", quiet = TRUE)   # UTM 49S
kec_ll  <- st_transform(kec_utm, 4326)
kab_ll  <- st_transform(st_read("output/batas_kab_semarang.shp", quiet = TRUE), 4326)

# (BARU) Titik label nama kecamatan (titik yang pasti berada di dalam tiap kecamatan)
label_kec <- st_transform(st_point_on_surface(kec_utm), 4326)

# (BARU) Gaya tulisan nama kecamatan
gaya_label <- labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                           style = list("font-weight" = "bold",
                                        "font-size"   = "11px",
                                        "color"       = "black",
                                        "text-shadow" = "1px 1px 2px white"))

# --- 1.3 Klasifikasi tutupan lahan dalam bentuk poligon ---
# Raster diubah menjadi poligon per kelas lalu diproyeksikan dengan sf,
# karena fungsi proyeksi pada terra bentrok dengan instalasi PostGIS.
# Proses ini hanya berjalan sekali; hasilnya disimpan di folder output.
f_poly <- "output/klasifikasi_poligon_wgs84.gpkg"
if (!file.exists(f_poly)) {
  k  <- rast("output/peta_polos_klasifikasi.tif")
  pk <- as.polygons(k, dissolve = TRUE)        # 1 poligon per kelas
  names(pk) <- "kode"
  pk <- st_as_sf(pk)
  pk <- st_simplify(pk, dTolerance = 15)       # perhalus agar ringan
  pk <- st_transform(pk, 4326)
  st_write(pk, f_poly, quiet = TRUE)
}
kls_poly <- st_read(f_poly, quiet = TRUE)

# --- 1.4 Tabel hasil script 1 ---
luas  <- read.csv("output/luas_kelas_semarang.csv")
zonal <- read.csv("output/zonal_stats_semarang.csv")

# --- 1.5 Luas tiap kelas tutupan lahan per kecamatan ---
# Dihitung dari raster klasifikasi 10 m (1 piksel = 100 m2).
# Hanya dihitung sekali; hasilnya disimpan di folder output.
f_luas_kec <- "output/luas_kelas_per_kecamatan.csv"
if (!file.exists(f_luas_kec)) {
  kr <- rast("output/klasifikasi_semarang.tif")
  kec_utm$id <- seq_len(nrow(kec_utm))
  zona <- rasterize(vect(kec_utm), kr, field = "id")
  v  <- na.omit(cbind(values(zona, mat = FALSE), values(kr, mat = FALSE)))
  tb <- table(factor(v[, 1], levels = kec_utm$id), factor(v[, 2], levels = 1:4))
  km2 <- matrix(as.numeric(tb) * 100 / 1e6, nrow = nrow(tb))
  luas_kec <- data.frame(Kecamatan = kec_utm$WADMKC, round(km2, 2))
  names(luas_kec)[2:5] <- nama_kelas
  luas_kec$Total <- rowSums(luas_kec[, 2:5])
  write.csv(luas_kec, f_luas_kec, row.names = FALSE)
}
luas_kec <- read.csv(f_luas_kec, check.names = FALSE)

# --- 1.6 Sampel nilai indeks untuk histogram ---
# Diambil 100.000 piksel acak (cukup mewakili, lebih ringan daripada semua piksel).
f_sampel <- "output/sampel_indeks.csv"
if (!file.exists(f_sampel)) {
  idx_r <- rast("output/indeks_semarang.tif")
  names(idx_r) <- indeks
  sampel <- spatSample(idx_r, size = 100000, method = "random", na.rm = TRUE)
  write.csv(sampel, f_sampel, row.names = FALSE)
}
sampel <- read.csv(f_sampel)


# ==============================================================
# 2. TAMPILAN DASHBOARD (UI)
# ==============================================================
ui <- dashboardPage(
  title = "Informasi Spasial Interaktif Kabupaten Semarang",   # judul tab browser
  
  # (BARU) Judul dua baris
  dashboardHeader(
    title = tags$div(
      tags$span("Informasi Spasial Interaktif", class = "judul-utama"),
      tags$span("Kabupaten Semarang", class = "judul-sub")
    ),
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320,                                               # (BARU) samakan lebar
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps",
        icon = icon("globe"),
        startExpanded = TRUE,
        menuSubItem("Klasifikasi Tutupan Lahan", tabName = "m_lahan",   # (BARU)
                    icon = icon("map"), selected = TRUE),
        menuSubItem("Zonal Indeks per Kecamatan", tabName = "m_indeks",
                    icon = icon("layer-group")),
        menuSubItem("Statistik per Kecamatan", tabName = "m_stat",
                    icon = icon("table")),
        menuSubItem("Luas Kelas per Kecamatan", tabName = "m_luas",
                    icon = icon("chart-bar"))
      )
    )
  ),
  
  dashboardBody(
    tags$style(type = "text/css", "
      #peta_lahan, #peta_indeks {height: calc(100vh - 80px) !important;}

      /* (BARU) Tata letak judul dua baris di header */
      .main-header .logo {line-height: normal !important;
                          text-align: left; padding-top: 7px;}
      .judul-utama {display: block; font-size: 17px; font-weight: bold;}
      .judul-sub   {display: block; font-size: 13px; margin-top: 2px;}
    "),
    tabItems(
      # --- Tab 1: Peta klasifikasi tutupan lahan ---
      tabItem(tabName = "m_lahan",
              leafletOutput("peta_lahan", height = 700)),
      
      # --- Tab 2: Peta zonal indeks per kecamatan ---
      tabItem(tabName = "m_indeks",
              leafletOutput("peta_indeks", height = 700)),
      
      # --- Tab 3: Statistik indeks ---
      tabItem(tabName = "m_stat",
              fluidRow(
                box(title = "Luas Tiap Kelas Tutupan Lahan", width = 4,
                    status = "primary", solidHeader = TRUE,
                    tableOutput("tabel_luas")),
                box(title = "Zonal Statistics per Kecamatan", width = 8,
                    status = "primary", solidHeader = TRUE,
                    selectInput("pilih_indeks", "Pilih indeks:", choices = indeks),
                    tableOutput("tabel_zonal"))
              ),
              fluidRow(
                box(title = "Histogram Nilai Indeks", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotOutput("histogram")),
                box(title = "Rata-rata Indeks per Kecamatan", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotOutput("grafik_kec"))
              )),
      
      # --- Tab 4: Luas kelas per kecamatan ---
      tabItem(tabName = "m_luas",
              fluidRow(
                box(title = "Luas Kelas Tutupan Lahan per Kecamatan", width = 12,
                    status = "primary", solidHeader = TRUE,
                    tableOutput("tabel_luas_kec"))
              ),
              fluidRow(
                box(title = "Komposisi Tutupan Lahan per Kecamatan (%)", width = 12,
                    status = "primary", solidHeader = TRUE,
                    plotOutput("grafik_komposisi", height = 550))
              ))
    )
  )
)

# -------------------------------------------------------------------------



# ==============================================================
# 3. ISI DASHBOARD (SERVER)
# ==============================================================
server <- function(input, output, session) {
  
  # ---------- Tab 1: Peta klasifikasi tutupan lahan ----------
  output$peta_lahan <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Citra Satelit")
    
    # 1 layer per kelas tutupan lahan
    for (i in 1:4) {
      m <- m %>%
        addPolygons(data = kls_poly[kls_poly$kode == i, ],
                    fillColor = warna[i], fillOpacity = 0.8, stroke = FALSE,
                    group = nama_kelas[i])
    }
    
    m %>%
      addPolygons(data = kec_ll, color = "black", weight = 1, fill = FALSE,
                  group = "Batas Kecamatan") %>%
      addPolygons(data = kab_ll, color = "black", weight = 3, fill = FALSE,
                  group = "Batas Kabupaten") %>%
      # (BARU) Nama kecamatan
      addLabelOnlyMarkers(data = label_kec, label = ~WADMKC,
                          labelOptions = gaya_label,
                          group = "Nama Kecamatan") %>%
      addLegend(position = "bottomright", colors = warna,
                labels = paste0(nama_kelas, " (", luas$persen, "%)"),
                title = "Kelas Tutupan Lahan", opacity = 1) %>%
      addLayersControl(
        baseGroups    = c("OpenStreetMap", "Citra Satelit"),
        overlayGroups = c("Batas Kabupaten", "Batas Kecamatan",
                          "Nama Kecamatan", nama_kelas),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addResetMapButton()
  })
  
  # ---------- Tab 2: Peta zonal indeks per kecamatan ----------
  output$peta_indeks <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Citra Satelit")
    
    # 1 layer per indeks (warna = nilai rata-rata kecamatan)
    for (ix in indeks) {
      nilai <- kec_ll[[paste0(ix, "_mean")]]
      pal   <- colorNumeric(palet[ix], nilai)
      isi_popup <- paste0(
        "<b>Kecamatan ", kec_ll$WADMKC, "</b><br>",
        "<b>", ix, "</b><br>",
        "Mean: ",   round(nilai, 3), "<br>",
        "Median: ", round(kec_ll[[paste0(ix, "_median")]], 3), "<br>",
        "Min: ",    round(kec_ll[[paste0(ix, "_min")]], 3), "<br>",
        "Max: ",    round(kec_ll[[paste0(ix, "_max")]], 3), "<br>",
        "SD: ",     round(kec_ll[[paste0(ix, "_sd")]], 3)
      )
      m <- m %>%
        addPolygons(data = kec_ll, fillColor = pal(nilai), fillOpacity = 0.75,
                    color = "white", weight = 1,
                    popup = isi_popup,
                    label = paste0(kec_ll$WADMKC, " | ", ix, ": ", round(nilai, 3)),
                    group = ix) %>%
        addLegend(position = "bottomright", pal = pal, values = nilai,
                  title = paste(ix, "(rata-rata)"), opacity = 1, group = ix)
    }
    
    m %>%
      addPolygons(data = kec_ll, color = "black", weight = 1, fill = FALSE,
                  group = "Batas Kecamatan") %>%
      addPolygons(data = kab_ll, color = "black", weight = 3, fill = FALSE,
                  group = "Batas Kabupaten") %>%
      # (BARU) Nama kecamatan
      addLabelOnlyMarkers(data = label_kec, label = ~WADMKC,
                          labelOptions = gaya_label,
                          group = "Nama Kecamatan") %>%
      addLayersControl(
        baseGroups    = c("OpenStreetMap", "Citra Satelit"),
        overlayGroups = c("Batas Kabupaten", "Batas Kecamatan",
                          "Nama Kecamatan", indeks),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("NDBI", "NDWI", "NDCI")) %>%   # awal dibuka: hanya NDVI
      addResetMapButton()
  })
  
  # ---------- Tab 3: Statistik indeks ----------
  output$tabel_luas <- renderTable({
    data.frame("Kelas"      = luas$kelas,
               "Luas (km2)" = luas$luas_km2,
               "Persen (%)" = luas$persen,
               check.names = FALSE)
  }, digits = 2)
  
  output$tabel_zonal <- renderTable({
    ix <- input$pilih_indeks
    tb <- zonal[, c("Kecamatan", paste0(ix, "_", stat))]
    names(tb) <- c("Kecamatan", "Mean", "Min", "Max", "Median", "Std. Deviasi")
    tb
  }, digits = 4, striped = TRUE)
  
  output$histogram <- renderPlot({
    ix <- input$pilih_indeks
    hist(sampel[[ix]], breaks = 100, col = warna_indeks[ix], border = NA,
         main = paste("Histogram", ix, "Kabupaten Semarang"),
         xlab = paste("Nilai", ix), ylab = "Frekuensi")
  })
  
  output$grafik_kec <- renderPlot({
    ix <- input$pilih_indeks
    v  <- zonal[[paste0(ix, "_mean")]]
    o  <- order(v)
    par(mar = c(4, 9, 2, 1))
    barplot(v[o], names.arg = zonal$Kecamatan[o], horiz = TRUE, las = 1,
            col = warna_indeks[ix], cex.names = 0.8,
            main = paste("Rata-rata", ix, "per Kecamatan"),
            xlab = paste("Rata-rata", ix))
  })
  
  # ---------- Tab 4: Luas kelas per kecamatan ----------
  output$tabel_luas_kec <- renderTable({
    km2 <- luas_kec[, nama_kelas]
    pct <- round(100 * km2 / luas_kec$Total, 2)
    names(km2) <- paste(nama_kelas, "(km2)")
    names(pct) <- paste(nama_kelas, "(%)")
    cbind(Kecamatan = luas_kec$Kecamatan, km2,
          "Total (km2)" = luas_kec$Total, pct)
  }, digits = 2, striped = TRUE)
  
  output$grafik_komposisi <- renderPlot({
    pct <- 100 * as.matrix(luas_kec[, nama_kelas]) / luas_kec$Total
    rownames(pct) <- luas_kec$Kecamatan
    pct <- pct[order(pct[, "Vegetasi"]), ]        # urut dari vegetasi terkecil
    par(mar = c(4, 9, 2, 9), xpd = TRUE)
    barplot(t(pct), horiz = TRUE, las = 1, col = warna, border = NA,
            cex.names = 0.8, xlab = "Persentase luas (%)",
            main = "Komposisi Tutupan Lahan per Kecamatan")
    legend("right", inset = c(-0.2, 0), legend = nama_kelas,
           fill = warna, bty = "n")
  })
}

shinyApp(ui, server)