# Global
library(rlang)
library(shiny)
library(dplyr)
library(rio)
library(sf)
library(leaflet)
library(leaflet.minicharts)
library(leaflet.extras)
library(googledrive)
library(tablerDash)
library(highcharter)

## Load data from Google Spreadsheet
# drive_auth(
#   path = "data/sardjito-1579003820387-f895623110c5.json"
# )

#drive_download(as_id("1rHnGVp6lg5QnjRY6OwNBhEQrPwT95_uSXCLPjJI0x5Q"), path = "data/data_filantropi.xlsx", overwrite = T)
data_filantropi <- import("data/data_filantropi1.xlsx")

# Load map polygon
provcoord <- readRDS("data/geocodes1.rds")
provmap <- readRDS("data/provmap.rds")


# UI
ui <- tablerDashPage(
  navbar = NULL,
  title = "Dashboard Filantropi Kesehatan",
  body = tablerDashBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "sdg_goal",
        width = "100%",
        label = "SDG",
        choices = c("Poin 1: Tanpa Kemiskinan",
                    "Poin 2: Tanpa Kelaparan",
                    "Poin 3: Kehidupan Sehat dan Sejahtera",
                    "Poin 4: Pendidikan Berkualitas",
                    "Poin 5: Kesetaraan Gender",
                    "Poin 6: Air Bersih dan Sanitasi Layak",
                    "Poin 7: Energi Bersih dan Terjangkau",
                    "Poin 8: Pekerjaan Layak dan Pertumbuhan Ekonomi",
                    "Poin 9: Industri, Inovasi dan Infrastruktur",
                    "Poin 10: Berkurangnya Kesenjangan",
                    "Poin 11: Kota dan Pemukiman yang Berkelanjutan",
                    "Poin 12: Konsumsi dan Produksi yang Bertanggung Jawab",
                    "Poin 13: Penanganan Perubahan Alam",
                    "Poin 14: Ekosistem Lautan",
                    "Poin 15: Ekosistem Daratan",
                    "Poin 16: Perdamaian, Keadilan dan Kelembagaan yang Tangguh",
                    "Poin 17: Kemitraan untuk Mencapai Tujuan"),
        multiple = FALSE
      )
    ),
    column(
      width = 6,
      selectInput(
        inputId = "provinsi",
        width = "100%",
        label = "Lokasi operasi",
        choices = c("Nasional", as.character(provmap$Provinsi)),
        multiple = FALSE
      )
    )
  ),
  fluidRow(
    leafletOutput("map", height = 600)
  ),
  fluidRow(
    tablerCard(
      title = "Jumlah donasi",
      width = 6,
      closable = FALSE,
      collapsed = FALSE,
      h4(textOutput("pilihansdg")),
      h2("Rp100000000")
    ),
    tablerCard(
      title = "Jumlah lembaga",
      width = 6,
      closable = FALSE,
      collapsed = FALSE,
      highchartOutput("chartSDG", height = "300px")
    )
  ),
))


# Server
server <- function(input, output, session) {
  mapdata <- reactive({
    dat <- data_filantropi %>%
      filter(SDG_goal == input$sdg_goal)
    if (input$provinsi != "Nasional") {
      dat <- dat %>%
        filter(Lokasi == input$provinsi)
    }
    dat <- dat %>%
      group_by(Lokasi) %>%
      summarise(Donasi = sum(Donasi), NamaLembaga = paste0(NamaLembaga, collapse = ", "))
    
    dat <- inner_join(
      provcoord, dat, by = c("Provinsi" = "Lokasi")
    )
    dat
  })
  
  data_list <- reactiveValues()
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = FALSE,
      scrollWheelZoom = FALSE,
      maxZoom = 12
    )) %>%
      htmlwidgets::onRender("function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }") %>% 
      setMapWidgetStyle(list(background = "transparent")) %>%
      setView(
        lng = 117.1932286,
        lat = -2.252581,
        zoom = 5
      ) %>%
      addPolygons(
        data = provcoord,
        fillColor = "gray",
        color = "white",
        opacity = 1,
        weight = 1
      )
  })
  
  observe({
    input$provinsi 
    input$sdg_goal
    leafletProxy("map", session) %>%
      clearMarkers()  %>%
        addCircleMarkers(
          lng = mapdata()$Longitude,
          lat = mapdata()$Latitude,
          label = paste(mapdata()$NamaLembaga,"-",mapdata()$Provinsi),
          radius = mapdata()$Donasi/100000000,
        )
  })
  
  output$pilihansdg <- renderText({
    pilihansdg <- input$sdg_goal
  })
  
  output$jumlah_donasi <- renderText({
    jml <- data_filantropi %>%
      group_by(SDG_goal)
    jumlah <- aggregate(Donasi ~ SDG_goal, jml, sum)
    
  })
    
  output$chartSDG <- renderHighchart({
    chartdat <- data_filantropi %>%
      group_by(SDG_goal) %>%
      summarise(`Jumlah Lembaga` = n())
    
    hchart(chartdat, "column", hcaes(x = SDG_goal, y = `Jumlah Lembaga`))
  })
  
}

shinyApp(ui, server)
