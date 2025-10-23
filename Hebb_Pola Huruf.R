library(shiny)

# Fungsi aktivasi: sign function
sign_func <- function(x) {
  ifelse(x >= 0, 1, -1)
}

# Pola input (5x5 -> vektor 25 elemen) untuk angka 4 dan 5
x_4 <- c(
  -1,  1, -1,  1, -1,
  -1,  1, -1,  1, -1,
  -1,  1,  1,  1, -1,
  -1, -1, -1,  1, -1,
  -1, -1, -1,  1, -1
)

x_5 <- c(
  -1,  1,  1,  1, -1,
  -1,  1, -1, -1, -1,
  -1,  1,  1,  1, -1,
  -1, -1, -1,  1, -1,
  -1,  1,  1,  1, -1
)

# Target output
t_4 <- 1   # angka 4
t_5 <- -1  # bukan 4 (angka 5)

# Hitung bobot menggunakan aturan Hebb
w_4 <- x_4 * t_4
w_5 <- x_5 * t_5

# Bobot akhir adalah penjumlahan semua kontribusi
W <- w_4 + w_5

# Tampilkan bobot akhir
cat("Bobot akhir (W):\n")
print(matrix(W, nrow = 5, ncol = 5, byrow = TRUE))

# Fungsi recall
recall <- function(input, weights) {
  output <- sum(input * weights)
  return(sign_func(output))
}

# Uji pola 4 dan 5
output_4 <- recall(x_4, W)
output_5 <- recall(x_5, W)

cat("\nHasil recall:\n")
cat("Pola 4 ->", output_4, ifelse(output_4 == 1, "(Benar)", "(Salah)"), "\n")
cat("Pola 5 ->", output_5, ifelse(output_5 == -1, "(Benar)", "(Salah)"), "\n")

# --- Bobot hasil pelatihan untuk aplikasi Shiny
hebb_weights <- W

# UI -----
ui <- fluidPage(
  titlePanel("Hebbian Learning: Pengenalan Angka 4"),
  
  h4("Klik kotak untuk mengisi pola angka (5x5)"),
  
  # === Matriks 5x5 ===
  fluidRow(
    column(6, align = "center",
           tags$table(
             style = "border-collapse: collapse; margin: 0 auto;",
             lapply(0:4, function(i) {
               tags$tr(
                 lapply(1:5, function(j) {
                   idx <- i * 5 + j
                   tags$td(
                     style = "border: 1px solid #ccc; width: 40px; height: 40px;",
                     checkboxInput(inputId = paste0("cell", idx), label = NULL, value = FALSE)
                   )
                 })
               )
             })
           )
    )
  ),
  
  br(),
  fluidRow(
    column(6, align = "center",
           actionButton("check", "Cek Hasil"),
           br(), br(),
           h4("Hasil:"),
           verbatimTextOutput("result"),
           h4("Vector Input (25 elemen):"),
           verbatimTextOutput("inputVec"),
           h4("Visualisasi Pola:"),
           verbatimTextOutput("visualization"),
           h4("Bobot Hebb (5x5):"),
           verbatimTextOutput("weights")
    )
  ),
  
  # Contoh pola referensi
  fluidRow(
    column(6,
           h4("Contoh Pola:"),
           tags$div(
             style = "display: flex; justify-content: space-around;",
             tags$div(
               h5("Angka 4"),
               tags$pre("░ █ ░ █ ░\n░ █ ░ █ ░\n░ █ █ █ ░\n░ ░ ░ █ ░\n░ ░ ░ █ ░")
             ),
             tags$div(
               h5("Angka 5"),
               tags$pre("░ █ █ █ ░\n░ █ ░ ░ ░\n░ █ █ █ ░\n░ ░ ░ █ ░\n░ █ █ █ ░")
             )
           )
    )
  )
)

# SERVER -----
server <- function(input, output) {
  
  observeEvent(input$check, {
    # Ambil input dari 25 kotak
    input_vector <- sapply(1:25, function(i) {
      if (input[[paste0("cell", i)]]) 1 else -1
    })
    
    # Hitung output dari Hebbian
    hasil <- recall(input_vector, hebb_weights)
    
    # Interpretasi hasil
    label <- if (hasil == 1) "Pola dikenali sebagai angka 4 ✓" else "Pola BUKAN angka 4 (kemungkinan angka 5) ✓"
    
    # Buat visualisasi pola
    vis_matrix <- matrix(input_vector, nrow = 5, ncol = 5, byrow = TRUE)
    visualization <- apply(vis_matrix, 1, function(row) {
      paste(ifelse(row == 1, "█", "░"), collapse = " ")
    })
    
    # Format bobot sebagai matriks 5x5
    weight_matrix <- matrix(hebb_weights, nrow = 5, ncol = 5, byrow = TRUE)
    
    # Output ke UI
    output$result <- renderText({
      paste("Output:", hasil, "-", label)
    })
    
    output$inputVec <- renderText({
      paste("Input:", paste(input_vector, collapse = ", "))
    })
    
    output$visualization <- renderText({
      paste(visualization, collapse = "\n")
    })
    
    output$weights <- renderText({
      paste(apply(weight_matrix, 1, function(row) paste(sprintf("%2d", row), collapse = " ")), collapse = "\n")
    })
  })
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)