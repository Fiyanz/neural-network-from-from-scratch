# Fungsi aktivasi: sign function
sign_func <- function(x) {
  ifelse(x >= 0, 1, -1)
}

# Pola input (3x3 -> vektor 9 elemen)
# T dan U berdasarkan deskripsi awal
x_T <- c(1, 1, 1,
         -1, 1, -1,
         -1, 1, -1)

x_U <- c(1, -1, 1,
         1, -1, 1,
         1, 1, 1)

# Target output
t_T <- 1   # huruf T
t_U <- -1  # bukan T

# Hitung bobot
# ??w = x_i * target
w_T <- x_T * t_T
w_U <- x_U * t_U

# Bobot akhir adalah penjumlahan semua kontribusi
W <- w_T + w_U

# Tampilkan bobot akhir
cat("Bobot akhir (W):\n")
print(W)

# Fungsi recall
recall <- function(input, weights) {
  output <- sum(input * weights)
  return(sign_func(output))
}

# Uji pola T dan U
output_T <- recall(x_T, W)
output_U <- recall(x_U, W)

cat("\nHasil recall:\n")
cat("Pola T ???", output_T, ifelse(output_T == 1, "(Benar)", "(Salah)"), "\n")
cat("Pola U ???", output_U, ifelse(output_U == -1, "(Benar)", "(Salah)"), "\n")


library(shiny)

# --- Bobot hasil pelatihan
hebb_weights <- W

# Fungsi aktivasi sign
sign_func <- function(x) {
  ifelse(x >= 0, 1, -1)
}

# Fungsi recall
recall <- function(input, weights) {
  output <- sum(input * weights)
  return(sign_func(output))
}

# UI -----
ui <- fluidPage(
  titlePanel("Hebbian Learning: Pengenalan Huruf T"),
  
  h4("Klik kotak untuk mengisi pola huruf (3x3)"),
  
  # === Matriks 3x3 ===
  fluidRow(
    column(4, align = "center",
           tags$table(
             lapply(0:2, function(i) {
               tags$tr(
                 lapply(1:3, function(j) {
                   idx <- i * 3 + j
                   tags$td(
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
    column(4, align = "center",
           actionButton("check", "Cek Hasil"),
           br(), br(),
           h4("Hasil:"),
           verbatimTextOutput("result"),
           h4("Vector Input:"),
           verbatimTextOutput("inputVec"),
           h4("Bobot Hebb:"),
           verbatimTextOutput("weights")
    )
  )
)

# SERVER -----
server <- function(input, output) {
  
  observeEvent(input$check, {
    # Ambil input dari 9 kotak
    input_vector <- sapply(1:9, function(i) {
      if (input[[paste0("cell", i)]]) 1 else -1
    })
    
    # Hitung output dari Hebbian
    hasil <- recall(input_vector, hebb_weights)
    
    # Interpretasi hasil
    label <- if (hasil == 1) "Pola dikenali sebagai huruf T ???" else "Pola BUKAN huruf T ???"
    
    # Output ke UI
    output$result <- renderText({
      paste("Output:", hasil, "-", label)
    })
    
    output$inputVec <- renderText({
      paste("Input:", paste(input_vector, collapse = ", "))
    })
    
    output$weights <- renderText({
      paste("Bobot:", paste(hebb_weights, collapse = ", "))
    })
  })
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)
