rm(list = ls())
# Langkah Input dataset
x1 = c(1.5, 1.2, 1.0, 0.4, 0.5)
x2 = c(0.5, 0.6, 0.7, 0.8, 1.0)
y = c(1, 1, 1, -1, -1)
data = data.frame (x1,x2,y)

# Langkah Pemodelan JST Perceptron
# Inisialisasi bobot dan bias
weights <- c(0, 0)
bias <- 0  
learning_rate <- 1
theta <- 0.2
epochs <- 10

# Fungsi Aktivasi (Step Function)
activation_function <- function(net_input, theta) {
  if (net_input > theta) {
    return(1) 
  } else if (net_input >= -theta && net_input <= theta) {
    return(0)  
  } else {
    return(-1) 
  }
}

# Proses pelatihan perceptron
for (epoch in 1:epochs) {
  cat("Epoch:", epoch, "\n")
  total_error <- 0  # Inisialisasi total error untuk setiap epoch
  
  for (i in 1:nrow(data)) {
    # Hitung output perceptron
    input <- as.numeric(data[i, 1:2])
    target <- data$y[i]
    
    # Perhitungan output linear
    net_input <- sum(input * weights) + bias
    output <- activation_function(net_input, theta)  # Output dari fungsi aktivasi
    
    # Hanya lakukan pembaruan bobot dan bias jika output tidak sesuai dengan target
    if (output != target) {
      # Update bobot dan bias jika output tidak sesuai dengan target
      error <- target - output
      weights <- weights + (learning_rate * target * input)  # Update bobot
      bias <- bias + (learning_rate * target)  # Update bias
      total_error <- total_error + 1  # Tambahkan error untuk menghitung total error di akhir epoch
    }
    
    cat(paste("Input:", paste(input, collapse=" "), 
              "| Target:", target, 
              "| Output:", output, 
              "| Error:", error, 
              "| Bobot:", paste(weights, collapse=" "), 
              "| Bias:", bias), "\n")
    }
  
  # Jika tidak ada error pada epoch ini, kita bisa berhenti lebih awal (opsional)
  if (total_error == 0) {
    cat("Semua output sudah benar pada epoch:", epoch, "\n")
    break
  }
}

cat("\nBobot hasil pelatihan:", weights, "\n")
cat("Bias hasil pelatihan :", bias, "\n")

