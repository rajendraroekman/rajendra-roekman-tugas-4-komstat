# NOMOR 1: Distribusi Poisson
# Rata-rata kedatangan (lambda) = 3
lambda <- 3

# Menghitung P(X >= 5) = 1 - P(X <= 4)
prob_1 <- ppois(q = 4, lambda = lambda, lower.tail = FALSE)

cat("Nomor 1:\n")
cat("P(X >= 5) =", prob_1, "\n\n")

# NOMOR 2: Distribusi Hipergeometrik
# Pengambilan tanpa pengembalian menggunakan Distribusi Hipergeometrik:
# m = jumlah bola merah (sukses) = 20
# n = jumlah bola bukan merah (gagal) = 80
# k = jumlah bola yang diambil (ukuran sampel) = 10

m <- 20
n <- 80
k <- 10

# Contoh memodelkan nilai PMF untuk seluruh kemungkinan jumlah bola merah (0 s.d. 10):
x_vals <- 0:k
pmf_hyper <- dhyper(x = x_vals, m = m, n = n, k = k)

# Menampilkan tabel distribusi peluang
tabel_hiper <- data.frame(Jumlah_Bola_Merah = x_vals, Peluang = pmf_hyper)

cat("Nomor 2:\n")
print(tabel_hiper)
cat("\n")

# NOMOR 3: Simulasi Binomial vs PMF Teoretis
set.seed(123) # Mengatur seed agar hasil simulasi konsisten

n_sim <- 1000 # Jumlah percobaan simulasi
size <- 15    # Banyaknya percobaan binomial (n)
prob <- 0.4   # Peluang sukses (p)

# 1. Melakukan simulasi 1.000 percobaan Binomial
simulasi <- rbinom(n = n_sim, size = size, prob = prob)

# 2. Menghitung PMF Teoretis
x_bin <- 0:size
pmf_teoretis <- dbinom(x = x_bin, size = size, prob = prob)

# 3. Membandingkan Histogram Hasil Simulasi dengan PMF Teoretis
# Membuat histogram peluang relatif dari hasil simulasi
hist_data <- hist(simulasi, 
                  breaks = seq(-0.5, size + 0.5, by = 1), 
                  probability = TRUE,
                  col = "lightblue",
                  xlab = "Jumlah Sukses (X)",
                  ylab = "Peluang / Probabilitas",
                  main = "Simulasi Binomial (n=15, p=0.4) vs PMF Teoretis")

# Menambahkan titik dan garis untuk PMF Teoretis
lines(x_bin, pmf_teoretis, type = "b", col = "red", pch = 19, lwd = 2)

# Menambahkan legenda
legend("topright", 
       legend = c("Simulasi (1000 iterasi)", "PMF Teoretis"),
       fill = c("lightblue", NA), 
       col = c(NA, "red"), 
       lty = c(NA, 1), 
       pch = c(NA, 19), 
       lwd = c(NA, 2))