import numpy as np

# Fungsi aktivasi: sign function
def sign_func(x):
    # Fungsi signum: mengembalikan 1 jika x >= 0, -1 jika tidak
    return 1 if x >= 0 else -1

# Pola input (5x5 -> vektor 9 elemen)

x_4 = np.array([
    -1,  1, -1,  1, -1,
    -1,  1, -1,  1, -1,
    -1,  1,  1,  1, -1,
    -1, -1, -1,  1, -1,
    -1, -1, -1,  1, -1
])  

x_5 = np.array([
    -1,  1,  1,  1, -1,
    -1,  1, -1, -1, -1,
    -1,  1,  1,  1, -1,
    -1, -1, -1,  1, -1,
    -1,  1,  1,  1, -1
])

# x_6 = np.array([
    #  1,  1,  1,  1,  1,
    #  1, -1, -1, -1, -1,
    #  1,  1,  1,  1,  1,
    #  1, -1, -1, -1,  1,
    #  1,  1,  1,  1,  1
# ])

t_4 = 1
t_5 = -1

w_4 = x_4 * t_4
w_5 = x_5 * t_5
print(w_4)
print(w_5)

W = w_4 + w_5
print("Bobot akhir (W):")
print(W.reshape(5, 5))

# Fungsi recall untuk menguji pola
def recall(input_vector, weights):
    # Kalikan input dengan bobot dan jumlahkan (dot product)
    output = np.dot(input_vector, weights)
    # Terapkan fungsi aktivasi sign
    return sign_func(output)

# Uji pola T dan U
output_4 = recall(x_4, W)
output_5 = recall(x_5, W)

print("\n--- Pengujian dengan Pola Asli ---")
print(f"Pola -> {output_4} {'(Benar)' if output_4 == 1 else '(Salah)'}")
print(f"Pola -> {output_5} {'(Benar)' if output_5 == -1 else '(Salah)'}")



