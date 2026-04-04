// Definiciones de variables aleatorias discretas
X ~ Bin(20, 0.3)          // Binomial
Y ~ Poi(4.5)              // Poisson
Z ~ Geo(0.2)              // Geométrica
W ~ BN(5, 0.5)            // Pascal / Binomial Negativa
H ~ HG(100, 20, 10)       // Hipergeométrica

// Estadísticas de las distribuciones
esperanzaX = E(X)
varianzaY = V(Y)
desvioZ = SD(Z)

print(esperanzaX)
print(V(X) + V(Y))

// Distribución personalizada (Vectores de valores y probabilidades)
CustomD ~ [(1, 2, 3, 4), (0.1, 0.2, 0.4, 0.3)]
print(E(CustomD))

table(CustomD)
table(H)