X ~ Bin(10, 0.5)
Z ~ N(0, 1)

// Probabilidades puntuales y desigualdades
prob1 = P(X = 5)
prob2 = P(X <= 3)
prob3 = P(X > 7)

// Probabilidades invertidas (ej: n < X)
prob4 = P(2 < X)

// Probabilidades de rango (Between)
// P(inf < VAR < sup)
probRango1 = P(4 < X <= 6)
probRango2 = P(-1.96 < Z < 1.96) // Debería dar ~0.95

print(probRango2)