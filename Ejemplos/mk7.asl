A -> [(B,0.3),(C,0.7)]
B -> [(A,0.6),(C,0.4)]
C -> [(A,0.2),(B,0.3),(C,0.5)]

M = mk(A,B,C)

print (stationary(M))
print (ex(M,3))
plot (M)
X := Bin(5, 0.8)
print(X)