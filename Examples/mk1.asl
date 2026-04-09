A -> [(B,0.5),(C,0.5)]
B -> [(A,1.0)]
C -> [(C,1.0)]

M = mk(A,B,C)

print (ex(M,1))
print (stationary(M))

plot (M)