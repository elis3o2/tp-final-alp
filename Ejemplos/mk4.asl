A -> [(B,0.7),(C,0.3)]
B -> [(A,1.0)]
C -> [(C,1.0)]

M = mk(A,B,C)

print (simulate(M,10,start=A))