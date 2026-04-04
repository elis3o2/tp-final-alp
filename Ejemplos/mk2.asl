A -> [(A,0.2),(B,0.8)]
B -> [(A,0.5),(B,0.5)]

M = mk(A,B)

print (F 1 (M,A,B,))
print (F (M,[A,B,A,B]))

plot (M)