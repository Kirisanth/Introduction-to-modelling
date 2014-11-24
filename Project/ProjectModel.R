P <- 1:50 

P[1] = 1

D <- 1:50

D[1] = 1

#Plant growth rate without deer.
a <- 2
#Plant deer encounter rate
c <- 0.5
#efficiency for deer to convert plant to deer
e <- 0.3
#mortality rate deer
f <- 0.6
#hunting rate deer
h = 0

for (i in 1:50) {
  P[i+1] <- P[i] + a*P[i] - c*P[i]*D[i]
  D[i+1] <- D[i] + e*c*P[i]*D[i] - f*D[i] - h

}

print(P)
print(D)


plot(P)
#plot(D)
