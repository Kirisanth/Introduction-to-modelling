P <- 1:50 

P[1] = 100000

D <- 1:50

D[1] = 4

#Plant growth rate without deer.
a <- 0.20
#Plant deer encounter rate
c <- 0.50
#efficiency for deer to convert plant to deer
e <- 0.01
#mortality rate deer
f <- 0.01
#hunting rate deer
h = 1

k<-0.01

for (i in 1:50) {
  
  P[i+1] <- P[i] + a*P[i] - c*P[i]*D[i]
  D[i+1] <- D[i] + k*P[i]*D[i] - f*D[i] - h

}

print(D)
print(P)


plot(D)
#plot(P)
