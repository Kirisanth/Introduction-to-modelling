#Polar bears vs seals
S <- 1:50 #Seal population
P <- 1:50 #Polar bear population

S[1] = 60 #Initial Seal population
P[1] = 20 #Initial polar bear population

#Equation 1 seal population
rg <- 0.5 #Seal growth rate without polar bear or humans.
cps <- 0.01 #seal-polar bear encounter rate

#Equation 2 polar bear population
eps <- 0.1 #polar bear efficiency to convert one seal to another polar bear
dp <- 0.1 #polar bear mortality rate

#migration seals from conservation
ms <- 0.5 #seal migration out rate

#migration polarbear from conservation
mp <- 0 #polar bear migration out rate


for (t in 1:49) {
  
  if (S[t] < 0) {
    S[t] <- 0
  }
  
  if (P[t] < 0) {
    P[t] <- 0 
  }
  
  #Seal population
  S[t + 1] <- S[t] + rg*S[t] - cps*S[t]*P[t] + ms
  #Polar bear population
  P[t + 1] <- P[t] + eps*cps*S[t]*P[t] - dp*P[t]
}

t=seq(0,49,1)
# plot(t,S,type="l",col="blue", ylim=c(-20,20))
plot(t,S,type="l",col="blue")
lines(t,P,col="red")
grid()
legend("topleft", legend=c("Seals", "Polar bears"), lwd=c(3,3), col=c("blue","red"))




