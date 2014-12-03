S <- 1:50 #Seal population
P <- 1:50 #Polar bear population
H <- 1:50 #Human population
CS <- 1:50 #Seal conservation population

S[1] = 40 #Initial Seal population
P[1] = 5 #Initial polar bear population
H[1] = 5 #Initial human population
CS[1] = 2 #Initial seal conservation population

#Equation 1 seal population
rg <- 0.5 #Seal growth rate without polar bear or humans.
cps <- 0.1 #seal-polar bear encounter rate

#Equation 2 polar bear population
eps <- 0.2 #polar bear efficiency to convert one seal to another polar bear
dp <- 0.1 #polar bear mortality rate

#Equation 3 human population
ehs <- 0.2 #human efficiency to convert seals to human
ehp <- 0.02 #human efficiency to convert polar bears to human
chs <- 0.01 #seal human encounter rate
chp <- 0.01 #polar bear human encounter rate
dh <- 0.01 #human mortality rate

#Equation 4
KSC <- 80 #Capacity
rs <- 0.5 #seal growth rate in conservation area
ms <- 0.2 #seal migration out rate


for (t in 1:49) {
  #Seal population
  S[t + 1] <- S[t] + rg*S[t] - cps*S[t]*P[t] - chs*S[t]*H[t] + ms*CS[t]
  #Polar bear population
  P[t + 1] <- P[t] + eps*cps*S[t]*P[t] - dp*P[t] - chp*P[t]*H[t]
  #Human Population
  H[t+1] <- H[t] + ehs*chs*S[t]*H[t] + ehp*chp*P[t]*H[t] - dh*H[t]
  #Seal Conservation
  CS[t+1] <- CS[t] + rs*CS[t]*(1 - CS[t]/KSC) - ms*CS[t]  #1 dimensional
}

t=seq(0,49,1)
plot(t,S,type="l",col="blue")
lines(t,P,col="red")
lines(t,H, col="green")
lines(t,CS, pch=22, lty=2, col="blue")
grid()
legend("topright", legend=c("Seal", "Polar bear", "Human", "Seal Con"), lwd=c(1,1,1,1), lty=c(1,1,1,2), col=c("blue","red","green","blue"))




