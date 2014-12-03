S <- 1:50 #Seal population
P <- 1:50 #Polar bear population
H <- 1:50 #Human population
CS <- 1:50 #Seal conservation population
CP <- 1:50 #Polar bear conservation population
I <- 1:50 #Industrial shipping through northwest passage

S[1] = 40 #Initial Seal population
P[1] = 5 #Initial polar bear population
H[1] = 5 #Initial human population
CS[1] = 2 #Initial seal conservation population
CP[1] = 2 #Initial polar bear conservation population
I[1] = 1 #Initial amount of Industrial activity (shipping)

#Equation 1 seal population
rg <- 0.5 #Seal growth rate without polar bear or humans.
cps <- 0.1 #seal-polar bear encounter rate
is <- 0.1 #industrial effect on seal population
SC <- 100 #seal carrying capacity

#Equation 2 polar bear population
eps <- 0.2 #polar bear efficiency to convert one seal to another polar bear
dp <- 0.1 #polar bear mortality rate
ip <- 0.1 #industrial effect on polar bear population

#Equation 3 human population
ehs <- 0.01 #human efficiency to convert seals to human
ehp <- 0.01 #human efficiency to convert polar bears to human
chs <- 0.01 #seal human encounter rate
chp <- 0.01 #polar bear human encounter rate
dh <- 0.5 #human mortality rate
ih <- 0.05 #industrial effect on human population

#Equation 4
KSC <- 80 #Capacity
rs <- 1.1 #seal growth rate in conservation area
ms <- 0.9 #seal migration out rate

#Equation 5
KPC <- 30 #Capacity
rp <- 1.1 #polar growth rate in conservation area
mp <- 0.5 #polar bear migration out rate

#Equation 6
ri <- 0.10 #industrial shipping growth rate
EC <- 100 #Economic constraint represents diminishing returns of economic growth

for (t in 1:49) {
  #Seal population
  S[t + 1] <- S[t] + rg*S[t] - cps*S[t]*P[t] - chs*S[t]*H[t] - is*S[t]*I[t] + ms*CS[t]
  #Polar bear population
  P[t + 1] <- P[t] + eps*cps*S[t]*P[t] - dp*P[t] - chp*P[t]*H[t] - ip*P[t]*I[t] + mp*CP[t]
  #Human Population
  H[t+1] <- H[t] + ehs*chs*S[t]*H[t] + ehp*chp*P[t]*H[t] + ih*H[t]*I[t] - dh*H[t]
  #Seal Conservation
  CS[t+1] <- CS[t] + rs*CS[t]*(1 - CS[t]/KSC) - ms*CS[t]  #1 dimensional
  #Polar bear Conservation
  CP[t+1] <- CP[t] + rp*CP[t]*(1 - CP[t]/KPC) - mp*CP[t] #1 dimensional
  #Industial Shipping through Northwest Passage
  I[t+1] <- I[t] + ri*I[t]*(1 - I[t]/EC)
}

t=seq(0,49,1)
plot(t,S,type="l",col="blue")
# plot(t,S,type="l",col="blue", ylim=c(-20, 45))
# plot(t,S,type="l",col="blue", ylim=c(-20, 60))
lines(t,P,col="red")
lines(t,H, pch=22, lty=2, col="green")
lines(t,CS, pch=22, lty=2, col="blue")
lines(t,CP, pch=22, lty=2, col="red")
lines(t,I, col="green")
grid()
#legend("bottomleft", legend=c("Polar bear", "Seal","Human","Polar Con", "Seal Con"), lwd=c(3,3), col=c("red","green","purple","brown", "black", "yellow"))





