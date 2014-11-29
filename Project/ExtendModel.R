S <- 1:50
P <- 1:50
H <- 1:50
CS <- 1:50
CP <- 1:50
I <- 1:50

S[1] = 20
P[1] = 2
H[1] = 2
CS[1] = 2
CP[1] = 2
I[1] = 0

#Equation 1 seal population
rg <- 1.3 #Seal growth rate without polar bear or humans.
cps <- 0.2 #seal-polar bear encounter rate
chs <- 0.1 #seal-human hunting rate
is <- 0

#Equation 2 polar bear population
eps <- 0.10 #polar bear efficiency to convert one seal to another polar bear
dp <- 0.10 #polar bear mortality rate
mp <- 0
ip <- 0

#Equation 3 human population
ehs <- 0.05 #human efficiency to convert seals to human
ehp <- 0.1 #human efficiency to convert polar bears to human
chs <- 0.2 #seal human encounter rate
chp <- 0.2 #polar bear human encounter rate
dh <- 0.1 #human mortality rate
ih <- 0

#Equation 4
KSC <- 40 #Capacity
rs <- 0.8 #seal growth rate in conservation area
ms <- 0.8 #seal migration out rate

#Equation 5
KPC <- 20 #Capacity
rp <- 0.1 #polar growth rate in conservation area
mp <- 0.05 #polar bear migration out rate

#Equation 6
ri <- 0
EC <- 0

for (t in 1:49) {
  #Seal population
  S[t + 1] <- S[t] + rg*S[t] - cps*S[t]*P[t] - chs*S[t]*H[t] - is*P[t]*I[t] + ms*CS[t]
  #Polar bear population
  P[t + 1] <- P[t] + eps*cps*S[t]*P[t] - dp*P[t] - chp*P[t]*H[t] - ip*P[t]*I[t] + mp*CP[t]
  #Human Population
  H[t+1] <- H[t] + ehs*chs*S[t]*H[t] + ehp*chp*P[t]*H[t] - dh*H[t] + ih*H[t]*I[t]
  #Seal Conservation
  CS[t+1] <- CS[t] + rs*CS[t]*(1 - CS[t]/KSC) - ms*CS[t]  #1 dimensional
  #Polar bear Conservation
  CP[t+1] <- CP[t] + rp*CP[t]*(1 - CP[t]/KPC) - mp*CP[t] #1 dimensional
  #Industial Shipping through Northwest Passage
  I[t+1] <- I[t] + ri*I[t]*(1 - I[t]/EC)
}