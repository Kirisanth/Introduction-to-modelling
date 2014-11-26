S <- 1:50
P <- 1:50
H <- 1:50
CS <- 1:50
CP <- 1:50

S[1] = 20
P[1] = 2
H[1] = 2
CS[1] = 2
CP[1] = 2

#Equation 1 seal population
sg <- 1.3 #Seal growth rate without polar bear or humans.
sp <- 0.2 #seal-polar bear encounter rate
sh <- 0.1 #seal-human hunting rate

#Equation 2 polar bear population
eps <- 0.10 #polar bear efficiency to convert one seal to another polar bear
mp <- 0.10 #polar bear mortality rate

#Equation 3 human population
ehs <- 0.05 #human efficiency to convert seals to human
ehp <- 0.1 #human efficiency to convert polar bears to human
sh <- 0.2 #seal human encounter rate
ph <- 0.2 #polar bear human encounter rate
mh <- 0.1 #human mortality rate

#Equation 4
KS <- 40 #Capacity
csg <- 0.2 #seal growth rate in conservation area
ts <- 0.1 #seal migration out rate

#Equation 5
KP <- 20 #Capacity
cpg <- 0.1 #polar growth rate in conservation area
tp <- 0.05 #polar bear migration out rate

for (t in 1:49) {
  
  #Seal population
  S[t + 1] <- S[t] + sg*S[t] - sp*S[t]*P[t] - sh*S[t]*H[t] + ts*CS[t]
  #Polar bear population
  P[t + 1] <- P[t] + eps*sp*S[t]*P[t] - mp*P[t] + tp*CP[t]
  #Human Population
  H[t+1] <- H[t] + ehs*sh*S[t]*H[t] + ehp*ph*P[t]*H[t] - mh*H[t]
  #Seal Conservation
  CS[t+1] <- CS[t] + csg*CS[t]*(1 - CS[t]/KS) - ts*CS[t]  #1 dimensional
  #Polar bear Conservation
  CP[t+1] <- CP[t] + cpg*CP[t]*(1 - CP[t]/KP) - tp*CP[t] #1 dimensional
}

print(S)
#print(P)
#print(H)
#print(CS)
#print(CP)

plot(S, type='l')