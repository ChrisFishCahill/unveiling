SSB <- 1:100
ar = 1.7
br = 0.15
m = 0.5 #depensation parameter, ~ 2 for clear depensation
par(mfrow=c(2,2))
rec_ricker <- SSB*exp(ar-br*SSB)
ricker_dep <- (SSB^m)*exp(ar-br*SSB)

plot(log(rec_ricker/SSB) ~ SSB, main="Ricker", 
     ylab="ln(R/S)")
plot(log(ricker_dep/SSB) ~ SSB, main="Depensatory Ricker", 
     ylab="ln(R/S)")

rec_bh <- SSB*exp(ar)/(1+br*SSB)
bh_dep <- (SSB^m)*exp(ar)/(1+br*SSB)

plot(log(rec_bh/SSB) ~ SSB, main="Bev-Holt", 
     ylab="ln(R/S)")
plot(log(bh_dep/SSB) ~ SSB, main="Depensatory Bev-Holt", 
     ylab="ln(R/S)")
