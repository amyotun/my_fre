sbm_drift<-function(){
        
path<-c()
days<-252
price_t0=100
price_t252=130
drift<-(price_t252-price_t0)/days
year_volatility<-15
sd<-(days^(-0.5)*year_volatility)
M<-1000
sbm<-matrix(0,M,days+1)

for (i in 1:M){
        daily_price<-rnorm(days,0,sd)
        daily_price<-daily_price+drift
        daily_price<-c(price_t0,daily_price)
        sbm[i,]=cumsum(daily_price)
}

# Plot for standard Brownia Motion for 1000 trajectories
plot(sbm[1,],type='l',ylim = c(0, 300),xlab = "N", ylab = "")
for (i in 2:M){
        lines(sbm[i,])
}
summary(sbm[,253])

return (sbm)

}


