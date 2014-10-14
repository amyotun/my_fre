# Purpose: To simulate M trajectories of Standard Brownian Motion.
# Input parameters: Time, N_min ,N_max and N_step and M trajectory 
# Return : To plot Standard Brownian Motion with M tragectory
# 


simulate_sbm_path<-function(T=1,N=1000,nsim=10){

cat("T : ",T,"\n");

cat("No of Trajectories :",nsim,"\n")

# Variable declarication

sbm<-matrix(0,nsim,N)



        k= T/N
        
        # Simulate the trajectories and calculate qv for each trajectory
        for (j in 1:nsim){
                path=rnorm(N, 0, sqrt(k));
                sbm[i,]=cumsum(path)

        }

# Plot for standard Brownia Motion for 1000 trajectories
plot(sbm[1,],type='l',ylim = c(-70, 70),xlab = "N", ylab = "")
for (i in 2:nsim){
        
        lines(sbm[i,])
}


}