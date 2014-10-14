# Purpose: To simulate M trajectories of Standard Brownian Motions.To calculate Quadratic Varation(qv)
# Input parameters: Time, N_min ,N_max and N_step and M trajectory 
# Return : To plot the mesh(X axis) with respect to Average Quadratic varation(Y-axis) 
# Expect to see: Avg QV will converge to T when mesh(1/N) becomes very big


simulate_sbm_qv<-function(T=1,min=100,max=10000,step=100,nsim=10){

cat("T : ",T,"\n");
cat("N_min :",min,"\n");
cat("N_max :",max,"\n");
cat("N_step :",step,"\n");
cat("No of Trajectories :",nsim,"\n")

# Variable declarication
N<-seq(min,max,step)
qavg<-c()
iter<-max/step

#Looping iterations(iter) for N,started at N_min, moved with N_step until N_max
for (i in 1:iter){
        q<-c()
        k= T/N[i]
        
        # Simulate the trajectories and calculate qv for each trajectory
        for (j in 1:nsim){
                path=rnorm(N[i], 0, sqrt(k));
                yn=cumsum(path)
                q<-append(q,sum((diff(yn))^2))
        }
        qavg<-append(qavg,mean(q))  # Calculate average of qv for all trajectories
}

plot(log(1/N),log(qavg), xlab = "mesh log(1/N)", ylab = "log average of quadratic varation")
#plot(1/N,qavg, xlab = "Mesh(1/N)", ylab = "Average of Quadratic Varation")
cat("Log(",T,") = ",log(T),"\n")

}