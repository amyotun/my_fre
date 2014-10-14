# Purpose: To simulate M trajectories of Standard Brownian Motions.To study the characteristic of re-scaling factor on X(A) wrt X(1000)
# Input parameters: A such as 100,200,300,...1000 
# Return : To plot the CDFs of X(A) with re-scaling factor and X(1000)  
# Expect to see: Almost similar CDFs of X(A) with re-scaling factor and X(1000) 


simulate_sbm<-function(A=100){
        
        cat("a : ",A,"\n");        
        
        # Variable declariation and initialization
        M=1000
        N=1000
        k=A/N
        
        w_a<-c()
        w1000<-c()
        sbm<-matrix(0,M,N)
        
        #Simulate the 1000 trajectories and subsetting w(a) and w(1000) from the trajectories
        for (i in 1:M){
                path=rnorm(N, 0, sqrt(k));
        #        sbm[i,]=cumsum(path)
                w_a=append(w_a,cumsum(path)[A])
                w1000=append(w1000,cumsum(path)[1000])
        }
        
        # Plot for standard Brownia Motion for 1000 trajectories
        #plot(sbm[1,],type='l',ylim = c(-70, 70),xlab = "N", ylab = "")
        #for (i in 2:M){
        
        #        lines(sbm[i,])
        #}
        
        # Plot CDFs of X(A) with re-scaling factor and X(1000) 
        plot(ecdf(w_a*sqrt(N/A)),main="CDFs of W(a) with its scaling factor and W(1000) ")
        lines(ecdf(w1000),col="red")
        
        cat("variance when a =",A,"=",var(w_a),"\n")
        cat("variance when a = 1000 =",var(w1000),"\n")
        
}

