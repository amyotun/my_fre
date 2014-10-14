# Purpose: To simulate M trajectories of Standard Brownian Motions.To study the characteristic of re-scaling factor on X(A) wrt X(1000)
# Input parameters: A such as 100,200,300,...1000 
# Return : To plot the CDFs of X(A) with re-scaling factor and X(1000)  
# Expect to see: Almost similar CDFs of X(A) with re-scaling factor and X(1000) 


simulate_sbm<-function(A=100){

# Variable declariation and initialization
M=1000
N=1000
k=A/N

w_a<-c()
w1000<-c()

#Simulate the 1000 trajectories and subsetting w(a) and w(1000) from the trajectories
for (i in 1:M)
        result=rnorm(N, 0, sqrt(k));
        w_a=append(w_a,cumsum(result)[A])
        w1000=append(w1000,cumsum(result)[1000])
}

#plot(w_a,type='l')
#lines(w1000,col="red")

# Plot CDFs of X(A) with re-scaling factor and X(1000) 
plot(ecdf(w_a*sqrt(N/A)))
lines(ecdf(w1000),col="red")

#print(var(w_a))
#print(var(w1000))

cat("var(",w_a,") = ",var(w_a),"\n")
cat("var(1000) = ",var(w1000),"\n")

}


