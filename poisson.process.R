# Purpose: To simulate N events from a Poisson process
# Input parameters: lambda(Rate,default with 1) , N(no of event,default with 50) and initial.size, default with zero at N(0)
# Return : To plot 1 sample path 
# Note: Set the seed eg. set.seed(10) before running this function
poisson.process<- function(lambda = 0.1, N = 50, initial.size = 0)
{
        
        # Initialize the variables. 
        # W for wait time , X for no. of counts
        S <- rep(0, N)
        W <- S
        X <- S
        X[1] <- initial.size
        W[1] <- S[1]        	# time = 0
        
        ## To compute the waiting time for  the second event to Nth event
        for(i in 2:N) {
                S[i] <- rexp(1, rate = lambda) 
                W[i] <- W[i - 1] + S[i]
                X[i] <- X[i - 1] + 1
        }
        
        ## Plot the path 
        
        plot(W, X, type = "n", xlab = "Time", ylab = "Population Size")
        lines(W,X,type="s")
        
        return()
}
