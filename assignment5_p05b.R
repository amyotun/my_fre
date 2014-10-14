
show_pdf<-function(day=10,sbm){
        
        x <- sbm[,day+1] 
        h<-hist(x, breaks=10, col="red",xlab="Annual Stock Price",main="Histogram with Normal Curve") 
        xfit<-seq(min(x),max(x),length=40) 
        yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
        yfit <- yfit*diff(h$mids[1:2])*length(x) 
        lines(xfit, yfit, col="blue",lwd=2)
        

}