model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dWI <- r*WI*(1-WI/K) - c * WI * E; #effect wolven op eten??
    dE <- r*E*(1-E/K) - E*WO/(1+WO);
    dWO <- c * E*WO/(1+WO) - WOd*WO;
    return(list(c(dWI, dE, dWO)))  
  }) 
}  

p <- c(r=1,K=10,WOd=0.13,c=0.1,WId=0.03)
s <- c(WI=10,E=10,WO=0.01)
plane(xmax=20,ymax=10,eps=-0.01);f<-run(traject=T)
newton(c(WI=10,E=3.605551,WO=0),plot=T)
