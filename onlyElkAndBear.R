model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dE <- r*E*(1-E/K) - e*B*E/(1+E);
    dB <- c * B*E/(1+E) - deathB*B;
    return(list(c(dE, dB)))  
  }) 
}  

p <- c(r=1,e=80,K=3,c=0.07,deathB=1/24)
s <- c(E=1.5,B=0.015)
plane(xmax=3,ymax=0.02,eps=-0.001);f<-run(traject=T)
newton(c(E=1,B=1),plot=T)
