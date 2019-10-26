model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dELK <- r*ELK*(1-ELK/K) - e*B*ELK/(1+ELK);
    dB <- c * B*ELK/(1+ELK) - deathB*B;
    return(list(c(dELK, dB)))  
  }) 
}  

p <- c(r=1,e=80,K=3,c=0.07,deathB=1/24)
s <- c(ELK=1.5,B=0.015)
plane(xmax=3,ymax=0.02,eps=-0.001);f<-run(traject=T)
newton(c(ELK=1,B=1),plot=T)

