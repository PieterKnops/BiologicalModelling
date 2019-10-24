model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dWILG <- r*WILG*(1-WILG/K) - p * WILG * ELK; #effect wolven op eten?
    dELK <- r*ELK*(1-ELK/K) - ELK*WOLF/(1+WOLF);
    dWOLF <- c * ELK*WOLF/(1+WOLF) - WOLFd*WOLF;
    return(list(c(dWILG, dELK, dWOLF)))  
  })
}  

p <- c(r=1,K=10,WOLFd=0.13,c=0.1,WILGd=0.03,p=0.02)
s <- c(WILG=10,ELK=10,WOLF=1)
run()

plane(xmax=20,ymax=10,eps=-0.01);f<-run(traject=T)
newton(c(WILG=10,ELK=3.605551,WOLF=0),plot=T)