model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dWILG <- rWILG*WILG*(1-WILG/K) - p * WILG * ELK; # effect wolven op eten?
    dELK <- rELK*ELK*(1-ELK/K) - ELK*WOLF/(1+WOLF)- ELK*BEAR/(1+BEAR); # verzadging helpen? Juvenile? Effect on 1?
    dBEAR <- d * ELK*BEAR/(1+BEAR) - deathBEAR*BEAR; # Mass action?
    dWOLF <- c * ELK*WOLF/(1+WOLF) - deathWOLF*WOLF; # Bears dont only eat ELKs
    return(list(c(dWILG, dELK, dBEAR, dWOLF)))  
  })
}  

p <- c(rWILG=1,rELK=0.8,K=10,deathWOLF=0.13,c=0.1,d=0.02,deathBEAR=0.05,deathWILG=0.03,p=0.02)
s <- c(WILG=10,ELK=10,BEAR=1,WOLF=1)
run(tmax=1000)

plane(xmax=20,ymax=10,eps=-0.01);f<-run(traject=T)
newton(c(WILG=10,ELK=3.605551,WOLF=0),plot=T)