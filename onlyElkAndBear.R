model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dELK  <- rELK*ELK*(1-ELK/K)  - e*ELK*BEAR/(h + ELK) - deathELK * ELK; # verzadging helpen? Juvenile? Effect on 1?
    dBEAR <- c*e*ELK*BEAR/(h+ELK) - deathBEAR*BEAR; # Mass action? Bears and Wolves birth rates were saturation of themselves instead of elk. I fixed this cause Im smart.
    return(list(c(dELK, dBEAR)))  # De conversiefactor van de beren was hetzelfde als hun death rate, heb ik ook veranderd.
  })
}  

p <- c(rELK=1.0,K=2,c=0.02,e=9,h=4.5,deathELK=0.05,deathBEAR=0.042)
s <- c(ELK=1.5,BEAR=0.02)
run(tmax=1000,ymax=10)
