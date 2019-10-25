model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dWILG <- rWILG*WILG*(1-WILG/K) - p * WILG * ELK; # effect wolven op eten?
    dELK <- rELK*ELK*(1-ELK/K) - ELK*WOLF/(1+WOLF)- ELK*BEAR/(1+BEAR); # verzadging helpen? Juvenile? Effect on 1?
    dBEAR <- d * ELK*BEAR/(1+ELK) - deathBEAR*BEAR; # Mass action? Bears and Wolves birth rates were saturation of themselves instead of elk. I fixed this cause Im smart.
    dWOLF <- c * ELK*WOLF/(1+ELK) - deathWOLF*WOLF; # Bears dont only eat ELKs
    return(list(c(dWILG, dELK, dBEAR, dWOLF)))  
  })
}  

p <- c(rWILG=1,rELK=0.8,K=20000,c=0.1,d=0.02,deathWOLF=0.13,deathBEAR=0.042,deathWILG=0.022,p=0.02)
s <- c(WILG=10,ELK=10,BEAR=1,WOLF=1)
run(tmax=1000)

plane(xmax=20,ymax=10,eps=-0.01);f<-run(traject=T)
newton(c(WILG=10,ELK=3.605551,WOLF=0),plot=T)

# life expectancy wolves: https://www.yellowstonepark.com/things-to-do/wildlife/wolves 
# life expectancy black bears, grizzly bear respectively: https://www.nps.gov/yell/learn/nature/black-bear.htm https://www.nps.gov/yell/learn/yellowstone-grizzly-bear-facts.htm
# life expectancy willow: https://nl.wikipedia.org/wiki/Wilg#Knotwilg <- TODO not sure about this one
# De Elk birth rate is vooral afhankelijk van de hoeveelheid beschikbare vegetatie en de death rate van de hoeveelheid Elk. Elk Population Processes in Yellowstone National Park Under the Policy of Natural Regulation Michael B. Coughenour and Francis J. Singer Ecological Applications Vol. 6, No. 2 (May, 1996), pp. 573-593
# De Carrying capacity van elk zonder de aanwezigheid van wolven was rond de 16400 +-2500 afhankelijk van de hoeveelheid consumeerbare vegetatie.Elk Population Processes in Yellowstone National Park Under the Policy of Natural Regulation Michael B. Coughenour and Francis J. Singer Ecological Applications Vol. 6, No. 2 (May, 1996), pp. 573-59