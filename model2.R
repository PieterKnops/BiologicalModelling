model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dWILG <- rWILG*WILG*(1-WILG/K1) - p * WILG * ELK; # effect wolven op eten? De wilg en elk waren allebei afhankelijk van dezelfde K, heb ik veranderd.
    dELK <- rELK*ELK*(1-ELK/K2) - ELK*WOLF/(1+WOLF)- ELK*BEAR/(1+BEAR); # verzadging helpen? Juvenile? Effect on 1?
    dBEAR <- c2 * ELK*BEAR/(1+ELK) - deathBEAR*BEAR; # Mass action? Bears and Wolves birth rates were saturation of themselves instead of elk. I fixed this cause Im smart.
    dWOLF <- c1 * ELK*WOLF/(1+ELK) - deathWOLF*WOLF; # Bears dont only eat ELKs
    return(list(c(dWILG, dELK, dBEAR, dWOLF)))  # De conversiefactor van de beren was hetzelfde als hun death rate, heb ik ook veranderd.
  })
}  

p <- c(rWILG=1,rELK=1.0,K1=30000,K2=20000,c1=0.1,c2=0.5,d=0.02,deathWOLF=0.13,deathBEAR=0.042,deathWILG=0.022,p=0.02)
s <- c(WILG=15000,ELK=16500,BEAR=400,WOLF=41)
run(tmax=1000)

plane(xmax=20,ymax=10,eps=-0.01);f<-run(traject=T)
newton(c(WILG=10,ELK=3.605551,WOLF=0),plot=T)

# life expectancy wolves: https://www.yellowstonepark.com/things-to-do/wildlife/wolves 
# life expectancy black bears, grizzly bear respectively: https://www.nps.gov/yell/learn/nature/black-bear.htm https://www.nps.gov/yell/learn/yellowstone-grizzly-bear-facts.htm
# life expectancy willow: https://nl.wikipedia.org/wiki/Wilg#Knotwilg <- TODO not sure about this one
# De Elk birth rate is vooral afhankelijk van de hoeveelheid beschikbare vegetatie en de death rate van de hoeveelheid Elk. Elk Population Processes in Yellowstone National Park Under the Policy of Natural Regulation Michael B. Coughenour and Francis J. Singer Ecological Applications Vol. 6, No. 2 (May, 1996), pp. 573-593
# De Carrying capacity van elk zonder de aanwezigheid van wolven in de Northern Range was rond de 16400 +-2500 afhankelijk van de hoeveelheid consumeerbare vegetatie.Elk Population Processes in Yellowstone National Park Under the Policy of Natural Regulation Michael B. Coughenour and Francis J. Singer Ecological Applications Vol. 6, No. 2 (May, 1996), pp. 573-59
# In 1995 werden wolven geintroduceerd. In 2004 had het aantal elk afgenomen tot 8335 in de Northern Range. In deze periode word de jacht geschat op 27 +- 5% per jaar.In 2003 doden wolven zo'n 1000 Elk per jaar. Dat is meer dan er gejaagd werd. De wolvenpopulatie groeide nog steeds op dit punt.Northern Yellowstone Elk after Wolf Restoration P. J. White and Robert A. Garrott Wildlife Society Bulletin (1973-2006) Vol. 33, No. 3 (Autumn, 2005), pp. 942-955
# 14 wolves werden geintroducedeerd in 1995, 17 in 1996.Nog 10 in 1997. In totaal dus 41. Er werden geen complete packs in gezet. Kunnen wij zorgen dat er precies die hoeveelheden wolven bijkomen in ons model op dat moment? https://www.nps.gov/yell/learn/nature/wolves.htm