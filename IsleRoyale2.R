model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dFIR <- rFIR*FIR*(1-FIR/K1) - p * FIR * MOOSE; # effect wolven op eten? De FIR en elk waren allebei afhankelijk van dezelfde K, heb ik veranderd.
    dMOOSE <- rMOOSE*MOOSE*(1-MOOSE/K2) - e*MOOSE*WOLF/(h+MOOSE) - deathMOOSE*MOOSE; # verzadging helpen? Juvenile? Effect on 1?
    dWOLF <- c*e*MOOSE*WOLF/(h+MOOSE) - deathWOLF*WOLF; # Bears dont only eat ELKs
    return(list(c(dFIR, dMOOSE, dWOLF)))  # De conversiefactor van de beren was hetzelfde als hun death rate, heb ik ook veranderd.
  })
}  

p <- c(rFIR=10,rMOOSE=0.5,K1=20,K2=5.6,c=1/15,e=10,h=4,deathWOLF=1/6,p=1.8,z=0.7,deathMOOSE = 1/10)
s <- c(FIR=13,MOOSE=1.68,WOLF=0.1)
run(tmax=50,tstep=0.1,after="
  if(t == 12) {
    state[\"WOLF\"] = 0.2 * state[\"WOLF\"];
    parms[\"deathWOLF\"] = 1/3; parms[\"c\"] = 1/30;
  };
  if(t == 26) {
    state[\"MOOSE\"] = 0.4 * state[\"MOOSE\"];
  };
  if(t == 28) {
    parms[\"deathWOLF\"] = 1/6;
    state[\"WOLF\"] = state[\"WOLF\"] + 1 / 535;
    parms[\"c\"] = 1/15
  }"
    ,ymax=5)

plane(xmax=20,ymax=10,eps=-0.01);f<-run(traject=T)
newton(c(FIR=10,MOOSE=3.605551,WOLF=0),plot=T)

#Heb de elk en beer per km2 voor wolven ingevuld in de starthoeveelheden.
#FIRen komen maar op 0.4 tot 0.8 % van het park voor. 0.6 x 1554 = 9.324 km2 Nu alleen nog schatten hoeveel bomen per km2.
# Kunnen we FIRen op de een of andere manier scalen? Maakt de absolute hoeveelheid uit voor de dinamiek?
# Heb de kill rate aangepast want die kan niet boven de 1 zijn.
# Heb de saturatieconstate van de beer gezet op iets meer dan de helft van de elk. Dus bij onze begin hoeveelheden zou de groei dus meer dan half maximaal zijn
# De start hoeveelheden elk en beer en wolf komen letterlijk uit de literatuur.
# De c's e's en h's heb ik uitgezocht wat een beetje werkt, terwijl ik enigszins realistisch probeerde te blijven. De hoge e en hoge c's zijn misschien wel een probleem. Even over nadenken.


# life expectancy wolves: https://www.yellowstonepark.com/things-to-do/wildlife/wolves 
# life expectancy black bears, grizzly bear respectively: https://www.nps.gov/yell/learn/nature/black-bear.htm https://www.nps.gov/yell/learn/yellowstone-grizzly-bear-facts.htm
# life expectancy willow: https://nl.wikipedia.org/wiki/FIR#KnotFIR <- TODO not sure about this one
# De Elk birth rate is vooral afhankelijk van de hoeveelheid beschikbare vegetatie en de death rate van de hoeveelheid Elk. Elk Population Processes in Yellowstone National Park Under the Policy of Natural Regulation Michael B. Coughenour and Francis J. Singer Ecological Applications Vol. 6, No. 2 (May, 1996), pp. 573-593
# De Carrying capacity van elk zonder de aanwezigheid van wolven in de Northern Range was rond de 16400 +-2500 afhankelijk van de hoeveelheid consumeerbare vegetatie.Elk Population Processes in Yellowstone National Park Under the Policy of Natural Regulation Michael B. Coughenour and Francis J. Singer Ecological Applications Vol. 6, No. 2 (May, 1996), pp. 573-59
# In 1995 werden wolven geintroduceerd. In 2004 had het aantal elk afgenomen tot 8335 in de Northern Range. In deze periode word de jacht geschat op 27 +- 5% per jaar.In 2003 doden wolven zo'n 1000 Elk per jaar. Dat is meer dan er gejaagd werd. De wolvenpopulatie groeide nog steeds op dit punt.Northern Yellowstone Elk after Wolf Restoration P. J. White and Robert A. Garrott Wildlife Society Bulletin (1973-2006) Vol. 33, No. 3 (Autumn, 2005), pp. 942-955
# 14 wolves werden geintroducedeerd in 1995, 17 in 1996.Nog 10 in 1997. In totaal dus 41. Er werden geen complete packs in gezet. Kunnen wij zorgen dat er precies die hoeveelheden wolven bijkomen in ons model op dat moment? https://www.nps.gov/yell/learn/nature/wolves.htm
# Willow komt voor op ongeveer 0.4 tot 0.8 van yellowstone's Northern Range. Hiermee kunnen wij k uitrekenen als wij ook de hoeveelheid willows per m2 weten en de totale m2 van de northern range. Willow on Yellowstone's Northern Range: Evidence for a Trophic Cascade? Hawthorne L. Beyer, Evelyn H. Merrill, Nathan Varley and Mark S. Boyce Ecological Applications Vol. 17, No. 6 (Sep., 2007), pp. 1563-1571
# De Northern Range van yellowstone is 1553.99287 km2
# Ik kan geen goede wetenschappelijke bron vinden maar ik schat zo'n 500000 tot 1 miljoen FIRen per km2. Ik weet dat dit belachelijk hoog lijkt maar is echt zo.Grootste deel is kleine zaadlingen. Zoek aub een bron.
# We moeten p schatten zodat er zonder wolven vrijwel geen groei van FIRen plaatsvind maar met wolven een beetje.
# Denk niet dat we die conversiefactor van elk naar beren uit de literatuur gaan halen, dus laten we maar een redelijke schatting maken.
# The introduction of wolves has had a positive effect on the amount of bears. Should we simulate that? Impact of Wolf Reintroduction on Bison and Grizzly Bear Populations and Trophic Cascades in Yellowstone National Park Galina Lipkin Department of Biology Lake Forest College Lake Forest, Illinois
# Tegen de tijd dat wolven geintroduceerd werden was de beren populatie al sterk gegroeid tot zo'n 760 grizzly's en 550 zwarte beren. Hamlin et al. 2009, Barber-Meyer et al. 2008
# Heb kill rates e1 en e2 toegevoegd.