library(ggplot2)
library(dplyr)
library(corrplot)
library(leaflet)
library(ggpubr)
library(plotly)
library(tidyr)
library(tidyverse)
library(maps)
library(mapproj)

GlobalSpace = read.csv("C:/Users/giaco/Desktop/NuovoProgettoDati/GlobalSpace/GlobalSpaceLaunches.csv")

View(GlobalSpace)

GlobalSpace = GlobalSpace %>%
  select(Company.Name,Detail,Status.Rocket,Status.Mission,Country.of.Launch,Companys.Country.of.Origin,Private.or.State.Run,Year)

View(GlobalSpace)

#### Quali sono le Compagnie che hanno effettuato più missioni? #### 

nLancicomp = GlobalSpace %>%  #conto il numero di lanci per ciascuna compagnia
  group_by(Company.Name) %>%
  summarise(count = n())

View(nLancicomp) 

nLancicomp = nLancicomp %>% filter(count >= 10) #filtro solo le compagnie che hanno effettuato almeno 10 lanci

ncomp = as.vector(nLancicomp$count) #trasformo in vettore la colonna contenente il numero di lanci per compagnia
ncomp

comp = as.vector(nLancicomp$Company.Name) #trasformo in vettore la colonna contenente il nome delle compagnie
comp

tot = colSums(nLancicomp[,-1])

percent = vector(mode="character", length=28)

for(i in 1:28){
  
  percent[i] = ((ncomp[i] / tot)*100) #calcolo le percentuali del numero di lanci per ciascuna compagnia
  
}

Percentuale = as.double(percent)

#Creo un nuovo data frame contenente i nomi delle compagnie e le relative percentali di lanci

data <- data.frame(
  Compagnia = comp,
  Percentuale
)

#Creazione grafico a torta:

ggplot(data, aes(x="", y=Percentuale, fill=Compagnia)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)

#Creazione barplot:

Compagnia = nLancicomp$Company.Name
NumeroLanci = nLancicomp$count

dataLanci = data.frame(
  Compagnia,
  NumeroLanci
)

r = ggplot(data=dataLanci, aes(x=Compagnia, y=NumeroLanci,fill=Compagnia)) +
  geom_bar(stat="identity")

r + coord_flip()


#### Quali sono gli Stati che hanno effettuato più missioni spaziali? #### 


gb = select(GlobalSpace,c("Company.Name","Companys.Country.of.Origin")) 

gb_count = gb %>% #conto il numero di lanci per ciascuno Stato
  group_by(Companys.Country.of.Origin) %>%
  summarise(count = n())

View(gb_count)

state = as.vector(gb_count$Companys.Country.of.Origin)
state

gnumb = as.vector(gb_count$count)
gnumb

percent2 = vector(mode="character", length=17)

tot = colSums(gb_count[,-1])

for(i in 1:17){
  
  percent2[i] = ((gnumb[i] / tot)*100) #calcolo le percentuali del numero di lanci per ciascuno Stato
  
}

percent2 = as.double(percent2)

dataGb <- data.frame(
  Stato = state,
  Percentuale2 = percent2
)

#Creazione grafico a torta:

ggplot(dataGb, aes(x="", y=Percentuale2, fill=Stato)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)


#Creazione barplot:

n_comp = GlobalSpace %>% 
  group_by(Companys.Country.of.Origin) %>%
  summarise(count = n())

View(n_comp)

Stato = n_comp$Companys.Country.of.Origin
NumeroLanci = n_comp$count

dataLanci = data.frame(
  Stato,
  NumeroLanci
)

View(dataLanci)

dataLanci$Stato <- factor(dataLanci$Stato,levels = dataLanci$Stato[order(dataLanci$NumeroLanci)]) #riordino in ordine decrescente il numero di lanci

ggplot(dataLanci, aes(Stato, NumeroLanci, fill=Stato)) +                                    
  geom_bar(stat = "identity") +
  coord_flip()

#### RAPPRESENTAZIONE SU MAPPA DEL NUMERO LANCI PER STATO #### 

gb_count$Companys.Country.of.Origin[gb_count$Companys.Country.of.Origin == "England"] <- "UK" #Cambio il nome England in UK

worldtb = map_data("world") %>%
  as_tibble()

StateMap = left_join(worldtb,gb_count,by=c("region" = "Companys.Country.of.Origin")) #unisco dataset numero lanci per stato con dataset contenente le coordinate regioni del mondo
View(StateMap)

StateMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = StateMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=count), color="black") +
  theme_minimal()+
  labs(
    title = "Stati che hanno effettuato missioni spaziali", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="navyblue"),
    legend.position = "right"
  )

#### DA CHE STATI VENGONO LANCIATI I RAZZI? #### v

countryLaunch = select(GlobalSpace,c("Country.of.Launch"))

View(countryLaunch)

launchCount = countryLaunch %>% 
  group_by(Country.of.Launch) %>%
  summarise(count = n())

View(launchCount)

worldtb = map_data("world") %>%
  as_tibble()

launchMap = left_join(worldtb,launchCount,by=c("region" = "Country.of.Launch"))
View(launchMap)

launchMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = launchMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=count), color="black") +
  scale_fill_gradient2(low="royalblue1",mid="lightskyblue",high="navyblue",midpoint = 270) +
  theme_minimal()+
  labs(
    title = "Countries of launch", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="navyblue"),
    legend.position = "right"
  )

#### QUANTE DI QUESTE MISSIONI SONO STATE UN SUCCESSO E QUANTE UN FALLIMENTO? #### 

statusData = select(GlobalSpace,c("Status.Mission"))
View(statusData)

status = as.vector(statusData$Status.Mission)
status

success = 0
partial = 0
failure = 0

for(i in 1:4324){ #Conto il numero di successi, fallimenti e parziali fallimenti
  
  if(status[i] == "Success"){
    success = success + 1
  } else if(status[i] == "Failure"){
    failure = failure + 1
  } else{
    partial = partial + 1
  }
}

#Calcolo le percentuali dei valori appena trovati

PercentSuccess = (success / 4323) * 100 # 89.7% 

PercentFailure = (failure / 4323) * 100 # 7.8%

PercentPartial = (partial / 4323) * 100 # 2.4%

PercentSuccess = round(PercentSuccess,3) #arrotondo a 3 cifre dopo la virgola
PercentSuccess

PercentFailure = round(PercentFailure,3)
PercentFailure

PercentPartial = round(PercentPartial,3)
PercentPartial

dataStatus = data.frame(status = c("Successi","Fallimenti",
                                   "Fallimenti Parziali"), 
                        vals = c(PercentSuccess,PercentFailure,
                                 PercentPartial))

#Creo grafico a torta diviso in base alle percentuali dei tre Status

ggplot(dataStatus, aes(x="", y=vals,fill=status)) +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("Successi" = "springgreen3",
                             "Fallimenti" = "red3",
                             "Fallimenti Parziali" = "royalblue1"
  )) +
  geom_col() +
  geom_text(aes(label = vals),
            position = position_stack(vjust = 0.5))
#Creo barplot con numero di lanci per i tre Status

barplot(height=dataStatus$vals, names=dataStatus$status,  col=c("springgreen3","red3","royalblue1"))

#### QUANTE DI QUESTE MISSIONI SONO PRIVATE E QUANTE STATALI? ####

state = 0
private = 0

run = as.vector(GlobalSpace$Private.or.State.Run)

length(run)

for(i in 1:4324){ #Conto il numero di volte in cui compare la lettera S nella colonna Private.or.State.Run del dataset
  if(run[i] == "S"){
    state = state + 1
  } else{
    private = private + 1
  }
}

data = c(state,private)

barplot(data,names.arg = c("State Missions","Private Missions"), col = c("brown2","royalblue1"))

#### IN MEDIA HANNO PIU SUCCESSO LE MISSIONI PRIVATE O QUELLE STATALI? ####

stateSucc = 0
stateFail = 0
privSucc = 0
privFail = 0

privstat = as.vector(GlobalSpace$Private.or.State.Run)

status = as.vector(GlobalSpace$Status.Mission)

for(i in 1:4324){ #Conto il numero di volte in cui compare sia la lettera S/P e lo status Success/Failure
  if(privstat[i] == "S" && status[i] == "Success"){
    stateSucc = stateSucc + 1
  } else if(privstat[i] == "S" && status[i] == "Failure"){
    stateFail = stateFail + 1
  } else if(privstat[i] == "P" && status[i] == "Success"){
    privSucc = privSucc + 1
  } else if(privstat[i] == "P" && status[i] == "Failure"){
    privFail = privFail + 1
  }
}

MediafailState = (stateFail / (stateFail + stateSucc))* 100 #Percentuale fallimento State

MediafailPrivate = (privFail / (privFail + privSucc))* 100 #Percentuale fallimento Private 

MediasuccState = (stateSucc / (stateFail + stateSucc))* 100 #Percentuale successo State

MediasuccPrivate = (privSucc / (privFail + privSucc))* 100 #Percentuale successo Private

Percentstate = c(MediasuccState,MediafailState)

Percentpriv = c(MediasuccPrivate,MediafailPrivate)

test2 <- rbind(Percentstate,Percentpriv)

#Creo barplot formato da numero di successi e fallimneti per le missioni private e statali

barplot(test2,beside=T,names.arg = c("Successi","Fallimenti"), col = c("brown2","royalblue1"))
legend("topright",
       c("State Missions","Private Missions"),
       fill = c("brown2","royalblue1")
)

#### NUMERO LANCI PER ANNO #### 

nLaunch = GlobalSpace %>% 
  group_by(Year) %>%
  summarise(count = n())

View(nLaunch)

newDataL = data.frame(
  Year = nLaunch$Year,
  Num = nLaunch$count
)

View(newDataL)

ggplot(newDataL,aes(x = Year, y = Num)) + 
  geom_point() + 
  geom_smooth(method = "loess", span=0.3)

#Creo grafico numero lanci annuali delle principali Compagnie

GlobalSpace2 = GlobalSpace %>% 
  filter(Company.Name == "RVSN USSR" | Company.Name == "NASA" | Company.Name == "CASIC" | Company.Name == "General Dynamics" | Company.Name == "SpaceX" |  Company.Name == "US Air Force" |  Company.Name == "Arianespace")
  
View(GlobalSpace2)

g <- ggplot(GlobalSpace2, aes(Year))  

g + geom_bar(aes(fill=Company.Name)) + theme_dark()  #

#Creo grafico numero lanci annuali per i principali Stati

GlobalSpace3 = GlobalSpace %>% 
  filter(Companys.Country.of.Origin == "Russia" | Companys.Country.of.Origin == "USA" | Companys.Country.of.Origin == "Multi" | Companys.Country.of.Origin == "China" | Companys.Country.of.Origin == "Japan" |  Companys.Country.of.Origin == "India")

View(GlobalSpace3)

g <- ggplot(GlobalSpace3, aes(Year))  
g + geom_bar(aes(fill=Companys.Country.of.Origin)) + theme_dark() 

#Creo grafico numero lanci annuali per compagnie private e statali

g <- ggplot(GlobalSpace, aes(Year))  
g + geom_bar(aes(fill=Private.or.State.Run))

#PERCENTUALI SUCCESSI FALLIMENTI PER COMPAGNIA 

n_comp = GlobalSpace %>% 
  group_by(Company.Name) %>%
  summarise(count = n())

View(n_comp)

compagnie = as.vector(n_comp$Company.Name)

i = 0

success = vector(mode="numeric", length=55)
fail = vector(mode="numeric", length=55)
partial = vector(mode="numeric", length=55)

for(i in 1:55){ #conto numero volte in cui compare una certo status per ciascuna compagnia
  
  count = GlobalSpace %>%
    filter(Company.Name == compagnie[i])
  
    success[i] = length(which(count$Status.Mission == "Success")) 
    fail[i] = length(which(count$Status.Mission == "Failure"))
    partial[i] = length(which(count$Status.Mission == "Partial Failure"))
}

countComp = n_comp$count

percentSuccessi = vector(mode="double", length=55)
percentFallimenti = vector(mode="double", length=55)
percentParz = vector(mode="double", length=55)

as.vector(success)

for(i in 1:55){
  percentSuccessi[i] = (success[i]/countComp[i])*100
  percentFallimenti[i] = (fail[i]/countComp[i])*100
  percentParz[i] = (partial[i]/countComp[i])*100
}

succComp = data.frame(
  compagnie,
  countComp,
  success,
  partial,
  fail,
  percentSuccessi,
  percentFallimenti,
  percentParz
)

View(succComp)


succComp$compagnie <- factor(succComp$compagnie,                                 
                             levels = succComp$compagnie[order
                                                         (succComp$percentSuccessi)])
ggplot(succComp, aes(compagnie, percentSuccessi, fill=compagnie)) +                                 
  geom_bar(stat = "identity") +
  coord_flip() 






