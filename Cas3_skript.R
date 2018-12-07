
# LEPmler2018_Čas3: Uvod u analizu mešovitih efekata


# Linearni modeli


# Klasa modela u kojima variranje u okviru neke mere (ZV)
# pokušavamo da predvidimo variranjem na jednoj ili više nekih drugih varijabli (NV)

# Da bismo ih primenili, potrebno je da bude zadovoljeno nekoliko uslova
# linearni odnos između NV i ZV
# (proverava se pre analize, ali i tokom analize)
# nezavisnost merenja, odsustvo kolinearnosti između prediktora
# (proverava se pre analize)
# homoskedascitet (odsustvo heteroskedasciteta) - ujednačenost varijanse
# (proverava se pri kraju analize)
# normalnost distribucije reziduala
# (proverava se pri kraju analize)
# odsustvo uticajnih tačaka (zapravo isto što i prethodno navedeno)
# (proverava se i pre analize i pred kraj analize)

# konačno, ZV mora pratiti odgovarajuću ditribuciju
# (proverava se pre analize)

# Za svaku od ovih provera postoje različite strategije,
# ali time ćemo se baviti na sledećem času


# Za početak, učitaćemo potrebne pakete


library(lme4) # da bismo pravili modele
library(lmerTest) # da nam budu prikazane p vrednosti
library(ggplot2) # za grafikone
library(gridExtra) # za uređivanje višestrukih grafikona
library(languageR) # za razne stvari
library(lattice) # za grafikone

# I učitaćemo jedan dataframe


dat.im=read.table("Dusica.imenice.2004.txt",sep="\t",T)

# proverimo dimenzije
dim(dat.im)


# mali uvid u podatke
head(dat.im)

# napravimo uvid u strukturu podataka
str(dat.im)

# zadrzimo u data frame-u samo reči

dat.im = dat.im[dat.im$Leksikalnost == "word",]

# zadržimo u data frame-u samo tačne odgovore

dat.im = dat.im[dat.im$Error_code == "C",]


# proverimo da li je ZV normalno distribuirana

g1 = ggplot(dat.im, aes(RT)) + geom_density()
g2 = ggplot(dat.im, aes(sample=RT)) +
  stat_qq() + stat_qq_line()


# na sledećem času ćemo se baviti različitim transformacijama
# za sada ćemo primeniti inverznu transformaciju RT
dat.im$RT = log(dat.im$RT)

g3 = ggplot(dat.im, aes(RT)) + geom_density()
g4 = ggplot(dat.im, aes(sample=RT)) +
  stat_qq() + stat_qq_line()


grid.arrange(g1,g2,g3,g4, layout_matrix = rbind(c(1,2),
                                                c(3,4)))



# transformišemo frekvenciju reči, jer znamo da stoji u log odnosu
# sa RT (SETITE SE USLOVA O LINEARNOSTI)
# kasnije ćemo i formalno proveravati da li je odnos linearan
# ali o tome na sledećem času

dat.im$frekv = log(dat.im$Frekvencija)

g1 = ggplot(dat.im, aes(x=Frekvencija, y=RT)) + 
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)
g2 = ggplot(dat.im, aes(x=frekv, y=RT)) + 
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)
grid.arrange(g1, g2, ncol=2)


# pored toga, kontinuirane prediktore treba centrirati na nulu
# kasnije ćemo videti zašto (kad budemo diskutovali o smislenosti intercepta)
# a još je bolje normaliizovati vrednosti:

dat.im$frekv.sirovo = dat.im$frekv
dat.im$frekv = scale(dat.im$frekv)

# da vidimo šta smo uradili sa frekv:
ggplot(dat.im, aes(x=frekv.sirovo, y=frekv)) + 
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)

mean(dat.im$frekv.sirovo)
exp(mean(dat.im$frekv.sirovo))
round(mean(dat.im$frekv),5)

# skaliramo i broj značenja
dat.im$NoS = scale(dat.im$NoS)

# sledeći važan uslov je nezavisnost merenja
# da pogledamo kakva je situacija u našem data frame-u

xtabs(~ Subject + Rec, data = dat.im)

# vidimo da je svaki ispitanik video sve reči
# (i pseudoreči, ali smo izbacili te podatke, otud nule kod pseudoreči)

# To znači da nemamo nezavisna merenja!

# Kako se rešava ovaj problem?



# TRADICIONALNI NAČIN 

# Izvođenje dve odvojene analize

# Uprosečavanje vrednosti ZV svih reči za pojedinačne ispitanike 
# Izvođenje analize po ispitanicima (tzv. F1 test)
# u kojoj se ispitanici tretiraju kao izvor slučajnih efekata

# Uprosečavanje vrednosti ZV svih ispitanika za pojedinačne reči (tzv. F2 test)

# Hajde, za početak to da uradimo

###############################################################################
# da narapravimo prosek po recima, koristimo funkciju aggregate()
# prvi argument je data frame
# drugi argument je varijabal koju uprosečavamo
# treći argument je lista koja definiše grupe za koje uprosečavamo

imenice = aggregate(dat.im$RT, list(dat.im$Rec), mean)


# damo naziv kolonama
colnames(imenice) = c("Rec", "MeanRT")


# sada iz velikog data frame-a uzmemo kolone koje su nam potrebne
imenice_by_item = dat.im[, c("Rec", "Duzina", "frekv", "Frekod", "NoS", 
                             "Viseznacnost", "Brzina.ispitanika")]


# a onda se otarasimo svih ponovljenih redova, uzmemo samo
# po jedan red za svaku rec

imenice_by_item = unique (imenice_by_item)


# konacno, spojimo dva nova data frame-a, koristeci kolonu Rec kao
# kljuc za spajanje u primajućem dataframe-u (imenice_by_item, by.x=)
# i u davajućem data frame-u (imenice, by.y=)

imenice_by_item = merge(imenice_by_item, imenice, by.x = "Rec", by.y = "Rec")


# sada imamo dataframe sa prosečnim vremenima reagovanja za reči
head(imenice_by_item)


##############################################################################

# INTERPRETACIJA KOEFICIJENATA U LINEARNOM MODELU


# S obzirom na to da je interpretacija koeficijenata za fiksne efekte 
# identična u lm i lmer
# kao i da ume da bude komplikovana
# prvo ćemo se pozabaviti njom



# SLUČAJ JEDNOG KONTINUIRANOG PREDIKTORA

# da pogledamo u kakvom odnosu stoje frekvencija reči i vreme prepoznavanja:

ggplot(imenice_by_item, aes(x=frekv, y=MeanRT)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)



# svaki linearni model sastoji se od
# nečega što razumemo, o čemu nešto znamo (prediktori)
# i nečega što ne razumemo, o čemu ništa ne znamo (greška)

# ZV ~ NV + greška

# Za deo koji razumemo (efekat prediktora)
# model računa intercept i nagib
# ono što ne razumemo postaju reziduali:

# ZV ~ Intercept    +    nagib * NV     +   rezidual

# Da bismo napravili linearni model u R-u
# koristimo ovakvu notaciju:

lm1 = lm(MeanRT ~ frekv, data = imenice_by_item)

# napravili smo jedan objekat, koji sadrži različite informacije
# a ono što nam treba dobijamo naredbom summary()

summary(lm1)

# npr. objekat lm1 sadrži koeficijente: intercept i nagib
# da dobijem samo koeficijente:

coef(lm1)

# ovaj ispis nam daje potrebne koeficijente
# pa bi gornja jednačina mogla da se napiše kao:

# MeanRT = 6.45842 -0.02553 * frekv + rezidual

# Intercept daje podatak o vremenu reakcije za frekv = 0 
# Pošto nulta frekvencija nema smisla, centrirali smo podatke
# kako bi intercept bio interpretabilan
# Dakle, interecept nam kaže za koje vreme se prepozna reč 
# koja ima prosečnu frekvencu (od reči iz eksperimenta)

# nagib nam govori za koliko se promeni vreme reakcije kada se za jedan stepen
# pomerimo na x osi


# važan elemenat linearnog modela je tzv. model matrix

head(model.matrix(lm1), n=10)

model.matrix(lm1)






# SLUČAJ JEDNOG KATEGORIČKOG PREDIKTORA

ggplot(imenice_by_item, aes(x=Viseznacnost, y=MeanRT)) + 
  scale_x_discrete(limits=c("malo", "srednje", "mnogo")) +
  coord_cartesian(ylim = c(6.2, 6.6)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) 


# pravimo linearni model:

lm2 = lm(MeanRT ~ Viseznacnost, data = imenice_by_item)

summary(lm2)


# izračunamo proseke za tri kategorije varijable Viseznacnost

tapply(imenice_by_item$MeanRT, imenice_by_item$Viseznacnost, mean)

# da vidimo samo koeficijente:
coef(lm2)

# intercept zapravo predstavlja prosek grupe Viseznavnost "malo"
# koeficijent za Viseznavnost "mnogo" nam govori za koju vrednost treba uvećati 
# vrednost intercepta da bismo dobili prosek grupe Viseznavnost "mnogo"

# to možemo i da proverimo

malo = mean(imenice_by_item[imenice_by_item$Viseznacnost=="malo",]$MeanRT)

srednje = mean(imenice_by_item[imenice_by_item$Viseznacnost=="srednje",]$MeanRT)

mnogo = mean(imenice_by_item[imenice_by_item$Viseznacnost=="mnogo",]$MeanRT)

malo - srednje # dobijamo prvi koeficijent
malo - mnogo # dobijamo drugi koeficijent

# odnosno:

malo + (- 0.03893902) # dobijemo prosek za Viseznacnost "srednje"
malo + (- 0.02825612) # dobijemo prosek za Viseznacnost "mnogo"


# podrazumevano kodiranje u R-u je tzv. treatment coding, ili dummy coding
# ovo kodiranje podrazumeva da se prvom nivou varijable dodeljuje vrednost 0,
# čime se ovaj nivo mapira na intercept

# sledeći nivo dobija vrednost 1 i računa se vrednost koju treba dodati interceptu 
# da bismo stigli na prosek za taj nivo


# U ovom slučaju za prvi nivo je proglašen "malo", jer je prvi u abecednom redu

head(model.matrix(lm2), n=10)

model.matrix(lm2)

contrasts(imenice_by_item$Viseznacnost)



# Ako želimo da mapiramo intercept na drugi nivo, koristimo relevel

imenice_by_item$Viseznacnost <- relevel(imenice_by_item$Viseznacnost, ref = "srednje")

# pa ponovo fitujemo model
lm2a = lm(MeanRT ~ Viseznacnost, data = imenice_by_item)
coef(lm2a)

# sada je intercept mapiran na Viseznacnost "srednje" i jednak proseku ove grupe reči

# prosek grupe Viseznacnost "malo" dobijemo tako što na vrednost intercepta 
# dodamo vrednost koeficijenta za Viseznacnost "malo"

6.45282895 + 0.02825612

# prosek grupe Viseznacnost "mnogo" dobijemo tako što na vrednost intercepta 
# dodamo vrednost koeficijenta za Viseznacnost "mnogo"

6.45282895 -0.01068290

# ko ne veruje, može da uporedi:
tapply(imenice_by_item$MeanRT, imenice_by_item$Viseznacnost, mean)



##############################################################################
# Da rezimiramo značenje intercpeta i nagiba:
# intercept se uvek mapira na nivo nula za sve prediktore
# ako je kontinuirani prediktor, onda je to vrednost nula
#   (u tom slučaju je najbolje centrirati podatke na nulu,
#   kako bi vrednost intercepta bila interpretabilna, tj.
#   kako bi označavala nivo ZV za proseke svih prediktora, 
#   jer često nulta vrednost prediktora po sebi nije smislena);
#     u tom slučaju nagib označava za koliko se promeni vrednost 
#     na y osi kada se na x osi pomerimo za 1
# ako je kategorički prediktor, onda intercept predstavlja nivo 
#   koji je mapiran na nulu
#     u tom slučaju nagib označava takođe za koliko se promeni 
#     vrednost na y osi ako se na x osi pomerimo za 1
#     samo što je sada pomeraj za 1 na x osi jednak promeni kategorije
##############################################################################


# SLUČAJ JEDNOG KATEGORIČKOG I JEDNOG KONTINUALNOG PREDIKTORA

# BEZ INTERAKCIJE

ggplot(imenice_by_item, aes(x = frekv, y = MeanRT, colour = Viseznacnost)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


lm3 = lm(MeanRT ~ Viseznacnost + frekv, data = imenice_by_item)

summary(lm3)

# intercept je ponovo mapiran na nulte nivoe oba prediktora
# što znači na Viseznacnost = 0 (tj. "srednje") i frekv = 0
# svaki sledeci koeficijent nam govori sta se desava kad se na x osi pomerimo
# za jedan korak u terminima odgovarajućeg prediktora

# Coefficients:
#                    Estimate   
# (Intercept)        6.452953   
# Viseznacnostmalo   0.032183 
# Viseznacnostmnogo -0.015850    
# frekv             -0.028097   


# Jednacina za Viseznacnost "srednje": 
#    MeanRT ~ 6.452953 - 0.028097 * frekv

# Jednacina za Viseznacnost "malo": 
#    MeanRT ~ (6.452953 + 0.032183) - 0.028097 * frekv

# Jednacina za Viseznacnost "mnogo": 
#    MeanRT ~ (6.452953 - 0.015850) - 0.028097 * frekv



# SA INTERAKCIJOM

lm4 = lm(MeanRT ~ Viseznacnost * frekv, data = imenice_by_item)

summary(lm4)

# Coefficients:
#                          Estimate  
# (Intercept)              6.452974 # nulti nivoi svih prediktora
# Viseznacnostmalo         0.031858 # razlika između "srednje" i "malo" kada je frekv=0
# Viseznacnostmnogo       -0.015441 # razlika između "srednje" i "mnogo" kada je frekv=0 
# frekv                   -0.032819 # nagib efekta frekv kada je Viseznacnost=0 ("srednje")
# Viseznacnostmalo:frekv   0.006830 # korekcija nagiba efekta frekv za Viseznacnost "malo"   
# Viseznacnostmnogo:frekv  0.007117 # korekcija nagiba efekta frekv za Viseznacnost "mnogo"


# interakcija u ovom, konkretnom slučaju nije statistički značajna,
# odnosno, potreba za korekcijom nagiba efkta frekv za različite nivoe 
# varijable Viseznacnost ne postoji
# ali u didaktičke svrhe, pozabavićemo se računanjem kao kad bi bila


# Jednacina za Viseznacnost "srednje"
    MeanRT = 6.452974 - 0.032819 * frekv

# Jednacina za Viseznacnost "malo"
    MeanRT = (6.452974 + 0.031858) + (-0.032819 + 0.006830) * frekv

# Jednacina za Viseznacnost "mnogo"
    MeanRT = (6.452974 - 0.015441) + (-0.032819 + 0.007117) * frekv


    
# SLUČAJ INTERAKCIJE DVA KATEGORIČKA PREDIKTORA

ggplot(imenice_by_item, aes(x=Frekod, y=MeanRT, fill=Viseznacnost)) + 
  coord_cartesian(ylim = c(6.2, 6.6)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) 


lm5 = lm(MeanRT ~ Viseznacnost * Frekod, data = imenice_by_item)

summary(lm5)

# Coefficients:
#                             Estimate  
# (Intercept)                 6.476518  # nulti nivo svih prediktora (srednje, NF) 
# Viseznacnostmalo            0.023208  # razlika između srednje, NF i malo, NF
# Viseznacnostmnogo          -0.005841  # razlika između srednje, NF i mnogo, NF  
# FrekodVF                   -0.050761  # razlika između srednje, NF i srednje, VF 
# Viseznacnostmalo:FrekodVF   0.019694  # dodatna korekcija za malo, VF preko  
                                        # razlike između srednje, NF i malo, NF i
                                        # razlike između srednje, NF i srednje, VF
# Viseznacnostmnogo:FrekodVF -0.015079  # dodatna korekcija za mnogo, VF preko  
                                        # razlike između srednje, NF i mnogo, NF i
                                        # # razlike između srednje, NF i srednje, VF



# srednje, NF: 6.476518
# malo, NF: 6.476518 + 0.023208 = 6.499726
# mnogo, NF: 6.476518  - 0.005841 = 6.470677

# srednje, VF: 6.476518 - 0.050761 = 6.425757
# malo, VF: 6.476518 + 0.023208 - 0.050761 + 0.019694 = 6.468659
# mnogo, VF: 6.476518 - 0.005841 - 0.050761 - 0.015079 = 6.404837

# Da proverimo račun:

with(imenice_by_item, tapply(MeanRT, list(Viseznacnost, Frekod), mean))

#         NF       VF
# srednje 6.476518 6.425756
# malo    6.499725 6.468658
# mnogo   6.470677 6.404837







# SLUČAJ INTERAKCIJE DVA KONTINUALNA PREDIKTORA


ggplot(imenice_by_item, aes(x=NoS, y=MeanRT, colour = frekv)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)



lm6 = lm(MeanRT ~ NoS * frekv, data = imenice_by_item)

summary(lm6)

# Coefficients:
#              Estimate     
# (Intercept)  6.4580464  # RT kad je NoS = 0 i frekv = 0 (oba mapirana na prosek)
# NoS         -0.0203097  # nagib za Nos kad je frekv = 0
# frekv       -0.0280606  # nagib za frekv kad je NoS = 0  
# NoS:frekv   -0.0008158  # za koliko treba korigovati nagib za Nos ako se 
                          # na skali frekv pomerimo za jedno mesto;
                          # odnosno, za koliko treba korigovati nabib za frekv,
                          # ako se na skali NoS pomerimo za jedno mesto.


# NAJLAKŠE JE VIDETI ŠTA SE DEŠAVA AKO SE PLOTUJE OVAKVA INTERAKCIJA
# (ako je značajna)


# DOSTA O KOEFICIJENTIMA, VRATIMO SE MEŠOVITIM EFEKTIMA



#################################################################################

# ZBOG ČEGA JE VAŽNO DA ISTOVREMENO ANALIZIRAMO I ISPITANIKE I REČI?

# Uzorkovali smo iz dve populacije na koje želimo da generalizujemo nalaze:
# iz populacije govornika datog jezika i iz populacije reči datog jezika.
# Želimo da naši rezultati važe ne samo za skup stimulusa/govornika koji su
# uključeni u eksperiment, već da važe za sve govornike i sve reči.
# Da bismo to mogli, potrebno je da tretiramo govornike i reči kao 
# izvore tzv. slučajnih efekata


# U ČEMU JE RAZLIKA IZMEĐU FIKSNIH I SLUČAJNIH EFEKATA?

## Fiksni efekti

#- Efekti varijabli čije smo (sve postojeće?) nivoe 
# uključili u nacrt tako da su "ponovljivi", tj. tako da 
# možemo nove slučajeve (nova merenja) nanovo dodeljivati tim nivoima.


## Slučajni efekti

#- Efekti kategoričkih varijabli čiji su nivoi slučajno uzorkovani iz populacije: 
# nismo obuhvatili sve postojeće nivoe;
# ne možemo pronaći nove slučajeve koji bi "popunjavali" tu kategoriju 
# (ne postoje dve iste reči, dva ista ispitanika)

#- Npr. uzorkovali smo određeni broj ispitanika iz populacije govornika nekog jezika, 
# ili određeni broj reči iz populacije reči nekog jezika, itd.

#- Želimo da naše fiksne efekte generalizujemo na sve pripadnike date populacije, 
# a ne samo da utvrdimo njihovo postojanje na odabranom uzorku.

## Modelovanje slučajnih efekata

#- Modeluju se kao slučajne varijable čija je aritmetička sredina jednaka nuli, 
# a standardna devijacija je nepoznata

#- Npr. brzine pojedinačnih ispitanika se razlikuju, u proseku se dodaje nula 
# da se aritmetička sredina prilagodila pojedinačnom ispitaniku

#- Međutim, konkretnom ispitaniku se ne dodaje nula!
  
#  - SD slučajnih efekata je parametar koji se procenjuje




# Za početak,
# čak i da ništa ne znamo o nacrtu istraživanja,
# znamo da se pojedinačni ispitanici razlikuju po brzini reagovanja

# da vidimo kako izgledaju prosečna vremena reagovanja pojedinačnih ispitanika
ggplot(dat.im, aes(x= Subject, y=RT)) + 
  geom_boxplot()


# napravimo model koji informišemo o tome da očekujemo različit intercept za
# svakog ispitanika, tj. očekujemo da se ispitanici razlikuju međusobno po
# brzini reagovanja

# pošto nismo ispitali sve govornike srpskog jezika, a dodatno,
# želimo da svoje nalaze generalizujemo na čitavu populaciju govornika
# ispitanike ne možemo tretirati kao fiksne efekte
# već kao slučajne


lmer1 = lmer( RT ~ 1 + (1|Subject), data = dat.im)



# pored toga što očekujemo da se ispitanici razlikuju po brzini,
# očekujemo i da vreme reagovanja neće biti isto za sve reči

# da vidimo kako izgledaju prosečna vremena reagovanja na pojedinačne reči

ggplot(dat.im, aes(x= Rec, y=RT)) + 
  geom_boxplot()


# napravimo model koji informišemo o tome da očekujemo različit intercept za
# svaku reč, tj. očekujemo da se reči razlikuju međusobno po
# brzini kojom se reaguje na njih

# pošto nismo prikazali sve reči srpskog jezika, a dodatno,
# želimo da svoje nalaze generalizujemo na čitavu populaciju reči našeg jezika
# ni reči ne možemo tretirati kao fiksne efekte, već kao slučajne

lmer2 = lmer( RT ~ 1 + (1|Rec), data = dat.im)


# možemo da napravimo model koji istovremeno informišemo da očekujemo 
# i razlike između ispitanika i razlike između reči

lmer3 = lmer( RT ~ 1 + (1|Subject) + (1|Rec), data = dat.im)
summary(lmer3)


# možemo i da proverimo da li je opravdano uključiti svaki od 
# ova dva slučajna efekta

# prvo napravimo modele zasnovane na ML umesto na REML
# da bismo omogućili poređenje

lmer1a = update(lmer1, REML = "FALSE")
lmer2a = update(lmer2, REML = "FALSE")
lmer3a = update(lmer3, REML = "FALSE")

# uporedimo model koji sadrži samo ispitanike i 
# model koji sadrži i ispitanike i stimuluse
# da bismo proverili da li je opravdano uključiti stimuluse kao random efekat
anova(lmer1a, lmer3a)

# jeste

# uporedimo model koji sadrži samo stimuluse i 
# model koji sadrži i ispitanike i stimuluse
# da bismo proverili da li je opravdano uključiti ispitanike kao random efekat
anova(lmer2a, lmer3a)

# jeste

# dakle, model sa oba izvora slučajnih efekata je opradvan 
# i dizajnom (dva izvora zavisnosti merenja: ispitanici i reči)
# i podacima

# KAKO DODAJEMO FIKSNE EFEKTE?

# na isti način kao u običnim linearnim modelima:

lmer4 = lmer( RT ~ frekv + (1|Subject) + (1|Rec), data = dat.im)

# Da li dodavanje frekvencije kao fiksnog efekta čini da model 
# bolje opisuje podatke? Da li je opravdan podacima ili nepotrebno usložnjava model?
# Nekontrolisano dodavanje prediktora može da dovede do tzv. overfitting-a

lmer4a = update(lmer4, REML = "FALSE")
anova(lmer3a, lmer4a)

# vidimo da je opravdano uključiti frekvenciju
# model koji nju sadrži ima manji AIC, manji BIC i veći loglikelyhood

# Tek kad utvrdimo da dodavanje prediktora čini model opravdano boljim
# gledamo koeficijente iz modela

summary(lmer4)

# ŠTA DOBIJAMO KAD PRIKAŽEMO REZIME MODELA

# Prve linije daju osnovne podatke o algoritmu, formuli koju smo primenili i podacima
# Potom dobijamo REML (Restricted Maximum Likelyhood) kriterijum konvergiranja 
# (koji može da posluži kao indeks za goodness of fit te i za poređenje modela)

# Dobijamo osnovne podatke o distribuciji reziduala 
# (za sada se čini da je simetrična, kasnije ćemo to dalje proveravati)

# Dolazimo do dela ispisa u kom su prikazani parametri za slučajne efekte
# Rekli smo da se za njih procenjuje varijansa/standardna devijacija
# Vidimo procenu za slučajni intercept za reči, procenu za slučajni intercept za
# ispitanike i rezidual
# Rezidual je ono što smo u običnom linearnom modelu označavali kao grešku
# (ono čiju strukturu ne razumemo).
# Možemo da kažemo i da smo grešku iz lm razdvojili 
# na deo čiju strukturu razumemo (različite prosečne brzine ispitanika i reči)
# i deo čiju strukturu ne razumemo (grešku)

# Na kraju, prikazani su koeficijenti za fiksne efekte.
# Mi imamo jedan kontinuirani prediktor.
# To znači da nam intercept kaže koju vrednost ima ZV kada je vrednost NV jednaka nuli.
# Da bi ovo bilo smisleno, centrirali smo prediktor na nulu, 
# što znači da nula sada označava prosečnu frekvencu, 
# te dobijamo podatak o vrednosti ZV (tj. RT) za prosečnu vrednost NV (tj. frekvence).

# Drugi koeficijent odnosi se na prediktor i govori nam za koliko se promeni vrednost ZV,
# kada se vrednost NV poveća za jedan. 
# Vidimo da je povećanje frekvence za jedno mesto na skali praćeno 
# skraćenjem vremena reakcije za 0.029, kao i da je ova promena statistički značajna

# parcijalni fiksni efekat prediktora možemo ovako da dobijemo:
plotLMER.fnc(lmer4)



# KAKO DOLAZIMO DO PREDIKCIJE MODELA ZA DATOG ISPITANIKA ZA DATU REČ?

# Sećate se, model je jedab objekat, koji ima različite karakteristike 
# i iz kog možemo da izvučemo različite podatke

# ovako tražimo vrednosti fiksnih koeficijenata (tj. koeficijenata za fiksne efekte)

fixef(lmer4)

# ovako tražimo vrednost random koeficijenata, koji se još zovu i 
# BLUPs (Best Linear Unbiased Predictors)

ranef(lmer4)

# da dobijemo vrednosti za koje treba korigovati intercept za svakog ispitanika:
ranef(lmer4)$Subject

# da dobijemo vrednosti za koje treba korigovati intercept za svaku reč:
ranef(lmer4)$Rec

# Možemo i grafički da ih prikažemo

print(dotplot(ranef(lmer4, condVar = TRUE))$Subject)	
print(dotplot(ranef(lmer4, condVar = TRUE))$Rec)	


# isto to može i ovako, da se malo prisetite prvog časa :)
print(dotplot(ranef(lmer4, condVar = TRUE))[[2]])	
print(dotplot(ranef(lmer4, condVar = TRUE))[[1]])	

# DA IZVEDEMO PREDIKCIJU ZA ISPITANIKA S9 ZA REČ ZVONO:

# Ovo su njihovi BLUPs:
# s9   0.129583215
# ZVONO  -0.0285428733

RT_s9_ZVONO = dat.im[dat.im$Subject == "s9" & dat.im$Rec == "ZVONO",]$RT
RT_s9_ZVONO

frekv_ZVONO = dat.im[dat.im$Subject == "s9" & dat.im$Rec == "ZVONO",]$frekv
frekv_ZVONO

Intercept = fixef(lmer4)[1]
Fix.frekv = fixef(lmer4)[2]

Random.int.subj = ranef(lmer4)$Subject$`(Intercept)`[24]
Random.int.rec = ranef(lmer4)$Rec$`(Intercept)`[90]

RT_s9_ZVONO_fitted_nase = Intercept + Fix.frekv * frekv_ZVONO  + 
                        Random.int.subj + Random.int.rec

RT_s9_ZVONO_fitted_nase


dat.im$RT.fitted = fitted(lmer4)

RT_s9_ZVONO_fitted = dat.im[dat.im$Subject == "s9" & dat.im$Rec == "ZVONO",]$RT.fitted
RT_s9_ZVONO_fitted






# Međutim, pored toga što informišemo model o tome da očekuje različita prosečna 
# vremena reagovanja od različitih ispitanika i za različite reči
# možemo da se zapitamo i da li je neki fiksni efekat baš isti za sve ispitanike.

# da vidimo kako izgleda efekat frekvencije odvojeno za svakog ispitanika
ggplot(dat.im, aes(x=frekv, y=RT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Subject) 

# Možemo da informišemo model o tome da očekujemo različit efekat frekvencije 
# za različite ispitanike
lmer5 = lmer( RT ~ frekv + (1 + frekv|Subject) + (1|Rec), data = dat.im)

# proverimo da li je ovo opravdano podacima
lmer5a = update(lmer5, REML = "FALSE")
anova(lmer4a, lmer5a)

# zapravo nije potreban slučajni nagib...
# moram to na kraju sve skockati sa komentarima

# Da pogledamo kako izgledaju brojke

summary(lmer5)

# Možemo da primetimo dve stvari:
# 1) variranje za nagib frekvence je mnogo manje nego variranje za intercept 
#    za ispitanike ili reči
# 2) korelacija između intercepta za ispitanike i nagiba efekta frekence jednaka je -1
#    što znači da su ispitanici koji su bili brži bili istovremeno i osetljiviji na
#    frekvenciju. Međutim, to što je korelacija ovako visoka je često znak da smo 
#    uključili nepotrebne parametre u model, 
#    što nam, uostalom, poređenje dva modela i sugeriše.

# da vidimo korelaciju između intercepta za ispitanika i nabiga po ispitaniku:
plot(ranef(lmer5)$Subject)


# Međutim, postoji gledište po kom variranje nagiba frekvence po ispitaniku treba ostaviti 
# u modelu, jer je opravdano nacrtom (čak i ako nije opravdano podacima), te doprinosi
# razrešavanju problema zavisnih merenja.

# Ako se odlučimo da ostavimo ovu tzv. "slučajnu interakciju", možemo da pokušamo 
# da isključimo koralaciju između intercepta za ispitanika i nabiga po ispitaniku:


lmer6 = lmer( RT ~ frekv + (1 + frekv||Subject) + (1|Rec), data = dat.im)

# to smo postigli sa dve vertikalne linije, a mogli smo isto to i ovako:

lmer6 = lmer( RT ~ frekv + (1 |Subject) + (0 + frekv|Subject) + 
                (1|Rec), data = dat.im)


lmer6a = update(lmer6, REML = "FALSE")
anova(lmer5a, lmer6a)

# vidimo da ni ovo nije opravdano podacima
# a kad pogledamo ispis, vidimo da je variarnje nagiba zaista blisko nuli

summary(lmer6)


# postoje i dodatni formalni načini da se izvode ovi testovi, 
# ali o tome na sledećem času


# za fiksne efekte treba da proverimo i da li postoji nelinearna komponenta

lmer6n = lmer( RT ~ poly(frekv,2) + (1 + frekv||Subject) + (1|Rec), data = dat.im)


lmer6na = update(lmer6n, REML = "FALSE")
anova(lmer6a, lmer6na)

# vidimo da ni ovo nije opravdano podacima
# model sa nelinearnim efektom čak ima nešto lošiji fit

# pogledaćemo ispis, tek da vidimo kako se izlazi na kraj sa nelinearnostima
# u linearnom modelu

# primetite da za prediktor frekv sada postoje dva koeficijenta
# prvi se odnosi na linearnu komponentu
# drugi se odnosi na kvadratnu kompenentu

summary(lmer6n)




# Na sličann način na koji smo se pitali da li postoji variranje nagiba efekta
# frekvencije po ispitanicima, možemo da se zapitamo i da li postoji analogno 
# variranje po rečima.
# Međutim, frekvencija nije ponovljena po rečima, tj. jedna reč je uvek iste frekvencije
# pa bi ovo pitanje bilo besmisleno, tj. ne bi bilo opravdano nacrtom.
# To možemo da učinimo za neki prediktor koji je ponovljen po rečima.
# U ovom slučaju, u te svrhe može da nam posluži varijabla 
# (koju sam napravila za potrebe demonstracije)
# koja se zove Brzina.ispitanika
# To je kategorijalna varijabla koja je napravljena tako što su ispitanici podeljeni
# u dve grupe (brzi, spori) na osnovu medijane varijable SubjSpeed 
# što je prosečno vreme reakcije ispitanika u eksperimentu

# da pogledamo prvo da li su brzi ispitanici brži na svim rečima, kao i da li su
# podjednako brži na različitim rečima:

ggplot(dat.im, aes(x=Brzina.ispitanika, y=RT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Rec) 

# napravimo model u koji unesemo informaciju o tome da očekujemo
# razlike u odnosima između brzih i sporih ispitanika za različite reči
# ovo je dozvoljeno, pošto su svaku reč videli i brzi i spori ispitanici
# odnosno, faktor Brzina.ispitanika je ponovljen po rečima


lmer7 = lmer( RT ~ frekv + (1 + frekv||Subject) + 
                (1 + Brzina.ispitanika|Rec), 
                data = dat.im)
lmer7a = update(lmer7, REML = "FALSE")
anova(lmer6a, lmer7a)

# ponovo, vidimo da ovo nije opravdano podacima
# a kad pogledamo rezime modela, vidimo i da je variranje vrlo nisko
# a korelacija ponovo veoma visoka:

summary(lmer7)

# ponovo isključimo korelaciju:

lmer8 = lmer( RT ~ frekv + (1 + frekv||Subject) + 
                (1 + Brzina.ispitanika||Rec), 
              data = dat.im)
lmer8a = update(lmer8, REML = "FALSE")
anova(lmer6a, lmer8a)

# ponovo, vidimo da ovo nije opravdano podacima
# a kad pogledamo rezime modela, vidimo i da je variranje vrlo nisko

summary(lmer8)

# ovo je svakako bilo u svrhu ilustracije
# vraćamo se na model lmer6

# na ovaj način možemo da dodajemo nove prediktore
# za njih testiramo nelinearnosti, fiksne i slučajne interakcije
# opravdanost podacima

# međutim, interpretacija fiksnih efekata u lmer-u je ista kao interpretacija
# u lm-u, kojom smo se bavili na početku
# stoga nećemo uključivati nove prediktore u ovu analizu




# Za koeficijente iz modela možemo da procenimo 95% intervale poverenja
# Vidimo da se naši efekti uvek nalaze sa iste strane nule
confint(lmer6, method="Wald")



# Sada ćemo da proverimo da li su prekršeni neki od preduslova za
# primenu linernog modela:

# napravimo kolonu sa predviđenim vrednostima ZV:
dat.im$RT.fitted = predict(lmer6)

# ako nas zanima procenat objašnene varijanse:

cor(dat.im$RT, dat.im$RT.fitted.t)^2


# napravimo kolonu sa rezidualima:
dat.im$RT.res = residuals(lmer6)

# plotujemo korelaciju između fitovanih vrednosti i reziduala
# da proverimo da li postoji homogenost varijanse
# ovo treba da bude jedno lepo "jaje"

ggplot(dat.im, aes(x=RT.fitted, y=RT.res)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) 

# da proverimo da li se reziduali normalno distribuiraju
# ovo treba da bude što sličnije ravnoj liniji:

ggplot(dat.im, aes(sample=RT.res)) +
  stat_qq() + stat_qq_line()


# isto to može i ovako:
par(mfcol=c(1,2))
qqnorm(resid(lmer6))
plot(fitted(lmer6), resid(lmer6))
par(mfcol=c(1,1))



# sad ćemo da izbacimo tačke sa velikim rezidualima
# da proverimo da li utiču previše na model
# refitujemo model na podskupu tačaka


lmer6t = lmer( RT ~ frekv + (1 + frekv||Subject) + (1|Rec), 
              data = dat.im, subset=abs(scale(resid(lmer6)))<2.5)

summary(lmer6t)


par(mfcol=c(1,2))
qqnorm(resid(lmer6t))
plot(fitted(lmer6t), resid(lmer6t))
par(mfcol=c(1,1))

# vidimo da sad reziduali izgledaju mnogo bolje
# efekti su opstali i kad smo se otarasili štrčaka

















