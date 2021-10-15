

# Prvo učitamo potrebne pakete


require(rms)
require(lme4)
require(languageR)
require(multcomp)
require(lsmeans)
require(multcompView)
require(pbkrtest)
require(lmerTest)
require(randomForest)
require(party)
require(gbm)
require(randomForest)
require(party)
require(MASS)
require(car)
require(mgcv)
require(itsadug)
require(methods)
require(effects)
require(RePsychLing)
require(knitr)
require(broom)

# potom proverimo sta nam je radni direktorijum
getwd()

# ako nije onaj koji zelimo, onda podesimo da bude takav (najzgodnije je da to bude onaj u kojem se nalazi fajl sa podacima)

setwd("C:/Sam/Dokumenti/Obuke/LEPlmer2021")

# da proverimo da li smo uspeli
getwd()


# Potom učitamo i razgledamo podatke

dat=read.table("FilipovicDurdevic.Kostic.SerbianPolysemy_short.txt",sep="\t",T)

dim(dat)

# [1] 27600    22

colnames(dat)

head(dat)

str(dat)

# ja volim da ono što će mi biti kategorijalni prediktor, odmah stavim u odgovarajući format

dat$Participant = as.factor(dat$Participant)
dat$Word = as.factor(dat$Word)

# pogledamo ponovo
str(dat)

# Proverimo da li ima ispitanika koji su pravili previše grešaka

sort(tapply(dat$Error_code, dat$Participant, sum)/150)


# na osnovu toga isključimo jednog ispitanika

dat=dat[dat$Participant!="Participant_57",]

dim(dat)




# Proverimo da li ima reči sa velikim brojem grešaka

sort(tapply(dat$Error_code, dat$Word, sum)/30)



# na osnovu toga isključimo nekoliko reči

dat=dat[dat$Word!="PISAK",]
dat=dat[dat$Word!="SFERA",]
dat=dat[dat$Word!="PATENT",]

dim(dat)


# Zadržimo samo tačne odgovore

dat=dat[dat$Error_code == "1",]
dim(dat)


# izbacili smo oko 5-6% podataka


# Vizuelna inspekcija distribucije RT

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))



# Isključimo nekoliko veoma (besmisleno) brzih odgovora

dat=dat[dat$RT>299,]
dim(dat)


# Proverimo koja transformacija RT je preporučena

powerTransform(dat$RT)



# Pošto je skor bliži -1 nego 0, primenimo inverznu transformaciju

dat$RT=-1000/dat$RT


# Pogledamo kako je distribuirano novo RT

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))


# Možemo i da testiramo normalnost

shapiro.test(dat$RT)
ks.test(jitter(dat$RT),"pnorm",mean(dat$RT),sd(dat$RT))




# Odmah definišemo i inverznu funkciju, koja će nam biti potrebna za plotovanje RT

trans <- function(x){
  rezultat <- -1000/x
  return(rezultat)
}


# pošto je glavna varijabla u ovoj analizi H
# iskoristićemo nju da ilustrujemo kako se koristi funkcija trans
# istovremeno, ovde već vidimo da ovaj efekat može biti nelinearan

ggplot(dat, aes(x=H, y=trans(RT))) + 
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)



# Logaritmujemo frekvenciju

dat$freko=log(dat$freko)


# Ispitamo kolinearnost među prediktorima

collin.fnc(dat[,c("len", "ort", "freko", "fam","conc", "H" )])$cnumber

# [1] 63.77733

# Kapa koeficijent je ogroman.
# 0-6 nizak
# 15-29 umeren
# > 29 visok



# Pošto je Kappa koeficijent katastrofalno veliki, moraćemo da rešimo problem kolinearnosti. Prvo da pogledamo kako izgleda odnos između varijabli

pairscor.fnc(dat[,c("RT", "len", "ort", "freko", "fam","conc", "H")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")


# jedna varijanta je da izaberemo prediktore koji su najuticajniji
# ili da iz svake grupe srodnih prediktora izaberemo jedan
# ili da od nekoliko srodnih napravimo novi skor
# ili da izvedemo analizu glavnih komponenti 


# za početak, da vidimo kako se rangiraju prediktori po svom uticaju 
# na zavisnu varijablu

# randomForest
rf1 <- randomForest(RT ~ ., data = dat[,c("RT", "len", "ort", "freko", "fam","conc", "H")], importance = TRUE)

varImpPlot(rf1)

importance(rf1)

#        %IncMSE IncNodePurity
# len   34.55021      13.50774
# ort   48.51037      21.38270
# freko 59.45814      57.58771
# fam   80.89769      79.10652
# conc  56.78653      30.48162
# H     57.38130      42.21663



# Gradient Boosting Machines

dat.gbm = dat[,c("RT", "len", "ort", "freko", "fam","conc", "H")]

set.seed(125)
gbm1 = summary(gbm1 <- gbm(RT ~
                             len + ort + freko + fam + conc + H,
                           data=dat.gbm, n.trees=500, interaction.depth=3, shrinkage=0.001, cv.folds=5))

gbm1

# var     rel.inf
# fam     fam 62.94603535
# freko freko 19.54703530
# conc   conc  8.67366718
# H         H  8.56610420
# len     len  0.23940410
# ort     ort  0.02775386


# pošto u ovoj analizi želimo da pokažemo da H radi
# preko kontrolnih, poznatih varijabli
# biramo drugi pristup, glavne komponente

# izvodimo analizu glavnih komponenti na skupu kontrolnih varijabli

dat.pca = prcomp(dat[,c("len", "ort","freko","fam", "conc")],
                 center = TRUE,
                 scale. = TRUE)


summary(dat.pca)

# Importance of components:
#                           PC1    PC2    PC3    PC4     PC5
# Standard deviation     1.3031 1.1068 1.0039 0.7688 0.69150
# Proportion of Variance 0.3396 0.2450 0.2016 0.1182 0.09564
# Cumulative Proportion  0.3396 0.5846 0.7862 0.9044 1.00000

# da plotujemo varijansu

plot(dat.pca, type = "l")

# da plotujemo procenat objašnjene varijanse

props = round((dat.pca$sdev ^ 2 / sum(dat.pca$sdev ^ 2)), 3)
barplot(props, col = as.numeric (props > 0.05), 
        xlab = "glavne komponente",
        ylab = "proporcija objasnjene varijanse")
abline (h = 0.05)

# svi objašnjavaju barem 5% varijanse
# zadržavamo ih sve

# da vidimo loadings
dat.pca$rotation

#           PC1         PC2        PC3        PC4         PC5
# len    0.5346042 -0.28910914 -0.3088105 0.73087223  0.03280387
# ort   -0.4333843  0.51325065  0.2999534 0.65457946 -0.17408471
# freko -0.5308631 -0.03568815 -0.5762546 0.10325227  0.61170279
# fam   -0.4648419 -0.55868469 -0.2163819 0.05673804 -0.64942522
# conc  -0.1687682 -0.58274132  0.6601315 0.15320336  0.41555386



# pišemo koordinate svih reči u prostoru novih dimenzija
# pravimo nove, nekolinearne kontrolne varijable

dat$pc_1 = dat.pca$x[,1]
dat$pc_2 = dat.pca$x[,2]
dat$pc_3 = dat.pca$x[,3]
dat$pc_4 = dat.pca$x[,4]
dat$pc_5 = dat.pca$x[,5]



# Ispitamo kolinearnost u novom setu

collin.fnc(dat[,c("pc_1","pc_2","pc_3", "pc_4","pc_5", "H" )])$cnumber

# [1] 10.31309

#- ovo je već prihvatljivije

pairscor.fnc(dat[,c("RT", "pc_1","pc_2","pc_3", "pc_4","pc_5", "H")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")

# vidimo da je H slabo-umereno korelirana sa nekim od komponenti
# međutim, ostavićemo ovako, Kappa koeficijent je umeren, prihvatljivije
# a mi želimo da pokažemo da H radi preko kontrolnih



# Standardizujemo prediktore

dat$trial.z = scale(dat$Trial_order)
dat$freko.z = scale(dat$freko)
dat$len.z = scale(dat$len)
dat$fam.z = scale(dat$fam)
dat$conc.z = scale(dat$conc)
dat$ort.z = scale(dat$ort)
dat$N.z = scale(dat$N)
dat$T.z = scale(dat$T)
dat$H.z = scale(dat$H)
dat$PC1 = scale(dat$pc_1)
dat$PC2 = scale(dat$pc_2)
dat$PC3 = scale(dat$pc_3)
dat$PC4 = scale(dat$pc_4)
dat$PC5 = scale(dat$pc_5)


# pogledamo kolinearnost posle skaliranja

collin.fnc(dat[,c("PC1","PC2","PC3", "PC4","PC5", "H.z" )])$cnumber

# [1] 1.474568



# pored Kappa koeficijenta, može da se koristi i 
# VIF (variance inflation factor)



# SAD MOŽEO DA PREĐEMO NA FITOVANJE MODELA

# VAŽNA NAPOMENA
# STATISTIČKO MODELOVANJE JE KOMBINACIJA NAUKE I VEŠTINE
# NIKADA NEĆETE DO KRAJA SAVLADATI TEHNIKU
# UVEK SE POJAVLJUJU NOVE PREOPORUKE
# I NOVA "PRAVILA" KOJA TREBA POŠTOVATI
# KAO I NOVI NAČINI DA SE TO UČINI




# STRATEGIJA 1
# krenemo od najjednostavnijeg modela
# dodajemo jedan po jedan parametar
# poredimo modele
# ako nema razlike, parametar nije opravdan
# ako ima razlike, parametar jeste opravdan, zadržavamo ga




lmer0a <- lmer(RT ~ 1 + 
                 (1 |Participant),
               data=dat, REML=FALSE)

lmer0b <- lmer(RT ~ 1 + 
                 (1 |Word),	
               data=dat, REML=FALSE)


lmer0 <- lmer(RT ~ 1 + 
                (1 | Participant) + 
                (1 | Word),	
              data=dat, REML=FALSE)

anova(lmer0a, lmer0)



anova(lmer0b, lmer0)



# dodajemo redosled izlaganja među prediktore
# prvo vizualizujemo, 
# potom pravimo model 
# i proveravamo da li je opravdano ukljucivanje tog parametra


ggplot(dat, aes(x=trial.z, y=RT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Participant) 



lmer1a <- lmer(RT ~ trial.z + 
                 (1 | Participant) + 
                 (1 | Word),	
               data=dat, REML=FALSE)
anova(lmer0, lmer1a)



# dozvoljavamo redosledu izlaganja da ima razlicit efekat 
# kod razlicitih ispitanika


lmer1b <- lmer(RT ~ trial.z + 
                 (1 + trial.z | Participant) + 
                 (1 | Word),	
               data=dat, REML=FALSE)		
anova(lmer1b, lmer1a)


# proveravamo da li je opravdana korelacija 
# između dva parametra u random interakcijama
# takok sto kreiramo model bez korelacije
# pa uporedimo fitove


lmer1bb <- lmer(RT ~ trial.z + 
                  (1 + trial.z || Participant) + 
                  (1 | Word),	
                data=dat, REML=FALSE)		
anova(lmer1b, lmer1bb)		



# nije opravdana korelacija





# slučajna interakcija redosleda sa rečima

ggplot(dat, aes(x=trial.z, y=RT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Word) 

# ako se ne vidi lepo slika, možemo da je sačuvamo pa gledamo otvoreni fajl

ggsave("random.trial.word.jpg", width = 25, height = 25)


lmer1c <- lmer(RT ~ trial.z + 
                 (1 + trial.z || Participant) + 
                 (1 + trial.z | Word),	
               data=dat, REML=FALSE)		

anova(lmer1bb, lmer1c)


# nije opravdana, vraćamo se na prethodni model, lmer1bb




# dodajem prvi sledeći prediktor
# i proveravamo da li je opravdano njegovo uključicanje u model
# tj. da li doprinosi objašnjenoj varijansi

lmer2 <- lmer(RT ~ trial.z + PC1 +
                (1 + trial.z || Participant) + 
                (1 | Word),	
              data=dat, REML=FALSE)		

anova(lmer1bb, lmer2)



# dodajemo ga u slučajne efekte

lmer2a <- lmer(RT ~ trial.z + PC1 +
                 (1 + PC1 | Participant) + (0 + trial.z | Participant) +
                 (1 | Word),	
               data=dat, REML=FALSE)		

anova(lmer2, lmer2a)


# da li je opravdana korelacija u slučajnim

lmer2b <- lmer(RT ~ trial.z + PC1 +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),	
               data=dat, REML=FALSE)		

anova(lmer2a, lmer2b)


# nije opravdana



# testiramo njegovu nelinearnost

lmer2c <- lmer(RT ~ trial.z + poly(PC1,2) +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),	
               data=dat, REML=FALSE)		

anova(lmer2b, lmer2c)


# i tako redom za sve prediktore, gradimo model
# i proveravamo da li je dodatna komplikacija opravdana podacima

# ako uđemo u zonu u kojoj konvergencija više nije moguća
# pojednostavimo strukturu slučajnih efekata
# kako bismo testirali fiksne
# pa kad napravimo konačnu strukturu fiksnih
# ponovimo testiranje sa dodatom složenijom strukturom slučajnih efekata



# sad ćemo, potkrepljenja radi odmah dodati H


lmer3a <- lmer(RT ~ trial.z + PC1 + H.z +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer2b, lmer3a)


# dodajemo ga u slučajne efekte

lmer3b <- lmer(RT ~ trial.z + PC1 + H.z +
                 (1 + H.z| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3a, lmer3b)


# da li je opravdana korelacija

lmer3c <- lmer(RT ~ trial.z + PC1 + H.z +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) + (0 + H.z | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3b, lmer3c)


# ne, ali ga ostavljamo za sada, jer je to naš ključni prediktor



# testiramo njegovu nelinearnost

lmer3d <- lmer(RT ~ trial.z + PC1 + poly(H, 2) +
                 (1 + H.z| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3c, lmer3d)


# nije nelinearan

# MODEL JE VEĆ PRIJAVIO DA IMA PROBLEMA SA KONFERGENCIJOM
# TO ZNAČI DA SMO VEĆ PREOPTERETILI SLUČAJNIM EFEKTIMA...
# A TEK SMO POČELI


# testiramo interakciju sa fiksnim prediktorima

lmer3e <- lmer(RT ~ trial.z * H.z + PC1 +
                 (1 + H.z| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3c, lmer3e)

# ova interakcija nije opravdana



lmer3f <- lmer(RT ~ trial.z + PC1 * H.z +
                 (1 + H.z| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3c, lmer3f)

# ova interakcija jeste opravdana, za sada je najbolji model lmer3f



# tek kad smo pokazali da neki prediktor doprinosi objašnjenoj varijansi
# smemo da pogledamo da li je koeficijent stastistički značajan

summary(lmer3f)




##########################################################		
##########################################################		
# STRATEGIJA 2 
# krenemo od najkomplikovanijeg smislenog modela
# izbacujemo jedan po jedan parametar
# poredimo modele
# ako nema razlike, taj parametar nije opravdan
# ako ima razlike, taj parametar je opravdan, zadržavamo ga

lmerXa <- lmer(RT ~ trial.z * PC1 * H.z +
                 (1 + trial.z * PC1 * H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

# prvo da pokušamo da pojednostavimo strukturu slučajnih efekata

summary(rePCA(lmerXa))

# ovo neće da konvergira	

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ trial.z * PC1 * H.z + (1 + trial.z * PC1 * H.z | Participant) +      (1 + trial.z | Word)
#    Data: dat
# 
#      AIC      BIC   logLik deviance df.resid 
#    510.5    815.6   -207.2    414.5     4209 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -6.5823 -0.6445 -0.0330  0.6165  3.9825 
# 
# Random effects:
#  Groups      Name            Variance  Std.Dev. Corr                                     
#  Word        (Intercept)     4.033e-03 0.063503                                          
#              trial.z         1.696e-06 0.001302 -1.00                                    
#  Participant (Intercept)     1.840e-02 0.135647                                          
#              trial.z         8.027e-04 0.028331 -0.23                                    
#              PC1             7.082e-05 0.008415  0.21 -0.05                              
#              H.z             1.453e-04 0.012053  0.57 -0.39  0.85                        
#              trial.z:PC1     6.039e-05 0.007771  0.22 -0.59  0.74  0.88                  
#              trial.z:H.z     1.924e-04 0.013870  0.04  0.28 -0.78 -0.54 -0.53            
#              PC1:H.z         1.426e-04 0.011944  0.34 -0.83  0.32  0.69  0.86 -0.17      
#              trial.z:PC1:H.z 2.325e-05 0.004822 -0.72 -0.31 -0.07 -0.12  0.36  0.01  0.38
#  Residual                    5.975e-02 0.244432                                          
# Number of obs: 4257, groups:  Word, 147; Participant, 30
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      -1.594071   0.025632  32.732313 -62.192  < 2e-16 ***
# trial.z           0.012125   0.007064  40.462718   1.716 0.093725 .  
# PC1               0.045003   0.006857 120.247816   6.563  1.4e-09 ***
# H.z              -0.026464   0.006942 108.768811  -3.812 0.000229 ***
# trial.z:PC1       0.001842   0.004952 143.912126   0.372 0.710522    
# trial.z:H.z      -0.005208   0.005398  50.465650  -0.965 0.339271    
# PC1:H.z          -0.020515   0.007461 113.765297  -2.750 0.006942 ** 
# trial.z:PC1:H.z   0.001466   0.005212 442.930638   0.281 0.778569    
# ---


#-----		

# vracamo se na jednostavniju strukturu slučajnih
# da olakšamo stvar

lmerX <- lmer(RT ~ trial.z * PC1 * H.z +
                 (1 |Participant) + 
                 (1 |Word),	
               data=dat, REML=FALSE)


# izbacimo jedan parametar, trostruku interakciju, pa napravimo novi model

lmerX1 <- lmer(RT ~ trial.z * PC1 + 
                    trial.z * H.z +
                    PC1 * H.z +
                    (1 |Participant) + 
                    (1 |Word),	
                    data=dat, REML=FALSE)


anova(lmerX1, lmerX)

# nije opravdana trostruka interakcija


# sad iizbacujem po jednu dvostruku

lmerX2 <- lmer(RT ~ trial.z * PC1 + 
                 trial.z * H.z +
                 (1 |Participant) + 
                 (1 |Word),	
               data=dat, REML=FALSE)


anova(lmerX1, lmerX2)

# interakcija PC1 i H.z je opravdana


lmerX3 <- lmer(RT ~ trial.z * PC1 + 
                 PC1 * H.z +
                 (1 |Participant) + 
                 (1 |Word),	
               data=dat, REML=FALSE)


anova(lmerX1, lmerX3)

# interakcija trial.z i H.z nije opravdana


lmerX4 <- lmer(RT ~ 
                 trial.z * H.z +
                 PC1 * H.z +
                 (1 |Participant) + 
                 (1 |Word),	
               data=dat, REML=FALSE)


anova(lmerX1, lmerX4)

# interakcija trial.z i PC1 nije opravdana


# -----------------------
# na osnovu ovih poređenja ostavljamo u modelu
# samo one parametre čije izbacivanje pravi razliku

# ostavljam samo interakciju koja je opravdana



lmerX5 <- lmer(RT ~ trial.z + 
                 PC1 * H.z +
                 (1 |Participant) + 
                 (1 |Word),	
               data=dat, REML=FALSE)


anova(lmerX1, lmerX5)

# nema razlike, dakle jednostavniji model lmerX5 je podjednako uspešan 
# kao komplikovani lmerX1




# proverimo da li su preostali bez interakcije potrebni
# šta je sa glavnim efektom trial.z


lmerX6 <- lmer(RT ~ 
                 PC1 * H.z +
                 (1 |Participant) + 
                 (1 |Word),	
               data=dat, REML=FALSE)


anova(lmerX5, lmerX6)

# opravdan je
# i dalje je najbolji model lmerX5


# sada vraćam slučajne interakcije

lmerY <- lmer(RT ~ trial.z +
                 PC1 * H.z +
                 (1 + trial.z + PC1 * H.z | Participant) + 
                 (1 + trial.z | Word),	
               data=dat, REML=FALSE)


anova(lmerX6, lmerY)

# sve ove slučajne interakcije doprinose zajedno
# ali
# da proverimo da li su svi slučajni efekti opravdani
# AKO MODEL NEĆE DA KONVERGIRA, PRVO IZBACIVATI KORELACIJE IZMEĐU SLUČAJNIH EFEKATA

summary(rePCA(lmerY))

# $Word
# Importance of components:
#                          [,1]      [,2]
# Standard deviation     0.2547 4.855e-05
# Proportion of Variance 1.0000 0.000e+00
# Cumulative Proportion  1.0000 1.000e+00
# 
# $Participant
# Importance of components:
#                          [,1]    [,2]    [,3]      [,4] [,5]
# Standard deviation     0.5542 0.12173 0.04132 6.485e-06    0
# Proportion of Variance 0.9489 0.04579 0.00527 0.000e+00    0
# Cumulative Proportion  0.9489 0.99473 1.00000 1.000e+00    1


# VIDIMO DA JE U SLUČAJU REČI KOMPLETNA VARIJANSA 
# OBJAŠNJENA VEĆ SA JEDNOM GLAVNOM KOMPONENTOM,
# A U SLUČAJU ISPITANIKA VEĆ SA 3 GLAVNE KOMPONENTE
# TO ZNAČI DA JE OPRAVDANO IZBACITI 2 PARAMETARA IZ STRUKTURE SLUČAJNIH
# EFEKATA PO ISPITANICIMA
# TAKOĐE, OPRAVDANO JE ISKLJUČITI JEDAN PARAMETAR IZ STRUKTURE SLUČAJNIH EFEKATA
# PO REČIMA

# po WORD
# dovoljan jedan, zadržavamo onaj sa većom varijasnom

# po PARTICIPANT
# dovoljna 3
# izbacujemo dva sa najmanjom varijansom


# PRIKAŽEMO KOEFICIJENTE DA BISMO DETEKTOVALI KOMPONENTE SA NAJMANJOM VARIJANSOM
# KOJE ĆEMO IZBACITI

lmerY

# Random effects:
# Groups      Name        Std.Dev. Corr                   
# Word        (Intercept) 0.062482                        
#             trial.z     0.001096 -1.00                  
# Participant (Intercept) 0.135619                        
#             trial.z     0.028617 -0.19                  
#             PC1         0.006151  0.31 -0.19            
#             H.z         0.010234  0.64 -0.42  0.90      
#             PC1:H.z     0.011264  0.34 -0.88  0.63  0.78
# Residual                0.245344                        



lmerY1 <- lmer(RT ~ trial.z +
                PC1 * H.z +
                (1 + trial.z + H.z | Participant) + 
                (1 | Word),	
              data=dat, REML=FALSE)


anova(lmerY, lmerY1)

# nema razlike između dva modela
# dakle, komplikacije su bile nepotrebne

# konačni model je lmerY1

######################################################
######################################################
######################################################

# tek sad smemo da pogledamo koeficijente

summary(lmerY1)







# treba još proveriti da li su efekti nelinearni


###################################################
###################################################

# na kraju prelazimo na dijagnostiku

confint(lmerY1, method="Wald")

# Vidimo da se naši efekti uvek nalaze sa iste strane nule
# osim efekta redosleda, koji nije baš pouzdan

# Sada ćemo da proverimo da li su prekršeni 
# neki od preduslova za primenu linernog modela:


# Napravimo kolonu sa predviđenim vrednostima ZV:

dat$RT.fitted = predict(lmerY1)

# Napravimo kolonu sa rezidualima:

dat$RT.res = residuals(lmerY1)

# Plotujemo korelaciju između fitovanih vrednosti i reziduala
# Da proverimo da li postoji homogenost varijanse
# Ovo treba da bude jedno lepo “jaje”

ggplot(dat, aes(x=RT.fitted, y=RT.res)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) 


# Da proverimo da li se reziduali normalno distribuiraju
# Ovo treba da bude što sličnije ravnoj liniji:
  
ggplot(dat, aes(sample=RT.res)) +
  stat_qq() + stat_qq_line()



# Isto to može i ovako:
  
par(mfcol=c(1,2))
qqnorm(resid(lmerY1))
plot(fitted(lmerY1), resid(lmerY1))
par(mfcol=c(1,1))


#################################################################

# Sad ćemo da izbacimo tačke sa velikim rezidualima
# I da proverimo da li utiču previše na model
# Refitujemo model na podskupu tačaka čiji su reziduali unutar opsega +/-2.5 sigme


lmerY1t <- lmer(RT ~ trial.z +
                 PC1 * H.z +
                 (1 + trial.z + H.z | Participant) + 
                 (1 | Word),	
                 data=dat, 
                 subset=abs(scale(resid(lmerY1)))<2.5,
                 REML=FALSE)


# Uporedimo koeficijente iz pročišćenog i originalnog modela
# ne bi trebalo da se dese drastične promene

summary(lmerY1t)

# ponovimo dijagnostiku 
# sad bi trebalo da je situacija još bolja

par(mfcol=c(1,2))
qqnorm(resid(lmerY1t))
plot(fitted(lmerY1t), resid(lmerY1t))
par(mfcol=c(1,1))



# viizualizujemo finalno efekat glavne varijable

ggplot(dat, aes(x=H, y=trans(RT.fitted))) + 
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)


ggsave("H.lmerY1.jpg", width = 5, height = 5)

# vizualizujemo interakciju


quantiles <- quantile(dat$PC1, prob = seq(0, 1, length = 4), type = 5)
dat$cutted <- cut2(dat$PC1, cuts = as.numeric(quantiles))


slika.interakcija.H.PC1.lmerY1 = ggplot(dat, aes(H.z, trans(RT.fitted), colour=cutted)) +
  theme_bw()+
  geom_point(alpha=0.1) +
  scale_colour_discrete(name  ="PC1 split", labels=c("Low", "Average", "High"))+	
  xlab("Entropy") + ylab("Predicted RT (ms)") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20))+
  stat_smooth(method = lm, formula = y ~ x, size = 2)

slika.interakcija.H.PC1.lmerY1

ggsave("H.PC1.int.lmerY1.jpeg", slika.interakcija.H.PC1.lmerY1, width = 5, height = 5)


##################


