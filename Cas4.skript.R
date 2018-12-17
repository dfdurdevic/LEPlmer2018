

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




# Potom učitamo i razgledamo podatke

dat=read.table("FilipovicDurdevic.Kostic.SerbianPolysemy.txt",sep="\t",T)

dim(dat)

# [1] 27600    22

colnames(dat)

head(dat)

str(dat)



# Proverimo da li ima ispitanika koji su pravili previše grešaka

sort(tapply(dat$Error_code, dat$Participant, sum)/150)


# na osnovu toga isključimo jednog ispitanika

dat=dat[dat$Participant!="Participant_57",]

dim(dat)

#[1] 27450    22


# Proverimo da li ima reči sa velikim brojem grešaka

sort(tapply(dat$Error_code, dat$Word, sum)/183)



# na osnovu toga isključimo nekoliko reči

dat=dat[dat$Word!="PISAK",]
dat=dat[dat$Word!="SFERA",]
dat=dat[dat$Word!="BERBA",]
dat=dat[dat$Word!="PATENT",]

dim(dat)

# [1] 26718    22

(27450 - 26718)/27450

# [1] 0.02666667




# Zadržimo samo tačne odgovore

dat=dat[dat$Error_code == "1",]
dim(dat)

#[1] 25758    22

(27450 - 25758)/27450

# [1] 0.06163934
# izbacili smo oko 6% podataka


# Vizuelna inspekcija distribucije RT

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))



# Isključimo nekoliko veoma (besmisleno) brzih odgovora

dat=dat[dat$RT>299,]
dim(dat)

# [1] 25752    22

# Proverimo koja transformacija RT je preporučena

powerTransform(dat$RT)



# Primenimo inverznu transformaciju

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

# [1] 64.12669

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
# PC1    PC2    PC3    PC4     PC5
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

# PC1         PC2        PC3        PC4         PC5
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

# Models:
# lmer0a: RT ~ 1 + (1 | Participant)
# lmer0: RT ~ 1 + (1 | Participant) + (1 | Word)
# Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer0a  3 10709.5 10734.0 -5351.7  10703.5                             
# lmer0   4  8486.3  8518.9 -4239.1   8478.3 2225.2      1  < 2.2e-16 ***


anova(lmer0b, lmer0)

# lmer0b: RT ~ 1 + (1 | Word)
# lmer0: RT ~ 1 + (1 | Participant) + (1 | Word)
# Df     AIC     BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
# lmer0b  3 16578.3 16602.8 -8286.1  16572.3                            
# lmer0   4  8486.3  8518.9 -4239.1   8478.3  8094      1  < 2.2e-16 ***


# dodajemo redosled izlaganja među prediktore

ggplot(dat, aes(x=trial.z, y=RT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Participant) 



lmer1a <- lmer(RT ~ trial.z + 
                 (1 | Participant) + 
                 (1 | Word),	
               data=dat, REML=FALSE)
anova(lmer0, lmer1a)

# Models:
# lmer0: RT ~ 1 + (1 | Participant) + (1 | Word)
# lmer1a: RT ~ trial.z + (1 | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmer0   4 8486.3 8518.9 -4239.1   8478.3                         
# lmer1a  5 8486.0 8526.7 -4238.0   8476.0 2.3347      1     0.1265


lmer1b <- lmer(RT ~ trial.z + 
                 (1 + trial.z | Participant) + 
                 (1 | Word),	
               data=dat, REML=FALSE)		
anova(lmer1b, lmer1a)

# Models:
# lmer1a: RT ~ trial.z + (1 | Participant) + (1 | Word)
# lmer1b: RT ~ trial.z + (1 + trial.z | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer1a  5 8486.0 8526.7 -4238.0   8476.0                             
# lmer1b  7 8192.2 8249.3 -4089.1   8178.2 297.73      2  < 2.2e-16 ***



lmer1bb <- lmer(RT ~ trial.z + 
                  (1 + trial.z || Participant) + 
                  (1 | Word),	
                data=dat, REML=FALSE)		
anova(lmer1b, lmer1bb)		

# Models:
# lmer1bb: RT ~ trial.z + (1 + trial.z || Participant) + (1 | Word)
# lmer1b: RT ~ trial.z + (1 + trial.z | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmer1bb  6 8190.2 8239.2 -4089.1   8178.2                         
# lmer1b   7 8192.2 8249.3 -4089.1   8178.2 0.0119      1     0.9132

# nije opravdana korelacija


# slučajna interakcija redosleda sa rečima

lmer1c <- lmer(RT ~ trial.z + 
                 (1 + trial.z || Participant) + 
                 (1 + trial.z | Word),	
               data=dat, REML=FALSE)		

anova(lmer1bb, lmer1c)

# Models:
# lmer1bb: RT ~ trial.z + (1 + trial.z || Participant) + (1 | Word)
# lmer1c: RT ~ trial.z + (1 + trial.z || Participant) + (1 + trial.z | 
# lmer1c:     Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmer1bb  6 8190.2 8239.2 -4089.1   8178.2                         
# lmer1c   8 8190.4 8255.6 -4087.2   8174.4 3.8808      2     0.1436

# nije opravdana, vraćamo se na prethodni model, lmer1bb




# dodajem prvi sledeći prediktor

lmer2 <- lmer(RT ~ trial.z + PC1 +
                (1 + trial.z || Participant) + 
                (1 | Word),	
              data=dat, REML=FALSE)		

anova(lmer1bb, lmer2)

# lmer1bb: RT ~ trial.z + (1 + trial.z || Participant) + (1 | Word)
# lmer2: RT ~ trial.z + PC1 + (1 + trial.z || Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer1bb  6 8190.2 8239.2 -4089.1   8178.2                             
# lmer2    7 8150.0 8207.1 -4068.0   8136.0 42.234      1  8.097e-11 ***



# dodajemo ga u slučajne efekte

lmer2a <- lmer(RT ~ trial.z + PC1 +
                 (1 + PC1 | Participant) + (0 + trial.z | Participant) +
                 (1 | Word),	
               data=dat, REML=FALSE)		

anova(lmer2, lmer2a)

# lmer2: RT ~ trial.z + PC1 + (1 + trial.z || Participant) + (1 | Word)
# lmer2a: RT ~ trial.z + PC1 + (1 + PC1 | Participant) + (0 + trial.z | 
# lmer2a:     Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer2   7 8150.0 8207.1 -4068.0   8136.0                             
# lmer2a  9 8138.4 8211.8 -4060.2   8120.4 15.646      2  0.0004004 ***


# da li je opravdana korelacija u slučajnim

lmer2b <- lmer(RT ~ trial.z + PC1 +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),	
               data=dat, REML=FALSE)		

anova(lmer2a, lmer2b)

# Models:
# lmer2b: RT ~ trial.z + PC1 + (1 | Participant) + (0 + trial.z | Participant) + 
# lmer2b:     (0 + PC1 | Participant) + (1 | Word)
# lmer2a: RT ~ trial.z + PC1 + (1 + PC1 | Participant) + (0 + trial.z | 
# lmer2a:     Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmer2b  8 8136.4 8201.6 -4060.2   8120.4                         
# lmer2a  9 8138.4 8211.8 -4060.2   8120.4 0.0352      1     0.8511

# nije opravdana



# testiramo njegovu nelinearnost

lmer2c <- lmer(RT ~ trial.z + poly(PC1,2) +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),	
               data=dat, REML=FALSE)		

anova(lmer2b, lmer2c)

# Models:
# lmer2b: RT ~ trial.z + PC1 + (1 | Participant) + (0 + trial.z | Participant) + 
# lmer2b:     (0 + PC1 | Participant) + (1 | Word)
# lmer2c: RT ~ trial.z + poly(PC1, 2) + (1 | Participant) + (0 + trial.z | 
# lmer2c:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# lmer2b  8 8136.4 8201.6 -4060.2   8120.4                           
# lmer2c  9 8135.5 8208.9 -4058.7   8117.5 2.8992      1    0.08862 .



# dodajemo sledeći

lmer3a <- lmer(RT ~ trial.z + PC1 + PC2 +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer2b, lmer3a)

# Models:
# lmer2b: RT ~ trial.z + PC1 + (1 | Participant) + (0 + trial.z | Participant) + 
# lmer2b:     (0 + PC1 | Participant) + (1 | Word)
# lmer3a: RT ~ trial.z + PC1 + PC2 + (1 | Participant) + (0 + trial.z | 
# lmer3a:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmer2b  8 8136.4 8201.6 -4060.2   8120.4                             
# lmer3a  9 8123.8 8197.2 -4052.9   8105.8 14.607      1  0.0001324 ***




# dodajemo ga u slučajne efekte

lmer3b <- lmer(RT ~ trial.z + PC1 + PC2 +
                 (1 + PC2| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3a, lmer3b)

# Models:
# lmer3a: RT ~ trial.z + PC1 + PC2 + (1 | Participant) + (0 + trial.z | 
# lmer3a:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# lmer3b: RT ~ trial.z + PC1 + PC2 + (1 + PC2 | Participant) + (0 + trial.z | 
# lmer3b:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# lmer3a  9 8123.8 8197.2 -4052.9   8105.8                            
# lmer3b 11 8117.9 8207.6 -4047.9   8095.9 9.9125      2   0.007039 **


# da li je opravdana korelacija

lmer3c <- lmer(RT ~ trial.z + PC1 + PC2 +
                 (1 | Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) + (0 + PC2 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3b, lmer3c)

# Models:
# lmer3c: RT ~ trial.z + PC1 + PC2 + (1 | Participant) + (0 + trial.z | 
# lmer3c:     Participant) + (0 + PC1 | Participant) + (0 + PC2 | Participant) + 
# lmer3c:     (1 | Word)
# lmer3b: RT ~ trial.z + PC1 + PC2 + (1 + PC2 | Participant) + (0 + trial.z | 
# lmer3b:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# lmer3c 10 8125.3 8206.9 -4052.7   8105.3                            
# lmer3b 11 8117.9 8207.6 -4047.9   8095.9 9.4641      1   0.002095 **

# jeste opravdana, lmer3b je bolji



# testiramo njegovu nelinearnost

lmer3d <- lmer(RT ~ trial.z + PC1 + poly(PC2, 2) +
                 (1 + PC2| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3b, lmer3d)

# Models:
# lmer3b: RT ~ trial.z + PC1 + PC2 + (1 + PC2 | Participant) + (0 + trial.z | 
# lmer3b:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# lmer3d: RT ~ trial.z + PC1 + poly(PC2, 2) + (1 + PC2 | Participant) + 
# lmer3d:     (0 + trial.z | Participant) + (0 + PC1 | Participant) + (1 | 
# lmer3d:     Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmer3b 11 8117.9 8207.6 -4047.9   8095.9                         
# lmer3d 12 8119.8 8217.7 -4047.9   8095.8 0.0571      1     0.8111

# nije nelinearan

# OVDE JE MODEL VEĆ PRIJAVIO DA IMA PROBLEMA SA KONFERGENCIJOM
# TO ZNAČI DA SMO VEĆ PREOPTERETILI SLUČAJNIM EFEKTIMA...
# A TEK SMO POČELI


# testiramo interakciju sa fiksnim prediktorima

lmer3e <- lmer(RT ~ trial.z + PC1 * PC2 +
                 (1 + PC2| Participant) + (0 + trial.z | Participant) +
                 (0 + PC1 | Participant) +
                 (1 | Word),		
               data=dat, REML=FALSE)		

anova(lmer3b, lmer3e)

# Models:
# lmer3b: RT ~ trial.z + PC1 + PC2 + (1 + PC2 | Participant) + (0 + trial.z | 
# lmer3b:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# lmer3e: RT ~ trial.z + PC1 * PC2 + (1 + PC2 | Participant) + (0 + trial.z | 
# lmer3e:     Participant) + (0 + PC1 | Participant) + (1 | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# lmer3b 11 8117.9 8207.6 -4047.9   8095.9                           
# lmer3e 12 8115.2 8213.1 -4045.6   8091.2 4.6703      1    0.03069 *

# interakcija jeste opravdana, za sada je najbolji model lmer3e




# i tako redom za sve prediktore, gradimo model
# i proveravamo da li je dodatna komplikacija opravdana podacima

# ako uđemo u zonu u kojoj konvergencija više nije moguća
# pojednostavimo strukturu slučajnih efekata
# kako bismo testirali fiksne
# pa kad napravimo konačnu strukturu fiksnih
# ponovimo testiranje sa dodatom složenijom strukturom slučajnih efekata









##########################################################		
##########################################################		
# STRATEGIJA 2 
# krenemo od najkomplikovanijeg smislenog modela
# izbacujemo jedan po jedan parametar
# poredimo modele
# ako nema razlike, taj parametar nije opravdan
# ako ima razlike, taj parametar je opravdan, zadržavamo ga

lmerXa <- lmer(RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + PC1 + PC2 + PC3 + PC4 + PC5 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

summary(rePCA(lmerX1))

# ovo neće da konvergira	


#-----		

# vracamo se na jednostavniju strukturu slučajnih
# da olakšamo stvar

lmerX <- lmer(RT ~ 
                trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                PC2 * (PC3 + PC4 + PC5 + H) +
                PC3 * (PC4 + PC5 + H) +
                PC4 * (PC5 + H) +
                PC5 * H +
                (1 + trial.z + H.z|Participant) + 
                (1 + trial.z |Word),	
              data=dat, REML=FALSE)

# izbacimo jedan parametar, PC5 * H, pa napravimo novi model

lmerX1 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * (PC5 + H) +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)



lmerX2 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * PC5 +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)


lmerX3 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * H +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

lmerX4 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC4 + PC5) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)


lmerX5 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC4 + H) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

lmerX6 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5 + H) +
                 PC3 * (PC5 + H) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

lmerX7 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + PC5) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)


lmerX8 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC4 + H) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

lmerX9 <- lmer(RT ~ 
                 trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                 PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                 PC2 * (PC3 + PC5 + H) +
                 PC3 * (PC4 + PC5 + H) +
                 PC4 * (PC5 + H) +
                 PC5 * H +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)


lmerX10 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)

lmerX11 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)


lmerX12 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)


lmerX13 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)



lmerX14 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)



lmerX15 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)




lmerX16 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + PC5) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)




lmerX17 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC4 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)





lmerX18 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC3 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)




lmerX19 <- lmer(RT ~ 
                  trial.z * (PC1 + PC2 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)




lmerX20 <- lmer(RT ~ 
                  trial.z * (PC1 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)




lmerX21 <- lmer(RT ~ 
                  trial.z * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC1 * (PC2 + PC3 + PC4 + PC5 + H) +
                  PC2 * (PC3 + PC4 + PC5 + H) +
                  PC3 * (PC4 + PC5 + H) +
                  PC4 * (PC5 + H) +
                  PC5 * H +
                  (1 + trial.z + H.z|Participant) + 
                  (1 + trial.z |Word),	
                data=dat, REML=FALSE)

anova(lmerX1, lmerX)

# Models:
# lmerX1: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX1:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX1:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + (1 + trial.z + H.z | 
# lmerX1:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# lmerX1 38 323145 323455 -161534   323069                           
# lmerX  39 323141 323459 -161531   323063 5.7195      1    0.01678 *

anova(lmerX2, lmerX)

# Models:
# lmerX2: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX2:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX2:     (PC4 + PC5 + H) + PC4 * PC5 + PC5 * H + (1 + trial.z + H.z | 
# lmerX2:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmerX2 38 323140 323450 -161532   323064                        
# lmerX  39 323141 323459 -161531   323063 0.775      1     0.3787


anova(lmerX3, lmerX)

# Models:
# lmerX3: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX3:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX3:     (PC4 + PC5 + H) + PC4 * H + PC5 * H + (1 + trial.z + H.z | 
# lmerX3:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmerX3 38 323140 323450 -161532   323064                        
# lmerX  39 323141 323459 -161531   323063 0.969      1     0.3249


anova(lmerX4, lmerX)

# Models:
# lmerX4: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX4:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX4:     (PC4 + PC5) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX4:     H.z | Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)  
# lmerX4 38 323143 323453 -161534   323067                          
# lmerX  39 323141 323459 -161531   323063 4.273      1    0.03872 *



anova(lmerX5, lmerX)

# Models:
# lmerX5: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX5:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX5:     (PC4 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX5:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# lmerX5 38 323145 323455 -161534   323069                           
# lmerX  39 323141 323459 -161531   323063 5.9096      1    0.01506 *

anova(lmerX6, lmerX)

# Models:
# lmerX6: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX6:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX6:     (PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX6:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX6 38 323139 323449 -161532   323063                         
# lmerX  39 323141 323459 -161531   323063 0.3675      1     0.5443



anova(lmerX7, lmerX)

# Models:
# lmerX7: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX7:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5) + PC3 * (PC4 + 
# lmerX7:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX7:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX7 38 323140 323450 -161532   323064                         
# lmerX  39 323141 323459 -161531   323063 1.4745      1     0.2246


anova(lmerX8, lmerX)

# Models:
# lmerX8: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX8:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + H) + PC3 * (PC4 + 
# lmerX8:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX8:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX8 38 323141 323451 -161533   323065                         
# lmerX  39 323141 323459 -161531   323063 2.0715      1     0.1501


anova(lmerX9, lmerX)

# Models:
# lmerX9: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX9:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC5 + H) + PC3 * (PC4 + 
# lmerX9:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX9:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX9 38 323139 323449 -161532   323063                         
# lmerX  39 323141 323459 -161531   323063 0.1514      1     0.6972

anova(lmerX10, lmerX)

# Models:
# lmerX10: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX10:     PC3 + PC4 + PC5 + H) + PC2 * (PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX10:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX10:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# lmerX10 38 323141 323451 -161533   323065                        
# lmerX   39 323141 323459 -161531   323063 2.013      1      0.156



anova(lmerX11, lmerX)

# Models:
# lmerX11: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX11:     PC3 + PC4 + PC5) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX11:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX11:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX11 38 323140 323450 -161532   323064                         
# lmerX   39 323141 323459 -161531   323063 1.2157      1     0.2702


anova(lmerX12, lmerX)

# Models:
# lmerX12: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX12:     PC3 + PC4 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX12:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX12:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# lmerX12 38 323142 323452 -161533   323066                           
# lmerX   39 323141 323459 -161531   323063 2.8094      1    0.09371 .



anova(lmerX13, lmerX)

# Models:
# lmerX13: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX13:     PC3 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX13:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX13:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX13 38 323139 323449 -161532   323063                         
# lmerX   39 323141 323459 -161531   323063 0.0423      1     0.8371


anova(lmerX14, lmerX)

# Models:
# lmerX14: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX14:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX14:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX14:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX14 38 323139 323449 -161532   323063                         
# lmerX   39 323141 323459 -161531   323063 0.4114      1     0.5213



anova(lmerX15, lmerX)

# Models:
# lmerX15: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC3 + 
# lmerX15:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX15:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX15:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX15 38 323139 323449 -161532   323063                         
# lmerX   39 323141 323459 -161531   323063 0.2991      1     0.5844


anova(lmerX16, lmerX)

# Models:
# lmerX16: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5) + PC1 * (PC2 + PC3 + 
# lmerX16:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX16:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX16:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX16 38 323140 323450 -161532   323064                         
# lmerX   39 323141 323459 -161531   323063 1.0019      1     0.3169


anova(lmerX17, lmerX)

# Models:
# lmerX17: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + H) + PC1 * (PC2 + PC3 + 
# lmerX17:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX17:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX17:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX17 38 323140 323449 -161532   323064                         
# lmerX   39 323141 323459 -161531   323063 0.5332      1     0.4653


anova(lmerX18, lmerX)

# Models:
# lmerX18: RT ~ trial.z * (PC1 + PC2 + PC3 + PC5 + H) + PC1 * (PC2 + PC3 + 
# lmerX18:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX18:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX18:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# lmerX18 38 323145 323455 -161535   323069                           
# lmerX   39 323141 323459 -161531   323063 6.3051      1    0.01204 *


anova(lmerX19, lmerX)

# Models:
# lmerX19: RT ~ trial.z * (PC1 + PC2 + PC4 + PC5 + H) + PC1 * (PC2 + PC3 + 
# lmerX19:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX19:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX19:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX19 38 323140 323450 -161532   323064                         
# lmerX   39 323141 323459 -161531   323063 0.7931      1     0.3732



anova(lmerX20, lmerX)

# Models:
# lmerX20: RT ~ trial.z * (PC1 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + PC3 + 
# lmerX20:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX20:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX20:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX20 38 323139 323449 -161532   323063                         
# lmerX   39 323141 323459 -161531   323063 0.0488      1     0.8251


anova(lmerX21, lmerX)

# Models:
# lmerX21: RT ~ trial.z * (PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + PC3 + 
# lmerX21:     PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * (PC4 + 
# lmerX21:     PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + H.z | 
# lmerX21:     Participant) + (1 + trial.z | Word)
# lmerX: RT ~ trial.z * (PC1 + PC2 + PC3 + PC4 + PC5 + H) + PC1 * (PC2 + 
# lmerX:     PC3 + PC4 + PC5 + H) + PC2 * (PC3 + PC4 + PC5 + H) + PC3 * 
# lmerX:     (PC4 + PC5 + H) + PC4 * (PC5 + H) + PC5 * H + (1 + trial.z + 
# lmerX:     H.z | Participant) + (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# lmerX21 38 323140 323450 -161532   323064                         
# lmerX   39 323141 323459 -161531   323063 0.6791      1     0.4099


# -----------------------
# na osnovu ovih poređenja ostavljamo u modelu
# samo one parametre čije izbacivanje pravi razliku


lmerY <- lmer(RT ~ trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H +
                trial.z * PC4 +
                PC3 * H +
                PC5 * H +
                PC3 * PC5 +
                (1 + trial.z + H.z|Participant) + 
                (1 + trial.z |Word),	
              data=dat, REML=FALSE)


# proverimo da li su preostali bez interakcije potrebni

lmerY1 <- lmer(RT ~ trial.z + PC2 + PC3 + PC4 + PC5 + H +
                 trial.z * PC4 +
                 PC3 * H +
                 PC5 * H +
                 PC3 * PC5 +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

lmerY2 <- lmer(RT ~ trial.z + PC1 + PC3 + PC4 + PC5 + H +
                 trial.z * PC4 +
                 PC3 * H +
                 PC5 * H +
                 PC3 * PC5 +
                 (1 + trial.z + H.z|Participant) + 
                 (1 + trial.z |Word),	
               data=dat, REML=FALSE)

anova(lmerY1, lmerY)

# Models:
# lmerY1: RT ~ trial.z + PC2 + PC3 + PC4 + PC5 + H + trial.z * PC4 + PC3 * 
# lmerY1:     H + PC5 * H + PC3 * PC5 + (1 + trial.z + H.z | Participant) + 
# lmerY1:     (1 + trial.z | Word)
# lmerY: RT ~ trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H + trial.z * PC4 + 
# lmerY:     PC3 * H + PC5 * H + PC3 * PC5 + (1 + trial.z + H.z | Participant) + 
# lmerY:     (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmerY1 21 323171 323342 -161565   323129                             
# lmerY  22 323123 323302 -161539   323079 50.143      1  1.429e-12 ***


anova(lmerY2, lmerY)

# Models:
# lmerY2: RT ~ trial.z + PC1 + PC3 + PC4 + PC5 + H + trial.z * PC4 + PC3 * 
# lmerY2:     H + PC5 * H + PC3 * PC5 + (1 + trial.z + H.z | Participant) + 
# lmerY2:     (1 + trial.z | Word)
# lmerY: RT ~ trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H + trial.z * PC4 + 
# lmerY:     PC3 * H + PC5 * H + PC3 * PC5 + (1 + trial.z + H.z | Participant) + 
# lmerY:     (1 + trial.z | Word)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmerY2 21 323141 323313 -161550   323099                             
# lmerY  22 323123 323302 -161539   323079 20.417      1  6.228e-06 ***

# jesu, opravdani su	
# Naš trenutno najbolji model je lmerY

# da proverimo da li su svi slučajni efekti opravdani

summary(rePCA(lmerY))

# $`Participant`
# Importance of components:
# [,1]    [,2]    [,3]
# Standard deviation     0.5532 0.13470 0.03729
# Proportion of Variance 0.9400 0.05573 0.00427
# Cumulative Proportion  0.9400 0.99573 1.00000

# $Word
# Importance of components:
# [,1] [,2]
# Standard deviation     0.195    0
# Proportion of Variance 1.000    0
# Cumulative Proportion  1.000    1

# ovi trenutni jeu opravdani
# malo je sumnjiv poslednji kod ispitanika
# on je kandidat za izbacivanje


summary(lmerY)

# Linear mixed model fit by maximum likelihood . t-tests use
# Satterthwaite's method [lmerModLmerTest]
# Formula: RT ~ trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H + trial.z * PC4 +  
# PC3 * H + PC5 * H + PC3 * PC5 + (1 + trial.z + H.z | Participant) +  
# (1 + trial.z | Word)
# Data: dat

# AIC       BIC    logLik  deviance  df.resid 
# 323122.9  323302.4 -161539.5  323078.9     25730 

# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -3.0411 -0.6093 -0.1795  0.3882  7.6048 

# Random effects:
# Groups      Name        Variance  Std.Dev. Corr       
# Participant (Intercept)  4788.178  69.197             
# trial.z       281.027  16.764  -0.01      
# H.z            31.315   5.596  -0.45 -0.31
# Word        (Intercept)   589.031  24.270             
# trial.z         6.772   2.602  1.00       
# Residual                15668.493 125.174             
# Number of obs: 25752, groups:  Participant, 183; Word, 146

# Fixed effects:
# Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept) 669.1212    12.5189 221.7123  53.449  < 2e-16 ***
# trial.z       2.0548     1.4874 189.1500   1.381 0.168770    
# PC1          17.0286     2.1932 145.2276   7.764 1.35e-12 ***
# PC2          10.2770     2.1970 146.3841   4.678 6.54e-06 ***
# PC3          36.8624    11.0334 145.6859   3.341 0.001061 ** 
# PC4           7.6936     2.1927 144.4742   3.509 0.000600 ***
# PC5          33.2291    10.8311 145.7074   3.068 0.002571 ** 
# H           -14.2016     3.7703 152.2661  -3.767 0.000236 ***
# trial.z:PC4   2.1228     0.8115 864.5841   2.616 0.009058 ** 
# PC3:H        -7.7791     3.7228 145.6471  -2.090 0.038392 *  
# PC5:H       -10.6097     3.7364 145.4650  -2.840 0.005165 ** 
# PC3:PC5       6.4611     2.4667 145.2901   2.619 0.009745 ** 



# Ipak, treba proveriti punu strukturu slučajnih!

# sada dodajemo punu strukturu u random deo
# pa proveravamo da li su svi opravdani


lmerZ <- lmer(RT ~ trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H.z +
                trial.z * PC4 +
                PC3 * H +
                PC5 * H +
                PC3 * PC5 +
                (1 + trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H.z + PC3 * H.z + PC5 * H.z + PC3 * PC5 |Participant) + 
                (1 + trial.z |Word),	
              data=dat, REML=FALSE)

summary(rePCA(lmerZ))

# $`Participant`
# Importance of components:
# [,1]    [,2]    [,3]    [,4]    [,5]    [,6]
# Standard deviation     0.5535 0.13612 0.06345 0.05114 0.04526 0.03459
# Proportion of Variance 0.9151 0.05535 0.01203 0.00781 0.00612 0.00357
# Cumulative Proportion  0.9151 0.97047 0.98249 0.99031 0.99643 1.00000
# [,7]     [,8]      [,9]     [,10] [,11]
# Standard deviation     0.0002262 5.12e-06 2.898e-06 4.316e-09     0
# Proportion of Variance 0.0000000 0.00e+00 0.000e+00 0.000e+00     0
# Cumulative Proportion  1.0000000 1.00e+00 1.000e+00 1.000e+00     1

# $Word
# Importance of components:
# [,1] [,2]
# Standard deviation     0.1965    0
# Proportion of Variance 1.0000    0
# Cumulative Proportion  1.0000    1


# VIDIMO DA JE U SLUČAJU ISPITANIKA KOMPLETNA VARIJANSA 
# OBJAŠNJENA VEĆ SA 6 GLAVNIH KOMPONENTI 
# TO ZNAČI DA JE OPRAVDANO IZBACITI 5 PARAMETARA IZ STRUKTURE SLUČAJNIH
# EFEKATA PO ISPITANICIMA
# TAKOĐE, OPRAVDANO JE ISKLJUČITI JEDAN PARAMETAR IZ STRUKTURE SLUČAJNIH EFEKATA
# PO REČIMA


# PRIKAŽEMO KOEFICIJENTE DA BISMO DETEKTOVALI KOMPONENTE SA NAJMANJOM VARIJANSOM
# KOJE ĆEMO IZBACITI

# AKO MODEL NEĆE DA KONVERGIRA, PRVO IZBACIVATI KORELACIJE IZMEĐU SLUČAJNIH EFEKATA

summary(lmerZ)

# Linear mixed model fit by maximum likelihood . t-tests use
# Satterthwaite's method [lmerModLmerTest]
# Formula: RT ~ trial.z + PC1 + PC2 + PC3 + PC4 + PC5 + H.z + trial.z *  
# PC4 + PC3 * H + PC5 * H + PC3 * PC5 + (1 + trial.z + PC1 +  
# PC2 + PC3 + PC4 + PC5 + H.z + PC3 * H.z + PC5 * H.z + PC3 *  
# PC5 | Participant) + (1 + trial.z | Word)
# Data: dat

# AIC       BIC    logLik  deviance  df.resid 
# 323159.8  323828.6 -161497.9  322995.8     25670 

# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -3.0532 -0.6090 -0.1824  0.3859  7.6570 

# Random effects:
# Groups      Name        Variance  Std.Dev. Corr                         
# Participant (Intercept)  4721.183  68.711                               
# trial.z       278.868  16.699  -0.02                        
# PC1             8.345   2.889   0.96  0.26                  
# PC2            12.921   3.595  -0.13 -0.24 -0.19            
# PC3            31.501   5.613   0.16 -0.18  0.11  0.56      
# PC4            24.645   4.964   0.41  0.02  0.40  0.49 -0.17
# PC5            23.368   4.834   0.04  0.13  0.07 -0.14  0.44
# H.z            27.803   5.273  -0.37 -0.33 -0.45  0.34 -0.24
# PC3:H.z        26.377   5.136  -0.46 -0.21 -0.50 -0.09 -0.16
# PC5:H.z        11.820   3.438   0.50 -0.07  0.47 -0.58 -0.56
# PC3:PC5        22.782   4.773   0.32 -0.18  0.26  0.20  0.55
# Word        (Intercept)   592.149  24.334                               
# trial.z         6.370   2.524  1.00                         
# Residual                15503.966 124.515                               







# -0.59                        
# 0.20 -0.08                  
# -0.40  0.17  0.76            
# 0.14  0.14  0.00 -0.12      
# -0.08  0.60 -0.40 -0.53  0.20



# Number of obs: 25752, groups:  Participant, 183; Word, 146

# Fixed effects:
# Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept) 628.2684     5.5535 237.5724 113.129  < 2e-16 ***
# trial.z       2.0129     1.4813 188.6805   1.359 0.175817    
# PC1          16.9601     2.2106 147.4081   7.672 2.13e-12 ***
# PC2          10.2883     2.2208 149.0816   4.633 7.81e-06 ***
# PC3          37.0202    11.2331 152.0327   3.296 0.001222 ** 
# PC4           7.7015     2.2268 150.5136   3.459 0.000706 ***
# PC5          33.5706    10.9349 148.0123   3.070 0.002547 ** 
# H.z          -8.6462     2.2973 151.5152  -3.764 0.000239 ***
# trial.z:PC4   2.0844     0.8114 908.2454   2.569 0.010357 *  
# PC3:H        -7.7890     3.7872 151.5934  -2.057 0.041435 *  
# PC5:H       -10.7434     3.7723 147.7633  -2.848 0.005027 ** 
# PC3:PC5       6.5155     2.5002 149.4253   2.606 0.010089 *  




# treba još proveriti da li su efekti nelinearni

# na kraju prelazimo na dijagnostiku

# treba plot


