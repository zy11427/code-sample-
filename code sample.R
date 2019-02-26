suppressMessages(library(dplyr))
library(stargazer)
library(haven)
library(lme4)


ADULT <- read_dta("C:/Users/Administrator/Desktop/GY2/AAAS/BC data/CHIS15_adult_stata/Data/ADULT.dta")

ADULT15 <- filter(ADULT, srsex==2, sras==1)

#Data15 <- data.frame(ADULT15$sraa, ADULT15$srage_p1, ADULT15$srai, ADULT15$sras, ADULT15$sraso, ADULT15$sraso2, ADULT15$srch, ADULT15$srh, ADULT15$srjp,ADULT15$srkr, ADULT15$sro, ADULT15$srph, ADULT15$srvt,ADULT15$srw, ADULT15$marit, ADULT15$marit2, ADULT15$marit_45,ADULT15$ac31, ADULT15$ac31_p1, ADULT15$ae_soda, ADULT15$aesoda_p1, ADULT15$ac49, ADULT15$ac50, ADULT15$ae15, ADULT15$ae16_p1, ADULT15$smkcur, ADULT15$smoking, ADULT15$ac32,ADULT15$aj9, ADULT15$lnghm_p1,ADULT15$spk_eng, ADULT15$yrus_p1, ADULT15$ah33new, ADULT15$ad14b,ADULT15$ad17b, ADULT15$ae95, ADULT15$mamscrn2,ADULT15$ah34new,ADULT15$ah35new, ADULT15$ah37, ADULT15$citizen2, ADULT15$pctlf_p, )

income <- as.numeric(ADULT15$ak22_p)/1000

Data15as <- data.frame(ADULT15$srage_p1, ADULT15$srai,  ADULT15$sraso, ADULT15$sraso2, ADULT15$srch, ADULT15$srh, ADULT15$srjp,ADULT15$srkr, ADULT15$sro, ADULT15$srph, ADULT15$srvt,  ADULT15$marit2, ADULT15$ac31, ADULT15$ac31_p1, ADULT15$ae_soda, ADULT15$aesoda_p1, ADULT15$ac49, ADULT15$ac50, ADULT15$ae15, ADULT15$ae16_p1, ADULT15$smkcur, ADULT15$smoking, ADULT15$ac32,ADULT15$aj9, ADULT15$lnghm_p1, ADULT15$yrus_p1, ADULT15$ah33new, ADULT15$ad14b,ADULT15$ad17b, ADULT15$ae95, ADULT15$mamscrn2,ADULT15$ah34new,ADULT15$ah35new, ADULT15$ah37, ADULT15$citizen2, ADULT15$pctlf_p,income, ADULT15$ahedc_p1, ADULT15$ai25)

Data15as$ethnicity <- NA

#recode ah34new, not born in US--> 0, born in US not mother --> 0, born in US and mother --> 1

for (i in 1:1028){
  if(Data15as$ADULT15.ah34new[i]==1){
    Data15as$ah34new[i] <-1
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ah34new[i]==-1){
    Data15as$ah34new[i] <-0
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ah34new[i]==2){
    Data15as$ah34new[i] <-0
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ah35new[i]==1){
    Data15as$ah35new[i] <-1
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ah35new[i]==-1){
    Data15as$ah35new[i] <-0
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ah35new[i]==2){
    Data15as$ah35new[i] <-0
  } }
#recode language speak at home
for (i in 1:1028){
  if(Data15as$ADULT15.lnghm_p1[i]==2 & Data15as$ADULT15.lnghm_p1[i]==3 & Data15as$ADULT15.lnghm_p1[i]==4 & Data15as$ADULT15.lnghm_p1[i]==5){
    Data15as$langhome[i] <-2} }
#???
for (i in 1:1028){
  if(is.na(Data15as$langhome[i])
    {Data15as$langhome[i] <-2}}


for (i in 1:1028){
  if(Data15as$ADULT15.lnghm_p1[i]==1){
    Data15as$langhome[i] <-4
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.lnghm_p1[i]==6){
    Data15as$langhome[i] <-1
  } }


for (i in 1:1028){
  if(Data15as$ADULT15.lnghm_p1[i]==8 & Data15as$ADULT15.lnghm_p1[i]==9 & Data15as$ADULT15.lnghm_p1[i]==10 & Data15as$ADULT15.lnghm_p1[i]==11){
    Data15as$langhome[i] <-3}}

#recode ethnicity 
for (i in 1:1028){
  if(Data15as$ADULT15.srch[i]<2){
    Data15as$ethnicity[i] <-1
  } }
  
for (i in 1:1028){
  if(Data15as$ADULT15.srkr[i]<2){
    Data15as$ethnicity[i] <-2
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.srjp[i]<2){
    Data15as$ethnicity[i] <-3
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.srph[i]<2){
    Data15as$ethnicity[i] <-4
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.srvt[i]<2){
    Data15as$ethnicity[i] <-5
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.sraso2[i]<2){
    Data15as$ethnicity[i] <-6
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.srh[i]<2){
    Data15as$ethnicity[i] <-7
  } }

#INSURANCE
for (i in 1:1028){
  if(Data15as$ADULT15.ai25[i]==-1){
    Data15as$insurance[i] <-0
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ai25[i]==1){
    Data15as$insurance[i] <-1
  } }

for (i in 1:1028){
  if(Data15as$ADULT15.ai25[i]==2){
    Data15as$insurance[i] <-1
  } }


mammoeli <- filter(Data15as, Data15as$ADULT15.srage_p1>39)
nomammo<- filter(Data15as,Data15as$ADULT15.mamscrn2==2|Data15as$ADULT15.mamscrn2==3)
yesmammo <- filter(Data15as,Data15as$ADULT15.mamscrn2==1 & Data15as$ADULT15.srage_p1>39)

yes <-as.data.frame (apply(yesmammo,2,mean))
no <-as.data.frame (apply(nomammo,2,mean))

count(yesmammo, ADULT15.ahedc_p1)
count(nomammo, ADULT15.ahedc_p1)

c(323,10,150,55)/538*100
c(93,1,73,2)/187*100   

count(yesmammo,ADULT15.marit2 )
count(nomammo,ADULT15.marit2 )

count(yesmammo,ADULT15.pctlf_p )
count(nomammo,ADULT15.pctlf_p )

c(43,103,171,81,140)/538*100
c(25,54,37,18,53)/187*100  

count(yesmammo,ADULT15.ah37)
count(nomammo, ADULT15.ah37)

c(160,91,124,110,53)/538*100
c(49,35,35,42,26)/187*100 

count(yesmammo, ADULT15.citizen2)
count(nomammo,ADULT15.citizen2 )
c(112,361,65)/538*100
c(39,125,23)/187*100 


stargazer (yesmammo, nomammo, summary=T)

h2<-as.numeric(nomammo$ethnicity)

h1<-as.numeric(mammoeli$ethnicity)

# xlab= c('Ch', "Kr","Jp","Phi", "Vt", "Other", "His ")


barplot(table(h1),col = 'pink',ylim = c(0,340),main = 'Percentage of mammogram use by ethnicity', xlab = 'ethnicity', ylab = 'number of people')
barplot(table(h2), col = 'red', add = T)


#variables
fastfood <- Data15as$ADULT15.ac31 # past week time of fastfood 
soda <- Data15as$ADULT15.ae_soda # past week time of soda 

cigarette <- 3- Data15as$ADULT15.smoking

smoke <- 2-as.numeric (Data15as$ADULT15.smkcur)

lm1 <- glm(fastfood~1+Data15as$ADULT15.srage_p1+Data15as$ADULT15.pctlf_p+Data15as$ADULT15.ah37+Data15as$ah34new+Data15as$ah35new+factor(Data15as$ADULT15.citizen2)+income+Data15as$ADULT15.ahedc_p1)


lm2 <- glm(soda~1+Data15as$ADULT15.srage_p1+Data15as$ADULT15.pctlf_p+Data15as$ADULT15.ah37+Data15as$ah34new+Data15as$ah35new+factor(Data15as$ADULT15.citizen2)+income+Data15as$ADULT15.ahedc_p1)


lm3 <- glm(cigarette~1+Data15as$ADULT15.srage_p1+Data15as$ADULT15.pctlf_p+Data15as$ADULT15.ah37+Data15as$ah34new+Data15as$ah35new+factor(Data15as$ADULT15.citizen2)+income+Data15as$ADULT15.ahedc_p1)


stargazer(lm1,lm2, lm3)

smoke <-2-as.numeric(Data15as$ADULT15.smkcur)
smkglm <- glm(smoke <- Data15as$ADULT15.srage_p1+Data15as$ADULT15.pctlf_p+Data15as$ADULT15.ah37+Data15as$ah34new+Data15as$ah35new+factor(Data15as$ADULT15.citizen2)+income+Data15as$ADULT15.ahedc_p1)


# acculturalization= citizenship(USborn/naturalized/noncitizen) + USborn/USborn not mother/not USborn + USborn/USborn not father/not USborn + Englishproficiency(speak only English/Speak English very well/Speak English well/Speak English not well/Not speak English at all)+ percentage of life lived in the US + language speak at home(Other language only/Other language/English + Other language/English only+years lived in the US)

#control(age, insurance, marriage)

#recode mammogram history
mammo <- as.numeric(2-mammoeli$ADULT15.ad14b)
datam <- data.frame(mammo,mammoeli$ADULT15.citizen2,mammoeli$ADULT15.srage_p1,mammoeli$ADULT15.ah37,mammoeli$ah34new,mammoeli$ah35new,mammoeli$langhome,mammoeli$ADULT15.yrus_p1,mammoeli$ADULT15.pctlf_p+)

a <-cor(datam)

glm1 <-glm(mammo~factor(mammoeli$ADULT15.citizen2)+1+mammoeli$ADULT15.srage_p1+mammoeli$ADULT15.ah37+mammoeli$ah34new+mammoeli$ah35new+mammoeli$langhome+mammoeli$ADULT15.yrus_p1+mammoeli$ADULT15.pctlf_p+mammoeli$income+mammoeli$ADULT15.ahedc_p1+mammoeli$insurance)

stargazer(glm1)









