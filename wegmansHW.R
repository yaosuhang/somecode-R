#call the library for getting data from other programs
library(foreign)
filnm = "wegmans"; #this is the name of the file
spssDataLab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssData <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE); #turning values into numbers, but be sure getting right values to numbers!

##1) Test whether sample matches with population for gender 
summary(spssData$Question33Areyou)
popSEX = c(.13,.87); #true population values
smpSEX=c(105,889)
cbind(popSEX,prop.table(smpSEX)); #creating table as matrix
chisq.test(smpSEX,p=popSEX)

#weights, if reweighting
w = popSEX/prop.table(smpSEX); ##formula is w=P/M
w

##Q2Describe the average importance rating#

#calculate means and standard errors of importance
importancerating6 = spssData[,47:59]
importancerating6[importancerating6==5]=NA#change"unsure" into NA
impmeans=colMeans(importancerating6,na.rm=TRUE)
impmeans
impSE = apply(importancerating6,2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(importancerating6)))
impSE

#create full names forimportance variables and form a dataframe
genVarsf = c("Allnatural","Blended", "Calorielevel","consistency",
             "Fatlevel","Fruit on the bottom","Organic","Price",
             "ProteinLevel","rbstFree","side by side cup","Taste","Texture")
perfImpDF = data.frame(genVarsf,impmeans,impSE)
spssData$Question6Allnatural
#Draw Error bar plot
library(ggplot2)
dodge = position_dodge(width=.2)
gi = ggplot(perfImpDF,aes(y=impmeans,x=reorder(genVarsf,-impmeans),
                          ymax=impmeans+impSE,ymin=impmeans-impSE))
gi + geom_bar(position=dodge,stat="identity",col=1,fill="skyblue",width=.75) + 
  geom_errorbar(position=dodge,width=.5) + labs(x="Attributes",y="Means of Importance")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Question4 Compare Brand attribute for Fage and Oikos
#calculate means and SE of Fage attributes importance
spssdata4=subset(spssData,spssData$Question1HaveyoupurchasedGreekYogurtinthepastmonth=="Yes")
spssdata4[spssdata4==6]=NA #Change "unsure" into NA
impmeans24=colMeans(spssdata4[c("Question24Allnatural",
                                "Question24Price" ,"Question24Taste")],na.rm=TRUE)
impmeans24
impSE24 = apply(spssdata4[c("Question24Allnatural","Question24Price" ,
                            "Question24Taste")],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(spssdata4[c("Question24Allnatural","Question24Price"
                                  ,"Question24Taste")])))
impSE24

#calculate means and SE of Oikos attributes importance
impmeans30=colMeans(spssdata4[c("Question30Allnatural","Question30Price" ,
                                "Question30Taste")],na.rm=TRUE)
impmeans30
impSE30 = apply(spssdata4[c("Question30Allnatural","Question30Price" ,
                            "Question30Taste")],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(spssdata4[c("Question30Allnatural","Question30Price" ,
                                  "Question30Taste")])))
impSE30
#Draw Error bar plot
comparebrand=read.csv("wegmansQ4.csv")
ggplot(comparebrand, aes(y=Means,x=Attributes,fill=Brand,ymax=Means+SE,ymin=Means-SE)) + 
  geom_bar(width=0.5, stat="identity",position='dodge')+
  geom_errorbar(position=dodge,width=.5) + labs(x="Attributes",y="Means of Importance")
t.test(spssdata4$Question24Allnatural,spssdata4$Question30Allnatural,paired = TRUE)
t.test(spssdata4$Question24Price,spssdata4$Question30Price,paired = TRUE)
t.test(spssdata4$Question24Taste,spssdata4$Question30Taste,paired = TRUE)

#Question5 usage situations Attibutes importance
#subset cooking group data and snack group data
cooking=subset(spssData,spssData$Question12DoyouuseGreekYogurtforcooking=="Yes")
cooking[cooking==5]=NA  #Change "unsure" into NA
snack=subset(spssData,spssData$Question12DoyouuseGreekYogurtforcooking=="No ")
snack[snack==5]=NA  #Change "unsure" into NA
#calculate means and SE of cooking group attributes importance
impmeanscook=colMeans(cooking[c("Question6Allnatural","Question6Organic",
                                "Question6rbSTfree","Question6Price")],na.rm=TRUE)
impmeanscook
impSEcook = apply(cooking[c("Question6Allnatural","Question6Organic",
                            "Question6rbSTfree","Question6Price")],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(cooking[c("Question6Allnatural","Question6Organic",
                                "Question6rbSTfree","Question6Price")])))
impSEcook
#calculate means and SE of snack group attributes importance
impmeanssnack=colMeans(snack[c("Question6Allnatural","Question6Organic",
                               "Question6rbSTfree","Question6Price")],na.rm=TRUE)
impmeanssnack
impSEsnack = apply(snack[c("Question6Allnatural","Question6Organic",
                           "Question6rbSTfree","Question6Price")],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(snack[c("Question6Allnatural","Question6Organic","Question6rbSTfree","Question6Price")])))
impSEsnack

#draw error bar plot
compareuse=read.csv("wegmansQ5.csv")
ggplot(compareuse, aes(y=means,x=attributes,fill=Usage,ymax=means+sd,ymin=means-sd)) + geom_bar(width=0.5, stat="identity",position='dodge')+
  geom_errorbar(position=dodge,width=.5) + labs(x="Attributes",y="Means of Importance")
t.test(cooking$Question6Allnatural,snack$Question6Allnatural)
t.test(cooking$Question6Organic,snack$Question6Organic)
t.test(cooking$Question6Price,snack$Question6Price)
t.test(cooking$Question6rbSTfree,snack$Question6)


