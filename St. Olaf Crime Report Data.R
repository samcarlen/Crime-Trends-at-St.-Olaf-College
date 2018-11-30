crimes1<-Criminal_Offenses_On_campus_Student_Housing_Facilities <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-04 153024/Criminal_Offenses_On_campus_Student_Housing_Facilities.csv")
require(mosaic)
library(MASS)
mean(crimes1$Rape)  

names(crimes1)
Arrests_Noncampus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Arrests_Noncampus.csv")
Arrests_On_campus_Student_Housing_Facilities <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Arrests_On_campus_Student_Housing_Facilities.csv")
Arrests_On_campus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Arrests_On_campus.csv")
Arrests_Public_Property <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Arrests_Public_Property.csv")
Criminal_Offenses_Noncampus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Criminal_Offenses_Noncampus.csv")
Criminal_Offenses_On_campus_Student_Housing_Facilities <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Criminal_Offenses_On_campus_Student_Housing_Facilities.csv")
Criminal_Offenses_On_campus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Criminal_Offenses_On_campus.csv")
Criminal_Offenses_Public_Property <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Criminal_Offenses_Public_Property.csv")
Detailed_Fire_Data <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Detailed_Fire_Data.csv")
Disciplinary_Actions_Noncampus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Disciplinary_Actions_Noncampus.csv")
Disciplinary_Actions_On_campus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Disciplinary_Actions_On_campus.csv")
Disciplinary_Actions_Public_Property <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Disciplinary_Actions_Public_Property.csv")
Disciplinary_Actions_Student_Housing_Facilities <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Disciplinary_Actions_Student_Housing_Facilities.csv")
Hate_Crimes_Noncampus <- read.delim2("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Hate_Crimes_Noncampus.csv")
Hate_Crimes_On_campus_Student_Housing_Facilities <- read.delim2("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Hate_Crimes_On_campus_Student_Housing_Facilities.csv")
Hate_Crimes_On_campus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Hate_Crimes_On_campus.csv", dec=",")
Hate_Crimes_Public_Property <- read.delim2("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Hate_Crimes_Public_Property.csv")
Total_Fires_On_campus_Student_Housing_Facilities <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Total_Fires_On_campus_Student_Housing_Facilities.csv")
Unfounded_Crimes <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/Unfounded_Crimes.csv")
VAWA_Offenses_Noncampus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/VAWA_Offenses_Noncampus.csv"
VAWA_Offenses_On_campus_Student_Housing_Facilities <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/VAWA_Offenses_On_campus_Student_Housing_Facilities.csv")
VAWA_Offenses_On_campus <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/VAWA_Offenses_On_campus.csv")
VAWA_Offenses_Public_Property <- read.csv("~/Downloads/OPE CSS Custom Data 2018-10-06 125138/VAWA_Offenses_Public_Property.csv")

names(Criminal_Offenses_On_campus_Student_Housing_Facilities)

tally(Criminal_Offenses_On_campus_Student_Housing_Facilities)
crimes.housing<-Criminal_Offenses_On_campus_Student_Housing_Facilities
crimes.housing[is.na(crimes.housing)]<-0
View(crimes.housing)
crimes.housing$assault<-apply(crimes.housing[,c('Sex.offenses...Forcible','Rape','Fondling')],1,function(x) sum(x))
View(crimes.housing)

#_____________________________________________________________
#Assault in student housing

crimes.housing1<-data.frame(crimes.housing[1:15,])
View(crimes.housing1)
r1<-lm(assault~Survey.year+Institution.Size,data=crimes.housing)
summary(r1)
r1a<-lm(assault~Survey.year,data=crimes.housing)
summary(r1a)
ggplot(crimes.housing,aes(x=Survey.year,y=assault))+geom_point(alpha=0.7)+xlab("Year")+ylab("Number of sexual assaults")+geom_smooth(method="lm")
r1b<-lm(Rape~Survey.year,data=crimes.housing)
summary(r1b)
r1c<-lm(Fondling~Survey.year,data=crimes.housing)
summary(r1c)
r1d<-lm(assault~Survey.year,data=crimes.housing1)
summary(r1d)
ggplot(crimes.housing1,aes(x=Survey.year,y=assault))+geom_point(alpha=0.7)+geom_smooth(method="lm")

#_____________________________________________________________
#Assault on campus (not in housing)

crimes.campus1<-data.frame(crimes.campus[1:15,])
crimes.campus<-Criminal_Offenses_On_campus
crimes.campus[is.na(crimes.campus)]<-0
crimes.campus$assault<-apply(crimes.campus[,c('Sex.offenses...Forcible','Rape','Fondling')],1,function(x) sum(x))
View(crimes.campus)
mean(~assault,data=crimes.campus1)
r2<-lm(assault~Survey.year+Institution.Size,data=crimes.campus)
summary(r2)
ggplot(crimes.campus,aes(x=Survey.year,y=assault))+geom_point(alpha=0.7)+geom_smooth(method="lm")
r2a<-lm(assault~Survey.year+Institution.Size,data=crimes.campus1)
summary(r2a)
ggplot(crimes.campus1,aes(x=Survey.year,y=assault))+geom_point(alpha=0.7)+geom_smooth(method="lm")

#_____________________________________________________________
#Disciplinary Action, Student Housing

disc.housing<-Disciplinary_Actions_Student_Housing_Facilities
names(disc.housing)
r3<-lm(Liquor.law.violations~Survey.year,data=disc.housing)
summary(r3)
r3a<-lm(Liquor.law.violations~Survey.year+Institution.Size,data=disc.housing)
summary(r3a)
xyplot(Liquor.law.violations~Survey.year,data=disc.housing)
ggplot(disc.housing,aes(x=Survey.year,y=Liquor.law.violations))+geom_point(alpha=0.9,size=2)+geom_smooth(method="lm",size=1,se=FALSE,color="black")+theme_bw()+xlab("Year")+ylab("Liquor policy violations")

r4<-lm(Drug.law.violations~Survey.year,data=disc.housing)
summary(r4)
r4a<-lm(Drug.law.violations~Survey.year+Institution.Size,data=disc.housing)
summary(r4a)
ggplot(disc.housing,aes(x=Survey.year,y=Drug.law.violations))+geom_point(alpha=1,size=2)+theme(panel.background = element_rect("#FDD168"))+geom_smooth(method="lm",se=FALSE,size=1,color="black")

#fdd168
#_______________________________________________________
#Disciplinary Action, On Campus
View(disc.campus)
disc.campus.no2016<-data.frame(disc.campus[1:15,])
disc.campus<-Disciplinary_Actions_On_campus
r5<-lm(Liquor.law.violations~Survey.year,data=disc.campus)
summary(r5)
r5a<-lm(Liquor.law.violations~Survey.year+Institution.Size,data=disc.campus)
summary(r5a)
plot(Liquor.law.violations~Survey.year,data=disc.campus)
r6<-lm(Drug.law.violations~Survey.year,data=disc.campus)
summary(r6)
studres(r6)
r6a<-lm(Drug.law.violations~Survey.year+Institution.Size,data=disc.campus)
summary(r6a)
r6b<-lm(Drug.law.violations~Survey.year,data=disc.campus.no2016)
summary(r6b)
ggplot(disc.campus.no2016,aes(x=Survey.year,y=Drug.law.violations))+geom_point(alpha=0.7)+geom_smooth(method="lm")
ggplot(disc.campus,aes(x=Survey.year,y=Drug.law.violations))+geom_point(alpha=1,size=2)+theme_bw()+geom_smooth(method="lm",se=FALSE,size=1,color="black")+xlab("Year")+ylab("Drug policy violations")


#_______________________________________
#Hate crimes

hate.campus<-Hate_Crimes_On_campus
hate.campus[is.na(hate.campus)]<-0
names(hate.campus)
hate.campus$hate<-apply(hate.campus[,c("Murder.Non.negligent.manslaughter","Destruction.damage..vandalism.of.property...National.origin")],1,function(x)sum(x))
View(hate.campus)

View(crimes.housing)
r6<-lm(Rape~Survey.year,data=crimes.housing)

#__________________________________________
#burglery on campus

r7<-lm(Burglary~Survey.year,data=crimes.campus)
summary(r7)
ggplot(crimes.campus,aes(x=Survey.year,y=Burglary))+geom_point(alpha=0.7,size=2)+theme_bw()+geom_smooth(method="lm",se=FALSE,color="black")+ylab("Number of burglaries")+xlab("Year")

stdres(r7)

studres(r7)

crimes.campus.no2001.2005<-data.frame(crimes.campus[1:16,])
cc.no2001.2005<-crimes.campus.no2001.2005[-c(1,5),]
r7a<-lm(Burglary~Survey.year,data=cc.no2001.2005)
summary(r7a)
studres(r7a)
ggplot(cc.no2001.2005,aes(x=Survey.year,y=Burglary))+geom_point(alpha=0.7)+geom_smooth(method="lm",se=FALSE)

#__________________________________________
#burglery in housing

r8<-lm(Burglary~Survey.year,data=crimes.housing)
summary(r8)
ggplot(crimes.housing,aes(x=Survey.year,y=Burglary))+geom_point(alpha=0.7)+geom_smooth(method="lm")

studres(r8)

crimes.housing.no2001.2005<-data.frame(crimes.campus[1:16,])
ch.no2001.2005<-crimes.housing.no2001.2005[-c(1,5),]
r8a<-lm(Burglary~Survey.year,data=ch.no2001.2005)
summary(r8a)
studres(r8a)
ggplot(ch.no2001.2005,aes(x=Survey.year,y=Burglary))+geom_point(alpha=0.7)+geom_smooth(method="lm",se=FALSE)

#___________________________________________
#Liquor Law Arrests

r9<-lm(Liquor.law.violations~Survey.year,data=Arrests_On_campus)
summary(r9)
ggplot(Arrests_On_campus,aes(x=Survey.year,y=Liquor.law.violations))+geom_point(alpha=0.7)+geom_smooth(method="lm",se=FALSE)
studres(r9)
Arrests_On_campus.no2011<-data.frame(Arrests_On_campus[1:16,])
aoc.no2011<-Arrests_On_campus.no2011[-11,]
r9a<-lm(Liquor.law.violations~Survey.year,data=aoc.no2011)
summary(r9a)

#________________________________________









