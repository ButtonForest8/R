library(readr)
library(dplyr)
library(stargazer)

    # IQ-EKSPERIMENTET

iqdata<-read.csv("iq.csv")
nrow(iqdata)

#Oppgave 2: Comparison between no payment and 10 cents

filterediqdata2<-iqdata %>% filter(group %in% c("No payment","10 cents"))
filterediqdata2$group2bool<-ifelse(filterediqdata2$group=="No payment",0,1)

regression2<-lm(correct~group2bool,data=filterediqdata2)
summary(regression2)

filterediqdata2 %>% 
  ggplot(aes(x = group2bool,y = correct)) + 
  geom_point()+
  geom_smooth(method="lm", se=FALSE) + theme_classic()


#Oppgave 3: Comparison between no payment and NIS1
  
filterediqdata3<-iqdata %>% filter(group %in% c("No payment","NIS 1"))
filterediqdata3$group3bool<-ifelse(filterediqdata2$group=="No payment",0,1)

regression3<-lm(correct~group3bool,data=filterediqdata3)

filterediqdata3 %>% 
  ggplot(aes(x=group3bool,y=correct))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()


#Opggave 3: Comparison between no payment and NIS3

filterediqdata4<-iqdata %>% filter(group %in% c("No payment","NIS 3"))
filterediqdata4$group4bool<-ifelse(filterediqdata4$group=="No payment",0,1)

regression4<-lm(correct~group4bool,data=filterediqdata4)

filterediqdata4 %>% 
  ggplot(aes(x=group4bool,y=correct))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme_classic()


#Oppgave 4: Export

stargazer(regression2,regression3,type="text")


#Oppgave 5: Visualization on foenem

means<-iqdata %>% group_by(group) %>% 
  summarise(
    n=n(),
    mean=mean(correct),
    sd=sd(correct),
    se=sd/sqrt(n),
    ci=qt(0.975,df=n-1)*se
  )

ggplot(means,aes(x=group,y=mean))+
         geom_errorbar(aes(ymin=mean-ci,ymax=mean+ci))+
         labs(title="Average Correct Answers by Incentive",
              subtitle="Bars are group means; 95% CIs",
              x=NULL,
              y="Mean correct answers")


#Oppgave 6: Regression to see low effort 

iqdata$loweffortbool<-ifelse(iqdata$correct<16,1,0)

groupz <- factor(iqdata$group,levels=c("NIS 3","No payment","10 cents","NIS1"))

effortregression <- lm(loweffortbool~groupz,data=iqdata)

summary(effortregression)

#Alternativ fremgang

iqdata <- iqdata %>% 
  mutate(loweffort = ifelse(correct<16,1,0),
         group2 = factor(group, levels = c("NIS 3","No payment","10 cents","NIS 1")))

loweffortreg <- lm(loweffort ~ group2, data = iqdata)

#Følgende kode er ikke en del av noe oppgave, bare syntes det var stilig å visualisere

iqdata %>% 
  mutate(group=factor(group,levels=c("No payment","10 cents","NIS 1","NIS 3"))) %>% 
  group_by(group) %>% 
  ggplot(aes(x=group,y=loweffortbool))+
  geom_bar(stat="identity",fill="skyblue")+ #Play up sky blues #CCFC
  theme_classic()+
  labs(x="Payment",
       y="Count",
       title="Number of low scores per payment group",
       subtitle="''Low score'' defined as sub-16")



    # DONASJONSEKSPERIMENTET

#Oppgave 1: Regressions

donationdata <- read.csv("donation.csv")
donationdata$onepctbool <- ifelse(donationdata$group=="1 percent",1,0)
donationdata$tenpctbool <- ifelse(donationdata$group=="10 percent",1,0)

donationgroups <- factor(donationdata$group,levels=c("No payment","1 percent","10 percent"))

donationregression<-lm(collection~donationgroups,data=donationdata)


#Ok now let's visualize that
#Denne er heller ikke nødvendig men ser skikkelig nice ut ngl

datameans<-donationdata %>% group_by(group) %>% summarise(avg=mean(collection,na.rm=TRUE))

donationdata %>% 
  ggplot(aes(x=group,y=collection))+
  geom_point(color="black", alpha=0.3)+
  geom_point(data=datameans,aes(x=group,y=avg),color="red",size=3,alpha=0.8)+
  labs(
    title="Money collected by volunteers, with different commissions",
    x="Commission",
    y="Money collected, NOK")+
  theme_classic()
