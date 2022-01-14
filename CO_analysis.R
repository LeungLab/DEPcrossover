library(tidyverse)
library(lsmeans)
library(ggeffects)
library(lubridate)
library(lme4)
library(gridExtra)
library(grid)
library(slider)
library(patchwork)
setwd("G:/My Drive/UU/GEMS/App/Phase_2")
CRF1 <- read_csv("C:/Users/u6020766/Google Drive/UU/GEMS/App/Phase_2/CRF1.csv")
CRF2 <- read_csv("C:/Users/u6020766/Google Drive/UU/GEMS/App/Phase_2/CRF-2.csv")

ClinDem=read_csv("ClinDem.csv",col_name=F)

Efile=read_csv("C:/Users/u6020766/Google Drive/UU/GEMS/App/Phase_2/1.0Folder_3_16_21.csv")


CRF1$admdate[which(CRF1$patid==3497)]="12/24/2020"
CRF1$admdate[which(CRF1$patid==3470)]="12/09/2020"
CRF1$clid[which(CRF1$clid==19)]=14
CRF1 = CRF1 %>% mutate(TRT=as.numeric(!is.na(q17)),abx=case_when(q18==2~0,q18==1~1))

CRF1 %>% group_by(clid) %>% summarize(sum(TRT==1),sum(TRT==0))

CRF1=CRF1 %>% mutate(pred=case_when(is.na(q17)~0,TRUE~q17/100))

#assign=CRF1 %>% group_by(clid) %>% summarize(assign=first(TRT)==1) %>% filter(assign==1)
#CRF1=CRF1 %>% mutate(Period=case_when(TRT==1 & (clid %in% assign$clid)~1,
#                                 TRT==0 & (clid %in% assign$clid)~2,
#                                 TRT==1 & !(clid %in% assign$clid)~2,
#                                 TRT==0 & !(clid %in% assign$clid)~1),Period=as.factor(Period))
CRF1 = CRF1 %>% mutate(date=mdy(admdate))

CRF1$date[which(CRF1$date=="2021-12-26")]=ymd("2020-12-26")

CRF1 = CRF1 %>% mutate(Period=case_when(site==31 & date<mdy("12/23/20") ~ 0,
                                        site==34 & date<mdy("12/24/20") ~ 0,
                                        site==37 & date<mdy("12/28/20") ~ 0,
                                        TRUE~1))
CRF1$clid[which(CRF1$clid==4 & CRF1$TRT==0 & CRF1$Period==1)]=14

CRF1 %>% split(.$clid) %>% purrr::map(. %>% group_by(TRT,Period) %>% count)



CRF1 = CRF1 %>% mutate(clid=as.factor(clid))


#mod1=glmer(abx~
#             TRT*Period+(1|site/clid),data=CRF1,family="binomial");summary(mod1)
#mod2=glmer(abx~pred:TRT*Period+(1|site/clid),data=CRF1,family="binomial");summary(mod2)

#CRF1$fit1=fitted(mod1)


#CRF1$fit2=fitted(mod2)

#a=ggplot(CRF1,aes(x=as.factor(TRT),y=fit1,group=as.factor(clid),color=as.factor(clid),shape=as.factor(Period))) + geom_line(color="black") + geom_point()+ 
#  ylab("Predicted Probability of Abx Prescription") + xlab("Received Treatment") + theme_bw();a
#b=ggplot(CRF1,aes(x=pred,y=fit2,group=as.factor(clid),color=as.factor(clid),shape=as.factor(Period))) + geom_line(color="black") + geom_point() + 
#  ylab("Predicted Probability of Abx Prescription") + xlab("Predicted Probability of Viral Etiology") + theme_bw();b

#c=rbind(data.frame(ggpredict(mod1,type="fe",term=c("TRT"),condition=c(Period=0)),Period=0),
#data.frame(ggpredict(mod1,type="fe",term=c("TRT"),condition=c(Period=1)),Period=1)) %>%
#  ggplot(aes(x=x,y=predicted,group=as.factor(Period),color=as.factor(Period))) + geom_line(color="black") + geom_point()+ 
#  ylab("Predicted Probability of Abx Prescription") + xlab("Received Treatment") + theme_bw();c

# d=rbind(data.frame(ggpredict(mod2,type="fe",term=c("pred"),condition=c(Period=0,TRT=1)),Period=0),
# data.frame(ggpredict(mod2,type="fe",term=c("pred"),condition=c(Period=1,TRT=1)),Period=1)) %>% 
#   ggplot(aes(x=x,y=predicted,color=as.factor(Period))) + geom_line() + geom_point() + 
#   ylab("Predicted Probability of Abx Prescription") + xlab("Predicted Probability of Viral Etiology") + theme_bw();d
# 
# grid.arrange(a,b,c,d)




CRF2 = CRF2 %>% mutate(TRT=as.numeric(!is.na(q17)),abx=case_when(q18=="Yes"~1,q18=="No"~0))


CRF2=CRF2 %>% mutate(pred=case_when(is.na(q17)~0,TRUE~as.numeric(substr(q17,1,2))/100))


CRF2=CRF2 %>% mutate(date=dmy(admdate))



CRF2 = CRF2 %>% mutate(Period=case_when(site=="41CSREF5" & date<mdy("1/30/21") ~ 0,
                                        site=="43CSREF6" & date<mdy("1/30/21") ~ 0,
                                        site=="45ASACOYIR" & date<mdy("1/30/21") ~ 0,
                                        site=="47ASACOSAB1" & date<mdy("1/30/21") ~ 0,
                                        TRUE~1))
CRF2 %>% split(.$clid) %>% purrr::map(. %>% group_by(TRT,Period) %>% count)


#write.csv(CRF2 %>% filter(clid %in% c(7,8,10,14)) %>% select(site,patid,clid,admdate,Period,q17,TRT,pred) %>% arrange(site,clid),
#          file="all_id.csv")

CRF2 %>% filter(clid %in% c(7,8,10,14)) %>% select(site,patid,clid,admdate,Period,q17,TRT,pred) %>% arrange(site,clid) %>% 
  filter((clid==7 & TRT==0 & Period==1)|(clid==8 & TRT==0 & Period==0)| 
  (clid==10 & TRT==1 & Period==0)|(clid==14 & TRT==0 & Period==0))


CRF2=CRF2 %>% filter(!(clid==7 & TRT==0 & Period==1)) %>% filter(!(clid==8 & TRT==0 & Period==0)) %>% 
                filter(!(clid==10 & TRT==1 & Period==0)) %>% filter(!(clid==14 & TRT==0 & Period==0))

CRF2 = CRF2 %>% mutate(clid=as.factor(clid))


#mod1=glmer(abx~
#             TRT*Period+(1|site/clid),data=CRF2,family="binomial");summary(mod1)
#mod2=glmer(abx~pred:TRT*Period+(1|site/clid),data=CRF2,family="binomial");summary(mod2)

# CRF2$fit1=fitted(mod1)
# 
# 
# CRF2$fit2=fitted(mod2)
# 
# a=ggplot(CRF2,aes(x=as.factor(TRT),y=fit1,group=as.factor(clid),color=as.factor(clid),shape=as.factor(Period))) + geom_line(color="black") + geom_point()+ 
#   ylab("Predicted Probability of Abx Prescription") + xlab("Received Treatment") + theme_bw();a
# b=ggplot(CRF2,aes(x=pred,y=fit2,group=as.factor(clid),color=as.factor(clid),shape=as.factor(Period))) + geom_line(color="black") + geom_point(size=2) + 
#   ylab("Predicted Probability of Abx Prescription") + xlab("Predicted Probability of Viral Etiology") + theme_bw();b
# 
# c=rbind(data.frame(ggpredict(mod1,type="fe",term=c("TRT"),condition=c(Period=0)),Period=0),
#         data.frame(ggpredict(mod1,type="fe",term=c("TRT"),condition=c(Period=1)),Period=1)) %>%
#   ggplot(aes(x=x,y=predicted,group=as.factor(Period),color=as.factor(Period))) + geom_line(color="black") + geom_point()+ 
#   ylab("Predicted Probability of Abx Prescription") + xlab("Received Treatment") + theme_bw();c
# 
# d=rbind(data.frame(ggpredict(mod2,type="fe",term=c("pred"),condition=c(Period=0,TRT=1)),Period=0),
#         data.frame(ggpredict(mod2,type="fe",term=c("pred"),condition=c(Period=1,TRT=1)),Period=1)) %>% 
#   ggplot(aes(x=x,y=predicted,color=as.factor(Period))) + geom_line() + geom_point() + 
#   ylab("Predicted Probability of Abx Prescription") + xlab("Predicted Probability of Viral Etiology") + theme_bw();d
# 
# grid.arrange(a,b,c,d)
# #


CRF=rbind(CRF1 %>% mutate(location="Bangladesh"),CRF2 %>% mutate(location="Mali") %>% select(one_of(colnames(CRF1)),location)) %>%
  mutate_at(vars(c("site","location")),~as.factor(.))
#
CRF=CRF %>% mutate(Period=recode_factor(Period,"0"="Period 1","1"="Period 2"))

CRF= CRF %>% arrange(location,date) %>% group_by(location) %>% mutate(count=1,Total=cumsum(count))
a=CRF %>% ggplot(aes(x=date,y=Total)) + geom_point(aes(color=Period),size=.2)  + 
  theme_bw()+ facet_wrap(~location)+ 
  scale_color_viridis_d(begin=.25,end=.75) + theme(strip.text = element_text(size=14)) + ggtitle("A.");a

CRF= CRF %>% arrange(site,date) %>% group_by(site) %>% mutate(count=1,Total=cumsum(count))
b=CRF %>% ggplot(aes(x=date,y=Total)) + geom_point(aes(color=Period),size=.2) + facet_wrap(~site) + theme_bw() + 
  scale_color_viridis_d(begin=.25,end=.75) + theme(strip.text = element_text(size=14))+ ggtitle("B.");b
#grd=grid.arrange(a,b,ncol=2)
#grid.draw(grd)

a+b
ggsave(filename="FigS3.tiff",units="in",width=16,height=8,dpi=600)


#tiff(filename="FigS3.tiff",units="in",width=16,height=8,res=600)

#
CRF=CRF %>% mutate(noTRT=as.numeric(TRT==0))
CRF=CRF %>% mutate(q5yn=case_when(q5yn=="2"~"No",q5yn=="1"~"Yes",TRUE~q5yn))
CRF=CRF %>% mutate(sex=case_when(sex=="2"~"Female",sex=="1"~"Male",TRUE~sex))
CRF=CRF %>% mutate(q8=case_when(q8=="2"~"No",q8=="1"~"Yes",TRUE~q8))
CRF=CRF %>% mutate(q11=case_when(q11=="2"~"No",q11=="1"~"Yes",TRUE~q11))
CRF=CRF %>% mutate(q30=case_when(q30=="2"~"No",q30=="1"~"Yes",TRUE~q30))
CRF=CRF %>% mutate(q33=case_when(q33=="2"~"No",q33=="1"~"Yes",TRUE~q33))
CRF=CRF %>% mutate(q31=case_when(q31=="2"~"No",q31=="1"~"Yes",TRUE~q31))

CRF=CRF %>% mutate(q9=case_when(q9=="2"~"Partial",q9=="1"~"Exclusive",q9=="3"~"No",TRUE~q9))

CRF=CRF %>% mutate(age=case_when(is.na(ageyr)~agemo,is.na(agemo)~ageyr*12,TRUE~ageyr*12+agemo))

t(CRF %>% ungroup() %>% summarize(N=n(),Providers=length(unique(clid)),
                                                     `Sex (Male)`= paste0(round(sum(sex=="Male"),2)," (",round(sum(sex=="Male")/N,2),")"),
                                           Age= paste0(round(median(age),2)," (",round(IQR(age),2),")"),
                                          `Bloody Diarrhea`= paste0(round(sum(q8=="Yes"),2)," (",round(sum(q8=="Yes")/N,2),")"),
                                          `Vomit`= paste0(round(sum(q11=="Yes"),2)," (",round(sum(q11=="Yes")/N,2),")"),
                                          `Partial`= paste0(round(sum(q9=="Partial"),2)," (",round(sum(q9=="Partial")/N,2),")"),
                                          `Exclusive`= paste0(round(sum(q9=="Exclusive"),2)," (",round(sum(q9=="Exclusive")/N,2),")"),
                                          MUAC= paste0(round(mean(q15),2)," (",round(sd(q15),2),")"),
                                          `Assigned to Treatment`= paste0(round(sum(TRT==1),2)," (",round(sum(TRT==1)/N,2),")"),
                                          `Antibiotic Ordered`= paste0(round(sum(abx==1),2)," (",round(sum(abx==1)/N,2),")"),))


t(CRF %>% group_by(location,Period) %>% summarize(N=n(),`Sex (Male)`= paste0(round(sum(sex=="Male"),2)," (",round(sum(sex=="Male")/N,2),")"),
                                           Age= paste0(round(mean(age),2)," (",round(sd(age),2),")"),
                                           `Bloody Diarrhea`= paste0(round(sum(q8=="Yes"),2)," (",round(sum(q8=="Yes")/N,2),")"),
                                           `Vomit`= paste0(round(sum(q11=="Yes"),2)," (",round(sum(q11=="Yes")/N,2),")"),
                                           `Partial`= paste0(round(sum(q9=="Partial"),2)," (",round(sum(q9=="Partial")/N,2),")"),
                                           `Exclusive`= paste0(round(sum(q9=="Exclusive"),2)," (",round(sum(q9=="Exclusive")/N,2),")"),
                                           MUAC= paste0(round(mean(q15),2)," (",round(sd(q15),2),")"),
                                           `Assigned to Treatment`= paste0(round(sum(TRT==1),2)," (",round(sum(TRT==1)/N,2),")"),
                                           `Antibiotic Ordered`= paste0(round(sum(abx==1),2)," (",round(sum(abx==1)/N,2),")"),))

t(CRF %>% group_by(TRT) %>% summarize(N=n(),`Antibiotic Ordered`= paste0(round(sum(abx==1),2)," (",round(sum(abx==1)/N,2),")"),
                                      `Diarrhea Resolved`= paste0(round(sum(q30=="Yes"),2)," (",round(sum(q30=="Yes")/N,2),")")))

#write.csv(cbind(CRF %>% ungroup() %>% count(location,TRT,Period,name="Number of Patients"),
#CRF %>% ungroup() %>% select(location,TRT,Period,clid) %>% distinct() %>% 
#            count(location,TRT,Period,name="Number of Clinicians"))[,c(1,2,3,4,8)],file="Numbers.csv")


clids1=CRF1 %>% split(.$clid) %>% purrr::map(. %>% group_by(TRT,Period) %>% count)
clids2=CRF2 %>% split(.$clid) %>% purrr::map(. %>% group_by(TRT,Period) %>% count)
clids1[which(unlist(clids1 %>% purrr::map(~dim(.)[1]!=2)))]
clids2[which(unlist(clids2 %>% purrr::map(~dim(.)[1]!=2)))]



CRF %>% group_by(location,site,Period) %>% summarize(n())
CRF %>% group_by(location,site,Period,clid) %>% summarize(n())

CRF %>% select(location,site,Period,clid) %>% distinct %>% group_by(location,site,Period) %>% count

###
performance::icc(glmer(abx~TRT + (1|site/clid),data=CRF ,family="binomial"),by_group=T)


mean(resid(mod1)^2)


mod1b=glmer(abx~TRT + (1|site/clid),data=CRF,family="binomial");summary(mod1b)

mod1=glmer(abx~
             TRT*Period +(1|site/clid),data=CRF,family="binomial");summary(mod1)



(plogis(1.6655)-plogis(1.6655-.3137))/plogis(1.6655)

ranef(mod1b)

ggpredict(mod1,terms=c("TRT"),type="fe")
ggpredict(mod1,terms=c("TRT"),type="fe",condition=c(Period = "Period 2"))


CRF %>% group_by(clid,TRT) %>% summarize(abx=mean(abx)) %>% summarize(diff=(last(abx)-first(abx))) %>%ungroup() %>%
summarize(mean(diff))




CRF$pred

#mod2=glmer(abx~noTRT + pred:TRT*Period +(1|site) + (pred|clid:site),data=CRF,family="binomial");summary(mod2)
#mod2=glmer(abx~TRT + pred:TRT*Period +(1|site) + (pred|clid:site),data=CRF,family="binomial");summary(mod2)
mod2=glmer(abx~TRT*Period + pred:TRT*Period +(1|site) + (pred|clid:site),data=CRF,family="binomial")# %>% mutate(pred=pred*100/10),family="binomial");summary(mod2)


#mod2b=glmer(abx~noTRT + pred:TRT +(1|site) + (pred|clid:site),data=CRF,family="binomial");summary(mod2b)

mod2b=glmer(abx~TRT + pred:TRT +(1|site) + (pred|clid:site),data=CRF,family="binomial")# %>% mutate(pred=pred*100/10),family="binomial");summary(mod2b)
ranef(mod2b)

#mod2c=glmer(abx~noTRT + age + pred:TRT +(1|site) + (pred|clid:site),data=CRF,family="binomial");summary(mod2c)
##
ggpredict(mod1,condition=c(TRT="0"),type="fe")
ggpredict(mod1,condition=c(TRT="1"),type="fe")
ggpredict(mod1b,type="fe")

exp(summary(mod1)$coef[2,1] + qnorm(.975)*summary(mod1)$coef[2,2])
exp(summary(mod1)$coef[2,1] - qnorm(.975)*summary(mod1)$coef[2,2])
exp(summary(mod1)$coef[4,1] + qnorm(.975)*summary(mod1)$coef[4,2])
exp(summary(mod1)$coef[4,1] - qnorm(.975)*summary(mod1)$coef[4,2])
exp(summary(mod1b)$coef[2,1] + qnorm(.975)*summary(mod1b)$coef[2,2])
exp(summary(mod1b)$coef[2,1] - qnorm(.975)*summary(mod1b)$coef[2,2])
#

ggpredict(mod2b,condition=c(TRT="1"),type="fe")

sv=seq(0,.9,by=.1) %>% purrr::map(function(x) {
tst=data.frame(ggpredict(mod2,condition=c(Period="Period 2",pred=x),type="fe"))
paste0(round(tst[2,2],2)," (",round(tst[2,4],2),", ",round(tst[2,5],2),")")}) 

matrix(unlist(sv),ncol=1)

sv=seq(0,.9,by=.1) %>% purrr::map(function(x) {
  tst=data.frame(ggpredict(mod2b,condition=c(pred=x),type="fe"))
  paste0(round(tst[2,2],2)," (",round(tst[2,4],2),", ",round(tst[2,5],2),")")}) 

matrix(unlist(sv),ncol=1)
exp(summary(mod2)$coef[2,1] - qnorm(.975)*summary(mod2)$coef[2,2])
exp(summary(mod2)$coef[2,1] + qnorm(.975)*summary(mod2)$coef[2,2])

exp(summary(mod2)$coef[4,1] - qnorm(.975)*summary(mod2)$coef[4,2])
exp(summary(mod2)$coef[4,1] + qnorm(.975)*summary(mod2)$coef[4,2])

exp(summary(mod2b)$coef[2,1] + qnorm(.975)*summary(mod2b)$coef[2,2])
exp(summary(mod2b)$coef[2,1] - qnorm(.975)*summary(mod2b)$coef[2,2])


exp(summary(mod2)$coef[5,1] - qnorm(.975)*summary(mod2)$coef[5,2])
exp(summary(mod2)$coef[5,1] + qnorm(.975)*summary(mod2)$coef[5,2])

exp(summary(mod2)$coef[6,1] - qnorm(.975)*summary(mod2)$coef[6,2])
exp(summary(mod2)$coef[6,1] + qnorm(.975)*summary(mod2)$coef[6,2])

exp(summary(mod2b)$coef[3,1] + qnorm(.975)*summary(mod2b)$coef[3,2])
exp(summary(mod2b)$coef[3,1] - qnorm(.975)*summary(mod2b)$coef[3,2])

#
qplot(date,age,data=CRF) + facet_wrap(~location)

CRF %>% select(site,clid,TRT,abx) %>%  arrange(clid) %>% group_by(site,clid) %>% summarize(mean(TRT==1)-mean(TRT==0))



a=data.frame(ggpredict(mod1b,terms="TRT",type="fe")) %>% ggplot(aes(x=as.factor(x),y=predicted,ymin=conf.low,ymax=conf.high)) + geom_point() + geom_errorbar() + 
  geom_errorbar(data=data.frame(ggpredict(mod1b,terms="TRT",type="re")),aes(ymin=conf.low,ymax=conf.high),color="red",alpha=.25) + theme_bw() + 
  xlab("")

b=data.frame(ggpredict(mod1b,terms=c("TRT","site","clid"),type="re")) %>% ggplot(aes(x=as.factor(x),y=predicted,group=facet,color=facet)) + geom_line() + 
  geom_point()+facet_wrap(~group) + 
  theme(legend.position="none") + theme_bw()+ 
  xlab("TRT")

grid.arrange(a,b,ncol=2)


CRF$pred1=predict(mod1,type="response")
CRF$pred2=predict(mod1,type="response",re.form=NA)
CRF$pred3=predict(mod2,type="response")
CRF$pred4=predict(mod2,type="response",re.form=NA)

CRF$pred5=predict(mod1b,type="response",re.form=NA)
CRF$pred6=predict(mod1b,type="response")

CRF$pred7=predict(mod2b,type="response",re.form=NA)
CRF$pred8=predict(mod2b,type="response")

CRF %>% ggplot(aes(x=TRT,pred6,group=clid,color=clid)) + geom_line() + 
  geom_line(aes(y=pred5),size=1.5,color=1) + facet_wrap(~location)


CRF=CRF %>% mutate(Period=recode_factor(Period,"0"="Period 1","1"="Period 2"))
CRF_pt=CRF %>% filter(TRT==0)

newdat=CRF %>% ungroup() %>% select(TRT,Period,pred,location,pred4) %>% distinct %>% mutate(TRT=as.factor(TRT))
newdat2=CRF %>% ungroup() %>% select(TRT,Period,pred,location,pred4) %>% distinct %>% mutate(TRT=as.factor(TRT))%>%
  filter(TRT==0)

ggplot(CRF %>% mutate(Period=recode_factor(Period,"0"="Period 1","1"="Period 2")) , aes(x=pred, y=abx)) + 
  #geom_point(size=3) +
  geom_point(data=CRF_pt,aes(y=pred3,group=interaction(clid,TRT),color=clid)) +
  geom_line(aes(y=pred3,group=interaction(clid,TRT),color=clid),size=.75,alpha=.75) + 
  geom_point(data=newdat2,aes(y=pred4,group=TRT),size=2.5) +
  geom_line(data=newdat,aes(y=pred4,group=TRT),size=1.5,linetype="twodash")  +
  facet_wrap(~location+Period) +
  theme_bw()  + ylab("Probability of Abx") + xlab("Calculator Prediction (0 is no DEP)")+ 
  theme(legend.position="none",
        strip.text = element_text(size=14)) 

ggsave(filename="FigS4.tiff",units="in",width=8,height=8,dpi=600)


#ggsave(filename="LocPredEf_pd.png",unit="in",width=8,height=8,dpi=600)



newdat=CRF %>% ungroup() %>% select(TRT,pred,location,pred7) %>% distinct %>% mutate(TRT=as.factor(TRT))
newdat2=CRF %>% ungroup() %>% select(TRT,pred,location,pred7) %>% distinct %>% mutate(TRT=as.factor(TRT)) %>%
  filter(TRT==0)

###
sv=seq(.1,1,by=.1) %>% purrr::map(~data.frame(ggpredict(mod2,condition=c(pred=.,Period="Period 1"))$TRT))
sv2=seq(.1,1,by=.1) %>% purrr::map(~data.frame(ggpredict(mod2,condition=c(pred=.,Period="Period 2"))$TRT))



sv=bind_rows(sv) %>% distinct %>% mutate(pred=seq(0,1,by=.1),Period="Period 1")
sv2=bind_rows(sv2) %>% distinct %>% mutate(pred=seq(0,1,by=.1),Period="Period 2")

newdat3=rbind(sv,sv2)%>% filter(pred>0)
#newdat3=rbind(newdat3,newdat3) %>% mutate(location=c(rep("Mali",10),rep("Bangladesh",10)),abx=1)

newdat4=rbind(sv,sv2) %>% filter(pred==0)
#newdat3$hj=c(rep(0,7),3,3,3,rep(0,10))
library(ggrepel)

Fig2=ggplot(newdat3 , aes(x=pred, y=predicted)) + 
  #geom_text(aes(hjust=hj,label=paste0(round(predicted,2)," (",round(conf.low,2),", ",round(conf.high,2), ")")),size=3) + 
  #geom_text_repel(aes(hjust=hj,label=paste0(round(predicted,2)," (",round(conf.low,2),", ",round(conf.high,2), ")")),size=3,nudge_x=.2) +
  geom_line(size=1.5,linetype="twodash") +
  geom_ribbon(data=newdat3,aes(x=pred,ymin=conf.low,ymax=conf.high),alpha=.15) +
  geom_point(data=newdat4,aes(x=pred)) +
  geom_errorbar(data=newdat4,aes(x=pred,ymin=conf.low,ymax=conf.high),alpha=.15,size=2) +
  
  theme_bw() + facet_wrap(~Period) + ylab("Probability of Abx") + 
  xlab("Calculator Prediction")+ theme(legend.position="none",
                                                     strip.text = element_text(size=14)) + 
  scale_x_continuous(breaks=seq(0,1,by=.25),limits=c(-.02,1),label=c("No TRT",seq(.25,1,by=.25)));Fig2
#ggsave(filename="LocPredEf.png",unit="in",width=8,height=6)
ggsave(filename="Fig2.tiff",units="in",width=8,height=6,dpi=600)


###
sv=seq(.1,1,by=.1) %>% purrr::map(~data.frame(ggpredict(mod2b,condition=c(pred=.))$TRT))

sv=bind_rows(sv) %>% distinct %>% mutate(pred=seq(0,1,by=.1))
newdat3=sv%>% filter(pred>0)
newdat3=rbind(newdat3,newdat3) %>% mutate(location=c(rep("Mali",10),rep("Bangladesh",10)),abx=1)

newdat4=sv %>% filter(pred==0)
newdat4=rbind(newdat4,newdat4) %>% mutate(location=c(rep("Mali",1),rep("Bangladesh",1)),abx=1)
newdat4=rbind(newdat4,newdat4) %>% mutate(pred=c(-.01,-.01,.01,.01))

plt1=ggplot(CRF , aes(x=pred, y=abx)) + 
  #geom_point(size=3) +
  geom_point(data=CRF_pt,aes(y=pred8,group=interaction(clid,site,TRT),color=clid)) +
  geom_line(aes(y=pred8,group=interaction(clid,site,TRT),color=clid),size=.75,alpha=.75) +
  geom_point(data=newdat2,aes(y=pred7,x=pred,group=TRT),size=2.5) +
  geom_line(data=newdat,aes(y=pred7,x=pred,group=TRT),size=1.5,linetype="twodash") +
  geom_ribbon(data=newdat3,aes(x=pred,ymin=conf.low,ymax=conf.high),alpha=.15) +
  geom_ribbon(data=newdat4,aes(x=pred,ymin=conf.low,ymax=conf.high),alpha=.15) +
  
  theme_bw() + facet_wrap(~location) + ylab("Probability of Abx") + 
  xlab("Calculator Prediction (0 is no DEP)")+ theme(legend.position="none",
                                                     strip.text = element_text(size=14)) + 
  scale_x_continuous(breaks=seq(0,1,by=.25),limits=c(-.02,1),label=c("No TRT",seq(.25,1,by=.25)));plt1
#ggsave(filename="LocPredEf.png",unit="in",width=8,height=6)

###
df_slp=data.frame(id=row.names(ranef(mod2)$`clid:site`),
                    slope=(summary(mod2)$coef[4,1] + ranef(mod2b)$`clid:site`[,2]))# %>% 

colnames(ClinDem)=c("Index","Site","ID","Age","Sex","location")

df_slp %>% ggplot(aes(x=slope)) + geom_histogram() + facet_wrap(~gsub(".*:","",id))
summary(lm(slope~I(relevel(factor(gsub(".*:","",id)),ref=7)),data=df_slp))


df_slp2=df_slp %>% separate(id,into=c("ID","Site")) %>% mutate(location=rep(c("Bangladesh","Mali"),15)) %>% mutate(ID=as.numeric(ID))
df_slp2$location[28]="Bangladesh"
df_slp2$location[29]="Mali"

df_slp2=df_slp2 %>% left_join(ClinDem,by=c("ID","location"))
df_slp2 = df_slp2 %>% mutate(Age=as.numeric(substr(Age,1,2)))

ggplot(df_slp2,aes(x=Age,y=slope)) + geom_point()
summary(lm(slope~Age+Site.x,data=df_slp2))


CRF=CRF %>% mutate(q8=case_when(q8=="2"~"No",q8=="1"~"Yes",TRUE~q8))
CRF=CRF %>% mutate(slope=df_slp$slope[match(paste0(clid,":",site),df_slp$id)])

CRF %>% select(clid,patid,)

CRF %>% group_by(location,site,clid,Period) %>% summarize(n=n(),slope=mean(slope),blood=mean(q8=="Yes")) %>% mutate(prop=n/(first(n)+last(n))) %>% filter(Period==0) %>%
  ggplot(aes(x=prop,y=slope)) + geom_point() + facet_wrap(~location)

ggplot(CRF,aes(x=date,y=slope)) + geom_point() + facet_wrap(~location)


CRF$q8

CRF$slope


summary(lm(CRF$slope~(CRF %>% mutate(q8=case_when(q8=="2"~"No",q8=="1"~"Yes",TRUE~q8)))$q8))

CRF %>% mutate(q8=case_when(q8=="2"~"No",q8=="1"~"Yes",TRUE~q8)) %>% group_by(clid) %>% summarize(slope=mean(slope),blood=mean(q8=="Yes",na.rm=T))



CRF %>% group_by(location) %>% summarize(min(admdate),max(admdate))

###

#newdat <- CRF %>% ungroup() %>% select(TRT,Period) %>% distinct
#newdat2 <- CRF %>% ungroup() %>% select(TRT,pred,Period) %>% distinct
newdat=CRF %>% ungroup() %>% select(TRT,location,Period,pred2) %>% distinct  %>% mutate(TRT=as.factor(TRT),Period=as.factor(Period))
newdat2=CRF %>% ungroup() %>% select(TRT,pred,location,Period,pred4) %>% distinct %>% mutate(TRT=as.factor(TRT),Period=as.factor(Period))


ggplot(CRF %>% mutate(TRT=as.factor(TRT),Period=as.factor(Period)), aes(x=TRT, y=abx)) +
  #geom_point(size=3) +
  geom_line(aes(y=pred1,group=interaction(clid,site),color=clid)) +
  geom_point(aes(y=pred1,group=interaction(clid,site),color=clid,shape=Period)) +
  geom_line(data=newdat,aes(y=pred2,x=TRT,group=Period),size=1.5) +
  geom_point(data=newdat,aes(y=pred2,x=TRT,group=Period,shape=Period),size=1.5) +
  
  theme_bw() + facet_wrap(~location) + theme(legend.position="none") + ylab("Probability of Abx") + xlab("DEP")


ggplot(CRF , aes(x=pred, y=abx)) + 
  #geom_point(size=3) +
  geom_point(aes(y=pred3,group=interaction(clid,site,TRT),color=clid)) +
  geom_line(aes(y=pred3,group=interaction(clid,site,TRT),color=clid)) +
  geom_point(data=newdat2,aes(y=pred4,x=pred,group=interaction(Period,TRT),shape=Period),size=2.5) +
  geom_line(data=newdat2,aes(y=pred4,x=pred,group=interaction(Period,TRT)),size=1.5) +
  
  theme_bw() + facet_wrap(~location) + ylab("Probability of Abx") + xlab("Calculator Prediction (0 is no DEP)")+ theme(legend.position="none") 



###



#

#
ggplot(CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==4,location=="Bangladesh"),aes(x=date,y=interaction(TRT,Period))) + 
  geom_point()

ggplot(CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==9,location=="Bangladesh"),aes(x=date,y=interaction(TRT,Period))) + 
  geom_point()
#
ggplot(CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==7,location=="Mali"),aes(x=date,y=interaction(TRT,Period))) + 
  geom_point()

ggplot(CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==8,location=="Mali"),aes(x=date,y=interaction(TRT,Period))) + 
  geom_point()

ggplot(CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==10,location=="Mali"),aes(x=date,y=interaction(TRT,Period))) + 
  geom_point()
ggplot(CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==14,location=="Mali"),aes(x=date,y=interaction(TRT,Period))) + 
  geom_point()

CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==7,location=="Mali")
CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==8,location=="Mali")
CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==10,location=="Mali")
CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==14,location=="Mali")

CRF %>% select(location,site,clid,patid,date,TRT,Period,pred) %>% filter(clid==7,location=="Mali")
CRF2 %>% filter(clid==8,patid==4335) %>% select(q17)
CRF2 %>% filter(clid==10,patid==4501) %>% select(q17)
CRF2 %>% filter(clid==14,patid==4746) %>% select(q17)

###
summary(mod2b)
sigma=matrix(c(1.049,-.16*1.024*1.463,-.16*1.024*1.463,2.141),nrow=2,ncol=2);sigma

qplot(summary(mod2b)$coef[3,1]+rmvnorm(1000,sigma=sigma)[,2])

###
list(mod1,mod1b,mod2,mod2b) %>% purrr::map(function(x){ 
  mod=summary(x)
  co=coef(mod)
  data.frame(Variable=row.names(co),
             Estimate=paste0(round(co[,1],2)," (",
                      round(co[,1]-qnorm(.975)*co[,2],2),"-",round(co[,1]+qnorm(.975)*co[,2],2),")"),
             `P-value`=round(co[,3],3))
  rbind(c("Group","","SD/Cor"),
  data.frame(mod$varcor) %>% select(Variable=grp,Estimate=var1,`P-value`=sdcor) %>% mutate(`P-value`=round(`P-value`,3))
  )
  
  })

library(stargazer)

modlist=list(mod1,mod2b,mod2)

summary(mod1b)$varcor;unlist(summary(mod1b)$varcor)
summary(mod1)$varcor;unlist(summary(mod1)$varcor)
summary(mod2b)$varcor;unlist(summary(mod2b)$varcor)
summary(mod2)$varcor;unlist(summary(mod2)$varcor)

unlist(modlist %>% purrr::map(~sqrt(unlist(summary(.)$varcor)[1])))
unlist(modlist %>% purrr::map(~sqrt(unlist(summary(.)$varcor)[2])))

stargazer(mod1b,mod1,mod2b,mod2,type="html",out="tstmod.doc",intercept.bottom=F,intercept.top=T,ci=T,digits=2,model.names=T,
          omit.table.layout = "s",
          column.labels=c("Period Effect","No Period Effect"),
          single.row=T,
          covariate.labels=c("Intercept","DEP","Period 2","DEP:Prediction","DEP:Prediction:Period 2"),
          add.lines=list(c("Groups","Std.Dev."),
                         c("Clin. ID (Intercept)",unlist(modlist %>% purrr::map(~round(sqrt(unlist(summary(.)$varcor)[1]),3)))),
                         c("Clin. ID (DEP Prediction)","","",unlist(modlist[3:4] %>% purrr::map(~round(sqrt(unlist(summary(.)$varcor)[4]),3)))),
                         c("Site (Intercept)",
                           unlist(modlist[1:2] %>% purrr::map(~round(sqrt(unlist(summary(.)$varcor)[2]),3))),
                           unlist(modlist[3:4] %>% purrr::map(~round(sqrt(unlist(summary(.)$varcor)[5]),3))))))

stargazer(mod1,mod2,type="html",out="tstmodV2.doc",intercept.bottom=F,intercept.top=T,ci=T,digits=2,model.names=T,
          omit.table.layout = "s",
          column.labels=c("Period Effect","No Period Effect"),
          single.row=T,
          #covariate.labels=c("Intercept","DEP","Period 2","DEP:Prediction","DEP:Prediction:Period 2"),
          apply.coef=function(x) exp(x))


mods=summary(mod2)
df_okq=data.frame(Variable=as.character(rownames(mods$coefficient)),
                  Estimate=mods$coefficients[,1],
                  SE=mods$coefficients[,2],
                  z=mods$coefficients[,3],
                  Pvalue=mods$coefficients[,4])

df_okq=df_okq %>% #[1:8,] %>% 
  mutate(OddsRatio=round(exp(Estimate),2),Low=round(exp(Estimate-qnorm(.975)*SE),2),High=round(exp(Estimate+qnorm(.975)*SE),2),
         Odds=paste(OddsRatio," (",Low," - ",High,")",sep=""),
         P.value=round(Pvalue,3),)  %>% select(Variable,Odds, P.value)



source("functions_2019.r")

CRF %>% ungroup() %>% group_by(Period,TRT) %>% dplyr::summarize(median(age),quantile(age,1/4),quantile(age,3/4))

t(CRF %>% ungroup() %>% summarize(N=n(),Providers=length(unique(clid)),
                                  `Sex (Male)`= paste0(round(sum(sex=="Male"),2)," (",round(sum(sex=="Male")/N,2),")"),
                                  Age= paste0(round(median(age),2)," (",round(IQR(age),2),")"),
                                  `Bloody Diarrhea`= paste0(round(sum(q8=="Yes"),2)," (",round(sum(q8=="Yes")/N,2),")"),
                                  `Vomit`= paste0(round(sum(q11=="Yes"),2)," (",round(sum(q11=="Yes")/N,2),")"),
                                  `Partial`= paste0(round(sum(q9=="Partial"),2)," (",round(sum(q9=="Partial")/N,2),")"),
                                  `Exclusive`= paste0(round(sum(q9=="Exclusive"),2)," (",round(sum(q9=="Exclusive")/N,2),")"),
                                  MUAC= paste0(round(mean(q15),2)," (",round(sd(q15),2),")"),
                                  `Assigned to Treatment`= paste0(round(sum(TRT==1),2)," (",round(sum(TRT==1)/N,2),")"),
                                  `Antibiotic Ordered`= paste0(round(sum(abx==1),2)," (",round(sum(abx==1)/N,2),")"),))

tbl1=CRF %>% select(sex,age,`Bloody Diarrhea`=q8,`Vomit`=q11,`Breastfeeding`=q9,MUAC=q15,`Antibiotic Ordered`=abx,`Diarrhea Resolution at 10 Days`=q33)
tp=c(2,0,2,2,2,0,2,2) # type of varyiables : 0 - continuos, 1- continuous & non parametric, 2- categorical

grp=CRF$TRT==1; ### grp nees to be true/f alse
#dnamesx=c("Travel injury","Diarrhea","Fever","Bugbites","Sleep","Jetlag","Sick return")
t3=my2grtab(data.frame(tbl1),gnames=c("DEP","No DEP"),gvar=grp,typev=tp,output="Table1.doc") ;t3


##
datDep=read_csv("DEPFolder_3_16_21.csv")

datDep$`Dehydration Status`


datDep %>% select(patid=`Patient Id`,`Dehydration Status`) %>% right_join(CRF) %>%
  group_by(location) %>% dplyr::summarize(mean(`Dehydration Status`==3,na.rm=T))



CRF=CRF %>% mutate(q31=case_when(q31=="2"~"No",q31=="1"~"Yes",TRUE~q31))
CRF=CRF %>% mutate(q32=case_when(q32=="2"~"No",q32=="1"~"Yes",TRUE~q32))
CRF=CRF %>% mutate(q34=case_when(q34=="2"~"No",q34=="1"~"Yes",TRUE~q34))
CRF=CRF %>% mutate(q35=case_when(q35=="2"~"No",q35=="1"~"Yes",TRUE~q35))
CRF=CRF %>% mutate(q36=case_when(q36=="2"~"No",q36=="1"~"Yes",TRUE~q36))

tbl2=CRF %>% ungroup() %>% select(`Able to contact`=q31,`Died after discharge`=q32,`Diarrhea Resolution at 10 Days`=q33,
                    Cough=q34,`Difficulty breath`=q35,`Runny nose`=q36)
tp=c(2,2,2,2,2,2) # type of varyiables : 0 - continuos, 1- continuous & non parametric, 2- categorical
grp=CRF$TRT==1; ### grp nees to be true/f alse

t3=my2grtab(data.frame(tbl2),gnames=c("DEP","No DEP"),gvar=grp,typev=tp,output="Table2.doc") ;t3




###

CRF %>% filter(patid==3404)


GEMS=CRF %>% ungroup() %>% mutate(ageyr=replace_na(ageyr,0),agemo=replace_na(agemo,0),base_age=12*ageyr+agemo) %>%
  mutate(dt=as.numeric(ymd(date))-13848,
    base_age = case_when(base_age <= 24 ~ base_age,
                              base_age>24 & base_age < 36 ~ as.double(24),
                              base_age>=36 & base_age < 48 ~ as.double(36),
                              base_age>=48 & base_age < 60 ~ as.double(48)),
         f4b_muac=q15,
         f4a_drh_blood=as.numeric(q8!="No" & q8!="2"),
         any_breast_fed=as.numeric(q9 %in% c("Partial","Exclusive","1","2")),
         f4a_drh_vomit=as.numeric(q11!="No"& q11!="2"),
    Seasonal_sine=sin(2*pi*dt/365.25),
    Seasonal_cosine=cos(2*pi*dt/365.25)) %>% select(clid,patid,Site=location,f4b_muac,base_age,f4a_drh_blood,any_breast_fed,f4a_drh_vomit,dt,Seasonal_sine,Seasonal_cosine,
                                                    TRT,Period,abx,pred,site)
GEMS = GEMS %>% mutate_at(vars(c("f4a_drh_blood","f4a_drh_vomit","any_breast_fed")),~as.factor(.))
GEMS = GEMS %>% mutate(Site=case_when(Site=="Bangladesh" ~ 6, TRUE ~ 2))

out=readRDS("C://Users//u6020766//Google Drive//UU//VIDA//PTOmods//CurPatMod")
xs=readRDS("C://Users//u6020766//Google Drive//UU//VIDA//PTOmods//xs")
OR=readRDS("C://Users//u6020766//Google Drive//UU//VIDA//PTOmods//CurPatOR")
closest=predict(out,newdata=GEMS,type="response") %>% purrr::map(function(y) vapply(y,function(x) which.min(abs(xs-x)),c(1)))
LR=OR[unlist(closest)]    
GEMS$LR=LR

mods2=readRDS("C://Users//u6020766//Google Drive//UU//VIDA//PTOmods//SeasMod")
OR_Seas=readRDS("C://Users//u6020766//Google Drive//UU//VIDA//PTOmods//OR_Seas")
closest_seas=GEMS%>% split(.$Site) %>% purrr::map(~predict(mods2[[.$Site[1]]],newdata=.,type="response")) %>% purrr::map(function(y) vapply(y,function(x) which.min(abs(xs-x)),c(1)))
num=GEMS %>% split(.$Site) %>% purrr::map(~.$Site[1])
LR_Seas=map2(closest_seas,num, function(x,y) OR_Seas[[y]][as.vector(unlist(x))])
GEMS$LR_Seas=c(LR_Seas[['6']],LR_Seas[['2']])
GEMS$LRprob=with(GEMS,plogis(LR))

#GEMS$LRprob=with(GEMS,plogis(LR))

GEMS = GEMS %>% mutate(LRprob = case_when(pred==0~LRprob,TRUE~pred))
GEMS = GEMS %>% mutate(TRT = case_when(TRT==0~"Control",TRUE~"TRT"))

GEMS = GEMS %>% arrange(LRprob)

GEMS = GEMS %>% group_by(TRT,Period) %>% mutate(abx_pred=unlist(slide_index(.x=abx,.i=LRprob,.f=~mean(.x),.before=.1,.after=.1)))

GEMS %>% ggplot(aes(x=LRprob,y=abx_pred,group=interaction(TRT,Period),color=interaction(TRT,Period))) + geom_line(size=1) + theme_bw() + 
  scale_color_viridis_d() + ylab("Abx Rate (Moving average)") + xlab("Predicted value")

cbind(CRF$pred,GEMS$LRprob)
###
GEMS %>% filter(patid==3106)

predict(out,GEMS %>% filter(patid==3106),type="response")

GEMS = GEMS %>% mutate(pred2=case_when(site %in% c("34","37") ~ plogis(LR),
                                       TRUE ~ plogis(LR+LR_Seas)),
                       pred=case_when(TRT=="Control"~pred2,TRUE~pred))
  

data.frame(GEMS %>% select(patid,site,pred,pred2))


#mod2=glmer(abx~pred:TRT*Period +(1|site) + (pred|clid:site),data=GEMS,family="binomial",
#           control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)));summary(mod2)

mod2=glmer(abx~pred*Period + pred:TRT*Period +(1|site) + (pred|clid:site),data=GEMS %>% mutate(pred=pred*100/10),family="binomial");summary(mod2)
#mod2=glmer(abx~ pred:TRT +(1|site) + (pred|clid:site),data=GEMS %>% mutate(pred=pred*100/10),family="binomial");summary(mod2)



exp(summary(mod2)$coef[5,1])
exp(summary(mod2)$coef[5,1] - qnorm(.975) * summary(mod2)$coef[5,2])
exp(summary(mod2)$coef[5,1] + qnorm(.975) * summary(mod2)$coef[5,2])


#mod2=glm(abx~pred*TRT*Period,data=GEMS,family="binomial");summary(mod2)



GEMS$pred3=predict(mod2,type="response")
GEMS$pred4=predict(mod2,type="response",re.form=NA)

GEMS=GEMS %>% mutate(Period=recode_factor(Period,"0"="Period 1","1"="Period 2"))
CRF_pt=GEMS %>% filter(TRT==0)

newdat=GEMS %>% ungroup() %>% select(TRT,Period,pred,pred3,pred4) %>% distinct %>% mutate(TRT=as.factor(TRT),
                                                                                          Group=recode_factor(TRT,"Control"="Control","TRT"="DEP"))
newdat2=GEMS %>% ungroup() %>% select(TRT,Period,pred,pred3,pred4) %>% distinct %>% mutate(TRT=as.factor(TRT))%>%
  filter(TRT==0)

ggplot(GEMS %>% mutate(Period=recode_factor(Period,"0"="Period 1","1"="Period 2"),
                       Site=recode_factor(Site,"6"="Bangladesh","2"="Mali")) , aes(x=pred, y=abx)) + 
  #geom_point(size=3) +
  #geom_point(data=CRF_pt,aes(y=pred3,group=interaction(clid,TRT),color=clid)) +
  #geom_line(aes(y=pred3,group=interaction(clid,TRT),color=clid),size=.75,alpha=.75) + 
  geom_line(data=newdat,aes(y=pred4,group=Group,color=Group),size=1.5,linetype="twodash")  +
  facet_wrap(~Period)  +
  theme_bw()  + ylab("Probability of Abx") + xlab("Calculator Prediction") +
  scale_color_viridis_d(begin=.25,end=.75)
ggsave(filename="FigS5.tiff",units="in",width=8,height=6,dpi=600)



write.csv(CRF[,1:72],file="Phase2deidCRF.csv")
