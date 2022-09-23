

#header #################################################################################
#'tidy_data.R'

#Title: title
#Project ID: pid
#Client: client
#Author: <Eduardo> <Costa>, UFRGS

#Description: description

#Start date: date
#Last Update: {6:date}

#R version: r.version
#Scriptversion: version

#Dependencies
#<-Downstream
  #-Main.R

#->Upstream

#Input:
#- "Data/Raw/populacao.xlsx"

#Output:
#- "Data/Processed/inc_fin.csv"
#- "Data/Processed/conf_inc.csv"


#Peer reviewer(s)

#Please ensure directories are relative. Please comment code sufficiently.

#Script start#############################################################################




#Reading the data
pop<-read_excel(here("Data","Raw","populacao.xlsx"))

# Subtract supplementary health data from the total population (ANS)


incidencia <- read_excel(here("Data","Raw","incidencia.xlsx"), 
                         col_types = c("date", "skip", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))

incidencian<- read_excel(here("Data","Raw","incidencian.xlsx"), 
                         col_types = c("date", "skip", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))

obito<- read_excel(here("Data","Raw",  "obito.xlsx"), 
                         col_types = c("date", "skip", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))

obiton<- read_excel(here("Data","Raw",  "obiton.xlsx"), 
                   col_types = c("date", "skip", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))

ans<-read.csv2(here("Data","Processed",  "ANS.csv"))


#Tidy the ANS data for linear interpolation


time1<-c(1:dim(incidencia)[1])


insurance<-list()
ANS<-list()

for (j in 3: 14){
  ANS1<-approxfun(ans[,2],ans[,j])
  insurance[[j]]<-ANS1(time1)
  insurance[[j]][1:2]<-insurance[[j]][3]
}

insurance1<-cbind.data.frame(incidencia[,1],insurance[[3]],insurance[[4]],insurance[[5]],insurance[[6]],insurance[[7]],insurance[[8]],
                 insurance[[9]],insurance[[10]],insurance[[11]],insurance[[12]],insurance[[13]],insurance[[14]])

names(insurance1)<-names(ans)[c(1,3:14)]


#Grouping ages 

#< 1 year  
#1-4 years 
#5 - 9 years
#10-19 years 
#20 - 59 years 
#60-79 years  
#> 80 years 

#insurance
insurance1$f1019<-apply(cbind(insurance1$f1014,insurance1$f1519),1,sum)

insurance1$f2059<-apply(cbind(insurance1$f2029,insurance1$f3039,insurance1$f4049,insurance1$f5059),1,sum)

insurance1$f6079<-apply(cbind(insurance1$f6069,insurance1$f7079),1,sum)


#incidence
incidencia$f1019<-apply(cbind(incidencia$f1014,incidencia$f1519),1,sum)

incidencia$f2059<-apply(cbind(incidencia$f2029,incidencia$f3039,incidencia$f4049,incidencia$f5059),1,sum)

incidencia$f6079<-apply(cbind(incidencia$f6069,incidencia$f7079),1,sum)

#non-incidence
incidencian$f1019<-apply(cbind(incidencian$f1014,incidencian$f1519),1,sum)

incidencian$f2059<-apply(cbind(incidencian$f2029,incidencian$f3039,incidencian$f4049,incidencian$f5059),1,sum)

incidencian$f6079<-apply(cbind(incidencian$f6069,incidencian$f7079),1,sum)




obito$f1019<-apply(cbind(obito$f1014,obito$f1519),1,sum)

obito$f2059<-apply(cbind(obito$f2029,obito$f3039,obito$f4049,obito$f5059),1,sum)

obito$f6079<-apply(cbind(obito$f6069,obito$f7079),1,sum)



obiton$f1019<-apply(cbind(obiton$f1014,obiton$f1519),1,sum)

obiton$f2059<-apply(cbind(obiton$f2029,obiton$f3039,obiton$f4049,obiton$f5059),1,sum)

obiton$f6079<-apply(cbind(obiton$f6069,obiton$f7079),1,sum)





##Calculating the adjustment variables
#For incidence
conf_inc1<-incidencian[,2:16]-0
#For death
conf_obito<-obiton[,2:16]-0

##Calculating lethality
#PNM
letha<-cbind(incidencia[,1],obito[,2:16]/incidencia[,2:16])
write.csv(letha, here("Data","Processed","letha.csv"),row.names = F)

#Non=PNM
conf_letha<-cbind(incidencia[,1],conf_obito/conf_inc1)
write.csv(conf_letha, here("Data","Processed","conf_letha.csv"),row.names = F)

#Tidying the total population
pop$age<-cut(pop$IDADE, c(-Inf,0,4,9,14,19,29,39,49,59,69,79,Inf))
table(pop$age)

pop_total<-gather(pop[,2:22],key="year",value="pop",-age)%>%
  group_by(year,age)%>%
  summarise(tot=sum(pop))

pop_total1<-spread(subset(pop_total,year!="2000" & year!= "2001"),key="age",-year)



pop_total1$`(9-19]`<-apply(cbind(pop_total1$`(9,14]`,pop_total1$`(14,19]`),1,sum)

pop_total1$`(19-59]`<-apply(cbind(pop_total1$`(19,29]`,pop_total1$`(29,39]`,pop_total1$`(39,49]`,pop_total1$`(49,59]`),1,sum)

pop_total1$`(59-79]`<-apply(cbind(pop_total1$`(59,69]`,pop_total1$`(69,79]`),1,sum)





incidencia$dataa<-as.character.Date(incidencia$dataa)



#Creating the total population dataset
pop_total1.1<-as.matrix.data.frame(rbind(
matrix(rep(pop_total1[1,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[2,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[3,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[4,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[5,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[6,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[7,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[8,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[9,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[10,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[11,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[12,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[13,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[14,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[15,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[16,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[17,2:16],12),nrow=12,byrow = T),
matrix(rep(pop_total1[18,2:16],12),nrow=12,byrow = T)
))

pop_total1.2<-as.data.frame(pop_total1.1)

names(pop_total1.2)<-names(pop_total1[c(2:16)])

#Subtracting the insurance from the pop

pop_total2<-pop_total1.2-insurance1[,c(2:16)]


##Calculating the incidence ratio per 100000 inhab

pop_total_target<-subset(pop_total,age %in% c("(-Inf,0]","(0,4]","(4,9]"))
pop_total_target$year<-as.double(pop_total_target$year)

inc_target<-incidencia[,1:4]
inc_target$dataa<-as.Date(inc_target$dataa)

inc_target<-inc_target%>%
    group_by(year=year(dataa))%>%
      summarise("(-Inf,0]"=sum(flt01),"(0,4]"=sum(f0104),"(4,9]"=sum(f0509))

inc_target<-gather(inc_target,key="age",value="inc",-year)

insu_target<-insurance1[,1:4]


insu_target<-insu_target%>%
  group_by(year=year(data))%>%
  summarise("(-Inf,0]"=mean(flt01),"(0,4]"=mean(f0104),"(4,9]"=mean(f0509))

insu_target<-gather(insu_target,key="age",value="inc",-year)
colnames(insu_target)[3]<-"insu"


non_inc<-incidencian[,1:4]

non_inc<-non_inc%>%
  group_by(year=year(dataa))%>%
  summarise("(-Inf,0]"=sum(flt01),"(0,4]"=sum(f0104),"(4,9]"=sum(f0509))

non_inc<-gather(non_inc,key="age",value="inc",-year)
colnames(non_inc)[3]<-"n_inc"



# 2010:2015; 2016:2019
total<-left_join(inc_target,pop_total_target)
total<-left_join(total,insu_target)
total<-left_join(total,non_inc)

total1<-subset(total,year>2010)
total1$vac<-ifelse(total1$year<2017,0,1)
total1$year1<-total1$year-2000
total1$pop<-total1$tot-total1$insu

#Young ages
summary(mod1<-glm(inc~year1*factor(vac)+offset(log(pop/100000)),data=subset(total1,age %in% c("(-Inf,0]")),family = poisson(link="log")))

inc_1n<-exp(9.1173324-0.0623807*c(11:19))

inc_1v<-exp(  9.1173324-(1.1441952*c(0,0,0,0,0,0,1,1,1))+ (-0.0623807+0.0679307*c(0,0,0,0,0,0,1,1,1))*c(11:19))


data1<-cbind.data.frame(year=11:19,"dose3"=inc_1n,"dose2"=inc_1v,"obs"=
                          
                          total1$inc[total1$age=="(-Inf,0]"]/total1$pop[total1$age=="(-Inf,0]"]*100000  
                          
                          )

ggplot(data=data1) + 
  theme_minimal()+
  geom_line(mapping=aes(y = dose2,x=year,colour="Fitted"),size=1)+
  geom_line(mapping=aes(y = dose3,x=year,colour="Projection"),size=1) + 
   geom_point(mapping=aes(y=obs,x=year))+
  scale_x_continuous(breaks = seq(10,19,1))+
  ggtitle("<1 year")+ # for the main title
  xlab("Year")+ # for the x axis label
  ylab("Incidence/100000 inhabitants")+ # for the y axis label
  labs(colour = " ")+
  geom_vline(xintercept = 16)
  



#Young ages
summary(mod2<-glm(inc~year1*factor(vac)+offset(log(pop/100000)),data=subset(total1,age %in% c("(0,4]")),family = poisson(link="log")))

inc_2n<-exp(8.0326757-0.0573750*c(11:19))

inc_2v<-exp(  (8.0326757-1.1257778*c(0,0,0,0,0,0,1,1,1))+ (-0.0573750+0.0689901*c(0,0,0,0,0,0,1,1,1))*c(11:19))

data2<-cbind.data.frame(year=11:19,"dose3"=inc_2n,"dose2"=inc_2v,"obs"=
                          
                          total1$inc[total1$age=="(0,4]"]/total1$pop[total1$age=="(0,4]"]*100000  )

ggplot(data2, aes(x=year)) + 
  theme_minimal()+
  geom_line(mapping=aes(y = dose2,x=year,colour="Fitted"),size=1)+
  geom_line(mapping=aes(y = dose3,x=year,colour="Projection"),size=1) + 
  geom_point(mapping=aes(y=obs,x=year))+
  scale_x_continuous(breaks = seq(10,19,1))+
  ggtitle("1 - 4 year")+ # for the main title
  xlab("Year")+ # for the x axis label
  ylab("Incidence/100000 inhabitants")+ # for the y axis label
  labs(colour = " ")+
  geom_vline(xintercept = 16)


#Young ages
summary(mod3<-glm(inc~year1*factor(vac)+offset(log(pop/100000)),data=subset(total1,age %in% c("(4,9]")),family = poisson(link="log")))

inc_3n<-exp(6.450373-0.053771*c(11:19))

inc_3v<-exp(  (6.450373-1.141105*c(0,0,0,0,0,0,1,1,1))+ (-0.053771+0.070342*c(0,0,0,0,0,0,1,1,1))*c(11:19))

data3<-cbind.data.frame(year=11:19,"dose3"=inc_3n,"dose2"=inc_3v,"obs"=
                          
                          total1$inc[total1$age=="(4,9]"]/total1$pop[total1$age=="(4,9]"]*100000  )

ggplot(data3, aes(x=year)) + 
  theme_minimal()+
  geom_line(mapping=aes(y = dose2,x=year,colour="Fitted"),size=1)+
  geom_line(mapping=aes(y = dose3,x=year,colour="Projection"),size=1) + 
  geom_point(mapping=aes(y=obs,x=year))+
  scale_x_continuous(breaks = seq(10,19,1))+
  ggtitle("4 - 9 year")+ # for the main title
  xlab("Year")+ # for the x axis label
  ylab("Incidence/100000 inhabitants")+ # for the y axis label
  labs(colour = " ")+
  geom_vline(xintercept = 16)


inc_fin<-cbind.data.frame(incidencia[,1],
incidencia[,2:16],pop_total2)
head(inc_fin)

write.csv(inc_fin, here("Data","Processed","inc_fin.csv"),row.names = F)



#Adjustment
conf_inc<-cbind.data.frame(incidencia[,1],
                          conf_inc1/pop_total2*100000)
head(conf_inc)

write.csv(conf_inc, here("Data","Processed","conf_inc.csv"),row.names = F)


##############
#Descriptives#
##############
#For incidence
ts_flt01 <- ts(inc_fin$flt01, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f0104 <- ts(inc_fin$f0104, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f0509 <- ts(inc_fin$f0509, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f1014 <- ts(inc_fin$f1014, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f1519 <- ts(inc_fin$f1519, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f2029 <- ts(inc_fin$f2029, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f3039 <- ts(inc_fin$f3039, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f4049 <- ts(inc_fin$f4049, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f5059 <- ts(inc_fin$f5059, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f6069 <- ts(inc_fin$f6069, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f7079 <- ts(inc_fin$f7079, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_fmm80 <- ts(inc_fin$fmm80, start=c(2002,1), end=c(2019, 12), frequency = 12 )

boxplot(ts_flt01~cycle(ts_flt01),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f0104~cycle(ts_f0104),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f0509~cycle(ts_f0509),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f1014~cycle(ts_f1014),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f1519~cycle(ts_f1519),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f2029~cycle(ts_f2029),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f3039~cycle(ts_f3039),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f4049~cycle(ts_f4049),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f5059~cycle(ts_f5059),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f6069~cycle(ts_f6069),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f7079~cycle(ts_f7079),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_fmm80~cycle(ts_fmm80),ylab='Incid?ncia',xlab='Meses')

#For incidence confounder
ts_flt01_2 <- ts(conf_inc$flt01, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f0104_2 <- ts(conf_inc$f0104, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f0509_2 <- ts(conf_inc$f0509, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f1014_2 <- ts(conf_inc$f1014, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f1519_2 <- ts(conf_inc$f1519, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f2029_2 <- ts(conf_inc$f2029, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f3039_2 <- ts(conf_inc$f3039, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f4049_2 <- ts(conf_inc$f4049, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f5059_2 <- ts(conf_inc$f5059, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f6069_2 <- ts(conf_inc$f6069, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_f7079_2 <- ts(conf_inc$f7079, start=c(2002,1), end=c(2019, 12), frequency = 12 )
ts_fmm80_2 <- ts(conf_inc$fmm80, start=c(2002,1), end=c(2019, 12), frequency = 12 )

boxplot(ts_flt01_2~cycle(ts_flt01),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f0104_2~cycle(ts_f0104),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f0509_2~cycle(ts_f0509),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f1014_2~cycle(ts_f1014),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f1519_2~cycle(ts_f1519),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f2029_2~cycle(ts_f2029),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f3039_2~cycle(ts_f3039),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f4049_2~cycle(ts_f4049),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f5059_2~cycle(ts_f5059),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f6069_2~cycle(ts_f6069),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_f7079_2~cycle(ts_f7079),ylab='Incid?ncia',xlab='Meses')
boxplot(ts_fmm80_2~cycle(ts_fmm80),ylab='Incid?ncia',xlab='Meses')

