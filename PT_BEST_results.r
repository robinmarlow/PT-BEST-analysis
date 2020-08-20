#PT BEST results as in paper
load("PT-BEST-JAMA.rdata")

#make neat datasubsets 
#but don't actually use them!
primary     <- subset(data, case_group=="B" & case_ageindays>=134)
all_IMD     <- subset(data, case_ageindays>=134)
B_onedose   <- subset(data, case_group=="B")
B_onedose$menBvaccinated<-B_onedose$prior_doses
all_onedose <- data
all_onedose$menBvaccinated<-all_onedose$prior_doses
  
#KEY POINTS
#Total number of subjects (cases + controls)
nrow(data)

#Total in primary analysis
nrow(subset(data,case_group=="B" & case_ageindays>=134))

# Primary outcome
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134), method="efron",robust=TRUE))


#ABSTRACT
nrow(subset(data,case))
nrow(subset(data,case & case_group=="B"))

#primary analysis
nrow(subset(data,case & case_group=="B" & case_ageindays>=134))
summary(subset(data,case & case_group=="B" & case_ageindays>=134)$ageinmonths)
summary(subset(data,case & case_group=="B" & case_ageindays>=134)$gender)
summary(subset(data,case & case_group=="B" & case_ageindays>=134)$hospital_duration)

nrow(subset(data,case & case_group=="B" & case_ageindays>=134 & menBvaccinated))
nrow(subset(data,!case & case_group=="B" & case_ageindays>=134 & menBvaccinated))

pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134), method="efron",robust=TRUE))

#all IMD
nrow(subset(data,case  & case_ageindays>=134 & menBvaccinated)) 
nrow(subset(data,case  & case_ageindays>=134))
nrow(subset(data,!case  & case_ageindays>=134 & menBvaccinated)) 
nrow(subset(data,!case  & case_ageindays>=134))
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data, case_ageindays>=134), method="efron",robust=TRUE))

# B >=1 dose
nrow(subset(data,case & case_group=="B"))
nrow(subset(data,case & case_group=="B" & case_ageindays>=134))

nrow(subset(data,case & case_group=="B" & prior_doses))
nrow(subset(data,case & case_group=="B"))

nrow(subset(data,!case & case_group=="B" & prior_doses))
nrow(subset(data,!case & case_group=="B"))

pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,case_group=="B"), method="efron",robust=TRUE))

# IMD >=1 dose
nrow(subset(data,case  & prior_doses))
nrow(subset(data,case))

nrow(subset(data,!case  & prior_doses))
nrow(subset(data,!case))

pretty_output(clogit(case ~ prior_doses + strata(case_id), data, method="efron",robust=TRUE))

##################
#outcomes of vaccinated cases
nrow(subset(data,case & prior_doses ))
nrow(subset(data,case & prior_doses & outcome!=2))
binom.test(0, 11)

#outcomes of unvaccinated cases
nrow(subset(data,case & !prior_doses ))
nrow(subset(data,case & !prior_doses & outcome!=2))
binom.test(23, 87)

#number of matches
subset(data,!case) %>% group_by(case_id) %>% summarise(count=n()) %>% select(count) %>% table()

summary(subset(data,case & prior_doses )$hospital_duration)

binom.test(7, 87)
binom.test(16, 87)
binom.test(23, 87)
summary(subset(data,case & !prior_doses )$ageinmonths)
summary(subset(data,case & !prior_doses )$hospital_duration)

#Socioeconomic analysis
#vaccine vs no vaccine
summary(subset(data,menBvac)$perCapita)
summary(subset(data,!menBvac)$perCapita)
wilcox.test(subset(data,menBvac)$perCapita,subset(data,!menBvac)$perCapita, conf.int = TRUE)

#case vs control
summary(subset(data,!case)$perCapita)
summary(subset(data,case)$perCapita)
#t.test(subset(data,case)$perCapita,subset(data,!case)$perCapita)
wilcox.test(subset(data,case)$perCapita,subset(data,!case)$perCapita,conf.int = TRUE)


#
#Screening method regresssion analysis
library("car")
screeningVac<-data.frame(
  cvacc=c(1,1,1,1),  
  #n=(subset(data,year>2014& case & nm_group=="B" ) %>% group_by(year) %>% tally(menBvaccinated==TRUE))$n,
  n=(subset(data,year>2014& case & nm_group=="B" & ageinmonths<=12) %>% group_by(year) %>% tally(no_doses_12m>=2))$n,
  year=c(2015,2016,2017,2018),
  ppv=c(0.328, 0.442, 0.535, 0.567) #12m
  #ppv=c(0.42, 0.496, 0.561, 0.569) #all
)

screeningNoVac<-data.frame(
  cvacc=c(0,0,0,0),  
  #n=(subset(data,year>2014& case & nm_group=="B" ) %>% group_by(year) %>% tally(menBvaccinated==FALSE))$n,
  n=(subset(data,year>2014& case & nm_group=="B" & ageinmonths<=12) %>% group_by(year) %>% tally(no_doses_12m<2))$n,
  year=c(2015,2016,2017,2018),
  ppv=c(0.328, 0.442, 0.535, 0.567)
  #ppv=c(0.42, 0.496, 0.561, 0.569) #all
  
)

screeningData<-rbind(screeningVac,screeningNoVac)
model<-(glm(cvacc ~ offset(logit(ppv)), weights = n, data = screeningData, family = "binomial")  )
summary(model)
exp(coef(model))
exp(confint(model))

####table 1
summary(subset(data,control & cc_ageinmonths<24)$cc_age_diff)
summary(subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60)$cc_age_diff)
summary(subset(data,control & cc_ageinmonths>=60)$cc_age_diff)

summary(subset(data,control)$cc_doa_diff)


# table 2

table2_column<- function(dataset){
  return(
    c(
      nrow(dataset),
      nrow(subset(dataset,case)),
      nrow(subset(dataset,!case)),
      paste(sprintf("%.1f",median(dataset$ageinmonths)),"\n(", sprintf("%.1f",summary(dataset$ageinmonths)[2]),'-', sprintf("%.1f",summary(dataset$ageinmonths)[5]),")"),
      nrow(subset(dataset, gender)),
      nrow(subset(dataset, !gender)),
      "",
      nrow(subset(dataset, diagnosis==3)),
      nrow(subset(dataset, diagnosis==2)),
      nrow(subset(dataset, diagnosis==1)),
      nrow(subset(dataset, diagnosis==4)),
      nrow(subset(dataset, diagnosis==5)),
      paste(median(dataset$hospital_duration),"\n(", sprintf("%.1f",summary(dataset$hospital_duration)[2]),'-',sprintf("%.1f",summary(dataset$hospital_duration)[5]),")"),
      "",
      #Outcome (Death=1; Survival with no sequelae=2; Survival with sequelae=3; Amputation=4; ENT=5; Development=6; Others=7)
      
      nrow(subset(dataset, outcome==2)),
      nrow(subset(dataset, outcome %in% c(4,5,6,"7 - Hydrocephalus + convulsions"))),
      nrow(subset(dataset, outcome==6)),
      nrow(subset(dataset, outcome==4)),
      nrow(subset(dataset, outcome==5)),
      nrow(subset(dataset, outcome=="7 - Hydrocephalus + convulsions")),
      nrow(subset(dataset, outcome==1)),
      "",
      nrow(subset(dataset, nm_group=="B")),
      nrow(subset(dataset, nm_group=="Y")),
      nrow(subset(dataset, nm_group=="W")),
      nrow(subset(dataset, nm_group=="Z"))
    )
  )
}

table2_column(subset(data, case))
table2_column(subset(data, case & case_group=="B"))
table2_column(subset(data, case & case_ageindays>=134))
table2_column(subset(data, case & case_group=="B" & case_ageindays>=134))



#table 3

table3_row<- function(sdata){
  
  Ca_Vp<-nrow(subset(sdata, case & menBvaccinated))
  Ca_Vn<-nrow(subset(sdata, case & !menBvaccinated))
  Ca_Vb<-nrow(subset(sdata, case))
  
  Co_Vp<-nrow(subset(sdata, !case & menBvaccinated))
  Co_Vn<-nrow(subset(sdata, !case & !menBvaccinated))
  Co_Vb<-nrow(subset(sdata, !case ))
  
  crudeOR<-(Ca_Vp/Ca_Vn)/(Co_Vp/Co_Vn)
  crudeORse<-sqrt(((1/Ca_Vp)+(1/Ca_Vn)+(1/Co_Vp)+(1/Co_Vn)))
  crudeOR_l<-exp( log(crudeOR)-(1.96*crudeORse))
  crudeOR_h<-exp( log(crudeOR)+(1.96*crudeORse))
  
  return(
    c(
      paste("Cases:",    Ca_Vp,"/", Ca_Vb," (",round((Ca_Vp/Ca_Vb)*100,1),"%)",
            "Controls:", Co_Vp,"/", Co_Vb," (",round((Co_Vp/Co_Vb)*100,1),"%)"),
      paste("Crude OR:",sprintf("%.2f",crudeOR),"(", sprintf("%.2f",crudeOR_l) , "-",  sprintf("%.2f",crudeOR_h),")"), 
      paste("Matched:", pretty_output(clogit(case ~ menBvaccinated + strata(case_id), sdata, method="efron",robust=TRUE)))
    )
  )
}

#"Group B eligible cases >134 days old (n=69)"
table3_row(subset(data, case_group=="B" & case_ageindays>=134))
table3_row(subset(data, case_ageindays>=134))

table3_row_any<- function(sdata){
  
  Ca_Vp<-nrow(subset(sdata, case & prior_doses))
  Ca_Vn<-nrow(subset(sdata, case & !prior_doses))
  Ca_Vb<-nrow(subset(sdata, case))
  
  Co_Vp<-nrow(subset(sdata, !case & prior_doses))
  Co_Vn<-nrow(subset(sdata, !case & !prior_doses))
  Co_Vb<-nrow(subset(sdata, !case ))
  
  crudeOR<-(Ca_Vp/Ca_Vn)/(Co_Vp/Co_Vn)
  crudeORse<-sqrt(((1/Ca_Vp)+(1/Ca_Vn)+(1/Co_Vp)+(1/Co_Vn)))
  crudeOR_l<-exp( log(crudeOR)-(1.96*crudeORse))
  crudeOR_h<-exp( log(crudeOR)+(1.96*crudeORse))
  
  return(
    c(
      paste("Cases:",    Ca_Vp,"/", Ca_Vb," (",round((Ca_Vp/Ca_Vb)*100,1),"%)",
            "Controls:", Co_Vp,"/", Co_Vb," (",round((Co_Vp/Co_Vb)*100,1),"%)"),
      paste("Crude OR:",sprintf("%.2f",crudeOR),"(", sprintf("%.2f",crudeOR_l) , "-",  sprintf("%.2f",crudeOR_h),")"), 
      paste("Matched:", pretty_output(clogit(case ~ prior_doses + strata(case_id), sdata, method="efron",robust=TRUE)))
    )
  )
}
table3_row_any(subset(data, case_group=="B"))
table3_row_any(data)