library(plyr) #need to load this first....
library(tidyverse)
library(readxl)
library(lubridate)
library(survival) #clogit
library(flextable)


setwd("/home/robin/Documents/current_writing/Portugual_mening_case_control")
data<-read_excel("Neisseria Database01.08.19_v3.xlsx",sheet="Folha1",
              #IDNUMBER,HOSPITAL,CASE (YES=1 NO=0),CONTROL (YES=1 NO=0),DoB,Date of admission,Age (months),Gender (M=1; F=0), Postcode
col_types = c("text","numeric","numeric","logical","logical","date","date","skip","skip","logical","numeric",
              #Diagnosis (Sepsis=1; Meningitis=2; Sepsis and Meningitis=3; Bacteriemia=4; Arthritis=5; Other=6),Duration of hospital stay (days), Transfered to another hospital (0=No; 1=Yes),
              "text","numeric","text",
              #Outcome (Death=1; Survival with no sequelae=2; Survival with sequelae=3; Amputation=4; ENT=5; Development=6; Others=7)	
             "text",
             #Biological sample (0=Blood; 1=CSF; 2=Joint fluid; 3=Other; 4=Blood+CSF),Method (Culture=0; PCR=1; Both=2),Nm group
             "logical","text","text",
             #MenB4C Vaccine (No=0; Yes=1) Number of doses	Date	Date	Date	Date
             "logical","numeric","date","date","date","date",
             #MenC Vaccine (No=0; Yes=1)	Number of doses	Date	Date	Date	Date	
             "logical","numeric","date","date","date","date",
             #MenACWY Vaccine (No=0; Yes=1)	Number of doses	Date	Date
             "logical","numeric","date","date",
             #Rotavirus Vaccine (No=0; Yes=1)	Number of doses	Date	Date	Date
             "logical","numeric","date","date","date",
             #Varicella Vaccine (No=0; Yes=1)	Number of doses	Date	Date
             "logical","numeric","date","date",
             #Notes -	Excluded
             "text","text","text"),
                 na=c("NA","Missing","missing"),
col_names=c("cc_id","site","area","case","control","dob","doa","gender","postcode","diagnosis",
            "hospital_duration","transferred","outcome","sample","method","nm_group",
            "menBvac","menBvac_doses","menBvac_date1","menBvac_date2","menBvac_date3","menBvac_date4",
            "menCvac","menCvac_doses","menCvac_date1","menCvac_date2","menCvac_date3","menCvac_date4",
            "menACWYvac","menACWYvac_doses","menACWYvac_date1","menACWYvac_date2",
            "rotavirus","rotavirus_doses","rotavirus_date1","rotavirus_date2","rotavirus_date3",
            "varicella","varicella_doses","varicella_date1","varicella_date2",
            "notes","excluded","reason"),skip=1
)

#create id to link cases and controls
data$case_id    <- as.numeric(substring(data$cc_id,0,4))
data$control_id <- as.numeric(substring(data$cc_id,6,8))

#create ages
data$ageinmonths<-(ymd(data$dob) %--% ymd(data$doa))/months(1)
data$ageinyears<-(ymd(data$dob) %--% ymd(data$doa))/years(1)
data$ageindays<-ymd(data$doa)-ymd(data$dob)
data$year<-year(ymd(data$dob))

data$ageCat<-as.factor( cut(data$ageinyears,c(-1,1,5,10,100),include.lowest = TRUE, right=FALSE,labels=c("<1", "1-4", "5-9","10-18")))
summary(subset(data,ageCat=="<1")$ageinyears)
summary(subset(data,ageCat=="1-4")$ageinyears)
summary(subset(data,ageCat=="5-9")$ageinyears)
summary(subset(data,ageCat=="10-18")$ageinyears)

nrow(subset(data,ageCat=="<1"))
nrow(subset(data,ageCat=="1-4"))
nrow(subset(data,ageCat=="5-9"))
nrow(subset(data,ageCat=="10-18"))


# populate controls with key fields from cases (dob, nm_group, age_in_days,case_doa)
groupdata<-data.frame(data$case_id,data$nm_group,data$dob,data$ageindays,data$doa)
names(groupdata)<-c('case_id','case_group','case_dob','case_ageindays','case_doa')
groupdata<-groupdata[!is.na(groupdata$case_group),]
data<-data %>% right_join(groupdata, by = "case_id")

#Determine virtual DOA for controls at same age as cases.....
data$doa_virtual<-ymd(data$dob)+data$case_ageindays
table(ymd(data[data$case,]$doa)-data[data$case,]$doa_virtual)
table(ymd(data[!data$case,]$doa)-data[!data$case,]$doa_virtual)

#store all cases so can include in summary table
data_preexclusion<-data

# import socio economic data
postcodes2SES<-read_excel("postcodes2EPCC.xlsx")
data<- data %>% left_join(postcodes2SES, by = "postcode")


###### Data cleaning #####

result_n_all_cases<-nrow(subset(data, case))
nrow(subset(data, case))

#data[data$cc_id=="1802,02",]$excluded<-"duplicate control!" new one was found 5/8/19
data[data$cc_id=="0213",]$excluded<-"vaccine status unclear see AF email 8/8/19"
data[data$cc_id=="0213,01",]$excluded<-"vaccine status unclear see AF email 8/8/19"
data[data$cc_id=="0213,02",]$excluded<-"vaccine status unclear see AF email 8/8/19"

#remove excluded cases
table(data$excluded)
data[!is.na(data$excluded),]$reason
data[!is.na(data$excluded),]$cc_id
excludeds<-data[!is.na(data$excluded),]
nrow(subset(excludeds, case))
subset(excludeds, case)$cc_id
subset(excludeds, case)$reason
data<-data[is.na(data$excluded),]
data<-(data[!data$case_id %in% excludeds[excludeds$case,]$case_id,]) #controls for excluded cases but careful as can remove innocent controls too
data<-(data[!data$cc_id %in% excludeds[!excludeds$case,]$cc_id,]) #controls for excluded cases but careful as can remove innocent controls too
nrow(subset(data, case))
table( ddply(data, .(case_id), summarise, no_controls=max(control_id, na.rm = TRUE))$no_controls)

#remove excluded controls 2803.02 as incomplete at the moment
#data<-data[!data$cc_id=="2803.02",]

#remove cases <74d old
length(subset(data,case & ageindays<74)$case_id)
#subset(data,case & ageindays<74)$case_id
#subset(data,case & ageindays<74)$ageindays
#data_too_young<-(data[data$case_id %in% subset(data,case & ageindays<74)$case_id,]) #cases and controls for cases under 74d old
#data<-(data[!data$case_id %in% subset(data,case & ageindays<74)$case_id,]) #removes cases and controls for cases under 74d old

#add an id column for ease
data$id<-c(1:nrow(data))

#check ages of controls
#if cases is less than <2 years old,controls have to have been born +/- 14 days (but not aged less than 2 months and 14 days); 
#if cases are aged 2-5 years, controls have to have been born +/- 60 days, 
#if cases are aged >=5 years or more, controls have to have been born +/-90 days; 

data$cc_age_diff<-(ymd(data$dob) %--% ymd(data$case_dob))/days(1)
data$cc_ageinmonths<-(ymd(data$case_dob) %--% ymd(data$doa))/months(1)
data$age_at_assessment<-((ymd(data$dob) %--% ymd(data$doa_virtual))/months(1))

table(subset(data,control & cc_ageinmonths<24)$cc_age_diff) # +/- 14d
summary(subset(data,control & cc_ageinmonths<24)$cc_age_diff)
#subset(data,control & cc_ageinmonths<24 & abs(cc_age_diff) > 14)$cc_id #0503,02 19d different DOB so is excluded.
subset(data,control & cc_ageinmonths<24 & abs(cc_age_diff) > 14)$ageinmonths
subset(data,control & cc_ageinmonths<24 & abs(cc_age_diff) > 14)$cc_age_diff
data<-(data[!data$cc_id %in% subset(data,control & cc_ageinmonths<24 & abs(cc_age_diff) > 14)$cc_id,])
result_cc_age_diff_0<-subset(data,control & cc_ageinmonths<24)$cc_age_diff

table(subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60)$cc_age_diff) # +/- 60d
summary(subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60)$cc_age_diff)
subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60 & abs(cc_age_diff) > 60)$cc_id
subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60 & abs(cc_age_diff) > 60)$cc_age_diff
result_cc_age_diff_24<-subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60)$cc_age_diff

table(subset(data,control & cc_ageinmonths>=60)$cc_age_diff)  # +/- 90d
summary(subset(data,control & cc_ageinmonths>=60)$cc_age_diff)
subset(data,control & cc_ageinmonths>=60 & abs(cc_age_diff) > 90)$cc_id
subset(data,control & cc_ageinmonths>=60 & abs(cc_age_diff) > 90)$cc_age_diff
result_cc_age_diff_60<-subset(data,control & cc_ageinmonths>=60)$cc_age_diff

#difference in dates of attendance
data$cc_doa_diff<-(ymd(data$doa) %--% ymd(data$case_doa))/days(1)
subset(data, cc_doa_diff > 30)$cc_id
summary(subset(data,control)$cc_doa_diff)
result_cc_doa_diff<-subset(data,control)$cc_doa_diff

#Sense check vaccine dates
#age at which had each dose
summary((ymd(data$dob) %--% ymd(data$menBvac_date1))/months(1))
summary((ymd(data$dob) %--% ymd(data$menBvac_date2))/months(1))
summary((ymd(data$dob) %--% ymd(data$menBvac_date3))/months(1))
summary((ymd(data$dob) %--% ymd(data$menBvac_date4))/months(1))

#interval between doses
summary((ymd(data$menBvac_date1) %--% ymd(data$menBvac_date2))/days(1))
summary((ymd(data$menBvac_date2) %--% ymd(data$menBvac_date3))/days(1))
summary((ymd(data$menBvac_date3) %--% ymd(data$menBvac_date4))/days(1))

#check for duplicate IDs, cases and controls.
data[duplicated(data$cc_id),]$cc_id
duplicate_dob<-data[data$dob %in% data[duplicated(data$dob),]$dob,]
double_dup2<-
duplicate_dob[duplicate_dob$postcode %in% duplicate_dob[duplicated(duplicate_dob$postcode),]$postcode,]
#case 0503 (06/2015) was also a control 0501,01 (11/2014) 



# remove cases if doesn't have 2 controls
#check_control<- function(cc_id){
#return(length(cc_id))
#  }
#data<-data %>% group_by(case_id) %>% dplyr::mutate(no_controls=check_control(cc_id))
#data<-subset(data,no_controls>=3)
#table(data$no_controls)



# Define functions for checking vaccine status

#For the primary analysis, children who have received the appropriate number 
# of vaccine doses for their age  will be considered vaccinated – i.e. 
#     - aged 4 to 15 mo with 2 or more doses with the last dose >=14 d before presentation 
#     - aged >=16 m with 2 or 3 doses before 1 y and one dose after 1 y of age ( booster dose at least 14 days before presentation)
#     - >=2 doses after the first birthday (with the second dose at least 14 days before presentation)."  

check_if_vaccinated<- function(dob, doa, hadvaccine, dose1, dose2, dose3, dose4){
  ageinmonths<-(ymd(dob) %--% ymd(doa))/months(1)

  agedose1<-(ymd(dob) %--% ymd(dose1))/months(1)
  agedose2<-(ymd(dob) %--% ymd(dose2))/months(1)
  agedose3<-(ymd(dob) %--% ymd(dose3))/months(1)
  agedose4<-(ymd(dob) %--% ymd(dose4))/months(1)

  dose_ages<-c(agedose1,agedose2,agedose3,agedose4)
  #dose_ages<-c(3,4,11)
  #ageinmonths<-10
  dose_ages<-dose_ages[dose_ages<=(ageinmonths-0.5)] #Removes any doses given 14 days before attendance.
  #dose_ages<-dose_ages[dose_ages<=(ageinmonths)] #Removes any doses given 14 days before attendance.
  
  dose_ages<-na.omit(dose_ages)
  
  message(paste("Age:", ageinmonths,
                "\tNo. Doses:",length(dose_ages),
                "\tAt:" ), appendLF = FALSE)
  message(dose_ages)

  if (!hadvaccine) {return(FALSE)}

  if (ageinmonths<4){return (FALSE)}

  if (ageinmonths>=4 & ageinmonths<=15){
    if( length(dose_ages) >= 2) {return (TRUE)}
    else return (FALSE)
  }

  if (ageinmonths>=16){
    message(">=16m")
    if( (length(subset(dose_ages,dose_ages < 12)) >= 2 ) & (length(subset(dose_ages,dose_ages >= 12)) >= 1 )) {return (TRUE)}
    if(                                                     length(subset(dose_ages,dose_ages >= 12)) >= 2 )  {return (TRUE)}
    else return (FALSE)
  }

return (FALSE)
}

check_if_vaccinated_days<- function(dob, doa, hadvaccine, dose1, dose2, dose3, dose4){
  ageindays<-(ymd(dob) %--% ymd(doa))/days(1)
  
  agedose1<-(ymd(dob) %--% ymd(dose1))/days(1)
  agedose2<-(ymd(dob) %--% ymd(dose2))/days(1)
  agedose3<-(ymd(dob) %--% ymd(dose3))/days(1)
  agedose4<-(ymd(dob) %--% ymd(dose4))/days(1)
  
  dose_ages<-c(agedose1,agedose2,agedose3,agedose4)
  dose_ages<-dose_ages[dose_ages<=(ageindays-14)] #removes any doses given 14 days before age at attendance.
  dose_ages<-na.omit(dose_ages)
  
  if (!hadvaccine) {return(FALSE)}
  
  if (ageindays<(4*30 + 14)){return (FALSE)} #too young to have had all the doses and them have worked
  
  if (ageindays>=(4*30 + 14) & ageindays<=(15*30)){
    if( length(dose_ages) >= 2 ) {return (TRUE)}
    else return (FALSE)
  }
  
  if (ageindays>=(16*30)){
    if  (length(subset(dose_ages,dose_ages < (12*30) ) >= 2 ) & (length(subset(dose_ages,dose_ages >= (12*30)) >= 1 )) ){return (TRUE)}
    if                                                          (length(subset(dose_ages,dose_ages >= (12*30)) >= 2 )  ){return (TRUE)}
    else return (FALSE)
  }
  
  return (FALSE)
}

#check_if_vaccinated('2006/07/21','2016/01/25',TRUE,'2016/02/08','2016/04/27',NA,NA)

temp<-data %>% filter(cc_id=="0109") %>% select(dob,doa,menBvac,menBvac_date1,menBvac_date2,menBvac_date3,menBvac_date4) %>% ungroup()
check_if_vaccinated(temp$dob, temp$doa, temp$menBvac, temp$menBvac_date1, temp$menBvac_date2, temp$menBvac_date3, temp$menBvac_date4)

temp<-data %>% filter(cc_id=="1207,01") %>% select(dob,doa,menBvac,menBvac_date1,menBvac_date2,menBvac_date3,menBvac_date4) %>% ungroup()
check_if_vaccinated(temp$dob, temp$doa, temp$menBvac, temp$menBvac_date1, temp$menBvac_date2, temp$menBvac_date3, temp$menBvac_date4)


# Test vaccination check routine ----
#     - aged 4 to 15 mo with 2 or more doses with the last dose >=14 d before presentation 
#     - aged >=16 m with 2 or 3 doses before 1 y and one dose after 1 y of age ( booster dose at least 14 days before presentation)
#     - >=2 doses after the first birthday (with the second dose at least 14 days before presentation)."
testdate<-dmy("01/01/2017")
#under 4m
check_if_vaccinated_days(testdate,testdate+months(3),  TRUE, testdate+months(1), testdate+months(2),testdate+months(3),testdate+months(4))

#4m
check_if_vaccinated(testdate,testdate+months(4),  TRUE, testdate+months(1), testdate+months(2),testdate+months(3),testdate+months(4))
check_if_vaccinated(testdate,testdate+months(4),  TRUE, testdate+months(2), testdate+months(3),NA,NA)
check_if_vaccinated(testdate,testdate+months(4),  TRUE, testdate+months(3), testdate+months(3)+days(14),NA,NA)
check_if_vaccinated(testdate,testdate+months(4),  TRUE, testdate+months(3), testdate+months(3)+days(16),NA,NA)
check_if_vaccinated(testdate,testdate+months(4),  TRUE, testdate+months(3), testdate+months(4),NA,NA)
check_if_vaccinated(testdate,testdate+months(15),  TRUE, testdate+months(3), testdate+months(4),NA,NA)

#>12m
check_if_vaccinated(testdate,testdate+months(16),  TRUE, testdate+months(1), testdate+months(2),testdate+months(3),testdate+months(4))
check_if_vaccinated(testdate,testdate+months(16),  TRUE, testdate+months(1), testdate+months(2),testdate+months(12),NA)
check_if_vaccinated(testdate,testdate+months(16),  TRUE, testdate+months(1), testdate+months(2),testdate+months(13),NA)

check_if_vaccinated(testdate,testdate+months(16),  TRUE, testdate+months(12), testdate+months(13),NA,NA)
check_if_vaccinated(testdate,testdate+months(16),  TRUE, testdate+months(15), testdate+months(16),NA,NA)


pretty_output<- function(regression_results){
  temp<-as.matrix(summary(regression_results)$conf.int)
  estimate<-round(1-temp[1],3)*100
  upper<-round(1-temp[3],3)*100
  lower<-round(1-temp[4],3)*100

  if(estimate>1000 & !is.na(estimate))        {estimate<-" HI"}
  else if(estimate < -1000 & !is.na(estimate)){estimate<-" LO"}
  
  if(upper>1000 & !is.na(upper))        {upper<-" HI"}
  else if(upper < -1000 & !is.na(upper)){upper<-"HI"}
  
  if(lower>1000 & !is.na(lower))        {lower<-">999"}
  else if(lower< -1000 & !is.na(lower)) {lower<-"LO "}
  
  output<-paste(estimate,"% (",lower,"-",upper,")",sep="")
  return(output)
}

table(data$menBvac)


##### Determine who is vaccinated #####

#calculate who has had complete vaccine schedule based on their age at attendance
data<-data %>% group_by(id) %>% dplyr::mutate(menBvaccinated=check_if_vaccinated(dob, doa_virtual, menBvac, menBvac_date1 , menBvac_date2, menBvac_date3, menBvac_date4 ))

table(subset(data,ageinmonths<4)$case,                  subset(data,ageinmonths<4)$menBvaccinated)
table(subset(data,ageinmonths>=4 & ageinmonths<16)$case, subset(data,ageinmonths>4 & ageinmonths<16)$menBvaccinated)
subset(data,ageinmonths>=4 & ageinmonths<16 & case & menBvaccinated)$cc_id
table(subset(data,ageinmonths>=16)$case,                subset(data,ageinmonths>=16)$menBvaccinated)
subset(data,ageinmonths>=16 & case & menBvaccinated)$cc_id


#calculate number of doses prior to attendance
data<-data %>% group_by(id) %>% dplyr::mutate(no_prior_doses=
                                         sum((c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4))
                                              < (ymd(doa_virtual)-days(14) )), na.rm = TRUE)
                                       )

#calculate number of doses prior to 12m of age
data<-data %>% group_by(id) %>% dplyr::mutate(no_doses_12m=
                                                sum( 
                                                  #(c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4))   <= (ymd(dob)+years(1))) & 
                                                  #  (c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4)) <= (ymd(doa_virtual)-days(14))),
                                                  (c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4))   <= (ymd(dob)+years(1))),
                                                  na.rm = TRUE
                                                )
)

table(data$no_doses_12m)
table(data[data$case,]$no_doses_12m)
table(data[!data$case,]$no_doses_12m)


#calculate number of doses prior to 16m of age
data<-data %>% group_by(id) %>% dplyr::mutate(no_doses_16m=
                                                sum( 
                                                  (c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4)) <= (ymd(dob)+months(16))) & 
                                                    (c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4)) <= (ymd(doa_virtual)-days(14))),
                                                  na.rm = TRUE
                                                )
)
table(data$no_doses_16m)
table(data[data$case,]$no_doses_16m)
table(data[!data$case,]$no_doses_16m)

#calculate number of doses after 12m of age
data<-data %>% group_by(id) %>% dplyr::mutate(no_doses_over_12m=
                                                sum( 
                                                  #(c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4)) >= (ymd(dob)+years(1))) & 
                                                  #(c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4)) <= (ymd(doa_virtual)-days(14))),
                                                  (c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4)) >= (ymd(dob)+years(1))),
                                                  na.rm = TRUE
                                                )
)

table(data$no_doses_over_12m)
table(data[data$case,]$no_doses_over_12m)
table(data[!data$case,]$no_doses_over_12m)


(c(ymd("2016/01/01"), ymd("2016/02/01"), ymd("2016/03/01"), ymd("2017/01/01")) >= ymd("2016/02/01")) &
  (c(ymd("2016/01/01"), ymd("2016/02/01"), ymd("2016/03/01"), ymd("2017/01/01")) >= ymd("2016/03/01"))


#had at least one dose of vaccine at point of illness
data$prior_doses<-(data$no_prior_doses>0)
table(data$prior_doses)

data$doses_1<-(data$no_prior_doses==1)
table(data$doses_1)
data$doses_2<-(data$no_prior_doses>=2)
table(data$doses_2)


#partially vaccinated cases removed.
table(data$prior_doses, data$menBvaccinated)
#c(TRUE,TRUE,FALSE,FALSE) & c(TRUE,FALSE,TRUE,FALSE)
#!c(TRUE,TRUE,FALSE,FALSE) & c(TRUE,FALSE,TRUE,FALSE)
data$partial_vaccinated<-(!data$menBvaccinated & data$prior_doses)
#data$control<-as.logical(data$control)
table(data$nm_group)
table(data$case, data$control)
data[data$case & data$menBvaccinated,]$cc_id
data[data$control & data$menBvaccinated,]$cc_id


# - aged 4 to 15 mo with 2 or more doses with the last dose >=14 d before presentation 
# - aged >=16 m with 2 or 3 doses before 1 y and one dose after 1 y of age ( booster dose at least 14 days before presentation)
#  >=2 doses after the first birthday (with the second dose at least 14 days before presentation)."


result_n_incl_cases<-nrow(subset(data, case))
result_n_incl_cases_vac<-nrow(subset(data, case & menBvaccinated))
result_n_incl_cases_pvac<-nrow(subset(data, case & partial_vaccinated))

result_n_too_young<-nrow(subset(data_preexclusion, case & ageindays<74))
result_n_hosp<-length(unique(data$site))
result_n_B<-nrow(subset(data, case & nm_group=="B"))
result_n_B_134<-nrow(subset(data,case & nm_group=="B" & ageindays>=134))

result_n_B_134_controls<-nrow(subset(data,control & case_group=="B" & ageindays>=134))
result_n_B_134_controls_vac<-nrow(subset(data,control & case_group=="B" & ageindays>=134 & menBvaccinated))
result_n_B_134_controls_vacp<-sprintf("%.1f",((result_n_B_134_controls_vac/result_n_B_134_controls)*100))

result_n_B_vac<-nrow(subset(data, case & nm_group=="B" & menBvaccinated))
result_n_B_134_vac<-nrow(subset(data,case & nm_group=="B" & ageindays>=134 & menBvaccinated))
result_n_B_134_vacp<-sprintf("%.1f",((result_n_B_134_vac/result_n_B_134)*100))


######  Crude analysis for B disease cases against all controls #####

Ca_Vp<-nrow(data[data$case_group=="B" & data$case & data$menBvaccinated & data$case_ageindays>=134,])
Ca_Vn<-nrow(data[data$case_group=="B" & data$case & !data$menBvaccinated & data$case_ageindays>=134,])

Co_Vp<-nrow(data[data$case_group=="B" & data$control & data$menBvaccinated & data$case_ageindays>=134,])
Co_Vn<-nrow(data[data$case_group=="B" & data$control & !data$menBvaccinated & data$case_ageindays>=134,])

crudeOR <- matrix(c(Ca_Vp,Ca_Vn,Co_Vp,Co_Vn),ncol=2,byrow=TRUE)
rownames(crudeOR) <- c("Case","Control")
colnames(crudeOR) <- c("Vaccine","No Vaccine")
as.table(crudeOR)

cat(paste("Cases vaccine = ", (Ca_Vp/(Ca_Vn+Ca_Vp))*100), "%")
cat(paste("Controls vaccine = ", (Co_Vp/(Co_Vn+Co_Vp))*100), "%")

crudeOR<-(Ca_Vp/Ca_Vn)/(Co_Vp/Co_Vn)
crudeORse<-sqrt(((1/Ca_Vp)+(1/Ca_Vn)+(1/Co_Vp)+(1/Co_Vn)))
crudeOR_l<-exp( log(crudeOR)-(1.96*crudeORse))
crudeOR_h<-exp( log(crudeOR)+(1.96*crudeORse))

cat(paste("Crude Odds Ratio = ", crudeOR))
cat(paste("SE log OR= ", crudeORse))
cat(paste("95% ", crudeOR_l , " to ",  crudeOR_h ))  
cat(paste("Vaccine effectiveness = ", (1-crudeOR)*100))
cat(paste("95% ", (1-crudeOR_h)*100 , " to ",  (1-crudeOR_l)*100 ))  


library(boot)
crudeData<-data.frame(
  caseControl<-c(rep(TRUE,  nrow(subset(data, case & case_ageindays>=134 & case_group=="B"))), 
                 rep(FALSE, nrow(subset(data,!case & case_ageindays>=134 & case_group=="B")))),
  vaccinated<-c(subset(data, case & case_ageindays>=134 & case_group=="B")$menBvaccinated,
                subset(data, !case & case_ageindays>=134 & case_group=="B")$menBvaccinated
                )
)
names(crudeData)<-c("case","vac")

# function to obtain R-Squared from the data 
rsq <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  or <- (nrow(subset(d,case&vac))/nrow(subset(d,case&!vac)))/
    (nrow(subset(d,!case&vac))/nrow(subset(d,!case&!vac)))
  return(1-or)
} 
# bootstrapping with 1000 replications 
results <- boot(data=crudeData, statistic=rsq, R=10000)
# view results
results 
boot.ci(results, type="bca")


###### Regression analysis ######

#Primary analysis - complete vaccine schedule to age against B disease.
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134) ))

pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134), method="efron",robust=TRUE))
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134), method="breslow",robust=TRUE))
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134), method="approximate",robust=TRUE))

rsq <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  or <-  exp(clogit(case ~ menBvaccinated + strata(case_id), d )$coef)
  return(1-or)
} 
# bootstrapping with 1000 replications 
results <- boot(data=subset(data,case_group=="B" & case_ageindays>=134), statistic=rsq, R=1000)
# view results
results 
boot.ci(results, type="all")

#library(foreign)
#write.dta( subset(data,case_group=="B" & case_ageindays>=134), "primary.dta")
#clogit case menBvaccinated, group(case_id) or vce(r)
#
#case | Odds Ratio   Std. Err.      z    P>|z|     [95% Conf. Interval]
#  menBvaccinated |   .2171479   .1254843    -2.64   0.008     .0699628    .6739748
#78% (33-93)


#>=1_doses against B disease.
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134) ))
#secondary analysis - partially vaccinated cases and controls removed
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & !partial_vaccinated) ))


#complete vaccine schedule to age against any disease.
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), data) )
#>=1_doses against any disease.
pretty_output(clogit(case ~ prior_doses + strata(case_id), data) )
#secondary analysis - partially vaccinated cases and controls removed
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,!partial_vaccinated) ))



#complete vaccine schedule to age against non-B disease.
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group!="B") ))

pretty_output(clogit(case ~ rotavirus + strata(case_id), data) )
pretty_output(clogit(case ~ varicella + strata(case_id), data) )
pretty_output(clogit(case ~ menACWYvac + strata(case_id), data) )



#list cases with men disease with complete or partial vaccination
data[data$case & (data$menBvaccinated | data$prior_doses),]$case_id




###### Socioeconomic analysis ######

histDatacc<-data.frame(
  case<-c(rep("Case",nrow(subset(data,case))), rep("Control",nrow(subset(data,!case)))),
  SES<-c(subset(data,case)$perCapita,subset(data,!case)$perCapita)
)

#histData<-data.frame(
#  case<-c(rep(TRUE,nrow(subset(data,case))), rep(FALSE,nrow(subset(data,!case)))),
#  SES<-c(subset(data,case)$perCapita,subset(data,!case)$perCapita)
#)
names(histDatacc)<-c("case","perCapita")

histDataMeancc <- ddply(histDatacc, .(case), summarise, meanLine=mean(perCapita))

ggplot(histDatacc, aes(x = perCapita, group = case, fill = case)) +
  geom_histogram(colour = "gray", binwidth = 25) +
  #geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  facet_wrap(~ case) +
  theme_bw()

plotSEScasecontrol<-
ggplot(histDatacc, aes(x = perCapita, fill=case))+
  geom_density(alpha=.3)+
  xlab("Purchasing Power Indicator per Capita")+
  ylab("Density")+
  geom_vline(data=histDataMeancc, aes(xintercept=meanLine,  colour=case),linetype="dashed", size=1)+
  theme_bw()+
  theme(legend.title = element_blank(), legend.position=c(0.85, 0.85), 
        legend.text = element_text(size = 10, margin = margin(l = 10), hjust = 0),
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'))

t.test(subset(data,case)$perCapita,subset(data,!case)$perCapita)
wilcox.test(subset(data,case)$perCapita,subset(data,!case)$perCapita)

#vaccine vs no vaccine
summary(subset(data,menBvac)$perCapita)
summary(subset(data,!menBvac)$perCapita)
welch.test(subset(data,menBvac)$perCapita,subset(data,!menBvac)$perCapita)
wilcox.test(subset(data,menBvac)$perCapita,subset(data,!menBvac)$perCapita)

histData_vu<-data.frame(
  menBvaccined<-c(rep("vaccinated",nrow(subset(data,menBvac))), rep("unvaccinated",nrow(subset(data,!menBvac)))),
  SES<-c(subset(data,menBvac)$perCapita,subset(data,!menBvac)$perCapita)
)
names(histData_vu)<-c("menBvaccined","perCapita")
histDataMean_vu <- ddply(histData_vu, .(menBvaccined), summarise, meanLine=mean(perCapita))

ggplot(histData_vu, aes(x = perCapita, group = menBvaccined, fill = menBvaccined)) +
  geom_histogram(colour = "gray", binwidth = 25) +
  facet_wrap(~ menBvaccined) +
  theme_bw()

plotSESvaccine<-
ggplot(histData_vu, aes(x = perCapita, fill=menBvaccined))+
  geom_density(alpha=.3)+
  xlab("Purchasing Power Indicator per Capita")+
  ylab("Density")+
  geom_vline(data=histDataMean_vu, aes(xintercept=meanLine,  colour=menBvaccined),linetype="dashed", size=1)+
  theme_bw()+
  scale_fill_brewer(palette="Set1")+
  theme(legend.title = element_blank(), legend.position=c(0.85, 0.85), 
        legend.text = element_text(size = 10, margin = margin(l = 10), hjust = 0),
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'))
  

library("ggpubr")
ggarrange(plotSESvaccine, plotSEScasecontrol,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

ggsave("/tmp/ptbest300.png", plot = last_plot(), dpi=300)
ggsave("/tmp/ptbest600.png", plot = last_plot(), dpi=600)

data$SESgrp<-as.factor(as.numeric(cut(data$perCapita,2,include.lowest=T)))

regression_results<-clogit(case ~ menBvaccinated + menBvaccinated:SESgrp + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134) )
summary(regression_results)
summary(regression_results)$conf.int


regression_results<-clogit(case ~ menBvaccinated + menBvaccinated:perCapita + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134) )
summary(regression_results)
summary(regression_results)$conf.int


#
#RV or varicella vaccine vs no vaccine
summary(subset(data,rotavirus)$perCapita)
summary(subset(data,!rotavirus)$perCapita)
hist(subset(data,rotavirus)$perCapita)
hist(subset(data,!rotavirus)$perCapita)
wilcox.test(subset(data,menBvac)$perCapita,subset(data,!menBvac)$perCapita)




#####  Data in order as paper  ######

#No cases in base data
nrow(subset(data_preexclusion,case))

#No cases excluded for other reasons
subset(excludeds,case)$reason

#No remaining cases
nrow(subset(data,case))
nrow(subset(data,case & ageindays>=134))

#number of sites
length(unique(data$site))

#Number of controls
table( ddply(data, .(case_id), summarise, no_controls=max(control_id, na.rm = TRUE))$no_controls)

#number of B cases
nrow(subset(data,case & nm_group=="B"))
#number of B > 134 days
nrow(subset(data,case & ageindays>=134 & nm_group=="B"))


#difference in age
summary(subset(data,control & cc_ageinmonths<24)$cc_age_diff)
summary(subset(data,control & cc_ageinmonths>=24 & cc_ageinmonths<60)$cc_age_diff)
summary(subset(data,control & cc_ageinmonths>=60)$cc_age_diff)

# difference in date of attendance between cases and controls
summary(subset(data,control)$cc_doa_diff)

#primary analysis
nrow(data[data$case_group=="B" & data$case & data$case_ageindays>=74,]) #82
nrow(data[data$case_group=="B" & data$case & data$menBvaccinated & data$case_ageindays>=74,])
nrow(data[data$case_group=="B" & data$case & data$prior_doses & data$case_ageindays>=74,])

nrow(data[data$case_group=="B" & data$case & data$case_ageindays>=134,]) # 69
nrow(data[data$case_group=="B" & data$case & data$menBvaccinated & data$case_ageindays>=134,])
nrow(data[data$case_group=="B" & !data$case & data$menBvaccinated & data$case_ageindays>=134,])
nrow(data[data$case_group=="B" & data$case & data$prior_doses & data$case_ageindays>=134,])

pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="B" & case_ageindays>=134), method="efron",robust=TRUE))

#all groups analysis
nrow(data[data$case & data$case_ageindays>=134,]) # 85
nrow(data[data$case & data$menBvaccinated & data$case_ageindays>=134,]) #6
nrow(data[!data$case & data$menBvaccinated & data$case_ageindays>=134,]) #38
nrow(data[data$case & data$prior_doses>0 & data$case_ageindays>=134,])
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_ageindays>=134), method="efron",robust=TRUE))


#all groups analysis
nrow(data[data$case & data$case_ageindays>=134 & data$age_at_assessment<=12,]) # 85
nrow(data[data$case & data$menBvaccinated & data$case_ageindays>=134 & data$age_at_assessment<16,]) #6
nrow(data[data$case & !data$menBvaccinated & data$case_ageindays>=134 & data$age_at_assessment<16,]) #6
nrow(data[!data$case & data$menBvaccinated & data$case_ageindays>=134 & data$age_at_assessment<16,]) #38
nrow(data[!data$case & !data$menBvaccinated & data$case_ageindays>=134 & data$age_at_assessment<16,]) #38


#all >74 any doses against B
nrow(data[data$prior_doses & data$case_ageindays>=74,])
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,data$case_group=="B" & case_ageindays>=74), method="efron",robust=TRUE))
nrow(data[data$case & data$case_group=="B" & data$case_ageindays>=74,])
nrow(data[data$case & data$case_group=="B" & data$prior_doses & data$case_ageindays>=74,])
nrow(data[!data$case & data$case_group=="B" & data$case_ageindays>=74,])
nrow(data[!data$case & data$case_group=="B" & data$prior_doses & data$case_ageindays>=74,])

#all >74 any doses against anything
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,case_ageindays>=74), method="efron",robust=TRUE))
nrow(data[data$case  & data$case_ageindays>=74,])
nrow(data[data$case  & data$prior_doses & data$case_ageindays>=74,])
nrow(data[!data$case & data$case_ageindays>=74,])
nrow(data[!data$case & data$prior_doses & data$case_ageindays>=74,])


#no vaccinated cases
nrow(data[data$case & data$prior_doses & data$case_ageindays>=74,])
summary(data[data$case & data$prior_doses & data$case_ageindays>=74,]$ageinmonths)
summary(data[data$case & data$prior_doses & data$case_ageindays>=74,]$hospital_duration)
table(data[data$case & data$prior_doses & data$case_ageindays>=74,]$outcome)

nrow(data[data$case & !data$prior_doses & data$case_ageindays>=74,])
summary(data[data$case & !data$prior_doses & data$case_ageindays>=74,]$ageinmonths)
summary(data[data$case & !data$prior_doses & data$case_ageindays>=74,]$hospital_duration)
table(data[data$case & !data$prior_doses & data$case_ageindays>=74,]$outcome)

prop.test(c(11, 51), c(11, 73))
prop.test(c(0, 23), c(11, 87))

fisher.test(matrix(c(0, 23, 11, 64), nrow = 2))
table( cut(
data[data$case & data$nm_group=="B" & data$menBvaccinated & data$case_ageindays>=74,]$ageinmonths
, breaks=seq(0,120,12)))


###### subgroups for Shamez ######

#all >74 any doses against anything
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,case_group=="B" & case_ageindays>=74), method="efron",robust=TRUE))
nrow(data[data$case  & data$case_ageindays>=74,])
nrow(data[data$case  & data$prior_doses & data$case_ageindays>=74,])
nrow(data[!data$case & data$case_ageindays>=74,])
nrow(data[!data$case & data$prior_doses & data$case_ageindays>=74,])


#Y
#fully vaccinated for age
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="Y" & case_ageindays>=134), method="efron",robust=TRUE))
#at least one dose
pretty_output(clogit(case ~ prior_doses    + strata(case_id), subset(data,case_group=="Y" & case_ageindays>=134), method="efron",robust=TRUE))
#just one dose
pretty_output(clogit(case ~ doses_1        + strata(case_id), subset(data,case_group=="Y" & case_ageindays>=134), method="efron",robust=TRUE))
#two doses
pretty_output(clogit(case ~ doses_2        + strata(case_id), subset(data,case_group=="Y" & case_ageindays>=134), method="efron",robust=TRUE))

#W
#fully vaccinated for age
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,case_group=="W" & case_ageindays>=134), method="efron",robust=TRUE))
#at least one dose
pretty_output(clogit(case ~ prior_doses    + strata(case_id), subset(data,case_group=="W" & case_ageindays>=134), method="efron",robust=TRUE))
#just one dose
pretty_output(clogit(case ~ doses_1    + strata(case_id), subset(data,case_group=="W" & case_ageindays>=134), method="efron",robust=TRUE))
#two doses
pretty_output(clogit(case ~ doses_2    + strata(case_id), subset(data,case_group=="W" & case_ageindays>=134), method="efron",robust=TRUE))

#W&Y
#fully vaccinated for age
pretty_output(clogit(case ~ menBvaccinated + strata(case_id), subset(data,(case_group=="Y"|case_group=="W") & case_ageindays>=134), method="efron",robust=TRUE))
#at least one dose
pretty_output(clogit(case ~ prior_doses    + strata(case_id), subset(data,(case_group=="Y"|case_group=="W") & case_ageindays>=134), method="efron",robust=TRUE))
#just one dose
pretty_output(clogit(case ~ doses_1    + strata(case_id), subset(data,(case_group=="Y"|case_group=="W") & case_ageindays>=134), method="efron",robust=TRUE))
#two doses
pretty_output(clogit(case ~ doses_2    + strata(case_id), subset(data,(case_group=="Y"|case_group=="W") & case_ageindays>=134), method="efron",robust=TRUE))


#against all cause by age
#<1
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,ageCat=="<1" & case_ageindays>=74), method="efron",robust=TRUE))
#1-4
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,ageCat=="1-4" & case_ageindays>=74), method="efron",robust=TRUE))
#5-9
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,ageCat=="5-9" & case_ageindays>=74), method="efron",robust=TRUE))
#10-18
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,ageCat=="10-18" & case_ageindays>=74), method="efron",robust=TRUE))

#against B by age
#<1
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,data$case_group=="B" & ageCat=="<1" & case_ageindays>=74), method="efron",robust=TRUE))
#1-4
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,data$case_group=="B" & ageCat=="1-4" & case_ageindays>=74), method="efron",robust=TRUE))
#5-9
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,data$case_group=="B" & ageCat=="5-9" & case_ageindays>=74), method="efron",robust=TRUE))
#10-18
pretty_output(clogit(case ~ prior_doses + strata(case_id), subset(data,data$case_group=="B" & ageCat=="10-18" & case_ageindays>=74), method="efron",robust=TRUE))





###### Screening method #####

library("car")

subset(data,year>2014& case & nm_group=="B" & ageinmonths<13) %>% group_by(year) %>% tally(menBvaccinated==TRUE)
subset(data,year>2014& case & nm_group=="B" & ageinmonths<13) %>% group_by(year) %>% tally(menBvaccinated==FALSE)

subset(data,case & nm_group=="B" & ageinmonths<=12 & year==2015)$menBvaccinated
subset(data,case & nm_group=="B" & ageinmonths<=12 & year==2016)$menBvaccinated
subset(data,case & nm_group=="B" & ageinmonths<=12 & year==2017)$menBvaccinated
subset(data,case & nm_group=="B" & ageinmonths<=12 & year==2018)$menBvaccinated


#control vaccine coverage
sum((subset(data,year>2014& !case   & ageinmonths<=12) %>% group_by(year) %>% tally(menBvaccinated==TRUE))$n)/
  nrow((subset(data,year>2014& !case & ageinmonths<=12))) #23% vaccinated as per protocol at time of attendence

sum((subset(data,year>2014& !case   & ageinmonths<=12) %>% group_by(year) %>% tally(no_doses_12m>=2))$n)/
  nrow((subset(data,year>2014& !case & ageinmonths<=12))) #56% had 2 doses by 12m

Vac_cov_controls_12m<-
  (subset(data,year>2014& !case   & ageinmonths<=12) %>% group_by(year) %>% tally(no_doses_12m>=2))$n /
(subset(data,year>2014& !case   & ageinmonths<=12) %>% group_by(year) %>% tally(!case))$n
mean(Vac_cov_controls_12m)
#data<-data %>% group_by(id) %>% mutate(doses_at=doses_at_age(menBvac_date1,menBvac_date2,menBvac_date3,menBvac_date4,doa,dob))


#population vaccine coverate
Vac_cov_popn_12m<-c(0.328, 0.442, 0.535, 0.567)
mean(Vac_cov_popn_12m) #46%
t.test(Vac_cov_popn_12m, conf.level = 0.95)$conf.int

t.test(Vac_cov_controls_12m,Vac_cov_popn_12m)
wilcox.test(Vac_cov_controls_12m,Vac_cov_popn_12m)

#crude analysis

# VE = 1- (PCV/1-PCV) x (1-PPV/PPV)
# PCV = proportion cases vaccinated
# PPV = proportion population vaccinated

pcv<- sum((subset(data,year>2014& case & nm_group=="B" & ageinmonths<13) %>% group_by(year) %>% tally(no_doses_12m>=2))$n)/
  nrow((subset(data,year>2014& case & nm_group=="B" & ageinmonths<13)))
ppv<-mean(c(0.328, 0.442, 0.535, 0.567))
1- ((pcv/(1-pcv))*((1-ppv)/ppv)) #74%

#regresssion analysis

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
1-exp(coef(model))
1-exp(confint(model))



#Men group, age, sex, diagnosis, duration of admission, outcome, vaccine status
table( subset(data,case)$nm_group )
#gender (true==male)
table( subset(data,case)$gender )
# age in months
summary ( subset(data,case)$ageinmonths )
# hospital duration
summary ( subset(data,case)$hospital_duration )
#outcome
table ( subset(data,case)$outcome )
table ( subset(data,case)$diagnosis)


#For Just men B cases
#Men group, age, sex, diagnosis, duration of admission, outcome, vaccine status
table( subset(data,case & nm_group=="B")$nm_group )
#gender (true==male)
table( subset(data,case & nm_group=="B")$gender )
# age in months
summary ( subset(data,case & nm_group=="B")$ageinmonths )
# hospital duration
summary ( subset(data,case & nm_group=="B")$hospital_duration )
#outcome
table ( subset(data,case & nm_group=="B")$outcome )
#diagnoses of cases  (Sepsis=1; Meningitis=2; Sepsis and Meningitis=3; Bacteriemia=4; Arthritis=5; Other=6)
table ( subset(data,case & nm_group=="B")$diagnosis)


#if vaccinated as per our criteria
table( subset(data,case)$menBvaccinated )
table( subset(data,case & nm_group=="B")$menBvaccinated )
table( subset(data,control)$menBvaccinated )
table( subset(data,control & case_group=="B")$menBvaccinated )

# any menB vaccine at all before becoming ill
table( subset(data,case)$prior_doses ) 

#number of menB doses before getting ill
table( subset(data,case)$no_prior_doses )
table( subset(data,case & nm_group=="B")$no_prior_doses )
table( subset(data,control)$no_prior_doses )
table( subset(data,control & case_group=="B")$no_prior_doses )

#diagnoses of cases  (Sepsis=1; Meningitis=2; Sepsis and Meningitis=3; Bacteriemia=4; Arthritis=5; Other=6)
table( subset(data,case)$diagnosis )

#method ofdiagnoses of cases (Culture=0; PCR=1; Both=2)
table( subset(data,case)$method )

#if controls vaccinated as per our criteria
table( subset(data,control)$menBvaccinated )

# any controls had menB vaccine at all before case becoming ill
table( subset(data,control)$prior_doses )

#how many matches.
temp <- data %>% group_by(case_id) %>% mutate( test = max(control_id, na.rm = TRUE))
  
#temp<-subset(data,control & case_group=="B")
temp<-subset(data,control)
table(temp$control_id)
table(temp$control_id, useNA=c("always"))




# Functions to output results -----------------------------------------------

rmm<- function(data){
  return( paste( sprintf("%.1f",mean(data, na.rm = TRUE)) ," (",min(data)," to ",max(data),")",sep="") )
  }
rmm(c(1,2,3))

mm<- function(data){
  return( paste( sprintf("%.1f",min(data))," to ",sprintf("%.1f",max(data)),sep="") )
}

sequele<-function(data,cmd){
  denom <- nrow(subset(data, case & !prior_doses))
  deaths_no<-nrow(subset(data, case & !prior_doses & outcome==1))
  deaths_pct<-sprintf("%.1f",(deaths_no/(nrow(subset(data, case & !prior_doses)))*100))
  
  sequele_no<-nrow(subset(data, case & !prior_doses & !outcome %in% c(1,2)))
  sequele_pct<-sprintf("%.1f",(sequele_no/(nrow(subset(data, case & !prior_doses)))*100))
  
  seq_or_dead_no<-nrow(subset(data, case & !prior_doses & outcome!=2))
  seq_or_dead_pct<-sprintf("%.1f",(seq_or_dead_no/(nrow(subset(data, case & !prior_doses)))*100))

  if(cmd=="deaths")    return  (paste(deaths_no,"(",deaths_pct,"%)",sep=""))
  if(cmd=="sequele")   return  (paste(sequele_no,"(",sequele_pct,"%)",sep=""))
  if(cmd=="combined")  return  (paste(seq_or_dead_no,"(",seq_or_dead_pct,"%)",sep=""))
  if(cmd=="sigtest")   return  (sprintf("%.2f",(prop.test(c(11,(denom-seq_or_dead_no)),c(11,denom))$p.value)))
}


results<- function(data,cmd){
  
  case_no<-nrow(subset(data, case))
  case_no_vaccinated<-nrow(subset(data, case  & menBvaccinated))
  case_no_partial_vaccinated<-nrow(subset(data, case & prior_doses))
  case_pct_vaccinated<-sprintf("%.1f",(case_no_vaccinated/case_no)*100)

  ctrl_no<-nrow(subset(data, !case))
  ctrl_no_vaccinated<-nrow(subset(data, !case  & menBvaccinated))
  ctrl_no_partial_vaccinated<-nrow(subset(data, !case & prior_doses))
  ctrl_pct_vaccinated<-sprintf("%.1f",(ctrl_no_vaccinated/ctrl_no)*100)
  
  if(cmd=="") return("what do you want to know")
  if(cmd=="case_no")  return  (case_no)
  if(cmd=="ctrl_no")  return  (ctrl_no)
  if(cmd=="case_vac") return  (paste (case_no_vaccinated," (",case_pct_vaccinated,"%)",sep="") )
  if(cmd=="ctrl_vac") return  (paste (ctrl_no_vaccinated," (",ctrl_pct_vaccinated,"%)",sep="") )
  if(cmd=="case_pvac") return (case_no_partial_vaccinated)
  if(cmd=="ctrl_pvac") return (ctrl_no_partial_vaccinated)

  if(cmd=="VE") return(pretty_output(clogit(case ~ menBvaccinated + strata(case_id), data )))
}
results(data,"case_no")
results(data,"case_vac")
results(data,"ctrl_vac")
results(data,"VE")



# Create analysis subsets -------------------------------------------------

#all_135<-subset(data,ageindays>=134)
#b_135<-subset(data,case_group=="B" & ageindays>=134)

#all_74<-data

#b_74_any<-subset(data,case_group=="B")
#b_74_any$menBvaccinated<-b_74_any$prior_doses #so can see effect of any doses

#all_74_any<-data
#all_74_any$menBvaccinated<-all_74_any$prior_doses #so can see effect of any doses




# Generate table 1 --------------------------------------------------------

results_table_column<- function(dataset){
return(
  c(
    nrow(dataset),
    paste(sprintf("%.1f",median(dataset$ageinmonths)),"\n(", sprintf("%.1f",min(dataset$ageinmonths)),'-', sprintf("%.1f",max(dataset$ageinmonths)),")"),
    nrow(subset(dataset, gender)),
    "",
    nrow(subset(dataset, diagnosis==1)),
    nrow(subset(dataset, diagnosis==2)),
    nrow(subset(dataset, diagnosis==3)),
    nrow(subset(dataset, diagnosis==4)),
    nrow(subset(dataset, diagnosis==5)),
    nrow(subset(dataset, !dataset$diagnosis %in% c(1,2,3,4,5))),
    paste(median(dataset$hospital_duration),"\n(", min(dataset$hospital_duration),'-',max(dataset$hospital_duration),")"),
    "",
    #Outcome (Death=1; Survival with no sequelae=2; Survival with sequelae=3; Amputation=4; ENT=5; Development=6; Others=7)
    nrow(subset(dataset, outcome==1)),
    nrow(subset(dataset, outcome==2)),
    nrow(subset(dataset, outcome %in% c(4,5,6,"7 - Hydrocephalus + convulsions"))),
    nrow(subset(dataset, outcome==4)),
    nrow(subset(dataset, outcome==5)),
    nrow(subset(dataset, outcome==6)),
    nrow(subset(dataset, outcome=="7 - Hydrocephalus + convulsions")),
    "",
    nrow(subset(dataset, nm_group=="B")),
    nrow(subset(dataset, nm_group=="Y")),
    nrow(subset(dataset, nm_group=="W")),
    nrow(subset(dataset, nm_group=="Z")),
    "",
    nrow(subset(dataset,ageCat=="<1")),
    nrow(subset(dataset,ageCat=="1-4")),
    nrow(subset(dataset,ageCat=="5-9")),
    nrow(subset(dataset,ageCat=="10-18"))
  )
)
}

#Diagnosis (Sepsis=1; Meningitis=2; Sepsis and Meningitis=3; Bacteriemia=4; Arthritis=5; Other=6)

table_cols=c("Number","Median age in months\n(min-max)","Male gender",
             "Diagnosis:","sepsis","meningitis","sepsis & meningitis","bacteraemia","Arthritis","other",
             "Median admission duration (days)\n(min-max)",
             "Outcome:","Death","Survival with no sequelae","Survival with sequelae","Amputation","Deafness","Development delay","Other",
             "NM Group:","B","Y","W","Z",
             "Age","<1","1-4","5-9","10-18"
)

table1_data<- data.frame(table_cols,
  Eligible   = results_table_column(subset(data,case & ageindays >73 )),
  Eligible_B = results_table_column(subset(data,case & ageindays >73 & nm_group=="B")),
  Older      = results_table_column(subset(data,case & ageindays>=134)),
  Older_B    = results_table_column(subset(data,case & ageindays>=134 & nm_group=="B"))
)

ft<-flextable(table1_data)
ft <- set_header_labels(ft, Eligible = "All eligibles >=74d", 
                            Eligible_B = "B eligibles >=74d",
                            Older = "All eligibles 134d",
                            Older_B = "B eligibles 134d" )
ft


ft_table1<- fontsize(ft_table1, size = 6, part = "all")
ft_table1<-align(ft_table1, i = c(4,12,20), j = c(1,1,1), align = "center", part = "body")
ft_table1<-bold(ft_table1, i = c(4,12,20), j = c(1,1,1), bold = TRUE, part = "body")
ft_table1<- autofit(ft_table1)

# Generate table 2 --------------------------------------------------------
#Case no / Year / Age (M) / Diagnosis / Outcome (duration admission) / Nm group / No of doses / Ages / Vaccination status / Genetic characterisation

table2<-subset(data, case & (menBvaccinated | partial_vaccinated))
#Diagnosis (Sepsis=1; Meningitis=2; Sepsis and Meningitis=3; Bacteriemia=4; Arthritis=5; Other=6)
dplyr::recode(table2$diagnosis, `1`="Sepsis", `2`="Meningitis", `3`="Sepsis and meningitis", `4`="Bacteriaemia", `5`="Arthritis", `6`="Other")
#Outcome (Death=1; Survival with no sequelae=2; Survival with sequelae=3; Amputation=4; ENT=5; Development=6; Others=7)
dplyr::recode(table2$outcome, `1`="Death", `2`="Survival with no sequelae", `3`="Survival with sequelae", `4`="Amputation", `5`="ENT", `6`="Development", `7`="Other")


table2<-table2 %>% group_by(id) %>% mutate(pre_doses=
                                         sum( 
                                           (c(ymd(menBvac_date1), ymd(menBvac_date2), ymd(menBvac_date3), ymd(menBvac_date4))
                                            < (ymd(doa)+days(14)) ), na.rm = TRUE
                                         )
)

doses_at_age<- function(d1,d2,d3,d4,doa,dob){
  doses<-na.omit(c(ymd(d1),ymd(d2),ymd(d3),ymd(d4)))
  doses<-doses[ doses< (ymd(doa)-days(14)) ]

  ages<-(ymd(dob) %--% doses / months(1))
  ages<-floor(ages)
  
    return( paste( paste( ages, collapse="M, "), "M", sep=""))
}
doses_at_age("2012-01-01","2012-02-01","2012-03-01","2012-04-01", "2012-03-01", "2011-03-01")
table2<-table2 %>% group_by(id) %>% mutate(doses_at=doses_at_age(menBvac_date1,menBvac_date2,menBvac_date3,menBvac_date4,doa,dob))

table2$vaccineStatus<-"unvaccinated"
table2[table2$partial_vaccinated,]$vaccineStatus<-"Partial"
table2[table2$menBvaccinated,]$vaccineStatus<-"Complete"

genotyping<-data.frame(
cc_id=   c("0106","0109","0303","0508","1206","1801","1904","3001","3003","3102","3602"),
genotype=c("-nk-","cc11","cc32","cc461","***","***","cc23","cc213","cc231","cc103","cc865"),
stringsAsFactors = FALSE
)

table2<-table2 %>% right_join(genotyping, by = "cc_id")

table2_data<- data.frame(
                        #ID = 1:nrow(table2),
                        ID = table2$cc_id,
                        Year = sprintf("%4.0f",year(table2$doa)),
                        Age =  sprintf("%4.0f",table2$ageinmonths),
                        Diagnosis = dplyr::recode(table2$diagnosis, `1`="Sepsis", `2`="Meningitis", `3`="Sepsis and meningitis", `4`="Bacteriaemia", `5`="Arthritis", `6`="Other"),
                        Outcome = dplyr::recode(table2$outcome, `1`="Death", `2`="Survival with no sequelae", `3`="Survival with sequelae", `4`="Amputation", `5`="ENT", `6`="Development", `7`="Other"),
                        Duration = sprintf("%4.0f",table2$hospital_duration),
                        Group = table2$nm_group,
                        'No. Doses' = table2$no_prior_doses,
                        'at' = table2$doses_at,
                        VaccineStatus = table2$vaccineStatus,
                        Genotype = table2$genotype
)


flextable(table2_data)

ft_table2 <- fontsize(ft_table2, size = 6, part = "all")
ft_table2 <- autofit(ft_table2)

# Update word files -------------------------------------------------------
infile<-"PT-BEST paper v1.3_template_results.docx"
outfile<-"PT-BEST paper v1.3_results.docx"

renderInlineCode(infile,outfile)

doc <- read_docx(outfile)

body_replace_flextable_at_bkm(doc, "t_table1", ft_table1, align = "center", split = FALSE)
body_replace_flextable_at_bkm(doc, "t_table2", ft_table2, align = "center", split = FALSE)
print(doc, target = outfile)



# Shamez table ------------------------------------------------------------

# - aged 4 to 15 mo with 2 or more doses with the last dose >=14 d before presentation 
# - aged >=16 m with 2 or 3 doses before 1 y and one dose after 1 y of age ( booster dose at least 14 days before presentation)
#  >=2 doses after the first birthday (with the second dose at least 14 days before presentation)."  

#2+0 (with IMD before their booster, or 16 months, whichever comes first)
table(data[data$case,]$sched_2_0)
pretty_output(clogit(case ~ sched_2_0 + strata(case_id), data , method="efron",robust=TRUE))

#3+ 0 (with IMD before their booster, or 16 months, whichever comes first)
table(data[data$case,]$sched_3_0)

#2/3+0 (with IMD before their booster, or 16 months, whichever comes first
table(data[data$case,]$sched_23_0)

#2/3+1 schedule (i.e. infant + booster from 12 months)
table(data[data$case,]$sched_23_1)

#2 doses after 1st birthday with disease before 5th birthday
table(data[data$case,]$sched_x_2_u5)

#2 doses with disease after 5th birthday 
table(data[data$case,]$sched_x_2_o5)

#All above cases with at least 2 doses
table(data[data$case,]$sched_x_2)

#Cases with only 1 dose of 4CMenB after 1 year of age
table(data[data$case,]$sched_0_1)

#Cases meeting full vaccination criteria
table(data[data$case,]$menBvaccinated)


results_table3_line<- function(case, schedule){
  temp<-data.frame(case,schedule)
  return(
  paste("Ca ", nrow(subset(temp, case & schedule)), ":", nrow(subset(temp, case & !schedule)), " / Co ", nrow(subset(temp, !case & schedule)), ":", nrow(subset(temp, !case & !schedule)),sep="") 
  )
}

results_table3_column<- function(dataset){
  return(
    c(
      nrow(subset(dataset, menBvaccinated)),
      results_table3_line(dataset$case,dataset$menBvaccinated),
      pretty_output(clogit(case ~ menBvaccinated + strata(case_id), dataset, method="efron",robust=TRUE)),
      
      nrow(subset(dataset, ageinmonths < 16 & sched_2_0)),
      results_table3_line(dataset[dataset$ageinmonths < 16,]$case,dataset[dataset$ageinmonths < 16,]$sched_2_0),
      pretty_output(clogit(case ~ sched_2_0 + strata(case_id), subset(dataset, ageinmonths < 16), method="efron",robust=TRUE)),
      
      nrow(subset(dataset, sched_3_0)),
      results_table3_line(dataset[dataset$ageinmonths < 16,]$case,dataset[dataset$ageinmonths < 16,]$sched_3_0),
      pretty_output(clogit(case ~ sched_3_0 + strata(case_id), subset(dataset, ageinmonths < 16), method="efron",robust=TRUE)),
      
      nrow(subset(dataset, sched_23_0)),
      results_table3_line(dataset[dataset$ageinmonths < 16,]$case,dataset[dataset$ageinmonths < 16,]$sched_23_0),
      pretty_output(clogit(case ~ sched_23_0 + strata(case_id), subset(dataset, ageinmonths < 16), method="efron",robust=TRUE)),

      nrow(subset(dataset, sched_23_1)),
      results_table3_line(dataset[dataset$ageinmonths >12 & dataset$ageinmonths <16,]$case,dataset[dataset$ageinmonths >12 & dataset$ageinmonths <16,]$sched_23_1),
      tryCatch(
      pretty_output(clogit(case ~ sched_23_1 + strata(case_id), subset(dataset, ageinmonths > 12 & ageinmonths < 16) , method="efron",robust=TRUE)),
      error=function(e) paste("Error!")),
      
      nrow(subset(dataset, sched_x_2_u5)),
      results_table3_line(dataset[dataset$ageinmonths < 60 & dataset$ageinmonths >= 12,]$case,dataset[dataset$ageinmonths < 60 & dataset$ageinmonths >=12,]$sched_x_2_u5),
      pretty_output(clogit(case ~ sched_x_2_u5 + strata(case_id), subset(dataset, ageinmonths < 60) , method="efron",robust=TRUE)),

      nrow(subset(dataset, sched_x_2_o5)),
      results_table3_line(dataset[dataset$ageinmonths >=60,]$case,dataset[dataset$ageinmonths >=60,]$sched_x_2_o5),
      tryCatch(
        pretty_output(clogit(case ~ sched_x_2_o5 + strata(case_id), subset(dataset, ageinmonths >= 60) , method="efron",robust=TRUE)),
      error=function(e) paste("Error!")),
      
      nrow(subset(dataset, sched_x_2)),
      results_table3_line(dataset[dataset$ageinmonths >12,]$case,dataset[dataset$ageinmonths >12,]$sched_x_2),
      pretty_output(clogit(case ~ sched_x_2 + strata(case_id), dataset , method="efron",robust=TRUE)),
      
      nrow(subset(dataset, sched_0_1)),
      results_table3_line(dataset[dataset$ageinmonths >12,]$case,dataset[dataset$ageinmonths >12,]$sched_0_1),
      pretty_output(clogit(case ~ sched_0_1 + strata(case_id), dataset , method="efron",robust=TRUE))
    )
  )
}


       

table3_cols=c( "PT schedule","","",
               "2+0 (<16m)","","",
               "3+0 (<16m)","","",
               "2/3+0 (<16m)","","",
               "2/3+1 (>12m)","","",
               "0+2 <5yr","","",
               "0+2 >5yr","","",
               "0+2 all","","",
               "0+1","",""
               )

table3_data<- data.frame(table3_cols,
                         B    = results_table3_column(subset(data, case_group=="B" & ageindays >=134 )),
                         nonB = results_table3_column(subset(data, case_group!="B" & ageindays >=134 )),
                         Y    = results_table3_column(subset(data, case_group=="Y" & ageindays >=134 )),
                         W    = results_table3_column(subset(data, case_group=="W" )),
                         all  = results_table3_column(subset(data,                 ageindays  >=134))
)

ft<-flextable(table3_data)
ft <- fontsize(ft, size = 14, part = "all")
ft

print(ft, preview = "docx")


#checks
#x+0
nrow(subset(data, ageinmonths < 16 & case & ageindays >=134))
nrow(subset(data, ageinmonths < 16 & !case & ageindays >=134))

#2/3+1
nrow(subset(data, ageinmonths < 16 & ageinmonths >12 & case & ageindays >=134))
nrow(subset(data, ageinmonths < 16 & ageinmonths >12 & !case & ageindays >=134))

#0+2 <5)
nrow(subset(data, ageinmonths < 60 & ageinmonths >12 & case & ageindays >=134))
nrow(subset(data, ageinmonths < 60 & ageinmonths >12 & !case & ageindays >=134))

#0+2 >5)
nrow(subset(data, ageinmonths > 60 & ageinmonths >12 & case & ageindays >=134))
nrow(subset(data, ageinmonths > 60 & ageinmonths >12 & !case & ageindays >=134))





data[data$sched_x_2,]
sched_0_2<-data[data$sched_x_2,]
table(sched_0_2$age_at_assessment)
sched_0_2$age_at_dose1<-(ymd(sched_0_2$dob) %--% ymd(sched_0_2$menBvac_date1))/months(1)
table(sched_0_2$age_at_dose1)
sched_0_2$age_at_dose2<-(ymd(sched_0_2$dob) %--% ymd(sched_0_2$menBvac_date2))/months(1)
table(sched_0_2$age_at_dose2)
