aggdf_Create<-function(agg.df=NA, df=inds, level=NA, ve.susc=ve.susc, ve.infect=ve.infect,
                       prob.test.sympt.vac=NA, prob.test.sympt.unvac=NA,
                       N=NA, N.vac=NA, N.unvac=NA, t=ts,test.start=test.start){
  
  
  index<-t+1
  
  #TRUE NUMBER OF THOSE WHO ARE CURRENTLY EXPOSED OR INFECTIOUS WITH COVID (IE INFECTED)
 
  agg.df$Covid_infect_unvac[index]<-sum((df[,"Covid_status"]=="E" | df[,"Covid_status"]=="I") & df[,"Vac_status"]=="unvac")  #UNVAC
  agg.df$Covid_infect_vac[index]<-sum((df[,"Covid_status"]=="E" | df[,"Covid_status"]=="I") & df[,"Vac_status"]=="vac") #VAC
  agg.df$Covid_infect[index]<-agg.df$Covid_infect_unvac[index] + agg.df$Covid_infect_vac[index]
  
  #TRUE PROPORTION OF THOSE WHO ARE CURRENTLY EXPOSED OR INFECTIOUS WITH COVID (IE INFECTED)
  
  agg.df$Covid_propInfect_unvac[index]<-agg.df$Covid_infect_unvac[index]/N.unvac
  agg.df$Covid_propInfect_vac[index]<-agg.df$Covid_infect_vac[index]/N.vac
  agg.df$Covid_propInfect[index]<-agg.df$Covid_infect[index]/N
  
  #TRUE NUMBER OF THOSE WHO ARE CURRENTLY INFECTED WITH SYMPTOMATIC COVID (IE SYMPTOMATIC INFECTIOUS)
  
  agg.df$Covid_symptInfect_unvac[index]<-sum(df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]=="symptomatic" & df[,"Vac_status"]=="unvac")  #UNVAC
  agg.df$Covid_symptInfect_vac[index]<-sum(df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]=="symptomatic" & df[,"Vac_status"]=="vac") #VAC
  agg.df$Covid_symptInfect[index]<-agg.df$Covid_symptInfect_unvac[index] + agg.df$Covid_symptInfect_vac[index]
  
  #TRUE PROPORTION OF THOSE WHO ARE CURRENTLY INFECTED WITH SYMPTOMATIC COVID (IE SYMPTOMATIC INFECTIOUS)
  agg.df$Covid_propSymptInfect_unvac[index]<-agg.df$Covid_symptInfect_unvac[index]/N.unvac
  agg.df$Covid_propSymptInfect_vac[index]<-agg.df$Covid_symptInfect_vac[index]/N.vac
  agg.df$Covid_propSymptInfect[index]<-agg.df$Covid_symptInfect[index]/N
    
  #TRUE CUMULATIVE NUMBER OF THOSE WHO ARE CURRENTLY EXPOSED OR INFECTIOUS WITH COVID (IE CUM. INFECTED)
  
  agg.df$Covid_cumInfect_unvac[index]<-sum(df[,"Covid_everInfected"]>0 & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$Covid_cumInfect_vac[index]<-sum(df[,"Covid_everInfected"]>0 & df[,"Vac_status"]=="vac") #VAC
  
  #TRUE NUMBER OF THOSE WHO ARE CURRENTLY INFECTED WITH SYMPTOMATIC NonCOVID (SYMPTOMATIC UNINFECTED)
  
  agg.df$NonCovid_symptInfect_unvac[index]<-sum(df[,"NonCovid_status"]=="I" & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$NonCovid_symptInfect_vac[index]<-sum(df[,"NonCovid_status"]=="I" & df[,"Vac_status"]=="vac") #VAC
  agg.df$NonCovid_symptInfect[index]<-agg.df$NonCovid_symptInfect_unvac[index] + agg.df$NonCovid_symptInfect_vac[index]
  
  #TRUE PROPORTION OF THOSE WHO ARE CURRENTLY INFECTED WITH SYMPTOMATIC NonCOVID (SYMPTOMATIC UNINFECTED)
  
  agg.df$NonCovid_propSymptInfect_unvac[index]<-agg.df$NonCovid_symptInfect_unvac[index]/N.unvac
  agg.df$NonCovid_propSymptInfect_vac[index]<-agg.df$NonCovid_symptInfect_vac[index]/N.vac
  agg.df$NonCovid_propSymptInfect[index]<-agg.df$NonCovid_symptInfect[index]/N
  
  #TRUE NUMBER OF THOSE WHO ARE CURRENTLY CO-INFECTED WITH COVID-19 (ASYMPTOMATIC OR SYMPTOMATIC) & A SYMPTOMATIC NonCOVID INFECTION (SYMPTOMATIC UNINFECTED)
  #All will be symptomatic given a NonCovid infection is assumed to be always symptomatic
  
  agg.df$CoInfect_unvac[index]<-sum(df[,"NonCovid_status"]=="I" & (df[,"Covid_status"]=="E" | df[,"Covid_status"]=="I") & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$CoInfect_vac[index]<-sum(df[,"NonCovid_status"]=="I" & (df[,"Covid_status"]=="E" | df[,"Covid_status"]=="I") & df[,"Vac_status"]=="vac") #VAC
  
  
  #TRUE NUMBER OF THOSE WHO ARE CURRENTLY CO-INFECTED WITH SYMPTOMATIC COVID & SYMPTOMATIC NonCOVID (SYMPTOMATIC UNINFECTED)
  
  agg.df$Sympt_coInfect_unvac[index]<-sum(df[,"NonCovid_status"]=="I" & df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]=="symptomatic" & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$Sympt_coInfect_vac[index]<-sum(df[,"NonCovid_status"]=="I" & df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]=="symptomatic" & df[,"Vac_status"]=="vac") #VAC
  
  #TRUE NUMBER OF THOSE WHO HAVE NEVER BEEN INFECTED WITH ANY INFECTION  (UNINFECTED)
 
  agg.df$NeverInfect_unvac[index]<-sum(df[,"NonCovid_everSymptInfected"]==0 & df[,"Covid_status"]=="S" & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$NeverInfect_vac[index]<-sum(df[,"NonCovid_everSymptInfected"]==0 & (df[,"Covid_status"]=="S" | df[,"Covid_status"]=="S_protected") & df[,"Vac_status"]=="vac") #VAC
 
  
#####  SYMPTOMATIC INFECTION VARIABLES USED FOR VE.SYMPT CALCULATIONS AND RELATED CALCULATIONS
  
  #TRUE CUMULATIVE NUMBER OF THOSE WITH SYMPTOMATIC COVID-19 (VE.RR AND VE.OR NUMERATOR)
  
  agg.df$Covid_cumSymptInfect_unvac[index]<-sum(df[,"Covid_everSymptInfected"]>0 & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$Covid_cumSymptInfect_vac[index]<-sum(df[,"Covid_everSymptInfected"]>0 & df[,"Vac_status"]=="vac")  #VAC
  agg.df$Covid_cumSymptInfect[index]<-agg.df$Covid_cumSymptInfect_unvac[index] + agg.df$Covid_cumSymptInfect_vac[index]
  
  #TRUE CUMULATIVE PROPORTION OF THOSE WITH SYMPTOMATIC COVID-19
  agg.df$Covid_cumPropSymptInfect_unvac[index]<-agg.df$Covid_cumSymptInfect_unvac[index]/N.unvac
  agg.df$Covid_cumPropSymptInfect_vac[index]<-agg.df$Covid_cumSymptInfect_vac[index]/N.vac
  agg.df$Covid_cumPropSymptInfect[index]<-agg.df$Covid_cumSymptInfect[index]/N
  
  #TRUE CUMULATIVE NUMBER OF THOSE WHO HAVE BEEN INFECTED WITH SYMPTOMATIC NonCOVID AND NEVER INFECTED WITH COVID-19 (1ST OPTION FOR VE.OR DENOMINATOR)
  
  agg.df$NonCovid_cumSymptInfect_covidSusc_unvac[index]<-sum(df[,"NonCovid_everSymptInfected"]>0 & df[,"Covid_status"]=="S" & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$NonCovid_cumSymptInfect_covidSusc_vac[index]<-sum(df[,"NonCovid_everSymptInfected"]>0 & (df[,"Covid_status"]=="S" | df[,"Covid_status"]=="S_protected") & df[,"Vac_status"]=="vac") #VAC #ONLY been infected with Noncovid
  
  #TRUE CUMULATIVE NUMBER OF THOSE WHO HAVE BEEN INFECTED WITH SYMPTOMATIC NonCOVID AND NEVER INFECTED WITH SYMPT COVID-19 (2ND OPTION FOR VE.OR DENOMINATOR)  
  
  agg.df$NonCovid_cumSymptInfect_unvac[index]<-sum(df[,"NonCovid_everSymptInfected"]>0 & df[,"Covid_everSymptInfected"]==0 & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$NonCovid_cumSymptInfect_vac[index]<-sum(df[,"NonCovid_everSymptInfected"]>0 & df[,"Covid_everSymptInfected"]==0 & df[,"Vac_status"]=="vac") #VAC
  agg.df$NonCovid_cumSymptInfect[index]<-agg.df$NonCovid_cumSymptInfect_unvac[index] + agg.df$NonCovid_cumSymptInfect_vac[index]

  
  #TRUE CUMULATIVE PROPORTION OF THOSE WHO HAVE BEEN INFECTED WITH SYMPTOMATIC NonCOVID AND NEVER INFECTED WITH SYMPT COVID-19 
  agg.df$NonCovid_cumPropSymptInfect_unvac[index]<-agg.df$NonCovid_cumSymptInfect_unvac[index]/N.unvac
  agg.df$NonCovid_cumPropSymptInfect_vac[index]<-agg.df$NonCovid_cumSymptInfect_vac[index]/N.vac
  agg.df$NonCovid_cumPropSymptInfect[index]<-agg.df$NonCovid_cumSymptInfect[index]/N
  
  
  ##### ACTUAL TRUE VE AGAINST SYMPTOMATIC INFECTION
  
  #RR
  if(agg.df$Covid_cumSymptInfect_unvac[index]==0){
    agg.df$VE_symptRR[index]<-NA
  }else{
    agg.df$VE_symptRR[index]<-1-((agg.df$Covid_cumSymptInfect_vac[index]/N.vac)/(agg.df$Covid_cumSymptInfect_unvac[index]/N.unvac))  
  }
  
  #OR (OPTION 1: Denominator contains those who have never been infected with Covid-19)
  if(agg.df$NonCovid_cumSymptInfect_covidSusc_vac[index]==0 | agg.df$NonCovid_cumSymptInfect_covidSusc_unvac[index]==0 | agg.df$Covid_cumSymptInfect_unvac[index]==0){
    agg.df$VE_symptSuscOR[index]<-NA
  }else{
    agg.df$VE_symptSuscOR[index]<-1-((agg.df$Covid_cumSymptInfect_vac[index]/agg.df$NonCovid_cumSymptInfect_covidSusc_vac[index])/(agg.df$Covid_cumSymptInfect_unvac[index]/agg.df$NonCovid_cumSymptInfect_covidSusc_unvac[index]))
  }
  
  #OR (OPTION 1: Denominator contains those who have never been infected with symptomatic Covid-19)
  if(agg.df$NonCovid_cumSymptInfect_vac[index]==0 | agg.df$NonCovid_cumSymptInfect_unvac[index]==0 | agg.df$Covid_cumSymptInfect_unvac[index]==0){
    agg.df$VE_symptOR[index]<-NA
  }else{
    agg.df$VE_symptOR[index]<-1-((agg.df$Covid_cumSymptInfect_vac[index]/agg.df$NonCovid_cumSymptInfect_vac[index])/(agg.df$Covid_cumSymptInfect_unvac[index]/agg.df$NonCovid_cumSymptInfect_unvac[index]))
  }

  ##### TESTING VARIABLES USED TO CALCULATE OBSERVED VE AND RELATED TESTING VARIABLES
  
  #OBSERVED CUMULATIVE NUMBER OF THOSE WHO HAVE EVER BEEN INFECTED WITH COVID AND WERE SYMPTOMATIC (VE.RR.OBSERVED AND VE.OR.OBSERVED NUMERATOR)
  
  agg.df$Covid_cumSymptExpInfect_tested_unvac[index]<-sum((df[,paste("Covid_testedSymptInf",level,sep="_")]>0 | df[,paste("Covid_testedSymptExp",level,sep="_")]>0) & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$Covid_cumSymptExpInfect_tested_vac[index]<-sum((df[,paste("Covid_testedSymptInf",level,sep="_")]>0 | df[,paste("Covid_testedSymptExp",level,sep="_")]>0) & df[,"Vac_status"]=="vac") #VAC
  agg.df$Covid_cumSymptExpInfect_tested[index]<-agg.df$Covid_cumSymptExpInfect_tested_unvac[index] + agg.df$Covid_cumSymptExpInfect_tested_vac[index]
  
  #OBSERVED CUMULATIVE PROPORTION OF THOSE WHO HAVE EVER BEEN INFECTED WITH COVID AND WERE SYMPTOMATIC
  agg.df$Covid_cumPropSymptExpInfect_tested_unvac[index]<-agg.df$Covid_cumSymptExpInfect_tested_unvac[index]/N.unvac
  agg.df$Covid_cumPropSymptExpInfect_tested_vac[index]<-agg.df$Covid_cumSymptExpInfect_tested_vac[index]/N.vac
  agg.df$Covid_cumPropSymptExpInfect_tested[index]<-agg.df$Covid_cumSymptExpInfect_tested[index]/N
  
  
  #OBSERVED CUMULATIVE NUMBER OF THOSE WHO HAVE BEEN SYMPTOMATIC BUT NEVER INFECTED WITH COVID-19 WHILE SYMPTOMATIC (VE.OR.OBSERVED DENOMINATOR)
  
  agg.df$NonCovid_cumSymptInfect_tested_unvac[index]<-sum(df[,paste("NonCovid_testedSymptInf",level,sep="_")]>0 & (df[,paste("Covid_testedSymptExp",level,sep="_")]==0 & df[,paste("Covid_testedSymptInf",level,sep="_")]==0) & df[,"Vac_status"]=="unvac") #unvac
  agg.df$NonCovid_cumSymptInfect_tested_vac[index]<-sum(df[,paste("NonCovid_testedSymptInf",level,sep="_")]>0 & (df[,paste("Covid_testedSymptExp",level,sep="_")]==0 & df[,paste("Covid_testedSymptInf",level,sep="_")]==0) & df[,"Vac_status"]=="vac") #vac
  agg.df$NonCovid_cumSymptInfect_tested[index]<-agg.df$NonCovid_cumSymptInfect_tested_unvac[index] + agg.df$NonCovid_cumSymptInfect_tested_vac[index]
 
  #OBSERVED CUMULATIVE PROPORTION OF THOSE WHO HAVE BEEN SYMPTOMATIC BUT NEVER INFECTED WITH COVID-19 WHILE SYMPTOMATIC
  
  agg.df$NonCovid_cumPropSymptInfect_tested_unvac[index]<-agg.df$NonCovid_cumSymptInfect_tested_unvac[index]/N.unvac
  agg.df$NonCovid_cumPropSymptInfect_tested_vac[index]<-agg.df$NonCovid_cumSymptInfect_tested_vac[index]/N.vac
  agg.df$NonCovid_cumPropSymptInfect_tested[index]<-agg.df$NonCovid_cumSymptInfect_tested[index]/N
  
  ##### OBSERVED VE AGAINST SYMPTOMATIC INFECTION
  
  #VE.RR Observed
  if(agg.df$Covid_cumSymptExpInfect_tested_unvac[index]==0){
    agg.df$VE_symptRR_observed[index]<-NA
  }else{
    agg.df$VE_symptRR_observed[index]<-1-((agg.df$Covid_cumSymptExpInfect_tested_vac[index]/N.vac)/(agg.df$Covid_cumSymptExpInfect_tested_unvac[index]/N.unvac))
  }
  
  #VE.OR Observed
  if(agg.df$NonCovid_cumSymptInfect_tested_vac[index]==0 |agg.df$NonCovid_cumSymptInfect_tested_unvac[index]==0 | agg.df$Covid_cumSymptExpInfect_tested_unvac[index]==0){
    agg.df$VE_symptOR_observed[index]<-NA
  }else{
    agg.df$VE_symptOR_observed[index]<-1-((agg.df$Covid_cumSymptExpInfect_tested_vac[index]/agg.df$NonCovid_cumSymptInfect_tested_vac[index])/(agg.df$Covid_cumSymptExpInfect_tested_unvac[index]/agg.df$NonCovid_cumSymptInfect_tested_unvac[index]))
  }
  
  
  
#####ADDITIONAL TESTING-RELATED VARIABLES
  
  #CUMULATIVE NUMBER OF INDIVIDUALS WHO HAVE EVER TESTED
  agg.df$EverTested_unvac[index]<-sum((df[,paste("Covid_testedSymptExp",level,sep="_")]>0 | df[,paste("Covid_testedSymptInf",level,sep="_")]>0 | df[,paste("NonCovid_testedSymptInf",level,sep="_")]>0) & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$EverTested_vac[index]<-sum((df[,paste("Covid_testedSymptExp",level,sep="_")]>0 | df[,paste("Covid_testedSymptInf",level,sep="_")]>0 | df[,paste("NonCovid_testedSymptInf",level,sep="_")]>0) & df[,"Vac_status"]=="vac") #VAC
  
  
  #CUMULATIVE NUMBER OF FALSE POSITIVES
  agg.df$CumNum_falsePos_unvac[index]<-sum((df[,paste("Covid_falseSymptPos",level,sep="_")]>0 & df[,"Vac_status"]=="unvac")) #UNVAC
  agg.df$CumNum_falsePos_vac[index]<-sum((df[,paste("Covid_falseSymptPos",level,sep="_")]>0 & df[,"Vac_status"]=="vac")) #VAC
  
  #CUMULATIVE PROPORTION OF FALSE POSITIVES
  if(agg.df$EverTested_unvac[index]==0){agg.df$CumProp_falsePos_unvac[index]<-NA}else{agg.df$CumProp_falsePos_unvac[index]<-agg.df$CumNum_falsePos_unvac[index]/agg.df$EverTested_unvac[index]} #UNVAC
  if(agg.df$EverTested_vac[index]==0){agg.df$CumProp_falsePos_vac[index]<-NA}else{agg.df$CumProp_falsePos_vac[index]<-agg.df$CumNum_falsePos_vac[index]/agg.df$EverTested_vac[index]} #VAC
  
  #CUMULATIVE NUMBER OF FALSE NEGATIVES
  agg.df$CumNum_falseNeg_unvac[index]<-sum((df[,paste("Covid_falseNeg",level,sep="_")]>0 & df[,"Vac_status"]=="unvac")) #UNVAC
  agg.df$CumNum_falseNeg_vac[index]<-sum((df[,paste("Covid_falseNeg",level,sep="_")]>0 & df[,"Vac_status"]=="vac")) #VAC
  
  #CUMULATIVE PROPORTION OF FALSE NEGATIVES
  if(agg.df$EverTested_unvac[index]==0){agg.df$CumProp_falseNeg_unvac[index]<-NA}else{agg.df$CumProp_falseNeg_unvac[index]<-agg.df$CumNum_falseNeg_unvac[index]/agg.df$EverTested_unvac[index]} #UNVAC
  if(agg.df$EverTested_vac[index]==0){agg.df$CumProp_falseNeg_vac[index]<-NA}else{agg.df$CumProp_falseNeg_vac[index]<-agg.df$CumNum_falseNeg_vac[index]/agg.df$EverTested_vac[index]} #VAC
  
  
  #CUMULATIVE NUMBER OF ALL MISCLASSIFICATION
  agg.df$CumNum_misclass_unvac[index]<-agg.df$CumNum_falsePos_unvac[index] + agg.df$CumNum_falseNeg_unvac[index]
  agg.df$CumNum_misclass_vac[index]<-agg.df$CumNum_falsePos_vac[index] + agg.df$CumNum_falseNeg_vac[index]
    
  #CUMULATIVE NUMBER OF ALL MISCLASSIFICATION
  if(agg.df$EverTested_unvac[index]==0){agg.df$CumProp_misclass_unvac[index]<-NA}else{agg.df$CumProp_misclass_unvac[index]<-agg.df$CumNum_misclass_unvac[index]/agg.df$EverTested_unvac[index]} #UNVAC
  if(agg.df$EverTested_vac[index]==0){agg.df$CumProp_misclass_vac[index]<-NA}else{agg.df$CumProp_misclass_vac[index]<-agg.df$CumNum_misclass_vac[index]/agg.df$EverTested_vac[index]} #VAC  
  
  #CREATE VARIABLE INDICATOR FOR VES
  agg.df$Label_veSusc[index]=paste("VE Susc.=", ve.susc,sep=" ")
  agg.df$Label_veInfect[index]=paste("VE Infect.=", ve.infect, sep=" ")
  agg.df$Label_veJoint[index]=paste(agg.df$Label_veSusc[index], agg.df$Label_veInfect[index])
  
  #Create variable indicator label for symptom testing increase
  agg.df$Label_testingDiff[index]=level
  
  ##################################################
  
  #HAS THE TESTING PERIOD STARTED
  agg.df$TestedStarted_bin[index]<-t >= test.start
  
  agg.df$t[index]=t
  
  return(agg.df)
}

