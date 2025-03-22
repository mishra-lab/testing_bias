aggdf_Create<-function(agg.df=NA, df=inds, level=NA, ve.susc=ve.susc, ve.infect=ve.infect,
                       prob.test.sympt.vac=NA, prob.test.sympt.unvac=NA,
                       N=NA, N.vac=NA, N.unvac=NA, t=ts){
  
  index<-t+1
  
  #PROPORTION INFECTED TO CALCULATE THE HIGHEST CHANGE POINT AND EPIDEMIC PEAK
  agg.df$Covid_infect[index]<-sum(df[,"Covid_status"]=="E" | df[,"Covid_status"]=="I") 
  agg.df$Covid_propInfect[index]<-agg.df$Covid_infect[index]/N
  
   #TRUE NUMBER OF THOSE WHO ARE CURRENTLY INFECTED WITH SYMPTOMATIC COVID (IE SYMPTOMATIC INFECTIOUS)
  agg.df$Covid_symptInfect_unvac[index]<-sum(df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]=="symptomatic" & df[,"Vac_status"]=="unvac")  #UNVAC
  agg.df$Covid_symptInfect_vac[index]<-sum(df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]=="symptomatic" & df[,"Vac_status"]=="vac") #VAC
  agg.df$Covid_symptInfect[index]<-agg.df$Covid_symptInfect_unvac[index] + agg.df$Covid_symptInfect_vac[index]
  
  #TRUE PROPORTION OF THOSE WHO ARE CURRENTLY INFECTED WITH SYMPTOMATIC COVID (IE SYMPTOMATIC INFECTIOUS)
  
  agg.df$Covid_propSymptInfect_unvac[index]<-agg.df$Covid_symptInfect_unvac[index]/N.unvac
  agg.df$Covid_propSymptInfect_vac[index]<-agg.df$Covid_symptInfect_vac[index]/N.vac
  agg.df$Covid_propSymptInfect[index]<-agg.df$Covid_symptInfect[index]/N
  
  #####  SYMPTOMATIC INFECTION VARIABLES USED FOR VE.SYMPT CALCULATIONS AND RELATED CALCULATIONS
  
  #TRUE CUMULATIVE NUMBER OF THOSE WITH SYMPTOMATIC COVID-19 (VE.RR AND VE.OR NUMERATOR)
  
  agg.df$Covid_cumSymptInfect_unvac[index]<-sum(df[,"Covid_everSymptInfected"]>0 & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$Covid_cumSymptInfect_vac[index]<-sum(df[,"Covid_everSymptInfected"]>0 & df[,"Vac_status"]=="vac")  #VAC
  
  #TRUE CUMULATIVE PROPORTION OF THOSE WITH SYMPTOMATIC COVID-19
  agg.df$Covid_cumPropSymptInfect_unvac[index]<-agg.df$Covid_cumSymptInfect_unvac[index]/N.unvac
  agg.df$Covid_cumPropSymptInfect_vac[index]<-agg.df$Covid_cumSymptInfect_vac[index]/N.vac
  
  #TRUE CUMULATIVE NUMBER OF THOSE WHO HAVE BEEN INFECTED WITH SYMPTOMATIC NonCOVID AND NEVER INFECTED WITH SYMPT COVID-19 (2ND OPTION FOR VE.OR DENOMINATOR)  
  
  agg.df$NonCovid_cumSymptInfect_unvac[index]<-sum(df[,"NonCovid_everSymptInfected"]>0 & df[,"Covid_everSymptInfected"]==0 & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$NonCovid_cumSymptInfect_vac[index]<-sum(df[,"NonCovid_everSymptInfected"]>0 & df[,"Covid_everSymptInfected"]==0 & df[,"Vac_status"]=="vac") #VAC

  #TRUE CUMULATIVE PROPORTION OF THOSE WHO HAVE BEEN INFECTED WITH SYMPTOMATIC NonCOVID AND NEVER INFECTED WITH SYMPT COVID-19 
  agg.df$NonCovid_cumPropSymptInfect_unvac[index]<-agg.df$NonCovid_cumSymptInfect_unvac[index]/N.unvac
  agg.df$NonCovid_cumPropSymptInfect_vac[index]<-agg.df$NonCovid_cumSymptInfect_vac[index]/N.vac
  
  ##### ACTUAL TRUE VE AGAINST SYMPTOMATIC INFECTION
  
  #RR
  if(agg.df$Covid_cumSymptInfect_unvac[index]==0){
    agg.df$VE_symptRR[index]<-NA
  }else{
    agg.df$VE_symptRR[index]<-1-((agg.df$Covid_cumSymptInfect_vac[index]/N.vac)/(agg.df$Covid_cumSymptInfect_unvac[index]/N.unvac))  
  }
  
  
  #OR (OPTION 2: Denominator contains those who have never been infected with symptomatic Covid-19)
  if(agg.df$NonCovid_cumSymptInfect_vac[index]==0 | agg.df$NonCovid_cumSymptInfect_unvac[index]==0 | agg.df$Covid_cumSymptInfect_unvac[index]==0){
    agg.df$VE_symptOR[index]<-NA
  }else{
    agg.df$VE_symptOR[index]<-1-((agg.df$Covid_cumSymptInfect_vac[index]/agg.df$NonCovid_cumSymptInfect_vac[index])/(agg.df$Covid_cumSymptInfect_unvac[index]/agg.df$NonCovid_cumSymptInfect_unvac[index]))
  }

  ##### TESTING VARIABLES USED TO CALCULATE OBSERVED VE AND RELATED TESTING VARIABLES
  
  #OBSERVED CUMULATIVE NUMBER OF THOSE WHO HAVE EVER BEEN INFECTED WITH COVID AND WERE SYMPTOMATIC (VE.RR.OBSERVED AND VE.OR.OBSERVED NUMERATOR)
  
  agg.df$Covid_cumSymptExpInfect_tested_unvac[index]<-sum((df[,paste("Covid_testedSymptInf",level,sep="_")]>0 | df[,paste("Covid_testedSymptExp",level,sep="_")]>0) & df[,"Vac_status"]=="unvac") #UNVAC
  agg.df$Covid_cumSymptExpInfect_tested_vac[index]<-sum((df[,paste("Covid_testedSymptInf",level,sep="_")]>0 | df[,paste("Covid_testedSymptExp",level,sep="_")]>0) & df[,"Vac_status"]=="vac") #VAC
  
  #OBSERVED CUMULATIVE PROPORTION OF THOSE WHO HAVE EVER BEEN INFECTED WITH COVID AND WERE SYMPTOMATIC
  agg.df$Covid_cumPropSymptExpInfect_tested_unvac[index]<-agg.df$Covid_cumSymptExpInfect_tested_unvac[index]/N.unvac
  agg.df$Covid_cumPropSymptExpInfect_tested_vac[index]<-agg.df$Covid_cumSymptExpInfect_tested_vac[index]/N.vac
  
  #OBSERVED CUMULATIVE NUMBER OF THOSE WHO HAVE BEEN SYMPTOMATIC BUT NEVER INFECTED WITH COVID-19 WHILE SYMPTOMATIC (VE.OR.OBSERVED DENOMINATOR)
  
  agg.df$NonCovid_cumSymptInfect_tested_unvac[index]<-sum(df[,paste("NonCovid_testedSymptInf",level,sep="_")]>0 & (df[,paste("Covid_testedSymptExp",level,sep="_")]==0 & df[,paste("Covid_testedSymptInf",level,sep="_")]==0) & df[,"Vac_status"]=="unvac") #unvac
  agg.df$NonCovid_cumSymptInfect_tested_vac[index]<-sum(df[,paste("NonCovid_testedSymptInf",level,sep="_")]>0 & (df[,paste("Covid_testedSymptExp",level,sep="_")]==0 & df[,paste("Covid_testedSymptInf",level,sep="_")]==0) & df[,"Vac_status"]=="vac") #vac
 
  #OBSERVED CUMULATIVE PROPORTION OF THOSE WHO HAVE BEEN SYMPTOMATIC BUT NEVER INFECTED WITH COVID-19 WHILE SYMPTOMATIC
  
  agg.df$NonCovid_cumPropSymptInfect_tested_unvac[index]<-agg.df$NonCovid_cumSymptInfect_tested_unvac[index]/N.unvac
  agg.df$NonCovid_cumPropSymptInfect_tested_vac[index]<-agg.df$NonCovid_cumSymptInfect_tested_vac[index]/N.vac
  
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
  
  

  #CREATE VARIABLE INDICATOR FOR VES
  agg.df$Label_veSusc[index]=paste("VE Susc.=", ve.susc,sep=" ")
  agg.df$Label_veInfect[index]=paste("VE Infect.=", ve.infect, sep=" ")
  agg.df$Label_veJoint[index]=paste(agg.df$Label_veSusc[index], agg.df$Label_veInfect[index])
  
  #Create variable indicator label for symptom testing increase
  agg.df$Label_testingDiff[index]=level
  

  
  agg.df$t[index]=t
  
  return(agg.df)
}





#Two options of sampling: Sample the first test or sample a random test
testSampling_function<-function(inds.test.df=NA){
  
  inds.test.df <- inds.test.df[order(inds.test.df$Ind_id, inds.test.df$Time_step), ]
  
  if(nrow(inds.test.df) == 0){
    warning("The dataframe is empty. Returning an empty dataframe.")
    return(inds.test.df)
  }
  
  inds.test.df$Flag_sample<-ave(inds.test.df$CovidTested_positive, 
                                inds.test.df$Ind_id, 
                                FUN = function(x) {
                                  pos_indices <- which(x == 1)
                                  #Create flags column for the dataframe
                                  flags <- rep(0, length(x))
                                  
                                  if (length(pos_indices)> 0){
                                    #if there is at least one positive, always return the first one
                                    flags[pos_indices[1]]<-1
                                    return(flags)
                                   
                                  } else {
                                    
                                    # Randomly select one negative test
                                    neg_indices <- which(x == 0)
                                    
                                    if(length(neg_indices) > 0){
                                      
                                        selected <- sample(neg_indices, 1)
                                        flags[selected] <- 1
                                        
                                      return(flags)
                                      
                                    } else {
                                      # If no tests exist, return all zeros
                                      return(rep(0, length(x)))
                                    }
                                  }
                                })
  #SUBSET DATA BASED ON THE FLAG
  sampled.data<-subset(inds.test.df, Flag_sample==1)
  
  return(sampled.data)
}


aggdf_Stats<-function(agg.df=NA, level=NA, ve.susc=NA, inds.test.df=NA, df.inds.identity=NA, 
                      N=NA, N.vac=NA, N.unvac=NA, ts=NA, test.start=NA, epi.real.num =NA){
  
  if(ts<test.start){
    return(agg.df)
  }else{
  
  index<-ts+1
  
  ## CREATE RANDOM TESTING DATASET BY SAMPLING A RANDOM TEST PER INDIVIDUAL
  sampled.df<-testSampling_function(inds.test.df=inds.test.df)
    
  #CALCULATE SAMPLE SIZES BY VAC AND COVID STATUS USING RANDOM TESTING SAMPLED DATA
  agg.df<-agg.df_addSampleSize_function(sampled.data=sampled.df,agg.df=agg.df, index=index)
  
  #CREATE DATASET FOR VE.RR USING RANDOM TESTING SAMPLED DATA & INDS IDENTITY DATA 
  sampled.df.pos.only<-subset(sampled.df, CovidTested_positive==1)
  
  sampled.df.everyone <- merge(sampled.df.pos.only, df.inds.identity, by = c("Ind_id", "Vac_status"), all.y = TRUE)
  
  sampled.df.everyone$CovidTested_positive[is.na(sampled.df.everyone$CovidTested_positive)]<-0
  sampled.df.everyone$Time_step[sampled.df.everyone$CovidTested_positive==0]<-ts
  sampled.df.everyone$Week[sampled.df.everyone$CovidTested_positive==0]<-sampled.df.everyone$Time_step[sampled.df.everyone$CovidTested_positive==0] %/% 7 + 1
  sampled.df.everyone$Flag_sample[sampled.df.everyone$CovidTested_positive==0]<-1
  sampled.df.everyone$Time_step_glm_VeRR<-sampled.df.everyone$Time_step + 1
  
  #USING RANDOM TESTING DATA WITH EVERYONE RUN THE VE_RR COXPH MODEL & ADD COEF, SE and P-VALUES TO THE AGG DF DATASET
  cox.ph.model.veRR <- coxph(Surv(Time_step, CovidTested_positive) ~ Vac_status, data = sampled.df.everyone)
  agg.df<-agg.df<-agg.df_addTimeDepCoefs_function(stats.model=cox.ph.model.veRR, agg.df=agg.df, ve.type="RR",
                                                  index=index)
                  
  #USING RANDOM TESTING DATA RUN THE REGRESSION MODEL WITH TIME & ADD COEF, SE and P-VALUES TO THE AGG DF DATASET
  
  model.timeDep.VeOR <- glm(CovidTested_positive ~ Vac_status + Time_step, data = sampled.df, family = binomial)
  
  agg.df<-agg.df_addTimeDepCoefs_function(stats.model=model.timeDep.VeOR, agg.df=agg.df,
                                             ve.type="OR", index=index)
  
  
  return(agg.df)

  }
}


agg.df_addSampleSize_function<-function(sampled.data=NA,agg.df=NA, index=NA){
  
  num.unvac.covid<-sum(sampled.data$Vac_status == "unvac" & sampled.data$CovidTested_positive==1)
  num.vac.covid<-sum(sampled.data$Vac_status == "vac" & sampled.data$CovidTested_positive==1)
  
  num.unvac.non.covid<-sum(sampled.data$Vac_status == "unvac" & sampled.data$CovidTested_positive==0)
  num.vac.non.covid<-sum(sampled.data$Vac_status == "vac" & sampled.data$CovidTested_positive==0)
  
  
  agg.df[index,"Stats_sampleSizeCovid_unvac"]<-num.unvac.covid
  agg.df[index,"Stats_sampleSizeCovid_vac"]<-num.vac.covid
  
  agg.df[index,"Stats_sampleSizeNonCovid_unvac"]<-num.unvac.non.covid
  agg.df[index,"Stats_sampleSizeNonCovid_vac"]<-num.vac.non.covid
  
  return(agg.df)
  
}


agg.df_addTimeDepCoefs_function<-function(stats.model=NA, agg.df=NA, ve.type=NA, index=NA){
  
  model.summary<-summary(stats.model)
  
  
  if(ve.type=="RR"){
    #Add vaccination to the dataset
    agg.df[index, c("VacStatus_coef_timeDep_VeRR", "VacStatus_se_timeDep_VeRR", 
                    "VacStatus_pValue_timeDep_VeRR")]<-model.summary$coefficients["Vac_statusvac",
                                                                        c("coef","se(coef)", "Pr(>|z|)")]  
    
  }else if(ve.type=="OR"){
    #Add intercept to the dataset
    agg.df[index, c("Intercept_coef_timeDep_VeOR", "Intercept_se_timeDep_VeOR", 
                    "Intercept_pValue_timeDep_VeOR")]<-model.summary$coefficients["(Intercept)",
                                                                        c("Estimate","Std. Error", "Pr(>|z|)")]
    
    #Add vaccination to the dataset
    agg.df[index, c("VacStatus_coef_timeDep_VeOR","VacStatus_se_timeDep_VeOR", 
                    "VacStatus_pValue_timeDep_VeOR")]<-model.summary$coefficients["Vac_statusvac",
                                                                        c("Estimate","Std. Error", "Pr(>|z|)")] 
    #Add the time coefficient, se and p value here.
    agg.df[index, c("TimeStep_coef_timeDep_VeOR", "TimeStep_se_timeDep_VeOR", 
                    "TimeStep_pValue_timeDep_VeOR")]<-model.summary$coefficients["Time_step",
                                                                         c("Estimate","Std. Error", "Pr(>|z|)")]
  }
    
  
  return(agg.df)
  
  }



