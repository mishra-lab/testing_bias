setwd("C:/Users/Korryn/Desktop/Testing Biases/Github Documents")

source("ABM_mainFunctions.R")
source("Aggdf_mainFunctions.R")
require(igraph)
require(data.table)
igraph_options(return.vs.es = T)

#Seed_num is the dataframes for the seed numbers used to create our analysis

#Set-up for scinet
args <- commandArgs(trailingOnly = TRUE)

ve.susc<- 0.1 #as.numeric(args[1]) #VE against Susc
ve.infect<- 0.2 #as.numeric(args[2])  #VE against Infect
covid.prob.trans<-0.1 #as.numeric(args[3])  #Probability of transmission per contact
epi.real.num<- 5 #as.numeric(args[4]) #simulationnumber

seed.no<-sample.int(1e9, 1) 

set.seed(seed.no)

covid.prob.asympt<-0.35 #as.numeric(args[4])
noncovid.prop.infect<-0.152 #as.numeric(args[5])

#start of the main analysis
time_steps<- 100#150 #length of observation period
N= 1000 #100000 #number of individuals per sample


######SET-UP THE AGGREGATE DATAFRAMES############

col_names.agg<-c( "Covid_infect_unvac", "Covid_infect_vac","Covid_infect",
                  "Covid_propInfect_unvac", "Covid_propInfect_vac","Covid_propInfect",
                  "Covid_cumInfect_unvac", "Covid_cumInfect_vac",
                  "Covid_symptInfect_unvac", "Covid_symptInfect_vac","Covid_symptInfect",
                  "Covid_propSymptInfect_unvac", "Covid_propSymptInfect_vac","Covid_propSymptInfect",
                  "NonCovid_symptInfect_unvac", "NonCovid_symptInfect_vac", "NonCovid_symptInfect",
                  "NonCovid_propSymptInfect_unvac", "NonCovid_propSymptInfect_vac", "NonCovid_propSymptInfect",
                  "CoInfect_unvac", "CoInfect_vac", "Sympt_coInfect_unvac", "Sympt_coInfect_vac",
                  "NeverInfect_unvac", "NeverInfect_vac",
                  "Covid_cumSymptInfect_unvac","Covid_cumSymptInfect_vac", "Covid_cumSymptInfect",
                  "Covid_cumPropSymptInfect_unvac", "Covid_cumPropSymptInfect_vac", "Covid_cumPropSymptInfect",
                  "NonCovid_cumSymptInfect_covidSusc_unvac", "NonCovid_cumSymptInfect_covidSusc_vac",
                  "NonCovid_cumSymptInfect_unvac", "NonCovid_cumSymptInfect_vac", "NonCovid_cumSymptInfect",
                  "NonCovid_cumPropSymptInfect_unvac", "NonCovid_cumPropSymptInfect_vac", "NonCovid_cumPropSymptInfect",
                  "Covid_cumSymptExpInfect_tested_unvac", "Covid_cumSymptExpInfect_tested_vac","Covid_cumSymptExpInfect_tested",
                  "Covid_cumPropSymptExpInfect_tested_unvac", "Covid_cumPropSymptExpInfect_tested_vac", "Covid_cumPropSymptExpInfect_tested",
                  "NonCovid_cumSymptInfect_tested_unvac", "NonCovid_cumSymptInfect_tested_vac","NonCovid_cumSymptInfect_tested",
                  "NonCovid_cumPropSymptInfect_tested_unvac", "NonCovid_cumPropSymptInfect_tested_vac", "NonCovid_cumPropSymptInfect_tested",
                  "EverTested_unvac", "EverTested_vac", 
                  "CumNum_falsePos_unvac", "CumNum_falsePos_vac", "CumProp_falsePos_unvac", "CumProp_falsePos_vac",
                  "CumNum_falseNeg_unvac", "CumNum_falseNeg_vac", "CumProp_falseNeg_unvac", "CumProp_falseNeg_vac",
                  "CumNum_misclass_unvac", "CumNum_misclass_vac", "CumProp_misclass_unvac", "CumProp_misclass_vac",
                  "VE_symptRR","VE_symptOR","VE_symptSuscOR","VE_symptRR_observed","VE_symptOR_observed",
                  "Label_veSusc","Label_veInfect","Label_veJoint","Label_testingDiff",
                  "t", "TestedStarted_bin")



#Three aggregate datasets for three different levels of testing
aggDF_equal<-data.frame(matrix(NA, nrow = time_steps+1, ncol =length(col_names.agg) ))
colnames(aggDF_equal)<-col_names.agg

aggDF_modUnequal<-aggDF_equal
aggDF_highUnequal<-aggDF_equal

######SET-UP THE IBM ############
#inds_hist<-NULL;  #track simulation history

#Set-up columns
col_names.ind<-c("Vac_status","HCE_status_equal", "HCE_status_modUnequal", "HCE_status_highUnequal","Covid_status", "Covid_newlyExposed", "Covid_everInfected", "Covid_everSymptInfected",
                 "Covid_exposed_timeLength", "Covid_timeLeft_exposed","Covid_infectious_timeLength",
                 "Covid_timeLeft_infectious", "Covid_symptomStatus",
                 "NonCovid_status", "NonCovid_newlyInfected", "NonCovid_everSymptInfected", 
                 "NonCovid_infected_timeLength", "NonCovid_timeLeft_infected",
                 "Covid_testedSymptExp_equal", "Covid_testedSymptExp_modUnequal", "Covid_testedSymptExp_highUnequal",
                 "Covid_newly_testedSymptExp_equal", "Covid_newly_testedSymptExp_modUnequal", "Covid_newly_testedSymptExp_highUnequal",
                 "Covid_newly_testedSymptInf_equal", "Covid_newly_testedSymptInf_modUnequal", "Covid_newly_testedSymptInf_highUnequal",
                 "Covid_testedSymptInf_equal", "Covid_testedSymptInf_modUnequal", "Covid_testedSymptInf_highUnequal",
                 "Covid_falseSymptPos_equal", "Covid_falseSymptPos_modUnequal", "Covid_falseSymptPos_highUnequal", 
                 "NonCovid_testedSymptInf_equal", "NonCovid_testedSymptInf_modUnequal", "NonCovid_testedSymptInf_highUnequal",
                 "NonCovid_newly_testedSymptInf_equal", "NonCovid_newly_testedSymptInf_modUnequal", "NonCovid_newly_testedSymptInf_highUnequal",
                 "Covid_falseNeg_equal", "Covid_falseNeg_modUnequal", "Covid_falseNeg_highUnequal",
                 "t")

#create inds matrix, ind_hist
inds<-array(data=0,dim=c(N,length(col_names.ind)))
rownames(inds)<-paste("ind",seq(1,N),sep="")
colnames(inds)<-col_names.ind


#PARAMETERS FOR IBM
avg.covid.inc.period=4
avg.covid.recovery.time=10
avg.noncovid.recovery.time=10
covid.prob.sympt.vac= (1-covid.prob.asympt) #(1 - 0.405) #what probability are symptomatic given vac #following MA et al. 0.35##Sah et al.
covid.prob.sympt.unvac= (1-covid.prob.asympt)#(1 - 0.405)#what probability are symptomatic given unvac
prop.infect.non.covid= noncovid.prop.infect #Prop.that have covid-like symptoms -> 0.152 
prop.vac=0.75 #proportion vaccinated against Covid-19 infection

#Number infected to start
initial.N.infectious<-10

#TESTING PARAMETERS

#needs to match vac.increase above
levels.tested<-c("equal","modUnequal", "highUnequal")

test.start=0

#low=1-(1-0.0105)^10 = 10%
#high=1-(1-0.206)^10

#TESTING:COVID AND VAC (symptomatic)
#prob.test.HCE_low<-10% (0.0105); 5% (0.0052); 15% (0.0165) 20% (0.022); 25% (0.0285)
#prob.test.HCE_high<-90% (0.206); 95% (0.26); 80% (0.15); 85% (0.175) 70% (0.114); 75% (0.13); 60% (0.088); 55% (0.077)

prob.test.HCE_low<-0.0165
prob.test.HCE_high<-0.175

covid.prob.test.sympt<-c(prob.test.HCE_low, prob.test.HCE_high)

non.covid.prob.test.sympt<-c(prob.test.HCE_low, prob.test.HCE_high)

#Assign HCE BY VACCINATION STATUS

#VACCINATED
prop.highHCE.testing.vac<-0.55
prop.lowHCE.testing.vac<-1 - prop.highHCE.testing.vac

prop.HCE.testing.vac<-c(prop.lowHCE.testing.vac, prop.highHCE.testing.vac) 


#UNVACCINATED
prop.highHCE.testing.unvac.equal<-prop.highHCE.testing.vac
prop.lowHCE.testing.unvac.equal<-1-prop.highHCE.testing.unvac.equal

prop.highHCE.testing.unvac.modUnequal<-prop.highHCE.testing.vac/2.5
prop.lowHCE.testing.unvac.modUnequal<-1 - prop.highHCE.testing.unvac.modUnequal

prop.highHCE.testing.unvac.highUnequal<-prop.highHCE.testing.vac/5
prop.lowHCE.testing.unvac.highUnequal<-1 - prop.highHCE.testing.unvac.highUnequal


prop.HCE.testing.unvac.equal<-c(prop.lowHCE.testing.unvac.equal,prop.highHCE.testing.unvac.equal)
prop.HCE.testing.unvac.modUnequal<-c(prop.lowHCE.testing.unvac.modUnequal,prop.highHCE.testing.unvac.modUnequal)
prop.HCE.testing.unvac.highUnequal<-c(prop.lowHCE.testing.unvac.highUnequal,prop.highHCE.testing.unvac.highUnequal)



#INITIAL SET-UP

#Determine number vaccinated and those immune while vaccinated
N.vac<-round(N*prop.vac)
N.unvac<-N - N.vac

N.vac.protected<-round(N.vac*ve.susc) #round so it is a whole number
#ensure that S_protected is less than or equal to vac
if(N.vac<N.vac.protected){N.vac.protected=N.vac}



#Number of Noncovid infected at the start
num.infect.non.covid<-round(N*prop.infect.non.covid)


ts<-0 

if (ts==0){
  
  #Assign vaccination status
  inds[1:N.vac,"Vac_status"]<-"vac"
  inds[(N.vac+1):N,"Vac_status"]<-"unvac"
  
  #Assign healthcare seeking behaviour status
  
  inds<-assign_HCE_Status(df=inds, level.diff=1, prop.HCE.testing.unvac=prop.HCE.testing.unvac.equal, prop.HCE.testing.vac=prop.HCE.testing.vac)
  inds<-assign_HCE_Status(df=inds, level.diff=2, prop.HCE.testing.unvac=prop.HCE.testing.unvac.modUnequal, prop.HCE.testing.vac=prop.HCE.testing.vac)
  inds<-assign_HCE_Status(df=inds, level.diff=3, prop.HCE.testing.unvac=prop.HCE.testing.unvac.highUnequal, prop.HCE.testing.vac=prop.HCE.testing.vac)
  
  #sum(inds[,"HCE_status_equal"]=="low" & inds[,"Vac_status"]=="unvac")
  #sum(inds[,"HCE_status_equal"]=="high" & inds[,"Vac_status"]=="unvac")
  
  #Assign Covid_status: Susceptible
  inds[,"Covid_status"]<-"S"
  
  #Assign Covid_status: S_protected
  inds[1:N.vac.protected,"Covid_status"]<-"S_protected"
  
  #Assign Covid_status: Infected (I)
  rows.initial.infect<- rownames(inds[inds[,"Covid_status"]=="S",]) #select rows that are vac and not immune from Covid-19
  
  
  infect<-sample(x=rows.initial.infect,size=initial.N.infectious, replace=F) #there will be an error if you try and infect too many individuals (i.e. I >Suscpt vac)
  
  inds[infect,"Covid_status"]<-"E"
  inds[infect,"Covid_newlyExposed"]<-1
  
  
  #sum(inds[,"Covid_status"]=="E" & inds[,"Vac_status"]=="vac")
  #sum(inds[,"Covid_status"]=="E" & inds[,"Vac_status"]=="unvac")
  
  
  #Create the Covid contact network
  contact_matrix<-covid_Create_Matrix(df=inds, no.contacts=6, N=N)
  
  #Make those individuals now infectious
  list.df<-covid_ExpToInf(df=inds, contact_matrix=contact_matrix, covid.prob.sympt.vac=covid.prob.sympt.vac, covid.prob.sympt.unvac=covid.prob.sympt.unvac, avg.covid.recovery.time=avg.covid.recovery.time)
  
  inds<-list.df[[1]]
  contact_matrix<-list.df[[2]]
  
  
  #INFECT x% with covid-like symptoms
  
  #Assign everyone to not be infected with Noncovid
  inds[,"NonCovid_status"]<-"N"
  
  #Use num.infect.non.covid instead of noncovid.replace for initial start
  inds<-nonCovid_Infection(df=inds, noncovid.replace=num.infect.non.covid, avg.noncovid.recovery.time=avg.noncovid.recovery.time)
  
  
  ##TESTING
  if (ts>=test.start){
    
    #Covid Testing
    
    #have three conditions for unvaccinated groups
    #[1]->equal, [2]->modUnequal, [3]->highUnequal
    inds<-covid_Testing(df=inds,level.diff=1,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
    inds<-covid_Testing(df=inds,level.diff=2,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
    inds<-covid_Testing(df=inds,level.diff=3,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt)

    
    
    
    #Noncovid Testing
    #have three conditions for vaccinated groups
    #[1]->equal, [2]->modUnequal, [3]->highUnequal
    inds<-nonCovid_Testing(df=inds,level.diff=1,
                           non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    inds<-nonCovid_Testing(df=inds,level.diff=2,
                           non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    inds<-nonCovid_Testing(df=inds,level.diff=3,
                           non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
    
  }
  
  #record the timestep
  inds[,"t"]<-ts #record time
  
  aggDF_equal<-aggdf_Create(agg.df=aggDF_equal, level=levels.tested[1], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts,test.start=test.start)
  aggDF_modUnequal<-aggdf_Create(agg.df=aggDF_modUnequal, level=levels.tested[2], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts,test.start=test.start)
  aggDF_highUnequal<-aggdf_Create(agg.df=aggDF_highUnequal, level=levels.tested[3], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts,test.start=test.start)
  
  
}





#SIMULATION SET-UP FOR T>1
#  inds_hist<-NULL;  #track simulation history
# inds_hist[[ts+1]]<-data.frame(inds) #add starting point to inds history


ts<-1

while(ts <= time_steps){
  
  print(ts)
  
  #reset all "new" variables as they are indicator variables that keep track of what happens at each time step 
  inds[,"Covid_newlyExposed"]=0 
  inds[,"NonCovid_newlyInfected"]=0
  
  #TESTING VARIABLES#####################
  inds[,paste("Covid_newly_testedSymptExp",levels.tested[1], sep="_")]=0
  inds[,paste("Covid_newly_testedSymptExp",levels.tested[2], sep="_")]=0
  inds[,paste("Covid_newly_testedSymptExp",levels.tested[3], sep="_")]=0
  
  inds[,paste("Covid_newly_testedSymptInf",levels.tested[1], sep="_")]=0
  inds[,paste("Covid_newly_testedSymptInf",levels.tested[2], sep="_")]=0
  inds[,paste("Covid_newly_testedSymptInf",levels.tested[3], sep="_")]=0
  
  
  inds[,paste("NonCovid_newly_testedSymptInf",levels.tested[1], sep="_")]=0
  inds[,paste("NonCovid_newly_testedSymptInf",levels.tested[2], sep="_")]=0
  inds[,paste("NonCovid_newly_testedSymptInf",levels.tested[3], sep="_")]=0
  
  #reduce everyone's time in infected, exposed, infectious categories as a day has passed
  covid.exp.index<-inds[,"Covid_timeLeft_exposed"]>0
  inds[covid.exp.index,"Covid_timeLeft_exposed"]= as.numeric(inds[covid.exp.index,"Covid_timeLeft_exposed"]) - 1
  
  covid.inf.index<-inds[,"Covid_timeLeft_infectious"]>0
  inds[covid.inf.index,"Covid_timeLeft_infectious"]= as.numeric(inds[covid.inf.index,"Covid_timeLeft_infectious"]) - 1
  
  noncovid.inf.index<-inds[,"NonCovid_timeLeft_infected"]>0
  inds[noncovid.inf.index,"NonCovid_timeLeft_infected"]= as.numeric(inds[noncovid.inf.index,"NonCovid_timeLeft_infected"]) - 1
  
  noncovid.replace<-sum(inds[,"NonCovid_status"]=="I" & inds[,"NonCovid_timeLeft_infected"]==0)
  
  #Move from exposed to infectious
  list.df<-covid_ExpToInf(df=inds, contact_matrix=contact_matrix, covid.prob.sympt.vac=covid.prob.sympt.vac, covid.prob.sympt.unvac=covid.prob.sympt.unvac, avg.covid.recovery.time=avg.covid.recovery.time)
  
  inds<-list.df[[1]]
  contact_matrix<-list.df[[2]]
  
  #Recover from Covid and Noncovid
  list.df<-recovery(df=inds, contact_matrix=contact_matrix)
  inds<-list.df[[1]]
  contact_matrix<-list.df[[2]]
  
  #Covid_Infection
  list.df<-covid_Infection_Network(df=inds,ve.infect=ve.infect,covid.prob.trans=covid.prob.trans, contact_matrix=contact_matrix, avg.covid.inc.period=avg.covid.inc.period)
  
  
  inds<-list.df[[1]]
  contact_matrix<-list.df[[2]]
  
  
  #NonCovid infection
  inds<-nonCovid_Infection(df=inds, noncovid.replace=noncovid.replace, avg.noncovid.recovery.time=avg.noncovid.recovery.time)
  
  if (ts>=test.start){
    #Covid Testing
    inds<-covid_Testing(df=inds,level.diff=1,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
    inds<-covid_Testing(df=inds,level.diff=2,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
    inds<-covid_Testing(df=inds,level.diff=3,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
    #Noncovid Testing
    inds<-nonCovid_Testing(df=inds,level.diff=1,
                          non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    inds<-nonCovid_Testing(df=inds,level.diff=2,
                           non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    inds<-nonCovid_Testing(df=inds,level.diff=3,
                           non.covid.prob.test.sympt=non.covid.prob.test.sympt)
    
  }
  
  inds[,"t"]<-ts #record time
  
  #inds_hist[[ts+1]]<-data.frame(inds) #add to history
  
  #AGGREGATED DF
  #########################################FIX HERE
  
  aggDF_equal<-aggdf_Create(agg.df=aggDF_equal, level=levels.tested[1], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts,test.start=test.start)
  aggDF_modUnequal<-aggdf_Create(agg.df=aggDF_modUnequal, level=levels.tested[2], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts,test.start=test.start)
  aggDF_highUnequal<-aggdf_Create(agg.df=aggDF_highUnequal, level=levels.tested[3], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts,test.start=test.start)
  
  
  ts<-ts + 1 #increase time step
  
  
}



#DETERMINE EPIDEMIC PEAK AND GREATEST TIME OF CHANGE
temp<-c(0,aggDF_equal$Covid_propInfect[1:length(aggDF_equal$Covid_propInfect)-1])
aggDF_equal$Covid_propInfect_lag<-temp
aggDF_equal$Covid_propInfect_change<-aggDF_equal$Covid_propInfect - temp

temp<-c(0,aggDF_equal$Covid_propInfect_change[1:length(aggDF_equal$Covid_propInfect_change)-1])

aggDF_equal$maxMin_change<-temp 

#time of greatest change
t_highChange<-aggDF_equal$t[aggDF_equal$maxMin_change==max(aggDF_equal$maxMin_change)]

if(length(t_highChange)>1){t_highChange=t_highChange[length(t_highChange)]}

#peak timepoint
t_peak<-aggDF_equal$t[aggDF_equal$Covid_propInfect==max(aggDF_equal$Covid_propInfect)]

if(length(t_peak)>1){t_peak=t_peak[length(t_peak)]}




#Underestimate at highest change in epidemic
aggDF_equal$VE_symptRR_degUnder_highChange_trueVeff<- ve.susc - aggDF_equal$VE_symptRR_observed[aggDF_equal$t==t_highChange]
aggDF_modUnequal$VE_symptRR_degUnder_highChange_trueVeff<- ve.susc - aggDF_modUnequal$VE_symptRR_observed[aggDF_modUnequal$t==t_highChange]
aggDF_highUnequal$VE_symptRR_degUnder_highChange_trueVeff<- ve.susc - aggDF_highUnequal$VE_symptRR_observed[aggDF_highUnequal$t==t_highChange]

aggDF_equal$VE_symptOR_degUnder_highChange_trueVeff<- ve.susc - aggDF_equal$VE_symptOR_observed[aggDF_equal$t==t_highChange]
aggDF_modUnequal$VE_symptOR_degUnder_highChange_trueVeff<- ve.susc - aggDF_modUnequal$VE_symptOR_observed[aggDF_modUnequal$t==t_highChange]
aggDF_highUnequal$VE_symptOR_degUnder_highChange_trueVeff<- ve.susc - aggDF_highUnequal$VE_symptOR_observed[aggDF_highUnequal$t==t_highChange]


aggDF_equal$VE_symptRR_degUnder_highChange_targEst<-aggDF_equal$VE_symptRR[aggDF_equal$t==t_highChange] - aggDF_equal$VE_symptRR_observed[aggDF_equal$t==t_highChange]
aggDF_modUnequal$VE_symptRR_degUnder_highChange_targEst<-aggDF_modUnequal$VE_symptRR[aggDF_modUnequal$t==t_highChange] - aggDF_modUnequal$VE_symptRR_observed[aggDF_modUnequal$t==t_highChange]
aggDF_highUnequal$VE_symptRR_degUnder_highChange_targEst<-aggDF_highUnequal$VE_symptRR[aggDF_highUnequal$t==t_highChange] - aggDF_highUnequal$VE_symptRR_observed[aggDF_highUnequal$t==t_highChange]

aggDF_equal$VE_symptOR_degUnder_highChange_targEst<-aggDF_equal$VE_symptOR[aggDF_equal$t==t_highChange] - aggDF_equal$VE_symptOR_observed[aggDF_equal$t==t_highChange]
aggDF_modUnequal$VE_symptOR_degUnder_highChange_targEst<-aggDF_modUnequal$VE_symptOR[aggDF_modUnequal$t==t_highChange] - aggDF_modUnequal$VE_symptOR_observed[aggDF_modUnequal$t==t_highChange]
aggDF_highUnequal$VE_symptOR_degUnder_highChange_targEst<-aggDF_highUnequal$VE_symptOR[aggDF_highUnequal$t==t_highChange] - aggDF_highUnequal$VE_symptOR_observed[aggDF_highUnequal$t==t_highChange]


##Underestimate at peak of epidemic

aggDF_equal$VE_symptRR_degUnder_peak_trueVeff<- ve.susc - aggDF_equal$VE_symptRR_observed[aggDF_equal$t==t_peak]
aggDF_modUnequal$VE_symptRR_degUnder_peak_trueVeff<- ve.susc - aggDF_modUnequal$VE_symptRR_observed[aggDF_modUnequal$t==t_peak]
aggDF_highUnequal$VE_symptRR_degUnder_peak_trueVeff<- ve.susc - aggDF_highUnequal$VE_symptRR_observed[aggDF_highUnequal$t==t_peak]

aggDF_equal$VE_symptOR_degUnder_peak_trueVeff<- ve.susc - aggDF_equal$VE_symptOR_observed[aggDF_equal$t==t_peak]
aggDF_modUnequal$VE_symptOR_degUnder_peak_trueVeff<- ve.susc - aggDF_modUnequal$VE_symptOR_observed[aggDF_modUnequal$t==t_peak]
aggDF_highUnequal$VE_symptOR_degUnder_peak_trueVeff<- ve.susc - aggDF_highUnequal$VE_symptOR_observed[aggDF_highUnequal$t==t_peak]


aggDF_equal$VE_symptRR_degUnder_peak_targEst<-aggDF_equal$VE_symptRR[aggDF_equal$t==t_peak] - aggDF_equal$VE_symptRR_observed[aggDF_equal$t==t_peak]
aggDF_modUnequal$VE_symptRR_degUnder_peak_targEst<-aggDF_modUnequal$VE_symptRR[aggDF_modUnequal$t==t_peak] - aggDF_modUnequal$VE_symptRR_observed[aggDF_modUnequal$t==t_peak]
aggDF_highUnequal$VE_symptRR_degUnder_peak_targEst<-aggDF_highUnequal$VE_symptRR[aggDF_highUnequal$t==t_peak] - aggDF_highUnequal$VE_symptRR_observed[aggDF_highUnequal$t==t_peak]

aggDF_equal$VE_symptOR_degUnder_peak_targEst<-aggDF_equal$VE_symptOR[aggDF_equal$t==t_peak] - aggDF_equal$VE_symptOR_observed[aggDF_equal$t==t_peak]
aggDF_modUnequal$VE_symptOR_degUnder_peak_targEst<-aggDF_modUnequal$VE_symptOR[aggDF_modUnequal$t==t_peak] - aggDF_modUnequal$VE_symptOR_observed[aggDF_modUnequal$t==t_peak]
aggDF_highUnequal$VE_symptOR_degUnder_peak_targEst<-aggDF_highUnequal$VE_symptOR[aggDF_highUnequal$t==t_peak] - aggDF_highUnequal$VE_symptOR_observed[aggDF_highUnequal$t==t_peak]



#FINAL CUMULATIVE PROP SYMPTOMATICALLY INFECTED
aggDF_equal$Covid_cumPropSymptInfect_highChange<-aggDF_equal$Covid_cumPropSymptInfect[aggDF_equal$t==t_highChange]
aggDF_modUnequal$Covid_cumPropSymptInfect_highChange<-aggDF_modUnequal$Covid_cumPropSymptInfect[aggDF_modUnequal$t==t_highChange]
aggDF_highUnequal$Covid_cumPropSymptInfect_highChange<-aggDF_highUnequal$Covid_cumPropSymptInfect[aggDF_highUnequal$t==t_highChange]

aggDF_equal$Covid_cumPropSymptInfect_peak<-aggDF_equal$Covid_cumPropSymptInfect[aggDF_equal$t==t_peak]
aggDF_modUnequal$Covid_cumPropSymptInfect_peak<-aggDF_modUnequal$Covid_cumPropSymptInfect[aggDF_modUnequal$t==t_peak]
aggDF_highUnequal$Covid_cumPropSymptInfect_peak<-aggDF_highUnequal$Covid_cumPropSymptInfect[aggDF_highUnequal$t==t_peak]

#Epidemic is over

aggDF_equal$Covid_cumPropSymptInfect_final<-aggDF_equal$Covid_cumPropSymptInfect[length(aggDF_equal$Covid_cumPropSymptInfect)]
aggDF_modUnequal$Covid_cumPropSymptInfect_final<-aggDF_modUnequal$Covid_cumPropSymptInfect[length(aggDF_modUnequal$Covid_cumPropSymptInfect)]
aggDF_highUnequal$Covid_cumPropSymptInfect_final<-aggDF_highUnequal$Covid_cumPropSymptInfect[length(aggDF_highUnequal$Covid_cumPropSymptInfect)]


#Drop these columns that have only been added to extra.col
extra.col<-c("Covid_propInfect_lag", "Covid_propInfect_change", "maxMin_change")

aggDF_equal<-aggDF_equal[,!(names(aggDF_equal) %in% extra.col)]


#COMBINE INTO ONE FINAL DATA FRAME
aggDF<-data.frame(rbind(aggDF_equal,aggDF_modUnequal,aggDF_highUnequal))


aggDF$t_highChange<-t_highChange
aggDF$t_peak<-t_peak


aggDF$VE_susc<- ve.susc
aggDF$VE_infect<- ve.infect
aggDF$Covid_probTrans<- covid.prob.trans
aggDF$Epi_simNum<-epi.real.num
aggDF$Seed_num<-seed.no
aggDF$Avg_numContacts<-mean(degree(contact_matrix)) 
aggDF$Label_testingDiff<-factor(aggDF$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))


#write the file
file.name<-paste("mainSim","_ve.S=",ve.susc,"_ve.I=",ve.infect,
"_prob.trans=",covid.prob.trans, "_epi.num=",epi.real.num, ".csv", sep="")

write.csv(aggDF,file.name)





