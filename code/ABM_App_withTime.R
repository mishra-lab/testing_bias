source("ABM_mainFunctions.R")
source("Aggdf_appFunctions_withTime.R")
source("IndTestdf_appFunctions.R")
require(igraph)
require(data.table)
require(survival)
igraph_options(return.vs.es = T)

#Seed_num is the dataframes for the seed numbers used to create our analysis

#Set-up for scinet
args <- commandArgs(trailingOnly = TRUE)

ve.susc<- as.numeric(args[1]) #VE against Susc 0.1
ve.infect<- as.numeric(args[2])  #VE against Infect 0.2
covid.prob.trans<- as.numeric(args[3])  #Probability of transmission per contact 0.1
epi.real.num<- as.numeric(args[4]) #simulationnumber

if(ve.susc == 0.55){
  seed.list<-c(627738822, 303826513, 966715654, 561197796, 9604551, 181428901, 481114856,
               356569673, 849370114, 494543171,   2112960, 348437993, 420820680, 726061388,
               781331243,  81227118, 658943916, 912084367, 197643976, 278443859, 238262229,
               158644775, 930977921, 272048473,331586709, 387679190, 951741029, 700006413,
               772549058, 632623490, 659195881,  46734848, 875055326,  56373452, 706347635,
               287707943, 67674544, 402842400, 220360243, 467185751, 975019078, 887373760,
               812724675, 391816323, 249586021,  49321139, 138122651, 184921548,498531964,
               781118736, 853323861, 445527550, 725670207, 445796455, 821164067, 357680892,
               103106203, 321461301, 301948178, 807021520, 212090423, 485610524, 311073094,
               697609893, 975586155, 935251834,  33043238,  58814372, 996071199, 466746503,
               168271540, 688091939, 24080037, 833117643, 567650992, 363361905, 890697241,
               108215980, 438186071, 560738674,  27956358, 746091831, 908536344, 437714814,
               592174960, 458776968, 752433480, 443948528,  78499032, 352777205, 367301126,
               911719760, 690974803, 364522024, 564783386, 958857900, 782009083, 116954427,
               99155383, 409497404)
}else if(ve.susc == 0.1){
  seed.list<-c(355862870, 962640839, 504830012, 289049712, 714417464, 778607570, 744970604,
              557024959, 248751140, 182265034, 510004107, 935563784, 905327509, 11028972,
              741033882, 517188585, 394113916, 760908032, 646160191, 844493827, 749276828,
               811791521, 836826831, 250263991, 90743830,  19295851, 619246965,  63578884,
                80916483, 831032612, 909425150, 169075383, 493723306, 739875495, 790086893,
                910651200, 693917808, 744124184, 292974629, 935245960, 158271535, 360350191,
                834952009, 280618213, 473307843, 781854991, 705962817,  18930118, 579624900,
                359981920, 378936077, 784376525, 828686433, 898811654, 350642970,  29293804,
                809648025, 532670592, 836869027, 489210586, 155887241, 347109002, 727787463,
                135256556, 637425774, 351354302, 761618947, 629600826, 133957979, 222547264,
                537251001, 601072222, 58659371, 446564549,  93127193, 445026065, 638456362,
                258485668, 684535364, 501297735, 954226885, 631037577, 295138646, 529290732,
                458163998, 685910807, 303738578, 754810212, 209209410, 570201585, 481162341,
                616735861, 817879352,  45713240, 860554130, 346642169,852299717, 386881142,
                31374259, 912133022)
}

seed.no<-seed.list[epi.real.num]

set.seed(seed.no)

covid.prob.asympt<-0.35 #as.numeric(args[4])
noncovid.prop.infect<-0.152 #as.numeric(args[5])

#start of the main analysis
time_steps<- 100 #length of observation period
N=100000 #number of individuals per sample


#CREATE VARIABLE NAMES FOR AGGREGATE DATAFRAME

#VE_symptOR
#Covid_cumSymptInfect_vac
#NonCovid_cumSymptInfect_vac
#Covid_cumSymptInfect_unvac
#NonCovid_cumSymptInfect_unvac

####TO CALCULATE OBSERVED VE
#VE_symptOR_observed
#Covid_cumSymptExpInfect_tested_vac
#NonCovid_cumSymptInfect_tested_vac
#Covid_cumSymptExpInfect_tested_unvac
#NonCovid_cumSymptInfect_tested_unvac

#Define basic Covid and Covid-like etiologies variables and cumulative VE RR and OR variables


###ADD prop infect unvac and vac

cols_base.agg<-c("Covid_symptInfect_unvac","Covid_symptInfect_vac","Covid_symptInfect",
                 "Covid_propSymptInfect_unvac","Covid_propSymptInfect_vac","Covid_propSymptInfect",
                
                "Covid_cumSymptInfect_unvac","Covid_cumSymptInfect_vac", 
                "Covid_cumPropSymptInfect_unvac", "Covid_cumPropSymptInfect_vac",
                  
                "NonCovid_cumSymptInfect_unvac", "NonCovid_cumSymptInfect_vac",
                "NonCovid_cumPropSymptInfect_unvac", "NonCovid_cumPropSymptInfect_vac",
                  
                "Covid_cumSymptExpInfect_tested_unvac", "Covid_cumSymptExpInfect_tested_vac",
                "Covid_cumPropSymptExpInfect_tested_unvac", "Covid_cumPropSymptExpInfect_tested_vac", 
                          
                "NonCovid_cumSymptInfect_tested_unvac", "NonCovid_cumSymptInfect_tested_vac",
                "NonCovid_cumPropSymptInfect_tested_unvac", "NonCovid_cumPropSymptInfect_tested_vac",
              
                "VE_symptRR","VE_symptOR","VE_symptRR_observed","VE_symptOR_observed",
                "Label_veSusc","Label_veInfect","Label_veJoint","Label_testingDiff",
                "t")

#Define Sampling Variables (to double check with cumulative measurements above)
cols_sample.vars.agg <- with(expand.grid(vac = c("unvac", "vac"),
                                      covid = c("Covid", "NonCovid"),
                                      stringsAsFactors = FALSE),
                          paste0("Stats_sampleSize", covid, "_", vac))                  


# Define stats vars in the desired order for symptomatic VE RR (CoxPh)
cols_time.dep.stats.veRR.agg<-c("VacStatus_coef_timeDep_VeRR",
"VacStatus_se_timeDep_VeRR", "VacStatus_pValue_timeDep_VeRR")

                          
# Define stats vars in the desired order for time ind and dep. for symptomatic VE OR
stat.vars.names.ve.or<- c("coef", "se", "pValue")
var.groups.ve.or <- list(timeDep_VeOR  = c("Intercept", "VacStatus", "TimeStep"))
                          
# Generate the stats variable names using lapply over the names of the list:
cols_time.dep.stats.veOR.agg <- unlist(lapply(names(var.groups.ve.or), function(sfx) {
                      paste(rep(var.groups.ve.or[[sfx]], each = length(stat.vars.names.ve.or)), stat.vars.names.ve.or, sfx, sep = "_")
                      }), use.names = FALSE)
                        


#Create the vector with all of the variable names
all.agg.vars<-c(cols_base.agg, cols_sample.vars.agg, cols_time.dep.stats.veRR.agg,
                cols_time.dep.stats.veOR.agg)


#Create three aggregate datasets for three different levels of testing and assign them variable names
aggDF_equal<-data.frame(matrix(NA, nrow = time_steps+1, ncol =length(all.agg.vars) ))
colnames(aggDF_equal)<-all.agg.vars

aggDF_modUnequal<-aggDF_equal
aggDF_highUnequal<-aggDF_equal

#Individual Testing Dataset
col_names.indTest<-c("Ind_id","Vac_status", "CovidTested_positive", "Time_step", "Week")


#Three individual-level testing datasets for three different levels of testing
indTestDF_equal<-data.frame(matrix(NA, nrow = 0, ncol =length(col_names.indTest) ))
colnames(indTestDF_equal)<-col_names.indTest

indTestDF_modUnequal<-indTestDF_equal
indTestDF_highUnequal<-indTestDF_equal


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
  
  #Create inds.identity dataset
  df.inds.identity <- data.frame(
    Ind_id = rownames(inds),
    Vac_status = inds[,"Vac_status"]
  )
  
  
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
    
    

  
  #record the timestep
  inds[,"t"]<-ts #record time
  
  #USE INDTESTdf function here ############################
  indTestDF_equal<-Indsdf_Create(indTest.df=indTestDF_equal, df=inds, level=levels.tested[1])
  indTestDF_modUnequal<-Indsdf_Create(indTest.df=indTestDF_modUnequal,df=inds, level=levels.tested[2])
  indTestDF_highUnequal<-Indsdf_Create(indTest.df=indTestDF_highUnequal,df=inds, level=levels.tested[3])
  
  #CREATE AGGREGATE INFORMATION ACROSS TESTING SCENARIOS
  aggDF_equal<-aggdf_Create(agg.df=aggDF_equal, level=levels.tested[1], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts)
  
  aggDF_modUnequal<-aggdf_Create(agg.df=aggDF_modUnequal, level=levels.tested[2], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts)
  
  aggDF_highUnequal<-aggdf_Create(agg.df=aggDF_highUnequal, level=levels.tested[3], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts)
  
  
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
    
  
  
  inds[,"t"]<-ts #record time
  
  #inds_hist[[ts+1]]<-data.frame(inds) #add to history
  
  #AGGREGATED DF
  ########################################
  #Create the individual level data needed to do the testing
  indTestDF_equal<-Indsdf_Create(indTest.df=indTestDF_equal, df=inds, level=levels.tested[1])
  #print("equal")
  #print(nrow(indTestDF_equal))
  indTestDF_modUnequal<-Indsdf_Create(indTest.df=indTestDF_modUnequal,df=inds, level=levels.tested[2])
  #print("modunequal")
  #print(nrow(indTestDF_modUnequal))
  indTestDF_highUnequal<-Indsdf_Create(indTest.df=indTestDF_highUnequal,df=inds, level=levels.tested[3])
  # print("highunequal")
  # print(indTestDF_highUnequal$Ind_id[indTestDF_highUnequal$Vac_status=="unvac" & indTestDF_highUnequal$CovidTested_positive==1])
  # 
  #Create the aggregate level dataframe
  aggDF_equal<-aggdf_Create(agg.df=aggDF_equal, level=levels.tested[1], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts)
  
  aggDF_equal<-aggdf_Stats(agg.df=aggDF_equal, level=levels.tested[1], ve.susc=ve.susc, inds.test.df=indTestDF_equal,
                           df.inds.identity=df.inds.identity, N=N, N.vac=N.vac, N.unvac=N.unvac, 
                           ts=ts,test.start=20, epi.real.num=epi.real.num)
  
  
  aggDF_modUnequal<-aggdf_Create(agg.df=aggDF_modUnequal, level=levels.tested[2], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts)
  
  aggDF_modUnequal<-aggdf_Stats(agg.df=aggDF_modUnequal, level=levels.tested[2], ve.susc=ve.susc, inds.test.df=indTestDF_modUnequal,
                                df.inds.identity=df.inds.identity,N=N, N.vac=N.vac, N.unvac=N.unvac,
                                ts=ts,test.start=20, epi.real.num=epi.real.num)
  
  aggDF_highUnequal<-aggdf_Create(agg.df=aggDF_highUnequal, level=levels.tested[3], df=inds, ve.susc=ve.susc, ve.infect=ve.infect,
                               N=N, N.vac=N.vac, N.unvac=N.unvac,t=ts)
  
  aggDF_highUnequal<-aggdf_Stats(agg.df=aggDF_highUnequal, level=levels.tested[3], ve.susc=ve.susc, inds.test.df=indTestDF_highUnequal,
                                 df.inds.identity=df.inds.identity, N=N, N.vac=N.vac, N.unvac=N.unvac,
                                 ts=ts,test.start=20, epi.real.num=epi.real.num)

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

#Combine IndTestDF into a final single dataframe
indTestDF_equal$Label_testingDiff<-"equal"
indTestDF_modUnequal$Label_testingDiff<-"modUnequal"
indTestDF_highUnequal$Label_testingDiff<-"highUnequal"

#COMBINE INTO ONE FINAL DATA FRAME
indTestDF<-data.frame(rbind(indTestDF_equal,indTestDF_modUnequal,indTestDF_highUnequal))

indTestDF$t_highChange<-t_highChange
indTestDF$t_peak<-t_peak

indTestDF$VE_susc<- ve.susc
indTestDF$VE_infect<- ve.infect
indTestDF$Covid_probTrans<- covid.prob.trans
indTestDF$Epi_simNum<-epi.real.num
indTestDF$Seed_num<-seed.no
indTestDF$Avg_numContacts<-mean(degree(contact_matrix)) 
indTestDF$Label_testingDiff<-factor(indTestDF$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))


#write the agg and indTest files
file.name.agg<-paste("mainSim","_ve.S=",ve.susc,"_ve.I=",ve.infect,
"_prob.trans=",covid.prob.trans, "_epi.num=",epi.real.num, ".csv", sep="")


write.csv(aggDF,file.name.agg)

file.name.ind<-paste("indTest_mainSim","_ve.S=",ve.susc,"_ve.I=",ve.infect,
                     "_prob.trans=",covid.prob.trans, "_epi.num=",epi.real.num, ".csv", sep="")

data.table::fwrite(indTestDF,file.name.ind)



