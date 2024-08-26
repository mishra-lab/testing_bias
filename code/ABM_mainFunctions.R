#Main Functions that will be used in IBM.R
#########################################################################################
require(igraph)
require(graph4lg)
require(data.table)

#DESCRIPTION: Generates a contact matrix that sets who is connected 
#with whom (for SARS-CoV-2 transmission) in the population.

covid_Create_Matrix<-function(df=inds,no.contacts=6, N=N){
  
  #create the contact matrix between all individuals (vac and unvac)
  
  contact_matrix=sample_gnm(n=N, m=N*no.contacts / 2, directed = FALSE, loops = FALSE)

  components <-clusters(contact_matrix, mode="weak")
  biggest_cluster_id <- which.max(components$csize) #returns index of largest component which is the membership
  

  list.remaining.cluster.ids<-unique(components$membership[components$membership!= biggest_cluster_id])
  
  if(length(list.remaining.cluster.ids)>0){
  # Ids of nodes
  mainConnected.node.ids <- V(contact_matrix)[components$membership == biggest_cluster_id]
  num.main.connected<-length(mainConnected.node.ids)
  
  new_edge_list=c()
  
  for (i in 1:length(list.remaining.cluster.ids)){
    
    #connect disconnected nodes back to the main network
    unconnected.node.ids <- V(contact_matrix)[components$membership==list.remaining.cluster.ids[i]] #identify nodes in that membership
    
    num.members.unconnected<-length(unconnected.node.ids)
    
    unconnected.nodeID.sample.index<-sample(num.members.unconnected,size=1)
    
    unconnected.nodeId.sample.name<-unconnected.node.ids[unconnected.nodeID.sample.index]
    
    main.nodeId.sample.index<-sample(num.main.connected,size=1)
    main.nodeId.sample.name<-mainConnected.node.ids[main.nodeId.sample.index]
    
    unconnected.nodeId.sample.name
    main.nodeId.sample.name
    
    new_edge_list<-c(new_edge_list,unconnected.nodeId.sample.name, main.nodeId.sample.name)
    new_edge_list
  }
  
  contact_matrix<-add_edges(contact_matrix, new_edge_list)
  
  }
  
  #set the attributes in the network
  contact_matrix<-set_vertex_attr(contact_matrix,name = "Covid_status", value = df[,"Covid_status"])
  contact_matrix<-set_vertex_attr(contact_matrix,name = "Vac_status", value = df[,"Vac_status"])
  contact_matrix<-set_vertex_attr(contact_matrix,name = "degree", value = degree(contact_matrix))
  contact_matrix<-set_vertex_attr(contact_matrix,name = "name", value = seq(1,N,1))
  
  
  return(contact_matrix)
  
}


#DESCRIPTION: Assigns level of healthcare engagement to individuals 
#(“low” for low healthcare engagement; “high” for high healthcare engagement) 
# based on vaccination status and healthcare engagement scenario (equal, modUnequal, highUnequal)
#that is set by level.diff

assign_HCE_Status<-function(df=inds, level.diff=NA, prop.HCE.testing.unvac=NA, prop.HCE.testing.vac=NA){
  
  if(level.diff==1){
    
    all.unvac<-rownames(df[df[,"Vac_status"]=="unvac",])
    
    num.unvac<-length(all.unvac)
    low.tested.unvac.id<-sample(all.unvac,size=round(num.unvac*prop.HCE.testing.unvac[1]),replace=F)
    high.tested.unvac.id<-all.unvac[!(all.unvac %in% low.tested.unvac.id)]
   
    df[low.tested.unvac.id,"HCE_status_equal"]<-"low"
    df[high.tested.unvac.id,"HCE_status_equal"]<-"high"
    
    #vac
    all.vac<-rownames(df[df[,"Vac_status"]=="vac",])
    
    num.vac<-length(all.vac)
    low.tested.vac.id<-sample(all.vac,size=round(num.vac*prop.HCE.testing.vac[1]),replace=F)
    high.tested.vac.id<-all.vac[!(all.vac %in% low.tested.vac.id)]
  
    #Set equal, modUnequal and highUnequal to all have the same vaccinated HCE behaviour
    df[low.tested.vac.id,"HCE_status_equal"]<-"low"
    df[high.tested.vac.id,"HCE_status_equal"]<-"high"
    
    df[low.tested.vac.id,"HCE_status_modUnequal"]<-"low"
    df[high.tested.vac.id,"HCE_status_modUnequal"]<-"high"
    
    df[low.tested.vac.id,"HCE_status_highUnequal"]<-"low"
    df[high.tested.vac.id,"HCE_status_highUnequal"]<-"high"
    
  
  }
  else if (level.diff==2 | level.diff==3){
    if(level.diff==2){HCE.col.name<-"HCE_status_modUnequal"}else{HCE.col.name<-"HCE_status_highUnequal"}
    
    
    all.unvac<-rownames(df[df[,"Vac_status"]=="unvac",])
    
    num.unvac<-length(all.unvac)
    
    low.tested.unvac.id<-sample(all.unvac,size=round(num.unvac*prop.HCE.testing.unvac[1]),replace=F)
    #length(low.tested.unvac.id) #check
    high.tested.unvac.id<-all.unvac[!(all.unvac %in% low.tested.unvac.id)]
    
    
    #length(high.tested.unvac.id) #check
    
    df[low.tested.unvac.id,HCE.col.name]<-"low"
    df[high.tested.unvac.id,HCE.col.name]<-"high"
    
    
  }
  
  

  return(df)
}



#DESCRIPTION: Infects individuals with SARS-CoV-2, sets their
#SARS-CoV-2 status to exposed, and assigns the amount of time that will
#be spent in the exposed state.

covid_Infection_Network<-function(df=inds,ve.infect=ve.infect,covid.prob.trans=covid.prob.trans, contact_matrix=contact_matrix, avg.covid.inc.period=avg.covid.inc.period){
  
  #V(contact_matrix)[[]]
  saved.matrix<-contact_matrix
  
  
  long.df<-as_long_data_frame(contact_matrix) %>% setDT()
  long.df.rev<-data.frame(cbind(long.df$to,long.df$from,long.df$to_Covid_status, long.df$to_Vac_status,long.df$to_degree,long.df$to_name,
                                long.df$from_Covid_status, long.df$from_Vac_status, long.df$from_degree, long.df$from_name))
  
  names(long.df.rev)<-c("from", "to", "from_Covid_status", "from_Vac_status","from_degree", "from_name",
                        "to_Covid_status", "to_Vac_status", "to_degree", "to_name")
  
  df.combined<-rbind(long.df, long.df.rev) 
  
  # Calculate the sum of neighbors' attributes by origin (from). This is really fast in data.table
  df.infected.by.vac <- df.combined[, .(contact_by_inf.vac = sum(to_Covid_status=="I" & to_Vac_status=="vac")), by= from]
  df.infected.by.unvac  <- df.combined[, .(contact_by_inf.unvac = sum(to_Covid_status=="I" & to_Vac_status=="unvac")), by= from]
  
  #add attributes to contact matrix
  contact_matrix<-add_nodes_attr(contact_matrix, input="df",data=df.infected.by.vac,index="from", include="contact_by_inf.vac")
  contact_matrix<-add_nodes_attr(contact_matrix, input="df",data=df.infected.by.unvac,index="from", include="contact_by_inf.unvac")
  
  #number of individuals in the population who are susceptible
  num_S=length(V(contact_matrix)[Covid_status=="S"]$contact_by_inf.unvac)
  
  S_names<-V(contact_matrix)[Covid_status=="S"]$name
  
  num.unvac.inf.contacts<-V(contact_matrix)[Covid_status=="S"]$contact_by_inf.unvac
  num.vac.inf.contacts<-V(contact_matrix)[Covid_status=="S"]$contact_by_inf.vac
  
  infected_unvac<-rbinom(n=num_S, size=num.unvac.inf.contacts,prob=covid.prob.trans) 
  infected_vac<-rbinom(n=num_S,size=num.vac.inf.contacts,prob=covid.prob.trans*(1-ve.infect)) 
  
  infected<-infected_unvac + infected_vac
  
  #Assign who becomes infected in both dataset and network
  temp<-data.frame(cbind(S_names,infected))
  
  index_new_infected<-temp$S_names[temp$infected>0]
  
  #change those who were infected into E
  contact_matrix<-set_vertex_attr(contact_matrix,name = "Covid_status", index=V(contact_matrix)[index_new_infected], value = "E")
  
  #delete the contact attributes
  contact_matrix<-delete_vertex_attr(contact_matrix, "contact_by_inf.vac")
  contact_matrix<-delete_vertex_attr(contact_matrix, "contact_by_inf.unvac")
  
  df[index_new_infected,"Covid_newlyExposed"]<-1
  
  num.covid.newly.exp<-length(index_new_infected)
  
  #Set covid-status variable
  df[index_new_infected,"Covid_status"]<-"E"
  
  #Set indicator variables
  
  df[index_new_infected,"Covid_everInfected"]<-1
  
  #Assign exposure time
  #covid.inc.boundary<-abs(avg.covid.inc.period/2)
  
  #exposed.time<-round(avg.covid.inc.period +sample(-covid.inc.boundary:covid.inc.boundary,num.covid.newly.exp,replace=T)) #gives equal distribution around 10
  
  exposed.time<-rpois(n=num.covid.newly.exp, lambda = avg.covid.inc.period)
  
  df[index_new_infected,"Covid_exposed_timeLength"]<-exposed.time
  df[index_new_infected,"Covid_timeLeft_exposed"]<-exposed.time
  
  list.df<-list(df,contact_matrix)
  
  return(list.df)
} 




#DESCRIPTION: Moves individuals to the SARS-CoV-2 infectious class, assigns 
#symptom status, and assigns amount of time to be spent as infectious. 

covid_ExpToInf<-function(df=inds, contact_matrix=contact_matrix, covid.prob.sympt.vac=covid.prob.sympt.vac, covid.prob.sympt.unvac=covid.prob.sympt.unvac, avg.covid.recovery.time=avg.covid.recovery.time){ 
  
  #subset by exposed (with zero time left) and exposed (with zero time left) by vaccination status
  covid.exp.index<-rownames(df[df[,"Covid_status"]=="E" & df[,"Covid_timeLeft_exposed"]==0,]) 
  covid.exp.index.network<-as.numeric(which(df[,"Covid_status"]=="E" & df[,"Covid_timeLeft_exposed"]==0))
  covid.exp.vac.index<-rownames(df[df[,"Covid_status"]=="E" & df[,"Covid_timeLeft_exposed"]==0 & df[,"Vac_status"]=="vac",]) 
  covid.exp.unvac.index<-rownames(df[df[,"Covid_status"]=="E" & df[,"Covid_timeLeft_exposed"]==0 & df[,"Vac_status"]=="unvac",]) 
  
  #number who will be newly infected altogether and by vac status
  num.covid.newly.infected<-length(covid.exp.index)
  num.covid.newly.infected.vac<-length(covid.exp.vac.index)
  num.covid.newly.infected.unvac<-length(covid.exp.unvac.index)
  
  
  #Assign infectious status
  df[covid.exp.index,"Covid_status"]<-"I"
  
  
  #Assign infectious status to network
  contact_matrix<-set_vertex_attr(contact_matrix,name = "Covid_status", index=V(contact_matrix)[covid.exp.index.network], value = "I")
  
  
  
  #Assign symptom status by vaccination status
  
  vac.sympt.temp<-rbinom(n=num.covid.newly.infected.vac,size=1,p=covid.prob.sympt.vac)
  
  df[covid.exp.vac.index,"Covid_symptomStatus"]<-ifelse(vac.sympt.temp==1,"symptomatic","asymptomatic")
  
  df[covid.exp.vac.index,"Covid_everSymptInfected"]<-ifelse(df[covid.exp.vac.index,"Covid_symptomStatus"]=="symptomatic",1,df[covid.exp.vac.index,"Covid_everSymptInfected"]) #this should put back the original value that doesn't meet the condition
  
  unvac.sympt.temp<-rbinom(n=num.covid.newly.infected.unvac,size=1,p=covid.prob.sympt.unvac)
  df[covid.exp.unvac.index,"Covid_symptomStatus"]<-ifelse(unvac.sympt.temp==1,"symptomatic","asymptomatic")
  
  df[covid.exp.unvac.index,"Covid_everSymptInfected"]<-ifelse(df[covid.exp.unvac.index,"Covid_symptomStatus"]=="symptomatic",1,df[covid.exp.unvac.index,"Covid_everSymptInfected"])
  
  
  #Assign recovery time
  #covid.recovery.boundary<-abs(avg.covid.recovery.time/2)
  
  #recovery.time<-avg.covid.recovery.time +sample(-covid.recovery.boundary:covid.recovery.boundary,num.covid.newly.infected,replace=T) #gives equal distribution around 10
  
  recovery.time<-rpois(n=num.covid.newly.infected, lambda = avg.covid.recovery.time)
  
  df[covid.exp.index,"Covid_infectious_timeLength"]<-recovery.time
  df[covid.exp.index,"Covid_timeLeft_infectious"]<-recovery.time
  
  list.df<-list(df,contact_matrix)
  return(list.df)
  
}



#DESCRIPTION: Moves individuals infected with SARS-CoV-2 to recovered and those 
#with symptoms due to other etiologies to uninfected.

recovery<-function(df=inds, contact_matrix=contact_matrix){
  
  
  covid.index<-df[,"Covid_status"]== "I" & df[,"Covid_timeLeft_infectious"]== 0
  covid.index.network<-as.numeric(which(df[,"Covid_status"]== "I" & df[,"Covid_timeLeft_infectious"]== 0))
  noncovid.index<-df[,"NonCovid_status"]== "I" & df[,"NonCovid_timeLeft_infected"]== 0
  
  covid.testing.index<-df[,"Covid_status"]== "I" & df[,"Covid_timeLeft_infectious"]== 0 & (df[,"NonCovid_status"]!="I" | df[,"NonCovid_status"]== "I" & df[,"NonCovid_timeLeft_infected"]== 0)
  noncovid.testing.index<-df[,"NonCovid_status"]== "I" & df[,"NonCovid_timeLeft_infected"]== 0 & (df[,"Covid_symptomStatus"]!="symptomatic" | df[,"Covid_symptomStatus"]== "symptomatic" & df[,"Covid_timeLeft_infectious"]== 0)
  
  if(sum(covid.index)>0){
    
    df[covid.index, "Covid_symptomStatus"] = 0
    df[covid.index, "Covid_status"] = "R" #sets to recovered from covid
    
    #Assign recovered status to network
    contact_matrix<-set_vertex_attr(contact_matrix,name = "Covid_status", index=V(contact_matrix)[covid.index.network], value = "R")
    
  }
  
  if(sum(noncovid.index)>0){
    
    
    df[noncovid.index, "NonCovid_status"] = "N" #resets to not infected with Noncovid
  }
  
  list.df<-list(df, contact_matrix)
  return(list.df)
  
} 



#DESCRIPTION: Recruits individuals to be symptomatic due to other etiologies, 
#sets their status to be infected and assigns the amount of time until 
#individual becomes non-symptomatic again.


nonCovid_Infection<-function(df=NA, noncovid.replace=NA, avg.noncovid.recovery.time=avg.noncovid.recovery.time){
  
  #sample those to be newly infected with Noncovid 
  
  noncovid.uninfect.index<-df[,"NonCovid_status"]=="N" #indicator for which rows are not infected with Noncovid
  
  noncovid.new.infect.index<-sample(x=rownames(df[noncovid.uninfect.index,]),size=noncovid.replace, replace=F)
  
  #set to infected
  df[noncovid.new.infect.index,"NonCovid_status"]<-"I"
  df[noncovid.new.infect.index,"NonCovid_newlyInfected"]<-1
  df[noncovid.new.infect.index,"NonCovid_everSymptInfected"]<-as.numeric(df[noncovid.new.infect.index,"NonCovid_everSymptInfected"])+1 
  
  #set recovery time
  #recovery.boundary<-abs(avg.noncovid.recovery.time/2) #set recovery time boundary for uniform distribution below
  #noncovid.recovery.time<-avg.noncovid.recovery.time +sample(-recovery.boundary:recovery.boundary,noncovid.replace,replace=T) #gives equal distribution around 10
  
  noncovid.recovery.time<-rpois(n=noncovid.replace, lambda = avg.noncovid.recovery.time)
  
  df[noncovid.new.infect.index,"NonCovid_infected_timeLength"]<-noncovid.recovery.time
  df[noncovid.new.infect.index,"NonCovid_timeLeft_infected"]<-noncovid.recovery.time
  
  return(df)
  
}


#TESTING FUNCTIONS

#########################################################################################


#DESCRIPTION: Sets which symptomatic individuals infected with SARS-CoV-2 tested
#for SARS-CoV-2 (and sets them testing positive); 
#also tracks whether individuals receive a false positive (when asymptomatic 
#with SARS-CoV-2 but symptomatic due to other etiologies)

covid_Testing<-function(df=inds, level.diff=NA,
                        covid.prob.test.sympt=covid.prob.test.sympt,
                        non.covid.prob.test.sympt=non.covid.prob.test.sympt){
  
  #ALTHOUGH VARIABLES BELOW ARE CALLED UNTESTED, INDIVIDUALS COULD HAVE BEEN TESTED BEFORE - JUST UNTESTED THIS TIMESTEP
  #NOTE THAT levels.tested refers to the difference in healthcare engagement levels and testing and not the combined difference in testing
  #and cumulative prob of testing
  
  if(level.diff==1){
    levels.tested<-"equal"
    
    }else if (level.diff==2){
    levels.tested<-"modUnequal"
    
    } else if(level.diff==3){
      levels.tested<-"highUnequal"
      
    } else{
      print("the value for level.diff is out of range")
        
      }
  
  #1. EXPOSED 
  #Exposed but symptomatic due to a Noncovid cause by HCE
  
  #index for HCE low, medium and high
  covid.untested.exp.HCELow.nonCI.index<-df[,"Covid_status"]=="E" & df[,"NonCovid_status"]=="I" &  df[,paste("HCE_status",levels.tested,sep="_")]=="low"   #low HCE + Noncovid infected
  covid.untested.exp.HCEHigh.nonCI.index<-df[,"Covid_status"]=="E" & df[,"NonCovid_status"]=="I" & df[,paste("HCE_status",levels.tested,sep="_")]=="high"  #high HCE + Noncovid infected
  
  #index for vaccinated to update 2.5x and 5x lower with equal
  covid.untested.exp.vac.nonCI.index<-df[,"Covid_status"]=="E" & df[,"NonCovid_status"]=="I" & df[,"Vac_status"]=="vac"
  
  
  #number of HCE low, med and high
  num.untested.covid.exp.HCELow.nonCI<-sum(covid.untested.exp.HCELow.nonCI.index) #number of  Low HCE + Noncovid infected
  num.untested.covid.exp.HCEHigh.nonCI<-sum(covid.untested.exp.HCEHigh.nonCI.index) #number of High HCE + Noncovid infected
  
  
  #Testing Assignment for Exposed & Symptomatic 
  #
  
  df[covid.untested.exp.HCELow.nonCI.index,paste("Covid_newly_testedSymptExp",levels.tested,sep="_")]<-rbinom(n=num.untested.covid.exp.HCELow.nonCI,size=1,prob=non.covid.prob.test.sympt[1]) #Noncovid infected
  df[covid.untested.exp.HCEHigh.nonCI.index,paste("Covid_newly_testedSymptExp",levels.tested,sep="_")]<-rbinom(n=num.untested.covid.exp.HCEHigh.nonCI,size=1,prob=non.covid.prob.test.sympt[2]) #Noncovid infected
  
  #IF VACCINATED WE WANT TO USE THE SAME INDIVIDUALS WHO TESTED POSITIVE AS LEVEL 1 SO REPLACE VAC WITH VALUES FROM equal
  if(level.diff==2 | level.diff==3){
    
   df[covid.untested.exp.vac.nonCI.index,paste("Covid_newly_testedSymptExp",levels.tested,sep="_")]<-df[covid.untested.exp.vac.nonCI.index,paste("Covid_newly_testedSymptExp","equal",sep="_")]
    
  }
  
  covid.tested.exp.index<-df[,paste("Covid_newly_testedSymptExp",levels.tested,sep="_")]==1

  #Number of total tests while exposed and symptomatic
  df[covid.tested.exp.index,paste("Covid_testedSymptExp",levels.tested,sep="_")]<- as.numeric(df[covid.tested.exp.index,paste("Covid_testedSymptExp",levels.tested,sep="_")]) + 1
  
  #Anyone who tested while exposed and symptomatic is a False Positive
  df[covid.tested.exp.index,paste("Covid_falseSymptPos",levels.tested,sep="_")]<- 1
 
  
  #2.	INFECTED
  
  #Unvac
 
  #Covid-19 Infected and Symptomatic (untested this time step)
  covid.untested.sympt.inf.HCELow.index<-df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]== "symptomatic" & df[,paste("HCE_status",levels.tested,sep="_")]=="low"  #low HCE + Covid Infected
  covid.untested.sympt.inf.HCEHigh.index<-df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]== "symptomatic" & df[,paste("HCE_status",levels.tested,sep="_")]=="high" #high HCE + Covid Infected
  
  #Used for False positives below
  covid.untested.sympt.inf.index<-df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]== "symptomatic" #Altogether symptomatic (not by HCE)
  
  #Used to update vac for modUnequal and highUnequal
  covid.untested.sympt.inf.vac.index<-df[,"Covid_status"]=="I" & df[,"Covid_symptomStatus"]== "symptomatic" &  df[,"Vac_status"]=="vac"
  
  
  #SARS-CoV-2 Infected & Asymptomatic BUT symptomatic due to a Noncovid cause (untested this time step)
  covid.untested.asympt.inf.HCELow.NonCI.index<-df[,"Covid_status"]=="I" & df[,"NonCovid_status"]=="I" & df[,"Covid_symptomStatus"]== "asymptomatic" & df[,paste("HCE_status",levels.tested,sep="_")]=="low" 
  covid.untested.asympt.inf.HCEHigh.NonCI.index<-df[,"Covid_status"]=="I" & df[,"NonCovid_status"]=="I" & df[,"Covid_symptomStatus"]== "asymptomatic" & df[,paste("HCE_status",levels.tested,sep="_")]=="high" 
  
  #Used to update vac for modUnequal and highUnequal
  covid.untested.asympt.inf.vac.NonCI.index<-df[,"Covid_status"]=="I" & df[,"NonCovid_status"]=="I" & df[,"Covid_symptomStatus"]== "asymptomatic" & df[,"Vac_status"]=="vac"
  
  
  #Number of Covid-19 Infected and Symptomatic
  num.untested.sympt.inf.HCELow.untested<-sum(covid.untested.sympt.inf.HCELow.index) 
  num.untested.sympt.inf.HCEHigh.untested<-sum(covid.untested.sympt.inf.HCEHigh.index) 
  
  ##SARS-CoV-2 Infected & Asymptomatic BUT symptomatic due to a Noncovid cause
  num.untested.asympt.inf.HCELow.NonCI<-sum(covid.untested.asympt.inf.HCELow.NonCI.index)
  num.untested.asympt.inf.HCEHigh.NonCI<-sum(covid.untested.asympt.inf.HCEHigh.NonCI.index)

  
  #Change previously False positive to true positive (1 -> 0) if now infected with symptomatic Covid-19
  
  df[covid.untested.sympt.inf.index,paste("Covid_falseSymptPos",levels.tested,sep="_")]<-0
 
  
  #TESTING Assignment
  
  #symptomatic Covid
  df[covid.untested.sympt.inf.HCELow.index,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]<-rbinom(n=num.untested.sympt.inf.HCELow.untested,size=1,prob=covid.prob.test.sympt[1]) #
  df[covid.untested.sympt.inf.HCEHigh.index,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]<-rbinom(n=num.untested.sympt.inf.HCEHigh.untested,size=1,prob=covid.prob.test.sympt[2])
  
 
  #asymptomatic Covid with symptomatic Noncovid
  df[covid.untested.asympt.inf.HCELow.NonCI.index,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]<-rbinom(n=num.untested.asympt.inf.HCELow.NonCI,size=1,prob=non.covid.prob.test.sympt[1]) #
  df[covid.untested.asympt.inf.HCEHigh.NonCI.index,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]<-rbinom(n=num.untested.asympt.inf.HCEHigh.NonCI,size=1,prob=non.covid.prob.test.sympt[2])
  
  
  if(level.diff==2 | level.diff==3){
    
    #covid.untested.sympt.inf.vac.index 
    #covid.untested.asympt.inf.vac.NonCI.index
    
    df[covid.untested.sympt.inf.vac.index,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]<-df[covid.untested.sympt.inf.vac.index,paste("Covid_newly_testedSymptInf","equal",sep="_")]
    df[covid.untested.asympt.inf.vac.NonCI.index,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]<-df[covid.untested.asympt.inf.vac.NonCI.index,paste("Covid_newly_testedSymptInf","equal",sep="_")]
    
  }
  

  covid.sympt.tested.index<-df[,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]==1 
  df[covid.sympt.tested.index,paste("Covid_testedSymptInf",levels.tested,sep="_")]<- as.numeric(df[covid.sympt.tested.index,paste("Covid_testedSymptInf",levels.tested,sep="_")]) + 1

  
  #Assign False positive to those with asymptomatic covid-19 but those who were tested due to symptoms with something else
  covid.false.pos.index<-df[,paste("Covid_newly_testedSymptInf",levels.tested,sep="_")]==1 & df[,"Covid_symptomStatus"]=="asymptomatic"
  df[covid.false.pos.index,paste("Covid_falseSymptPos",levels.tested,sep="_")]<-1
  
  return(df)
  
}



#DESCRIPTION: Sets which symptomatic individuals due to other etiologies tested
#for SARS-CoV-2 (and sets them to testing negative);
#also tracks whether individuals receive a false negative (when individuals have
#previously had symptomatic SARS-CoV-2 but were not tested).

nonCovid_Testing<-function(df=inds,level.diff=NA,
                           non.covid.prob.test.sympt=non.covid.prob.test.sympt){

  if(level.diff==1){
    levels.tested<-"equal"
    
  }else if (level.diff==2){
    levels.tested<-"modUnequal"
    
  } else if(level.diff==3){
    levels.tested<-"highUnequal"
    
  } else{
    return("level.diff value out of range")}
  
  
  #NonCOVID INFECTED
 
  non.covid.untested.HCELow.index<-df[,"NonCovid_status"]=="I" & df[,"Covid_status"]!="E"& df[,"Covid_status"]!="I" & df[,paste("HCE_status",levels.tested,sep="_")]=="low" 
  non.covid.untested.HCEHigh.index<-df[,"NonCovid_status"]=="I" & df[,"Covid_status"]!="E"& df[,"Covid_status"]!="I" & df[,paste("HCE_status",levels.tested,sep="_")]=="high" 
  
  
  #Keep so I can replace 2.5x lower and 5x lower with values from equal  
  non.covid.untested.index.vac<-df[,"NonCovid_status"]=="I" & df[,"Covid_status"]!="E"& df[,"Covid_status"]!="I" & df[,"Vac_status"]=="vac"
  
  #Number of Noncovid infected
  num.non.covid.untested.HCELow<-sum(non.covid.untested.HCELow.index)
  num.non.covid.untested.HCEHigh<-sum(non.covid.untested.HCEHigh.index)
  
  
  #TESTING BEGINS
  
  #HCE
  df[non.covid.untested.HCELow.index,paste("NonCovid_newly_testedSymptInf",levels.tested,sep="_")]<-rbinom(n=num.non.covid.untested.HCELow,size=1,prob=non.covid.prob.test.sympt[1]) 
  df[non.covid.untested.HCEHigh.index,paste("NonCovid_newly_testedSymptInf",levels.tested,sep="_")]<-rbinom(n=num.non.covid.untested.HCEHigh,size=1,prob=non.covid.prob.test.sympt[2])
  
  
  #IF STATEMENT
  #IF VACCINATED WE WANT TO USE THE SAME INDIVIDUALS WHO TESTED IN EQUAL SCENARIO TO BE IN 2.5x AND 5x
  if(level.diff==2 | level.diff==3){
    
    df[non.covid.untested.index.vac,paste("NonCovid_newly_testedSymptInf",levels.tested,sep="_")]<-df[non.covid.untested.index.vac,paste("NonCovid_newly_testedSymptInf","equal",sep="_")]
    
  }
  
  non.covid.tested.index<-df[,paste("NonCovid_newly_testedSymptInf",levels.tested,sep="_")]==1
  
  df[non.covid.tested.index,paste("NonCovid_testedSymptInf",levels.tested,sep="_")]<- as.numeric(df[non.covid.tested.index,paste("NonCovid_testedSymptInf",levels.tested,sep="_")]) + 1
  
  #False Negative indicator (testing negative when previously infected with Covid-19 and never tested positive)
  non.covid.false.neg<-df[,paste("NonCovid_newly_testedSymptInf",levels.tested,sep="_")]==1 & df[,"Covid_everSymptInfected"]==1 & df[,paste("Covid_testedSymptInf",levels.tested,sep="_")]==0
  
  df[non.covid.false.neg,paste("Covid_falseNeg",levels.tested,sep="_")]<-1

  
  return(df)
  
}


