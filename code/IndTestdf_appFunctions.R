
Indsdf_Create<-function(indTest.df=NA, df=NA, level=NA){
  
  #Find those who have tested positive for Covid at this timestep (altogether, vac and unvac)
  covid.tested.index<-(df[,paste("Covid_newly_testedSymptInf",level,sep="_")]==1 | df[,paste("Covid_newly_testedSymptExp",level,sep="_")]==1)
  
  #Find those who have tested negative for Covid at this timestep & who never tested positive for Covid (altogether, vac and unvac)
  noncovid.tested.index<-df[,paste("NonCovid_newly_testedSymptInf",level,sep="_")]==1 & (df[,paste("Covid_testedSymptExp",level,sep="_")]==0 & df[,paste("Covid_testedSymptInf",level,sep="_")]==0)
  
  #Create a temporary dataset for those who tested positive for Covid-19 at this timestep
  covid.indTest.df.temp<-data.frame(Ind_id=rownames(df)[covid.tested.index], 
                                    Vac_status=df[covid.tested.index,"Vac_status"],
                                    CovidTested_positive=rep(1, sum(covid.tested.index)),
                                    Time_step=as.numeric(df[covid.tested.index,"t"]))
  
  covid.indTest.df.temp$Week<- covid.indTest.df.temp$Time_step %/% 7 + 1
  
  #Create temporary dataframe to record those testing negative for Covid-19 (who have never tested positive) at this timestep
  noncovid.indTest.df.temp<-data.frame(Ind_id=rownames(df)[noncovid.tested.index], 
                                       Vac_status=df[noncovid.tested.index,"Vac_status"],
                                       CovidTested_positive=rep(0, sum(noncovid.tested.index)),
                                       Time_step=as.numeric(df[noncovid.tested.index,"t"]))
  
  noncovid.indTest.df.temp$Week<-noncovid.indTest.df.temp$Time_step %/% 7 + 1
  
  #Need to rbind the three dataframes together
  indTest.df<-rbindlist(list(indTest.df, covid.indTest.df.temp, noncovid.indTest.df.temp))
  
  
  return(indTest.df)
}




