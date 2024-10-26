################################################

#Appendix Analysis and Figures

################################################

#OVERALL SET-UP
library(tidyverse)
library(ggborderline)
library(data.table)
library(ggpubr)

#set pathway
setwd("..") #moves back a folder - don't run if your wd is already the data folder
pathway=getwd()

######################################################
#APPENDIX 1: TIMING OF TESTING & TABLE S1

#A function to simulate a cohort of individuals who are all symptomatic at the 
#same time to evaluate testing timing (and the proportion testing) by vaccination status
testing.timing<-function(testing.scenario = NULL){
  N=100000
  N.vac=N/2
  N.unvac=N-N.vac
  
  #set up basic data frame for simulation
  col_names.ind<-c("Vac_status", "Symptom_status", "HCE_status",
                   "Tested_bin", "Day_firstTested")
  
  #set seed to create analysis
  set.seed(45019029)
  
  #set-up the dataframe
  inds<-array(data=0,dim=c(N,length(col_names.ind)))
  rownames(inds)<-paste("ind",seq(1,N),sep="")
  colnames(inds)<-col_names.ind
  
  #Assign tested status (everyone starts at 0=no)
  inds[,"Tested_bin"]<-0
  
  #Assign everyone as symptomatic
  inds[,"Symptom_status"]<-"symptomatic"
  
  #Assign vaccination status
  inds[1:N.vac,"Vac_status"]<-"vac"
  inds[(N.vac+1):N,"Vac_status"]<-"unvac"
  
  #Assign proportion with high and low healthcare engagement by vaccination status
  
  #Vaccinated
  prop.highHCE.testing.vac<-0.55
  
  #Unvaccinated (proportion varies depending on scenario)
  if(testing.scenario == "equal"){
    prop.highHCE.testing.unvac<-prop.highHCE.testing.vac
    
  } else if (testing.scenario == "mod_unequal"){
    prop.highHCE.testing.unvac<-prop.highHCE.testing.vac/2.5
    
  } else if (testing.scenario == "high_unequal"){
    prop.highHCE.testing.unvac<-prop.highHCE.testing.vac/5
    
  }else{
    statement= "you did not specify an available testing.scenario"
    return(statement)
  }
  
  #separate individuals by vaccination status
  all.vac<-rownames(inds[inds[,"Vac_status"]=="vac",])
  all.unvac<-rownames(inds[inds[,"Vac_status"]=="unvac",])
  
  #Assign individuals healthcare engagement level using vaccinated healthcare engagement proportions
  high.tested.vac.id<-sample(all.vac,size=round(N.vac*prop.highHCE.testing.vac),replace=F)
  low.tested.vac.id<-all.vac[!(all.vac %in% high.tested.vac.id)]
  
  #Assign individuals healthcare engagement level using unvaccinated healthcare engagement proportions
  high.tested.unvac.id<-sample(all.unvac,size=round(N.unvac*prop.highHCE.testing.unvac),replace=F)
  low.tested.unvac.id<-all.unvac[!(all.unvac %in% high.tested.unvac.id)]
  
  #Set individuals to have their assigned healthcare engagement levels
  inds[high.tested.vac.id,"HCE_status"]<-"high"
  inds[low.tested.vac.id,"HCE_status"]<-"low"
  inds[high.tested.unvac.id,"HCE_status"]<-"high"
  inds[low.tested.unvac.id,"HCE_status"]<-"low"
  
  #Set testing probabilities by healthcare engagement level
  prob.test.HCE_low<-0.0165
  prob.test.HCE_high<-0.175
  
  #Run simulation for 10 days
  for(t in 1:10){
    
    #Isolate individuals who have not tested yet by healthcare enagement level
    untested.HCEHigh.index<-inds[,"HCE_status"]=="high" & inds[,"Tested_bin"]== "0"
    untested.HCELow.index<-inds[,"HCE_status"]=="low" & inds[,"Tested_bin"]== "0"
    
    #Count how many individuals meet above criteria
    num.highHCE.untested<-sum(untested.HCEHigh.index)
    num.lowHCE.untested<-sum(untested.HCELow.index)
    
    #Use a draw from a Bernoulli trial to determine if individuals test (by healthcare engagement level)
    inds[untested.HCEHigh.index, "Tested_bin"]<-rbinom(n=num.highHCE.untested,size=1,prob=prob.test.HCE_high) #high healthcare engagement
    inds[untested.HCELow.index, "Tested_bin"]<-rbinom(n=num.lowHCE.untested,size=1,prob=prob.test.HCE_low) #low healthcare engagement
    
    #Record the day if individuals tested
    inds[untested.HCEHigh.index, "Day_firstTested"]<-t #day of testing
    inds[untested.HCELow.index, "Day_firstTested"]<-t#day of testing
  }
  
  #Get appropriate indexes for vaccination status and tested by vaccination status
  tested.unvac.index<-inds[,"Vac_status"]=="unvac" & inds[,"Tested_bin"]== "1"
  unvac.index<-inds[,"Vac_status"]=="unvac"
  tested.vac.index<-inds[,"Vac_status"]=="vac" & inds[,"Tested_bin"]== "1"
  vac.index<-inds[,"Vac_status"]=="vac"
  
  #Calculate the average day that individuals first tested by vaccination status
  avg.unvac.DFT<-round(mean(as.numeric(inds[tested.unvac.index, "Day_firstTested"])),2)
  avg.vac.DFT<-round(mean(as.numeric(inds[tested.vac.index, "Day_firstTested"])),2) 
  
  #Calculate the average proportion that tested by vaccination status
  avg.unvac.prop.tested<-round(mean(as.numeric(inds[unvac.index, "Tested_bin"])),2)
  avg.vac.prop.tested<-round(mean(as.numeric(inds[vac.index, "Tested_bin"])),2)
  
  #Create table of testing information
  df.testing<-data.frame(matrix(c(avg.vac.DFT,avg.unvac.DFT,
                                  avg.vac.prop.tested, avg.unvac.prop.tested), nrow=2),
                         row.names=c("vac", "unvac"))
  names(df.testing)<-c("avg_first_day_tested", "prop_tested")
  
  #return table 
  return(df.testing)
  
}

#Testing Timing Information for Table S1
testing.timing(testing.scenario = "equal")
testing.timing(testing.scenario = "mod_unequal")
testing.timing(testing.scenario = "high_unequal")
######################################################
#APPENDIX 1: APPENDIX FIGURE 1
#MISCLASSIFICATION IN TEST-NEGATIVE DESIGN

##Evaluate using a higher and lower vaccine efficacy against susceptibility (0.55 & 0.1)

# Load in data
aggDF.ves0.1<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.1.csv",sep="/"))
aggDF.ves0.55<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.55.csv",sep="/"))

#Set-up for figure
ve.susc.0.1=0.1
ve.susc.0.55=0.55
t.start=20
t.end.0.1= 100
t.end.0.55=150
t.end.0.1.zoomed=50
t.end.0.55.zoomed=75 #will need to update
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

main.line.width=2.5
ribbon.line.width=1

##Appendix Figure 1a
#Cumulative False Negative by vaccination status for vaccine efficacy against susceptibility=0.55
Cum.Prop.False.Neg.0.55<-ggplot(aggDF.ves0.55, aes(x=t, y=CumProp_falseNeg_unvac.50.)) +
  theme_classic() +
  geom_ribbon(aes(ymin=CumProp_falseNeg_unvac.25., ymax=CumProp_falseNeg_unvac.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(0, 0.15), breaks= seq(0, 0.15, 0.05)) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_unvac.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  geom_ribbon(aes(ymin=CumProp_falseNeg_vac.25., ymax=CumProp_falseNeg_vac.75.),
              fill="lightgoldenrod4", color="lightgoldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_vac.50.), color="lightgoldenrod4",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="Cumulative Proportion of Misclassification from Non-Detection\n(i.e. False Negatives)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")

##Appendix Figure 1b
#Cumulative False Negative by vaccination status for vaccine efficacy against susceptibility=0.1
Cum.Prop.False.Neg.0.1<-ggplot(aggDF.ves0.1, aes(x=t, y=CumProp_falseNeg_unvac.50.)) +
  theme_classic() +
  geom_ribbon(aes(ymin=CumProp_falseNeg_unvac.25., ymax=CumProp_falseNeg_unvac.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(0, 0.15), breaks= seq(0, 0.15, 0.05)) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_unvac.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  geom_ribbon(aes(ymin=CumProp_falseNeg_vac.25., ymax=CumProp_falseNeg_vac.75.),
              fill="lightgoldenrod4", color="lightgoldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_vac.50.), color="lightgoldenrod4",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="Cumulative Proportion of Misclassification from Non-Detection\n(i.e. False Negatives)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")

#set pathway to save figures
appfig1.pathway<-paste(pathway,"figures/appendices_figs/app_fig1&2",sep="/")

#Save Figures (warning occurs as we include only a certain time range)
pdf(file=paste(appfig1.pathway,"AppFig.1a_CumPropFalseNeg.0.55.pdf",sep="/"), 
    width=7.5, height=6)
Cum.Prop.False.Neg.0.55
dev.off()

pdf(file=paste(appfig1.pathway,"AppFig.1b_CumPropFalseNeg.0.1.pdf",sep="/"), 
    width=7.5, height=6)
Cum.Prop.False.Neg.0.1
dev.off()

#####################################################
#APPENDIX 1: APPENDIX FIGURE 2
#MISCLASSIFICATION EXAMPLE 
#FIGURE CREATED OUTSIDE THE R ENVIRONMENT

######################################################
#APPENDIX 1: APPENDIX FIGURE 3
#OTHER ETIOLOGIES SENSITIVITY ANALYSIS FOR TEST-NEGATIVE DESIGN

#Load in data for etiologies 
df.ve.susc0.1_other.etiologies.sens.high<-read.csv(paste(pathway,"data/appendices/df.ve.susc0.1_other.etiologies.sens.high.csv",sep="/"))
df.ve.susc0.55_other.etiologies.sens.high<-read.csv(paste(pathway,"data/appendices/df.ve.susc0.55_other.etiologies.sens.high.csv",sep="/"))

#Only select epidemics where an outbreak actually occurs (defined as >0.001)
df.ve.susc0.1_other.etiologies.sens.high<-subset(df.ve.susc0.1_other.etiologies.sens.high,Covid_cumPropSymptInfect_highChange >0.001)
df.ve.susc0.55_other.etiologies.sens.high<-subset(df.ve.susc0.55_other.etiologies.sens.high,Covid_cumPropSymptInfect_highChange >0.001)

#Fixes underestimation var for RR and OR
df.ve.susc0.1_other.etiologies.sens.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.ve.susc0.1_other.etiologies.sens.high$VE_symptOR_degUnder_highChange_trueVeff
df.ve.susc0.1_other.etiologies.sens.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.ve.susc0.1_other.etiologies.sens.high$VE_symptRR_degUnder_highChange_trueVeff

df.ve.susc0.55_other.etiologies.sens.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.ve.susc0.55_other.etiologies.sens.high$VE_symptOR_degUnder_highChange_trueVeff
df.ve.susc0.55_other.etiologies.sens.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.ve.susc0.55_other.etiologies.sens.high$VE_symptRR_degUnder_highChange_trueVeff

#Make sure Testing Variable is a factor variable
df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff<-factor(df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
df.ve.susc0.55_other.etiologies.sens.high$Label_testingDiff<-factor(df.ve.susc0.55_other.etiologies.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

#Average timepoint at the highest epidemic growth rate
median(df.ve.susc0.1_other.etiologies.sens.high$t_highChange)
median(df.ve.susc0.55_other.etiologies.sens.high$t_highChange)

##Appendix Figure 3a
#Other Etiologies Sensitivity Analysis for vaccine efficacy against susceptibility = 0.55
plot.ve.susc0.55_other.etiologies.sens<-ggplot(df.ve.susc0.55_other.etiologies.sens.high,aes(x=as.factor(NonCovid_propSympt) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Prevalence of Covid-like Symptoms\n(due to Other Etiologies)") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenarios"))  + theme(legend.position="none")
#+ geom_point(position=position_jitterdodge(),alpha=0.3) 

##Appendix Figure 3b
#Other Etiologies Sensitivity Analysis for vaccine efficacy against susceptibility = 0.55
plot.ve.susc0.1_other.etiologies.sens<-ggplot(df.ve.susc0.1_other.etiologies.sens.high,aes(x=as.factor(NonCovid_propSympt) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Prevalence of Covid-like Symptoms\n(due to Other Etiologies)") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenarios"))  + theme(legend.position="none")
#+ geom_point(position=position_jitterdodge(),alpha=0.3) 


#set pathway to save figures
appfig5.pathway<-paste(pathway,"figures/appendices_figs/app_fig3",sep="/")

#Save Figures
pdf(file=paste(appfig5.pathway,"AppFig.3a_VE.Susc0.55.OtherEtiologies.pdf",sep="/"), 
    width=8, height=6)
plot.ve.susc0.55_other.etiologies.sens
dev.off()

pdf(file=paste(appfig5.pathway,"AppFig.3b_VE.Susc0.1.OtherEtiologies.pdf",sep="/"), 
    width=8, height=6)
plot.ve.susc0.1_other.etiologies.sens
dev.off()

##############################
#APPENDIX 1: APPENDIX FIGURE 4
#VE AGAINST SUSCEPTIBILITY SENSITIVITY ANALYSIS APPENDIX

#Load in Data
df.ve.susc.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.sens.csv",sep="/"))

#Separate by level of vaccine efficacy against susceptibility

df.ve.susc.sens.0.1<-subset(df.ve.susc.sens.agg, VE_susc==0.1)
df.ve.susc.sens.0.3<-subset(df.ve.susc.sens.agg, VE_susc==0.3)
df.ve.susc.sens.0.5<-subset(df.ve.susc.sens.agg, VE_susc==0.5)
df.ve.susc.sens.0.7<-subset(df.ve.susc.sens.agg, VE_susc==0.7)
df.ve.susc.sens.0.9<-subset(df.ve.susc.sens.agg, VE_susc==0.9)

#Set-up for figures
t.start=20
t.end.0.1= 100
t.end.0.3= 120
t.end.0.5= 140
t.end.0.7= 180
t.end.0.9= 380
main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Appendix Figure 4
#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against susceptibility=0.1
plot.cum.inf.ve.susc.0.1<-ggplot(df.ve.susc.sens.0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain")) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against susceptibility=0.3
plot.cum.inf.ve.susc.0.3<-ggplot(df.ve.susc.sens.0.3, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.3), breaks = seq(t.start,t.end.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against susceptibility=0.5
plot.cum.inf.ve.susc.0.5<-ggplot(df.ve.susc.sens.0.5, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.5), breaks = seq(t.start,t.end.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against susceptibility=0.7
plot.cum.inf.ve.susc.0.7<-ggplot(df.ve.susc.sens.0.7, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.7), breaks = seq(t.start,t.end.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against susceptibility=0.9
plot.cum.inf.ve.susc.0.9<-ggplot(df.ve.susc.sens.0.9, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.9), breaks = seq(t.start,t.end.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2)  
#when vaccine efficacy against susceptibility = 0.1
plot.cum.uninf.ve.susc.0.1<-ggplot(df.ve.susc.sens.0.1, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2)
#when vaccine efficacy against susceptibility = 0.3
plot.cum.uninf.ve.susc.0.3<-ggplot(df.ve.susc.sens.0.3, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.3), breaks = seq(t.start,t.end.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against susceptibility = 0.5
plot.cum.uninf.ve.susc.0.5<-ggplot(df.ve.susc.sens.0.5, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.5), breaks = seq(t.start,t.end.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against susceptibility = 0.7
plot.cum.uninf.ve.susc.0.7<-ggplot(df.ve.susc.sens.0.7, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.7), breaks = seq(t.start,t.end.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against susceptibility = 0.9
plot.cum.uninf.ve.susc.0.9<-ggplot(df.ve.susc.sens.0.9, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.9), breaks = seq(t.start,t.end.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Combine sub-figures
plotlist.ve.susc<- list(plot.cum.inf.ve.susc.0.1, plot.cum.inf.ve.susc.0.3, plot.cum.inf.ve.susc.0.5, 
                        plot.cum.inf.ve.susc.0.7, plot.cum.inf.ve.susc.0.9, 
                        plot.cum.uninf.ve.susc.0.1, plot.cum.uninf.ve.susc.0.3,plot.cum.uninf.ve.susc.0.5,
                        plot.cum.uninf.ve.susc.0.7,plot.cum.uninf.ve.susc.0.9)

#ggarrange(plotlist = plotlist.ve.susc, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig4.pathway<-paste(pathway,"figures/appendices_figs/app_fig4",sep="/")

#Save Figure 4 (warnings are just because we are only plotting part of the time we have recorded)
ggarrange(plotlist = plotlist.ve.susc, ncol = 5, nrow = 2) %>%
  ggexport(filename = paste(appfig4.pathway,"AppFig.4_VE.Susc.Infect.Dynamics.Fig.pdf",sep="/"))

###########################################
#APPENDIX 1: APPENDIX FIGURE 5
#VE AGAINST INFECTIOUSNESS SENSITIVITY ANALYSIS APPENDIX

##Higher vaccine efficacy against susceptibility (0.55)

#Load in data
df.ve.susc.0.55_ve.infect.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.55_ve.infect.sens.csv",sep="/"))

#Separate by level of vaccine efficacy against infectiousness
df.ve.susc.0.55_ve.infect.sens.0.1<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.1)
df.ve.susc.0.55_ve.infect.sens.0.3<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.3)
df.ve.susc.0.55_ve.infect.sens.0.5<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.5)
df.ve.susc.0.55_ve.infect.sens.0.7<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.7)
df.ve.susc.0.55_ve.infect.sens.0.9<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.9)

#Set-up for figures
t.start=20
t.end.0.1= 140
t.end.0.3= 180
t.end.0.5= 220
t.end.0.7= 260
t.end.0.9= 620
main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Appendix Figure 5
#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.1
plot.cum.inf.susc0.55.ve.infect.0.1<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain")) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when when vaccine efficacy against infectiousness=0.3
plot.cum.inf.susc0.55.ve.infect.0.3<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.3, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.3), breaks = seq(t.start,t.end.0.3,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when when vaccine efficacy against infectiousness=0.5
plot.cum.inf.susc0.55.ve.infect.0.5<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.5, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.5), breaks = seq(t.start,t.end.0.5,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when when vaccine efficacy against infectiousness=0.7
plot.cum.inf.susc0.55.ve.infect.0.7<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.7, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.7), breaks = seq(t.start,t.end.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when when vaccine efficacy against infectiousness=0.9
plot.cum.inf.susc0.55.ve.infect.0.9<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.9, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.9), breaks = seq(t.start,t.end.0.9,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when when vaccine efficacy against infectiousness=0.1
plot.cum.uninf.susc0.55.ve.infect.0.1<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.1, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when when vaccine efficacy against infectiousness=0.3
plot.cum.uninf.susc0.55.ve.infect.0.3<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.3, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.3), breaks = seq(t.start,t.end.0.3,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when when vaccine efficacy against infectiousness=0.5
plot.cum.uninf.susc0.55.ve.infect.0.5<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.5, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.5), breaks = seq(t.start,t.end.0.5,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2)
#when when vaccine efficacy against infectiousness=0.7
plot.cum.uninf.susc0.55.ve.infect.0.7<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.7, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.7), breaks = seq(t.start,t.end.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when when vaccine efficacy against infectiousness=0.9
plot.cum.uninf.susc0.55.ve.infect.0.9<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.9, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.9), breaks = seq(t.start,t.end.0.9,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Combine sub-figures
plotlist.ve.susc0.55.ve.infect<- list(plot.cum.inf.susc0.55.ve.infect.0.1, plot.cum.inf.susc0.55.ve.infect.0.3, plot.cum.inf.susc0.55.ve.infect.0.5, 
                                      plot.cum.inf.susc0.55.ve.infect.0.7, plot.cum.inf.susc0.55.ve.infect.0.9, 
                                      plot.cum.uninf.susc0.55.ve.infect.0.1, plot.cum.uninf.susc0.55.ve.infect.0.3,plot.cum.uninf.susc0.55.ve.infect.0.5,
                                      plot.cum.uninf.susc0.55.ve.infect.0.7,plot.cum.uninf.susc0.55.ve.infect.0.9)

#ggarrange(plotlist = plotlist.ve.susc0.55.ve.infect, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig5.pathway<-paste(pathway,"figures/appendices_figs/app_fig5&6",sep="/")

#Save Figure 5 (warnings are just because we are only plotting part of the time we have recorded)
ggarrange(plotlist = plotlist.ve.susc0.55.ve.infect, ncol = 5, nrow = 2) %>%
  ggexport(filename = paste(appfig5.pathway,"AppFig.5_VE.Susc0.55_VE.Infect.Combo.pdf",sep="/"))

###########################################
#APPENDIX 1: APPENDIX FIGURE 6
#VE AGAINST INFECTIOUSNESS SENSITIVITY ANALYSIS APPENDIX

##Lower vaccine efficacy against susceptibility (0.1)

#Load data
df.ve.susc.0.1_ve.infect.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_ve.infect.sens.csv",sep="/"))

#Separate by level of vaccine efficacy against infectiousness
df.ve.susc.0.1_ve.infect.sens.0.1<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.1)
df.ve.susc.0.1_ve.infect.sens.0.3<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.3)
df.ve.susc.0.1_ve.infect.sens.0.5<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.5)
df.ve.susc.0.1_ve.infect.sens.0.7<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.7)
df.ve.susc.0.1_ve.infect.sens.0.9<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.9)

#Set-up for figures
t.start=20
t.end.0.1= 100
t.end.0.3= 120
t.end.0.5= 140
t.end.0.7= 180
t.end.0.9= 380
main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Appendix Figure 6
#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.1
plot.cum.inf.susc0.1.ve.infect.0.1<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain")) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.3
plot.cum.inf.susc0.1.ve.infect.0.3<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.3, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.3), breaks = seq(t.start,t.end.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.5
plot.cum.inf.susc0.1.ve.infect.0.5<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.5, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.5), breaks = seq(t.start,t.end.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.7
plot.cum.inf.susc0.1.ve.infect.0.7<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.7, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.7), breaks = seq(t.start,t.end.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.9
plot.cum.inf.susc0.1.ve.infect.0.9<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.9, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.9), breaks = seq(t.start,t.end.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against infectiousness=0.1
plot.cum.uninf.susc0.1.ve.infect.0.1<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.1, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against infectiousness=0.3
plot.cum.uninf.susc0.1.ve.infect.0.3<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.3, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.3), breaks = seq(t.start,t.end.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against infectiousness=0.5
plot.cum.uninf.susc0.1.ve.infect.0.5<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.5, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.5), breaks = seq(t.start,t.end.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2)
#when vaccine efficacy against infectiousness=0.7
plot.cum.uninf.susc0.1.ve.infect.0.7<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.7, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.7), breaks = seq(t.start,t.end.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against infectiousness=0.9
plot.cum.uninf.susc0.1.ve.infect.0.9<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.9, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.9), breaks = seq(t.start,t.end.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Combine sub-figures
plotlist.ve.susc0.1.ve.infect<- list(plot.cum.inf.susc0.1.ve.infect.0.1, plot.cum.inf.susc0.1.ve.infect.0.3, plot.cum.inf.susc0.1.ve.infect.0.5, 
                                     plot.cum.inf.susc0.1.ve.infect.0.7, plot.cum.inf.susc0.1.ve.infect.0.9, 
                                     plot.cum.uninf.susc0.1.ve.infect.0.1, plot.cum.uninf.susc0.1.ve.infect.0.3,plot.cum.uninf.susc0.1.ve.infect.0.5,
                                     plot.cum.uninf.susc0.1.ve.infect.0.7,plot.cum.uninf.susc0.1.ve.infect.0.9)

#ggarrange(plotlist = plotlist.ve.susc0.1.ve.infect, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig6.pathway<-paste(pathway,"figures/appendices_figs/app_fig5&6",sep="/")

#Save Figure 6 (warnings are just because we are only plotting part of the time we have recorded)
ggarrange(plotlist = plotlist.ve.susc0.1.ve.infect, ncol = 5, nrow = 2) %>%
  ggexport(filename = paste(appfig6.pathway,"AppFig.6_VE.Susc0.1_VE.Infect.Combo.pdf",sep="/"))

###########################################
#APPENDIX 1: CONTENT FOR APPENDIX FIGURE 7 & 8
#ANALYSIS FOR CHANGING VACCINE EFFICACY AGAINST SUSCEPTIBILITY

#Load data
df.ve.susc.sens.agg_highChange<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.sens_highChange.csv",sep="/"))

#Separate by level of vaccine efficacy against susceptibility
df.ve.susc.sens.highChange.0.1<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.1)
df.ve.susc.sens.highChange.0.3<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.3)
df.ve.susc.sens.highChange.0.5<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.5)
df.ve.susc.sens.highChange.0.7<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.7)
df.ve.susc.sens.highChange.0.9<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.9)


##Considering VE_RR (cumulative numbers of tested symptomatic SARS-CoV-2 infected)
#Vaccine efficacy against susceptibility=0.1
cbind(Label_test_ve.susc0.1=df.ve.susc.sens.highChange.0.1$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.3
cbind(Label_test_ve.susc0.3=df.ve.susc.sens.highChange.0.3$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.5
cbind(Label_test_ve.susc0.5=df.ve.susc.sens.highChange.0.5$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.7
cbind(Label_test_ve.susc0.7=df.ve.susc.sens.highChange.0.7$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.9
cbind(Label_test_ve.susc0.9=df.ve.susc.sens.highChange.0.9$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_unvac)


#Considering VE_OR (cumulative numbers tested with other etiologies of Covid-like symptoms)
#Vaccine efficacy against susceptibility=0.1
cbind(Label_test_ve.susc0.1=df.ve.susc.sens.highChange.0.1$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.3
cbind(Label_test_ve.susc0.3=df.ve.susc.sens.highChange.0.3$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.5
cbind(Label_test_ve.susc0.5=df.ve.susc.sens.highChange.0.5$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.7
cbind(Label_test_ve.susc0.7=df.ve.susc.sens.highChange.0.7$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_unvac)

#Vaccine efficacy against susceptibility=0.9
cbind(Label_test_ve.susc0.9=df.ve.susc.sens.highChange.0.9$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_unvac)

###########################################
#APPENDIX 1: CONTENT FOR APPENDIX FIGURE 9 & 10
#ANALYSIS FOR CHANGING VACCINE EFFICACY AGAINST INFECTIOUSNESS

#Lower vaccine efficacy against susceptibility (0.1)

#Load data
df.ve.susc.0.1_ve.infect.sens.agg_highChange<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_ve.infect.sens_highChange.csv",sep="/"))


#Separate by level of vaccine efficacy against infectiousness
df.ve.susc.0.1_ve.infect.sens.highChange.0.1<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.1)
df.ve.susc.0.1_ve.infect.sens.highChange.0.3<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.3)
df.ve.susc.0.1_ve.infect.sens.highChange.0.5<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.5)
df.ve.susc.0.1_ve.infect.sens.highChange.0.7<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.7)
df.ve.susc.0.1_ve.infect.sens.highChange.0.9<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.9)


##Considering VE_RR (cumulative numbers of tested symptomatic SARS-CoV-2 infected)
#vaccine efficacy against infectiousness=0.1
cbind(Label_test_ve.infect0.1=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.3
cbind(Label_test_ve.infect0.3=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.5
cbind(Label_test_ve.infect0.5=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.7
cbind(Label_test_ve.infect.0.7=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.9
cbind(Label_test_ve.infect0.9=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_unvac)


#Considering VE_OR (cumulative numbers tested with other etiologies of Covid-like symptoms)
#vaccine efficacy against infectiousness=0.1
cbind(Label_test_ve.infect0.1=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.3
cbind(Label_test_ve.infect0.3=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.5
cbind(Label_test_ve.infect0.5=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.7
cbind(Label_test_ve.infect0.7=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_unvac)

#vaccine efficacy against infectiousness=0.9
cbind(Label_test_ve.infect0.9=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_unvac)

###########################################
#APPENDIX 1: APPENDIX FIGURE 11
#PROBABILITY OF TRANSMISSION ANALYSIS APPENDIX

##Higher vaccine efficacy against susceptibility (0.55) 

#Load Data
df.ve.susc.0.55_prob.trans.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.55_prob.trans.sens.csv",sep="/"))

#Separate by probability of transmission
df.ve.susc.0.55_prob.trans.sens.0.05<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.05)
df.ve.susc.0.55_prob.trans.sens.0.07<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.07)
df.ve.susc.0.55_prob.trans.sens.0.09<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.09)
df.ve.susc.0.55_prob.trans.sens.0.11<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.11)

#Set-up for figures
t.start=20
t.end.0.05= 520
t.end.0.07= 260
t.end.0.09= 180
t.end.0.11= 140

main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Appendix Figure 11
#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.05
plot.cum.inf.susc0.55.prob.trans.0.05<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.05, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.05), breaks = seq(t.start,t.end.0.05,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.07
plot.cum.inf.susc0.55.prob.trans.0.07<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.07, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.07), breaks = seq(t.start,t.end.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.09
plot.cum.inf.susc0.55.prob.trans.0.09<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.09, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.09), breaks = seq(t.start,t.end.0.09,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.11
plot.cum.inf.susc0.55.prob.trans.0.11<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.11, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.11), breaks = seq(t.start,t.end.0.11,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))


#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.05
plot.cum.uninf.susc0.55.prob.trans.0.05<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.05, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.05), breaks = seq(t.start,t.end.0.05,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.07
plot.cum.uninf.susc0.55.prob.trans.0.07<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.07, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.07), breaks = seq(t.start,t.end.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.09
plot.cum.uninf.susc0.55.prob.trans.0.09<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.09, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.09), breaks = seq(t.start,t.end.0.09,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.11
plot.cum.uninf.susc0.55.prob.trans.0.11<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.11, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.11), breaks = seq(t.start,t.end.0.11,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Combine sub-figures
plotlist.ve.susc0.55.prob.transmiss<- list(plot.cum.inf.susc0.55.prob.trans.0.05, plot.cum.inf.susc0.55.prob.trans.0.07, 
                                           plot.cum.inf.susc0.55.prob.trans.0.09, plot.cum.inf.susc0.55.prob.trans.0.11, 
                                           plot.cum.uninf.susc0.55.prob.trans.0.05,plot.cum.uninf.susc0.55.prob.trans.0.07,
                                           plot.cum.uninf.susc0.55.prob.trans.0.09,plot.cum.uninf.susc0.55.prob.trans.0.11)

#ggarrange(plotlist = plotlist.ve.susc0.55.prob.trans, ncol = 4, nrow = 2) #to view combined figure

#set pathway to save figures
appfig11.pathway<-paste(pathway,"figures/appendices_figs/app_fig11&12",sep="/")

#Save Figure 11 (warnings are just because we are only plotting part of the time we have recorded)
ggarrange(plotlist = plotlist.ve.susc0.55.prob.transmiss, ncol = 4, nrow = 2) %>%
  ggexport(filename = paste(appfig11.pathway,"AppFig.11_VE.Susc0.55_Prob.Transmiss.Combo.pdf",sep="/"))


###########################################
#APPENDIX 1: APPENDIX FIGURE 12
#PROBABILITY OF TRANSMISSION ANALYSIS APPENDIX

##Lower vaccine efficacy against susceptibility (0.1)

#Load in data
df.ve.susc.0.1_prob.trans.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_prob.trans.sens.csv",sep="/"))

#Separate by probability of transmission
df.ve.susc.0.1_prob.trans.sens.0.03<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.03)
df.ve.susc.0.1_prob.trans.sens.0.05<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.05)
df.ve.susc.0.1_prob.trans.sens.0.07<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.07)
df.ve.susc.0.1_prob.trans.sens.0.09<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.09)
df.ve.susc.0.1_prob.trans.sens.0.11<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.11)

##Set-up for figures
t.start=20
t.end.0.03= 520
t.end.0.05= 260
t.end.0.07= 220
t.end.0.09= 180
t.end.0.11= 140

main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Appendix Figure 12
#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.03
plot.cum.inf.susc0.1.prob.trans.0.03<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.03, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.03), breaks = seq(t.start,t.end.0.03,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain")) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.05
plot.cum.inf.susc0.1.prob.trans.0.05<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.05, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.05), breaks = seq(t.start,t.end.0.05,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.07
plot.cum.inf.susc0.1.prob.trans.0.07<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.07, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.07), breaks = seq(t.start,t.end.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.09
plot.cum.inf.susc0.1.prob.trans.0.09<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.09, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.09), breaks = seq(t.start,t.end.0.09,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.11
plot.cum.inf.susc0.1.prob.trans.0.11<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.11, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.11), breaks = seq(t.start,t.end.0.11,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))


#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.03
plot.cum.uninf.susc0.1.prob.trans.0.03<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.03, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.03), breaks = seq(t.start,t.end.0.03,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.05
plot.cum.uninf.susc0.1.prob.trans.0.05<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.05, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.05), breaks = seq(t.start,t.end.0.05,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.07
plot.cum.uninf.susc0.1.prob.trans.0.07<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.07, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.07), breaks = seq(t.start,t.end.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.09
plot.cum.uninf.susc0.1.prob.trans.0.09<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.09, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.09), breaks = seq(t.start,t.end.0.09,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.11
plot.cum.uninf.susc0.1.prob.trans.0.11<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.11, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.25., ymax=NonCovid_cumPropSymptInfect_unvac.75.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.11), breaks = seq(t.start,t.end.0.11,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.25., ymax=NonCovid_cumPropSymptInfect_vac.75.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Combine sub-figures
plotlist.ve.susc0.1.prob.transmiss<- list(plot.cum.inf.susc0.1.prob.trans.0.03, plot.cum.inf.susc0.1.prob.trans.0.05, plot.cum.inf.susc0.1.prob.trans.0.07, 
                                          plot.cum.inf.susc0.1.prob.trans.0.09, plot.cum.inf.susc0.1.prob.trans.0.11, 
                                          plot.cum.uninf.susc0.1.prob.trans.0.03, plot.cum.uninf.susc0.1.prob.trans.0.05,plot.cum.uninf.susc0.1.prob.trans.0.07,
                                          plot.cum.uninf.susc0.1.prob.trans.0.09,plot.cum.uninf.susc0.1.prob.trans.0.11)

#ggarrange(plotlist = plotlist.ve.susc0.1.prob.transmiss, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig12.pathway<-paste(pathway,"figures/appendices_figs/app_fig11&12",sep="/")

#Save Figure 12 (warnings are just because we are only plotting part of the time we have recorded)
ggarrange(plotlist = plotlist.ve.susc0.1.prob.transmiss, ncol = 5, nrow = 2) %>%
  ggexport(filename = paste(appfig12.pathway,"AppFig.12_VE.Susc0.1_Prob.Transmiss.Combo.pdf",sep="/"))

###########################################
#APPENDIX 1: CONTENT FOR APPENDIX FIGURE 13 & 14
#ANALYSIS FOR CHANGING PROBABILITY OF TRANSMISSION

#Load in data
df.ve.susc.0.1_prob.trans.sens.agg_highChange<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_prob.trans.sens_highChange.csv",sep="/"))

#Separate by probability of transmission
df.ve.susc.0.1_prob.trans.sens.highChange.0.03<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.03)
df.ve.susc.0.1_prob.trans.sens.highChange.0.05<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.05)
df.ve.susc.0.1_prob.trans.sens.highChange.0.07<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.07)
df.ve.susc.0.1_prob.trans.sens.highChange.0.09<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.09)
df.ve.susc.0.1_prob.trans.sens.highChange.0.11<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.11)


#Considering VE_RR (cumulative numbers of tested symptomatic SARS-CoV-2 infected)
#probability of transmission=0.03
cbind(Label_test_prob.trans0.03=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Covid_cumSymptExpInfect_tested_unvac)

#probability of transmission=0.05
cbind(Label_test_prob.trans0.05=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Covid_cumSymptExpInfect_tested_unvac)

#probability of transmission=0.07
cbind(Label_test_prob.trans0.07=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Covid_cumSymptExpInfect_tested_unvac)

#probability of transmission=0.09
cbind(Label_test_prob.trans0.09=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Covid_cumSymptExpInfect_tested_unvac)

#probability of transmission=0.11
cbind(Label_test_prob.trans0.11=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Covid_cumSymptExpInfect_tested_vac,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Covid_cumSymptExpInfect_tested_unvac)


#Considering VE_OR (cumulative numbers tested with other etiologies of Covid-like symptoms)
#probability of transmission=0.03
cbind(Label_test_prob.trans0.03=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$NonCovid_cumSymptInfect_tested_unvac)

#probability of transmission=0.05
cbind(Label_test_prob.trans0.05=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$NonCovid_cumSymptInfect_tested_unvac)

#probability of transmission=0.07
cbind(Label_test_prob.trans0.07=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$NonCovid_cumSymptInfect_tested_unvac)

#probability of transmission=0.09
cbind(Label_test_prob.trans0.09=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$NonCovid_cumSymptInfect_tested_unvac)

#probability of transmission=0.11
cbind(Label_test_prob.trans0.11=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$NonCovid_cumSymptInfect_tested_vac,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$NonCovid_cumSymptInfect_tested_unvac)


######################################################
#APPENDIX 2: APPENDIX FIGURES 15 & 16
#MODEL VERIFICATION (TARGET ESTIMATES & ESTIMATES BASED ON EQUAL TESTING)

##Evaluate using a higher and lower vaccine efficacy against susceptibility (0.55 & 0.1)

# Load in data
aggDF.ves0.1<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.1.csv",sep="/"))
aggDF.ves0.55<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.55.csv",sep="/"))

#Set-up for figures
ve.susc.0.1=0.1
ve.susc.0.55=0.55
t.start=20
t.end.0.1= 100
t.end.0.55=150

group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

main.line.width=2.5
ribbon.line.width=1
aggDF.ves0.1<-subset(aggDF.ves0.1, t>=t.start & t<=t.end.0.1)
aggDF.ves0.55<-subset(aggDF.ves0.55, t>=t.start & t<=t.end.0.55)

##Figures for vaccine efficacy against susceptibility = 0.55

##Appendix Figure 15a 
#Relative Risk - Cohort Design Target Estimate (Everyone included)
VE.RR.ves0.55_target<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptRR.25., ymax=VE_symptRR.75.), 
                                alpha=0.3, color="skyblue3", fill="skyblue3",linewidth=ribbon.line.width) +
  geom_borderline(linewidth=main.line.width, color="steelblue4", bordercolour="grey20", borderwidth=0.4) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Appendix Figure 15b 
#Odds Ratio - Test-Negative Design Target Estimate (everyone included)
VE.OR.ves0.55_target<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptOR.25., ymax=VE_symptOR.75.), 
                                alpha=0.3, color="skyblue3", fill="skyblue3",linewidth=ribbon.line.width) +
  geom_borderline(linewidth=main.line.width, color="steelblue4", bordercolour="grey20", borderwidth=0.4) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Appendix Figure 15c 
#Relative Risk - Cohort Design (equal testing only)
VE.RR.ves0.55.equal.testing<-ggplot(subset(aggDF.ves0.55, Label_testingDiff== "equal"), aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.25., ymax=VE_symptRR_observed.75.),
              fill="darkgreen", color="darkgreen",alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50.), color="darkgreen",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Appendix Figure 15d 
#Odds Ratio - Test-Negative Design (equal testing only)
VE.OR.ves0.55.equal.testing<-ggplot(subset(aggDF.ves0.55, Label_testingDiff== "equal"), aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.25., ymax=VE_symptOR_observed.75.),
              fill="darkgreen", color="darkgreen",alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50.), color="darkgreen",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))


#set pathway to save figures
appfig15.pathway<-paste(pathway,"figures/appendices_figs/app_fig15",sep="/")

#Save Figures

pdf(file=paste(appfig15.pathway,"AppFig.15a_VE.RR.0.55.Target.pdf",sep="/"), 
    width=7.5, height=6)
VE.RR.ves0.55_target
dev.off()

pdf(file=paste(appfig15.pathway,"AppFig.15b_VE.RR.0.55.Equal.Testing.pdf",sep="/"), 
    width=7.5, height=6)
VE.RR.ves0.55.equal.testing
dev.off()

pdf(file=paste(appfig15.pathway,"AppFig.15c_VE.OR.0.55.Target.pdf",sep="/"), 
    width=7.5, height=6)
VE.OR.ves0.55_target
dev.off()

pdf(file=paste(appfig15.pathway,"AppFig.15d_VE.OR.0.55.Equal.Testing.pdf",sep="/"), 
    width=7.5, height=6)
VE.OR.ves0.55.equal.testing
dev.off()

#Figures for vaccine efficacy against susceptibility = 0.1

##Appendix Figure 16a 
#Relative Risk - Cohort Design Target Estimate (Everyone included)
VE.RR.ves0.1_target<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptRR.25., ymax=VE_symptRR.75.), 
                                alpha=0.3, color="skyblue3", fill="skyblue3",linewidth=ribbon.line.width) +
  geom_borderline(linewidth=main.line.width, color="steelblue4", bordercolour="grey20", borderwidth=0.4) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Appendix Figure 16b 
#Odds Ratio - Test-Negative Design Target Estimate (everyone included)
VE.OR.ves0.1_target<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptOR.25., ymax=VE_symptOR.75.), 
                                alpha=0.3, color="skyblue3", fill="skyblue3",linewidth=ribbon.line.width) +
  geom_borderline(linewidth=main.line.width, color="steelblue4", bordercolour="grey20", borderwidth=0.4) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Appendix Figure 16c 
#Relative Risk - Cohort Design (equal testing only)
VE.RR.ves0.1.equal.testing<-ggplot(subset(aggDF.ves0.1, Label_testingDiff== "equal"), aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.25., ymax=VE_symptRR_observed.75.),
              fill="darkgreen", color="darkgreen",alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50.), color="darkgreen",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Appendix Figure 16d 
#Odds Ratio - Test-Negative Design (equal testing only)
VE.OR.ves0.1.equal.testing<-ggplot(subset(aggDF.ves0.1, Label_testingDiff== "equal"), aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.25., ymax=VE_symptOR_observed.75.),
              fill="darkgreen", color="darkgreen",alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50.), color="darkgreen",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  labs(y="VE against Symptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))


#set pathway to save figures
appfig16.pathway<-paste(pathway,"figures/appendices_figs/app_fig16",sep="/")

#Save Figures
pdf(file=paste(appfig16.pathway,"AppFig.16a_VE.RR.0.1.Target.pdf",sep="/"), 
    width=7.5, height=6)
VE.RR.ves0.1_target
dev.off()

pdf(file=paste(appfig16.pathway,"AppFig.16b_VE.OR.0.1.Target.pdf",sep="/"), 
    width=7.5, height=6)
VE.OR.ves0.1_target
dev.off()

pdf(file=paste(appfig16.pathway,"AppFig.16c_VE.RR.0.1.Equal.Testing.pdf",sep="/"), 
    width=7.5, height=6)
VE.RR.ves0.1.equal.testing
dev.off()

pdf(file=paste(appfig16.pathway,"AppFig.16d_VE.OR.0.1.Equal.Testing.pdf",sep="/"), 
    width=7.5, height=6)
VE.OR.ves0.1.equal.testing
dev.off()
