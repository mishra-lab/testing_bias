################################################

#Appendix Analysis and Figures

################################################

#OVERALL SET-UP
library(tidyverse)
library(ggborderline)
library(data.table)
library(ggpubr)
library(knitr)
library(kableExtra)
library(dplyr)
library(flextable)
library(officer)

#set pathway
setwd("..") #moves back a folder - don't run if your wd is already the data folder
pathway=getwd()

############################
#SUPPLEMENTARY FIGURE 1
#CUMULATIVE PROPORTIONS OF COVID-LIKE ETIOLOGIES (NEVER INFECTED WITH SYMPTOMATIC SARS-COV-2)

#read in data
aggDF.ves0.1<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.1_app.csv",sep="/"))
aggDF.ves0.55<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.55_app.csv",sep="/"))

#Set-up for Figure
t.start=30
t.end.fig1.0.55=150
t.end.fig1.0.1= 100

main.line.width=2.5
ribbon.line.width=1
aggDF.ves0.55<-subset(aggDF.ves0.55, t>=t.start & t<=t.end.fig1.0.55)
aggDF.ves0.1<-subset(aggDF.ves0.1, t>=t.start & t<=t.end.fig1.0.1)

#Supplementary Figure 1a
#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against susceptibility =0.55
plot.cum.uninf.susc0.55<-ggplot(aggDF.ves0.55, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig1.0.55), breaks = seq(t.start,t.end.fig1.0.55,10)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))


#Supplementary Figure 1b
plot.cum.uninf.susc0.1<-ggplot(aggDF.ves0.1, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig1.0.1), breaks = seq(t.start,t.end.fig1.0.1,10)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#PRINT FIGURE 1
ggarrange(plotlist = list(plot.cum.uninf.susc0.55, plot.cum.uninf.susc0.1),
          ncol = 2, nrow = 1)

#set pathway to save figures
appfig1.pathway<-paste(pathway,"figures/appendices_figs/app_fig1",sep="/")

#Save Figures (uncomment to save figures)
# pdf(file=paste(appfig1.pathway,"AppFig.1a_VE.Susc0.55.CumProp.OtherEtiologies.pdf",sep="/"), 
#     width=8, height=6)
# plot.cum.uninf.susc0.55
# dev.off()
# 
# pdf(file=paste(appfig1.pathway,"AppFig.1b_VE.Susc0.1.CumProp.OtherEtiologies.pdf",sep="/"), 
#     width=8, height=6)
# plot.cum.uninf.susc0.1
# dev.off()

######################################
#SUPPLEMENTARY 1: TIMING OF TESTING & SUPPLEMENTARY TABLE 1

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

#####################################
#SUPPLEMENTARY FIGURE 2
#COHORT DESIGN: CUMULATIVE VE AND VE ESTIMATES ACCOUNTING FOR TIME

#The following data and function are also used to create Supplementary Figure 6 &
#Supplementary Tables 2 and 3

#Read in data
df.ves0.1_withTimeAnalysis<-read.csv(paste(pathway,"data/appendices/df.ve.susc0.1_withTimeAnalysis.csv",sep="/"))
df.ves0.55_withTimeAnalysis<-read.csv(paste(pathway,"data/appendices/df.ve.susc0.55_withTimeAnalysis.csv",sep="/"))

#Prepare for figures
ve.susc.0.55=0.55
ve.susc.0.1=0.1
t.start=30
t.end.fig2.0.55=150
t.end.fig2.0.1= 100
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

main.line.width=2.5
ribbon.line.width=1

df.ves0.55_withTimeAnalysis<-subset(df.ves0.55_withTimeAnalysis, t>=t.start & t<=t.end.fig2.0.55)
df.ves0.1_withTimeAnalysis<-subset(df.ves0.1_withTimeAnalysis, t>=t.start & t<=t.end.fig2.0.1)

df.ves0.55_withTimeAnalysis$Label_testingDiff<-factor(df.ves0.55_withTimeAnalysis$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
df.ves0.1_withTimeAnalysis$Label_testingDiff<-factor(df.ves0.1_withTimeAnalysis$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))


##Supplementary Figure 2a
#Cohort Design (Cumulative VE) - Vaccine Efficacy against Susceptibility = 0.55
plot.VE.RR.ves0.55_cumulative<-ggplot(df.ves0.55_withTimeAnalysis, aes(x=t, y=VE_symptRR_observed.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.2.5., ymax=VE_symptRR_observed.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig2.0.55),  breaks = seq(t.start,t.end.fig2.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_RR\n(Cumulative)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))


##Supplementary Figure 2b
#Cohort Design (Cumulative VE) - Vaccine Efficacy against Susceptibility = 0.1
plot.VE.RR.ves0.1_cumulative<-ggplot(df.ves0.1_withTimeAnalysis, aes(x=t, y=VE_symptRR_observed.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.2.5., ymax=VE_symptRR_observed.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig2.0.1),  breaks = seq(t.start,t.end.fig2.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_RR\n(Cumulative)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Supplementary Figure 2c
#Cohort Design (With Time VE) - Vaccine Efficacy against Susceptibility = 0.55
plot.VE.RR.ves0.55_withTime<-ggplot(df.ves0.55_withTimeAnalysis, aes(x=t, y=VE_symptRR_timeDep.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_timeDep.2.5., ymax=VE_symptRR_timeDep.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig2.0.55),  breaks = seq(t.start,t.end.fig2.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_timeDep.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_RR\n(Account for Time)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Supplementary Figure 2d
#Cohort Design (With Time VE) - Vaccine Efficacy against Susceptibility = 0.1
plot.VE.RR.ves0.1_withTime<-ggplot(df.ves0.1_withTimeAnalysis, aes(x=t, y=VE_symptRR_timeDep.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_timeDep.2.5., ymax=VE_symptRR_timeDep.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig2.0.1),  breaks = seq(t.start,t.end.fig2.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_timeDep.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_RR\n(Account for Time)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

#Print Supplementary Figure 2
ggarrange(plotlist = list(plot.VE.RR.ves0.55_cumulative, plot.VE.RR.ves0.1_cumulative, plot.VE.RR.ves0.55_withTime, plot.VE.RR.ves0.1_withTime),
          ncol = 2, nrow = 2)

#set pathway to save figures
appfig2.pathway<-paste(pathway,"figures/appendices_figs/app_fig2",sep="/")

#Save Figures (uncomment to save figures)
# pdf(file=paste(appfig2.pathway,"AppFig.2a_VE.Susc0.55.VE.Cumul.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.RR.ves0.55_cumulative
# dev.off()
# 
# pdf(file=paste(appfig2.pathway,"AppFig.2b_VE.Susc0.1.VE.Cumul.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.RR.ves0.1_cumulative
# dev.off()
# 
# pdf(file=paste(appfig2.pathway,"AppFig.2c_VE.Susc0.55.VE.withTime.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.RR.ves0.55_withTime
# dev.off()
# 
# pdf(file=paste(appfig2.pathway,"AppFig.2d_VE.Susc0.1.VE.withTime.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.RR.ves0.1_withTime
# dev.off()

#######################################################
#SUPPLEMENTARY TABLE 2 AND 3

#Reorder Testing Difference Factor
df.ves0.55_withTimeAnalysis$Label_testingDiff<- factor(df.ves0.55_withTimeAnalysis$Label_testingDiff, 
                                               levels = c("equal", "modUnequal", "highUnequal"))
df.ves0.1_withTimeAnalysis$Label_testingDiff<- factor(df.ves0.1_withTimeAnalysis$Label_testingDiff, 
                                              levels = c("equal", "modUnequal", "highUnequal"))

VE.RR_statsTable_labels <- c(
  "Testing\nScenarios",
  "Time Step\n(Sampling Period End)",
  "Cohort Design VE Est.\n– Without Accounting for Time",
  "Cohort Design VE Est.\n– With Accounting for Time (Cox Model)",
  "Vac. Status Coef.\n (Cox Model)",
  "# Sympt. Tested Pos.\n– SARS-CoV-2 (Unvax)",
  "# Sympt. Tested Pos.\n– SARS-CoV-2 (Vax)"
)

# Function to Build Table for Cohort Design VE Estimates
# Your custom function to generate the flextable
VE.RR_statsTable_function <- function(table.df, VE.S) {
  
  table_data <- data.frame(
    TestingDiff = table.df$Label_testingDiff,
    t = table.df$t,
    
    VE_WithoutTime = paste0(
      sprintf("%.2f", table.df$VE_symptRR_observed.50.), " (",
      sprintf("%.2f", table.df$VE_symptRR_observed.2.5.), " - ",
      sprintf("%.2f", table.df$VE_symptRR_observed.97.5.), ")"
    ),
    
    VE_WithTime = paste0(
      sprintf("%.2f", table.df$VE_symptRR_timeDep.50.), " (",
      sprintf("%.2f", table.df$VE_symptRR_timeDep.2.5.), " - ",
      sprintf("%.2f", table.df$VE_symptRR_timeDep.97.5.), ")"
    ),
    
    VacStatusCoef_WithTime = paste0(
      sprintf("%.2f", table.df$VacStatus_coef_timeDep_VeRR.50.), " (",
      sprintf("%.2f", table.df$VacStatus_coef_timeDep_VeRR.2.5.), " - ",
      sprintf("%.2f", table.df$VacStatus_coef_timeDep_VeRR.97.5.), ")"
    ),
    
    NumCovidTested_unvac = paste0(
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_unvac.50.)), " (",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_unvac.2.5.)), " - ",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_unvac.97.5.)), ")"
    ),
    
    NumCovidTested_vac = paste0(
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_vac.50.)), " (",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_vac.2.5.)), " - ",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_vac.97.5.)), ")"
    ),
    
    stringsAsFactors = FALSE
  )
  
  table_data <- table_data %>% arrange(TestingDiff, t)
  
  
  ft <- flextable(table_data) %>%
    set_header_labels(values = setNames(VE.RR_statsTable_labels, names(table_data))) %>%
    add_header_row(values = paste0("Retrospective Cohort Design; VE.Susc = ", VE.S),
                   colwidths = ncol(table_data)) %>%
    autofit() %>%
    border_remove() %>%
    border(part = "all", border = fp_border(color = "black", width = 0.75)) %>%
    fontsize(size = 6, part = "all") %>%
    align(align = "center", part = "all") %>%
    padding(padding = 0, part = "all") %>%
    width(j = 1:ncol(table_data), width = 1.0) %>%
    line_spacing(space = 1.2, part = "body")
  
  
  return(ft)
}

# Generate the RR table for higher vaccine efficacy against susceptibility
ves0.55_RRtable <- VE.RR_statsTable_function(table.df = df.ves0.55_withTimeAnalysis, VE.S = 0.55)

# Create a landscape Word document
doc <- read_docx() %>%
  set_doc_properties(orientation = "portrait") %>%
  #body_add_par("VE estimates table", style = "heading 1") %>%
  body_add_flextable(ves0.55_RRtable)

# Save to file
#print(doc, target = "ves0.55_Cohort_Stats_Table.docx")

# Generate the table
ves0.1_RRtable <- VE.RR_statsTable_function(table.df = df.ves0.1_withTimeAnalysis, VE.S = 0.1)

# Create a landscape Word document
doc <- read_docx() %>%
  set_doc_properties(orientation = "portrait") %>%
  #body_add_par("VE estimates table", style = "heading 1") %>%
  body_add_flextable(ves0.1_RRtable)

# Save to file
#print(doc, target = "ves0.1_Cohort_Stats_Table.docx")

######################################################

#CODE FOR SUPPLEMENTARY FIGURE 3 AND 4 CAN BE FOUND AFTER SUPPLEMENTARY FIGURE 6
#SUPPLEMENTARY FIGURE 5 WAS CREATED OUTSIDE OF R

#######################################################
#SUPPLEMENTARY FIGURE 6
#TND: CUMULATIVE VE AND VE ESTIMATES ACCOUNTING FOR TIME

#This figure follows Supplementary Figure 2 as the same datasets 
#are used to create Supplementary Figure 6 

#Figure Set-up
ve.susc.0.55=0.55
ve.susc.0.1=0.1
t.start=30
t.end.fig6.0.55=150
t.end.fig6.0.1= 100
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

main.line.width=2.5
ribbon.line.width=1

##Supplementary Figure 6a
#Test-Negative Design (Cumulative VE) - Vaccine Efficacy against Susceptibility = 0.55
plot.VE.OR.ves0.55_cumulative<-ggplot(df.ves0.55_withTimeAnalysis, aes(x=t, y=VE_symptOR_observed.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.2.5., ymax=VE_symptOR_observed.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig6.0.55),  breaks = seq(t.start,t.end.fig6.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR\n(Cumulative)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

##Supplementary Figure 6b
#Test-Negative Design (Cumulative VE) - Vaccine Efficacy against Susceptibility = 0.1
plot.VE.OR.ves0.1_cumulative<-ggplot(df.ves0.1_withTimeAnalysis, aes(x=t, y=VE_symptOR_observed.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.2.5., ymax=VE_symptOR_observed.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig6.0.1),  breaks = seq(t.start,t.end.fig6.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR\n(Cumulative)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

##Supplementary Figure 6c 
#Test-Negative Design (With Time VE) - Vaccine Efficacy against Susceptibility = 0.55
plot.VE.OR.ves0.55_withTime<-ggplot(df.ves0.55_withTimeAnalysis, aes(x=t, y=VE_symptOR_timeDep.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_timeDep.2.5., ymax=VE_symptOR_timeDep.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig6.0.55),  breaks = seq(t.start,t.end.fig6.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_timeDep.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR\n(Account for Time)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

##Supplementary Figure 6d
#Test-Negative Design (With Time VE) - Vaccine Efficacy against Susceptibility = 0.1
plot.VE.OR.ves0.1_withTime<-ggplot(df.ves0.1_withTimeAnalysis, aes(x=t, y=VE_symptOR_timeDep.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_timeDep.2.5., ymax=VE_symptOR_timeDep.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig6.0.1),  breaks = seq(t.start,t.end.fig6.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_timeDep.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR\n(Account for Time)", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

#Print Supplementary Figure 6
ggarrange(plotlist = list(plot.VE.OR.ves0.55_cumulative, plot.VE.OR.ves0.1_cumulative, plot.VE.OR.ves0.55_withTime, plot.VE.OR.ves0.1_withTime),
          ncol = 2, nrow = 2)

#set pathway to save figures
appfig6.pathway<-paste(pathway,"figures/appendices_figs/app_fig6",sep="/")

#Save Figures (uncomment to save figures)
# pdf(file=paste(appfig6.pathway,"AppFig.6a_VE.Susc0.55.VE.Cumul.TND.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.OR.ves0.55_cumulative
# dev.off()
# 
# pdf(file=paste(appfig6.pathway,"AppFig.6b_VE.Susc0.1.VE.Cumul.TND.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.OR.ves0.1_cumulative
# dev.off()
# 
# pdf(file=paste(appfig6.pathway,"AppFig.6c_VE.Susc0.55.VE.withTime.TND.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.OR.ves0.55_withTime
# dev.off()
# 
# pdf(file=paste(appfig6.pathway,"AppFig.6d_VE.Susc0.1.VE.withTime.TND.pdf",sep="/"),
#     width=8, height=6)
# plot.VE.OR.ves0.1_withTime
# dev.off()

###########################################
#SUPPLEMENTARY TABLE 4 AND 5


#Reorder Testing Difference Factor
df.ves0.55_withTimeAnalysis$Label_testingDiff<- factor(df.ves0.55_withTimeAnalysis$Label_testingDiff, 
                                               levels = c("equal", "modUnequal", "highUnequal"))
df.ves0.1_withTimeAnalysis$Label_testingDiff<- factor(df.ves0.1_withTimeAnalysis$Label_testingDiff, 
                                              levels = c("equal", "modUnequal", "highUnequal"))

VE.OR_statsTable_labels <- c(
  "Testing\nScenarios",
  "Time Step\n(Sampling Period End)",
  "TND VE Est.\n– Without Adjusting for Time",
  "TND VE Est.\n– Time-Adjusted Logistic Model",
  "Intercept Coef\n– Time-Adjusted Logistic Model",
  "Vac. Status Coef\n– Time-Adjusted Logistic Model",
  "Time Coef\n– Time-Adjusted Logistic Model",
  "# Sympt. Tested Pos.\n– SARS-CoV-2 (Unvax)",
  "# Sympt. Tested Pos.\n– SARS-CoV-2 (Vax)",
  "# Sympt. Tested Neg.\n– Other Etiologies (Unvax)",
  "# Sympt. Tested Neg.\n– Other Etiologies (Vax)"
)



VE.OR_statsTable_function<-function(table.df, VE.S){
  
  table_data <- data.frame(
    TestingDiff = table.df$Label_testingDiff,
    t = table.df$t,
    
    
    VE_WithoutTime = paste0(
      sprintf("%.2f", table.df$VE_symptOR_observed.50.), " (",
      sprintf("%.2f", table.df$VE_symptOR_observed.2.5.), " - ",
      sprintf("%.2f", table.df$VE_symptOR_observed.97.5.), ")"
    ),
    
    VE_WithTime = paste0(
      sprintf("%.2f", table.df$VE_symptOR_timeDep.50.), " (",
      sprintf("%.2f", table.df$VE_symptOR_timeDep.2.5.), " - ",
      sprintf("%.2f", table.df$VE_symptOR_timeDep.97.5.), ")"
    ),
    
    Intercept_WithTime = paste0(
      sprintf("%.2f", table.df$Intercept_coef_timeDep_VeOR.50.), " (",
      sprintf("%.2f", table.df$Intercept_coef_timeDep_VeOR.2.5.), " - ",
      sprintf("%.2f", table.df$Intercept_coef_timeDep_VeOR.97.5.), ")"
    ),
    
    VacStatus_WithTime = paste0(
      sprintf("%.2f", table.df$VacStatus_coef_timeDep_VeOR.50.), " (",
      sprintf("%.2f", table.df$VacStatus_coef_timeDep_VeOR.2.5.), " - ",
      sprintf("%.2f", table.df$VacStatus_coef_timeDep_VeOR.97.5.), ")"
    ),
    
    Time_WithTime = paste0(
      sprintf("%.2f", table.df$TimeStep_coef_timeDep_VeOR.50.), " (",
      sprintf("%.2f", table.df$TimeStep_coef_timeDep_VeOR.2.5.), " - ",
      sprintf("%.2f", table.df$TimeStep_coef_timeDep_VeOR.97.5.), ")"
    ),
    
    NumCovidTested_unvac = paste0(
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_unvac.50.)), " (",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_unvac.2.5.)), " - ",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_unvac.97.5.)), ")"
    ),
    
    NumCovidTested_vac = paste0(
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_vac.50.)), " (",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_vac.2.5.)), " - ",
      sprintf("%d", round(table.df$Stats_sampleSizeCovid_vac.97.5.)), ")"
    ),
    
    NumNonCovidTested_unvac = paste0(
      sprintf("%d", round(table.df$Stats_sampleSizeNonCovid_unvac.50.)), " (",
      sprintf("%d", round(table.df$Stats_sampleSizeNonCovid_unvac.2.5.)), " - ",
      sprintf("%d", round(table.df$Stats_sampleSizeNonCovid_unvac.97.5.)), ")"
    ),
    
    NumNonCovidTested_vac = paste0(
      sprintf("%d", round(table.df$Stats_sampleSizeNonCovid_vac.50.)), " (",
      sprintf("%d", round(table.df$Stats_sampleSizeNonCovid_vac.2.5.)), " - ",
      sprintf("%d", round(table.df$Stats_sampleSizeNonCovid_vac.97.5.)), ")"
    ),
    
    stringsAsFactors = FALSE
  )
  
  table_data <- table_data %>% arrange(TestingDiff, t)
  
  ft <- flextable(table_data) %>%
    set_header_labels(values = setNames(VE.OR_statsTable_labels, names(table_data))) %>%
    add_header_row(values = paste0("Test Negative Design; VE.Susc = ", VE.S),
                   colwidths = ncol(table_data)) %>%
    autofit() %>%
    border_remove() %>%
    border(part = "all", border = fp_border(color = "black", width = 0.75)) %>%
    fontsize(size = 6, part = "all") %>%
    align(align = "center", part = "all") %>%
    padding(padding = 0, part = "all") %>%
    width(j = 1:ncol(table_data), width = 1.0) %>%
    line_spacing(space = 1.2, part = "body")
  
  return(ft)
}


ves0.55_ORtable <- VE.OR_statsTable_function(table.df = df.ves0.55_withTimeAnalysis, VE.S = 0.55)

# Create a landscape Word document
doc <- read_docx() %>%
  set_doc_properties(orientation = "landscape") %>%
  #body_add_par("VE estimates table", style = "heading 1") %>%
  body_add_flextable(ves0.55_ORtable )

# Save to file
#print(doc, target = "ves0.55_TND_Stats_Table.docx")

ves0.1_ORtable <- VE.OR_statsTable_function(table.df = df.ves0.1_withTimeAnalysis, VE.S = 0.1)

# Create a landscape Word document
doc <- read_docx() %>%
  set_doc_properties(orientation = "landscape") %>%
  #body_add_par("VE estimates table", style = "heading 1") %>%
  body_add_flextable(ves0.1_ORtable)

# Save to file
#print(doc, target = "ves0.1_TND_Stats_Table.docx")


###########################################
#SUPPLEMENTARY FIGURE 3
#TESTING SENSITIVITY - COHORT DESIGN

#Supplementary figure 3 follows Supplementary figure 6 as figure 6 uses 
#the same datasets as supplementary figure 2

#Note that the following data and function below are also used to create Supplementary Figure 7, 8 &
#Supplementary 1: Effects of Testing Sensitivity Analyses

#Read in data (you can also directly read in blow the pre-processed data - df.ves0.55_testSens and df.ves0.55_testSens)
df.ves0.1_highChange.peak<-read.csv(paste(pathway,"data/appendices/df.ves0.1_highChange.peak.csv",sep="/"))
df.ves0.55_highChange.peak<-read.csv(paste(pathway,"data/appendices/df.ves0.55_highChange.peak.csv",sep="/"))

#create factor variable for two epidemic phases:
df.ves0.1_highChange.peak$Label_epiPhase[df.ves0.1_highChange.peak$t == df.ves0.1_highChange.peak$t_highChange]<-"Highest Growth"
df.ves0.1_highChange.peak$Label_epiPhase[df.ves0.1_highChange.peak$t == df.ves0.1_highChange.peak$t_peak]<-"Peak"
df.ves0.1_highChange.peak$Label_epiPhase<-as.factor(df.ves0.1_highChange.peak$Label_epiPhase)

df.ves0.55_highChange.peak$Label_epiPhase[df.ves0.55_highChange.peak$t == df.ves0.55_highChange.peak$t_highChange]<-"Highest Growth"
df.ves0.55_highChange.peak$Label_epiPhase[df.ves0.55_highChange.peak$t == df.ves0.55_highChange.peak$t_peak]<-"Peak"
df.ves0.55_highChange.peak$Label_epiPhase<-as.factor(df.ves0.55_highChange.peak$Label_epiPhase)

#Check timepoint for highest growth and epidemic peak
mean(df.ves0.1_highChange.peak$t_highChange)
mean(df.ves0.1_highChange.peak$t_peak)

mean(df.ves0.55_highChange.peak$t_highChange)
mean(df.ves0.55_highChange.peak$t_peak)

# Function to recalculate VE after sensitivity has been applied
SA_df_generator<-function(df=NA, test.sens=NA){
  #Set the number of vaccinated and unvaccinated (values are those used in the simulations) 
  N.vac=75000
  N.unvac=25000
  
  #create new number of cumulative cases given a test sensitivity value
  #vaccinated
  TP_vac<-df$Covid_cumSymptExpInfect_tested_vac*test.sens #True Positive
  FN_vac<-df$Covid_cumSymptExpInfect_tested_vac*(1-test.sens) #False Negative
  
  #unvaccinated
  TP_unvac<-df$Covid_cumSymptExpInfect_tested_unvac*test.sens #True Positive
  FN_unvac<-df$Covid_cumSymptExpInfect_tested_unvac*(1-test.sens) #False Negative
  
  #Calculation to make sure everything works
  sensitivity_vac=unique(TP_vac/(TP_vac + FN_vac))
  print(sensitivity_vac)
  
  sensitivity_unvac=unique(TP_unvac/(TP_unvac + FN_unvac))
  print(sensitivity_unvac)
  
  #Create new numerators for VE calculation
  df$Covid_cumSymptExpInfect_tested_withTestSens_vac<-TP_vac
  df$Covid_cumSymptExpInfect_tested_withTestSens_unvac<-TP_unvac
  
  #Create new denominators for VE calculation
  df$NonCovid_cumSymptInfect_tested_withTestSens_vac = df$NonCovid_cumSymptInfect_tested_vac + FN_vac
  df$NonCovid_cumSymptInfect_tested_withTestSens_unvac = df$NonCovid_cumSymptInfect_tested_unvac + FN_unvac
  
  #Create False Negative Variable
  df$FalseNeg_withTestSens_vac<-FN_vac
  df$FalseNeg_withTestSens_unvac<-FN_unvac
  
  #VE_RR for Cohort
  df$VE_symptRR_withTestSens_observed<-1-((df$Covid_cumSymptExpInfect_tested_withTestSens_vac/N.vac)/(df$Covid_cumSymptExpInfect_tested_withTestSens_unvac/N.unvac))
  
  #VE_OR for TND
  df$VE_symptOR_withTestSens_observed<-1-((df$Covid_cumSymptExpInfect_tested_withTestSens_vac/df$NonCovid_cumSymptInfect_tested_withTestSens_vac)/(df$Covid_cumSymptExpInfect_tested_withTestSens_unvac/df$NonCovid_cumSymptInfect_tested_withTestSens_unvac))
  
  df$Test_sens<-test.sens
  
  return(df)
}

#Generate testing sensitivity (50%-100%) for vaccine efficacy against susceptibility = 0.1
df.ves0.1_testSens0.5<-SA_df_generator(df=df.ves0.1_highChange.peak,test.sens=0.5)
df.ves0.1_testSens0.7<-SA_df_generator(df=df.ves0.1_highChange.peak,test.sens=0.7)
df.ves0.1_testSens0.9<-SA_df_generator(df=df.ves0.1_highChange.peak,test.sens=0.9)
df.ves0.1_testSens1<-SA_df_generator(df=df.ves0.1_highChange.peak,test.sens=1)

df.ves0.1_testSens<-rbind(df.ves0.1_testSens0.5, df.ves0.1_testSens0.7, df.ves0.1_testSens0.9, df.ves0.1_testSens1)

#Generate testing sensitivity (50%-100%) for vaccine efficacy against susceptibility = 0.55
df.ves0.55_testSens0.5<-SA_df_generator(df=df.ves0.55_highChange.peak,test.sens=0.5)
df.ves0.55_testSens0.7<-SA_df_generator(df=df.ves0.55_highChange.peak,test.sens=0.7)
df.ves0.55_testSens0.9<-SA_df_generator(df=df.ves0.55_highChange.peak,test.sens=0.9)
df.ves0.55_testSens1<-SA_df_generator(df=df.ves0.55_highChange.peak,test.sens=1)

df.ves0.55_testSens<-rbind(df.ves0.55_testSens0.5, df.ves0.55_testSens0.7, df.ves0.55_testSens0.9, df.ves0.55_testSens1)

#Fix underestimation variable for RR and OR
df.ves0.1_testSens$VE_symptOR_degUnderNeg_testSens<--1*(0.1 - df.ves0.1_testSens$VE_symptOR_withTestSens_observed)
df.ves0.1_testSens$VE_symptRR_degUnderNeg_testSens<--1*(0.1 - df.ves0.1_testSens$VE_symptRR_withTestSens_observed)

df.ves0.55_testSens$VE_symptOR_degUnderNeg_testSens<--1*(0.55 - df.ves0.55_testSens$VE_symptOR_withTestSens_observed)
df.ves0.55_testSens$VE_symptRR_degUnderNeg_testSens<--1*(0.55 - df.ves0.55_testSens$VE_symptRR_withTestSens_observed)


#These are the datasets for Nat Comm Data Source Folder

df.ves0.1_testSens_subset<-df.ves0.1_testSens[,c("VE_susc","Label_testingDiff", "Test_sens",
                                                 "Label_epiPhase","VE_symptRR_degUnderNeg_testSens",
                                                 "VE_symptOR_degUnderNeg_testSens",
                                                 "FalseNeg_withTestSens_unvac", "FalseNeg_withTestSens_vac",
                                                 "NonCovid_cumSymptInfect_tested_withTestSens_unvac", "NonCovid_cumSymptInfect_tested_withTestSens_vac",
                                                 "totalNum_sims")]
df.ves0.55_testSens_subset<-df.ves0.55_testSens[,c("VE_susc","Label_testingDiff", "Test_sens",
                                                 "Label_epiPhase","VE_symptRR_degUnderNeg_testSens",
                                                 "FalseNeg_withTestSens_unvac", "FalseNeg_withTestSens_vac",
                                                 "NonCovid_cumSymptInfect_tested_withTestSens_unvac", "NonCovid_cumSymptInfect_tested_withTestSens_vac",
                                                 "VE_symptOR_degUnderNeg_testSens", "totalNum_sims")]

#write.csv(df.ves0.1_testSens_subset, paste0(pathway,"/data/appendices/df.ves0.1_testSens.csv"))
#write.csv(df.ves0.55_testSens_subset, paste0(pathway,"/data/appendices/df.ves0.55_testSens.csv"))

#Can also directly read in data that already has the test sensitivity function applied
#df.ves0.1_testSens<-read.csv(paste0(pathway,"/data/appendices/df.ves0.1_testSens.csv"))
#df.ves0.55_testSens<-read.csv(paste0(pathway,"/data/appendices/df.ves0.55_testSens.csv"))

#Reverse the Factor so Sensitivity runs from Highest to Lowest
df.ves0.1_testSens$Test_sens <- factor(df.ves0.1_testSens$Test_sens, levels = rev(levels(factor(df.ves0.1_testSens$Test_sens))))
df.ves0.55_testSens$Test_sens <- factor(df.ves0.55_testSens$Test_sens, levels = rev(levels(factor(df.ves0.55_testSens$Test_sens))))

#Create an Equal Testing, Moderately Unequal and Highly Unequal Testing Datasets
df.ves0.1_testSens_equalTest<-subset(df.ves0.1_testSens,Label_testingDiff =="equal" )
df.ves0.1_testSens_modUnequalTest<-subset(df.ves0.1_testSens, Label_testingDiff =="modUnequal")
df.ves0.1_testSens_highUnequalTest<-subset(df.ves0.1_testSens, Label_testingDiff =="highUnequal")

df.ves0.55_testSens_equalTest<-subset(df.ves0.55_testSens,Label_testingDiff =="equal" )
df.ves0.55_testSens_modUnequalTest<-subset(df.ves0.55_testSens, Label_testingDiff =="modUnequal")
df.ves0.55_testSens_highUnequalTest<-subset(df.ves0.55_testSens, Label_testingDiff =="highUnequal")

#Set-up for figure
group.colors <- c( "navyblue", "blue", "steelblue2", "lightblue")
names<-c("100", "90", "70", "50")

##Figures of Magnitude of Bias in VE estimates related to Test Sensitivity
#Cohort Design

##Supplementary Figure 3a 
#Moderately Unequal Testing - vaccine efficacy against susceptibility=0.55
plot.ve.susc0.55_test.sens_modUnequalTest_cohort<-ggplot(df.ves0.55_testSens_modUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptRR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_RR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")


##Supplementary Figure 3b
#Moderately Unequal Testing - vaccine efficacy against susceptibility=0.1
plot.ve.susc0.1_test.sens_modUnequalTest_cohort<-ggplot(df.ves0.1_testSens_modUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptRR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_RR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

##Supplementary Figure 3c
#Highly Unequal Testing - vaccine efficacy against susceptibility=0.55
plot.ve.susc0.55_test.sens_highUnequalTest_cohort<-ggplot(df.ves0.55_testSens_highUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptRR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_RR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none") 

##Supplementary Figure 3d
#Highly Unequal Testing - vaccine efficacy against susceptibility=0.1
plot.ve.susc0.1_test.sens_highUnequalTest_cohort<-ggplot(df.ves0.1_testSens_highUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptRR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of\nBias in VE_RR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

#Print Supplementary Figure 3
ggarrange(plotlist = list(plot.ve.susc0.55_test.sens_modUnequalTest_cohort, plot.ve.susc0.1_test.sens_modUnequalTest_cohort, 
                          plot.ve.susc0.55_test.sens_highUnequalTest_cohort, plot.ve.susc0.1_test.sens_highUnequalTest_cohort),
          ncol = 2, nrow = 2)

#set pathway to save figures
appfig3.pathway<-paste(pathway,"figures/appendices_figs/app_fig3",sep="/")

#Save Figures
# pdf(file=paste(appfig3.pathway,"AppFig.3a_VE.Susc0.55.ModUnequalTest.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.ve.susc0.55_test.sens_modUnequalTest_cohort
# dev.off()
# 
# pdf(file=paste(appfig3.pathway,"AppFig.3b_VE.Susc0.1.ModUnequalTest.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.ve.susc0.1_test.sens_modUnequalTest_cohort
# dev.off()
# 
# pdf(file=paste(appfig3.pathway,"AppFig.3c_VE.Susc0.55.HighUnequalTest.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.ve.susc0.55_test.sens_highUnequalTest_cohort
# dev.off()
# 
# pdf(file=paste(appfig3.pathway,"AppFig.3d_VE.Susc0.1.HighUnequalTest.CohortDesign.pdf",sep="/"),
#     width=8, height=6)
# plot.ve.susc0.1_test.sens_highUnequalTest_cohort
# dev.off()

######################################

#CODE FOR SUPPLEMENTARY FIGURE 4 CAN BE FOUND AFTER SUPPLEMENTARY FIGURE 7 & 8
#AND SUPPLEMENTARY 1: EFFECTS OF TESTING SENSITIVITY

######################################
#SUPPLEMENTARY FIGURE 7
#TESTING SENSITIVITY WITH UNEQUAL TESTING - TEST-NEGATIVE DESIGN 

#This figure follows Supplementary Figure 3 as the data and function used to create Supplementary Figure 7
#is created above in the Supplementary Figure 3 section 

#Set-up for figure
group.colors <- c( "navyblue", "blue", "steelblue2", "lightblue")
names<-c("100", "90", "70", "50")

##Supplementary Figure 7a
#Moderately Unequal Testing - vaccine efficacy against susceptibility=0.55
plot.ve.susc0.55_test.sens_modUnequalTest_tnd<-ggplot(df.ves0.55_testSens_modUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptOR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

##Supplementary Figure 7b
#Moderately Unequal Testing - vaccine efficacy against susceptibility=0.1
plot.ve.susc0.1_test.sens_modUnequalTest_tnd<-ggplot(df.ves0.1_testSens_modUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptOR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

##Supplementary Figure 7c
#Highly Unequal Testing - vaccine efficacy against susceptibility=0.55
plot.ve.susc0.55_test.sens_highUnequalTest_tnd<-ggplot(df.ves0.55_testSens_highUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptOR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

##Supplementary Figure 7d
#Highly Unequal Testing - vaccine efficacy against susceptibility=0.1
plot.ve.susc0.1_test.sens_highUnequalTest_tnd<-ggplot(df.ves0.1_testSens_highUnequalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptOR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

#Print Supplementary Figure 7
ggarrange(plotlist = list(plot.ve.susc0.55_test.sens_modUnequalTest_tnd, plot.ve.susc0.1_test.sens_modUnequalTest_tnd, 
                          plot.ve.susc0.55_test.sens_highUnequalTest_tnd, plot.ve.susc0.1_test.sens_highUnequalTest_tnd),
          ncol = 2, nrow = 2)

#set pathway to save figures
appfig7.pathway<-paste(pathway,"figures/appendices_figs/app_fig7",sep="/")

#Save Figures
# pdf(file=paste(appfig7.pathway,"AppFig.7a_VE.Susc0.55.ModUnequalTest.TND.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.55_test.sens_modUnequalTest_tnd
# dev.off()
# 
# pdf(file=paste(appfig7.pathway,"AppFig.7b_VE.Susc0.1.ModUnequalTest.TND.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.1_test.sens_modUnequalTest_tnd
# dev.off()
# 
# pdf(file=paste(appfig7.pathway,"AppFig.7c_VE.Susc0.55.HighUnequalTest.TND.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.55_test.sens_highUnequalTest_tnd
# dev.off()
# 
# pdf(file=paste(appfig7.pathway,"AppFig.7d_VE.Susc0.1.HighUnequalTest.TND.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.1_test.sens_highUnequalTest_tnd
# dev.off()

#Extra Analysis to see the Effect Size of Testing Sensitivity
###Measure the Maximum Size of the Effect of Testing Sensitivity (Between 50% and 100%) - Highly Unequal Testing At Epidemic Peak 

#Efficacy against Susceptibility = 0.1
VE_underestimate.ves0.1.peak.testSens.0.5<-df.ves0.1_testSens_highUnequalTest$VE_symptOR_degUnderNeg_testSens[df.ves0.1_testSens_highUnequalTest$Label_epiPhase=="Peak" & df.ves0.1_testSens_highUnequalTest$Test_sens=="0.5"]
VE_underestimate.ves0.1.peak.testSens.1<-df.ves0.1_testSens_highUnequalTest$VE_symptOR_degUnderNeg_testSens[df.ves0.1_testSens_highUnequalTest$Label_epiPhase=="Peak" & df.ves0.1_testSens_highUnequalTest$Test_sens=="1"]

mean(VE_underestimate.ves0.1.peak.testSens.1) - mean(VE_underestimate.ves0.1.peak.testSens.0.5)

#Efficacy against Susceptibility = 0.55
VE_underestimate.ves0.55.peak.testSens.0.5<-df.ves0.55_testSens_highUnequalTest$VE_symptOR_degUnderNeg_testSens[df.ves0.55_testSens_highUnequalTest$Label_epiPhase=="Peak" & df.ves0.55_testSens_highUnequalTest$Test_sens=="0.5"]
VE_underestimate.ves0.55.peak.testSens.1<-df.ves0.55_testSens_highUnequalTest$VE_symptOR_degUnderNeg_testSens[df.ves0.55_testSens_highUnequalTest$Label_epiPhase=="Peak" & df.ves0.55_testSens_highUnequalTest$Test_sens=="1"]

mean(VE_underestimate.ves0.55.peak.testSens.1 - VE_underestimate.ves0.55.peak.testSens.0.5)

#The difference between VE estimates is less than 0.05

######################################
#SUPPLEMENTARY FIGURE 8
#TESTING SENSITIVITY WITH EQUAL TESTING - TEST-NEGATIVE DESIGN 

#This section is out of order as the data and function used to create Supplementary Figure 8
#is created in the Appendix Figure 2 section

#Reverse the Factor so Sensitivity runs from Highest to Lowest
df.ves0.1_testSens$Test_sens <- factor(df.ves0.1_testSens$Test_sens, levels = rev(levels(factor(df.ves0.1_testSens$Test_sens))))
df.ves0.55_testSens$Test_sens <- factor(df.ves0.55_testSens$Test_sens, levels = rev(levels(factor(df.ves0.55_testSens$Test_sens))))

#Create an Equal Testing Datasets
df.ves0.1_testSens_equalTest<-subset(df.ves0.1_testSens,Label_testingDiff =="equal" )
df.ves0.55_testSens_equalTest<-subset(df.ves0.55_testSens,Label_testingDiff =="equal" )
 
#Set-up for figure
group.colors <- c( "navyblue", "blue", "steelblue2", "lightblue")
names<-c("100", "90", "70", "50")

##Supplementary Figure 8a
#Equal Testing - vaccine efficacy against susceptibility=0.55
plot.ve.susc0.55_test.sens_equalTest_tnd<-ggplot(df.ves0.55_testSens_equalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptOR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")


##Supplementary Figure 8b
#Equal Testing - vaccine efficacy against susceptibility=0.1
plot.ve.susc0.1_test.sens_equalTest_tnd<-ggplot(df.ves0.1_testSens_equalTest,
aes(x=as.factor(Label_epiPhase) ,y=VE_symptOR_degUnderNeg_testSens, fill=as.factor(Test_sens))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=as.factor(Test_sens)), trim=FALSE, position = position_dodge(width=0.92),
              width=0.9, scale="width", color="grey70") +   
  geom_boxplot(
    aes(fill=as.factor(Test_sens)), width = 0.3,
    position = position_dodge(0.92)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.7, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Phase of Epidemic") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Test\nSensitivity (%)"))  +
  theme(legend.position = "none")

#Print Supplementary Figure 8
ggarrange(plotlist = list(plot.ve.susc0.55_test.sens_equalTest_tnd, plot.ve.susc0.1_test.sens_equalTest_tnd),
          ncol = 2, nrow = 1) 

#set pathway to save figures
appfig8.pathway<-paste(pathway,"figures/appendices_figs/app_fig8",sep="/")

# pdf(file=paste(appfig8.pathway,"AppFig.8a_VE.Susc0.55.EqualTest.TND.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.55_test.sens_equalTest_tnd
# dev.off()
# 
# pdf(file=paste(appfig8.pathway,"AppFig.8b_VE.Susc0.1.EqualTest.TND.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.1_test.sens_equalTest_tnd
# dev.off()

######################################
#SUPPLEMENTARY 1: EFFECTS OF TESTING SENSITIVITY

#This section is out of order as the data and function used to create these analyses
#is created in the Appendix Figure 2 section

#Isolate highly unequal testing scenarios at the epidemic peak
df.ves0.1_lowTestSens_highUnequalTest_Peak<-subset(df.ves0.1_testSens_highUnequalTest,Label_epiPhase=="Peak" & Test_sens == "0.5")
df.ves0.55_lowTestSens_highUnequalTest_Peak<-subset(df.ves0.55_testSens_highUnequalTest,Label_epiPhase=="Peak" & Test_sens == "0.5")

#Calculate the contributions of the false negatives to the total number of controls across higher and lower efficacy when testing is uneqal

#Efficacy against Susceptibility=0.55
#Vac
mean(df.ves0.55_lowTestSens_highUnequalTest_Peak$FalseNeg_withTestSens_vac)/mean(df.ves0.55_lowTestSens_highUnequalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_vac)
#Unvac
mean(df.ves0.55_lowTestSens_highUnequalTest_Peak$FalseNeg_withTestSens_unvac)/mean(df.ves0.55_lowTestSens_highUnequalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_unvac)

#Efficacy against Susceptibility=0.1
#Vac
mean(df.ves0.1_lowTestSens_highUnequalTest_Peak$FalseNeg_withTestSens_vac)/mean(df.ves0.1_lowTestSens_highUnequalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_vac)
#Unvac
mean(df.ves0.1_lowTestSens_highUnequalTest_Peak$FalseNeg_withTestSens_unvac)/mean(df.ves0.1_lowTestSens_highUnequalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_unvac)


#Calculate the contributions of the false negatives to the total number of controls across higher and lower efficacy when testing is equal
df.ves0.55_lowTestSens_equalTest_Peak<-subset(df.ves0.55_testSens_equalTest,Label_epiPhase=="Peak" & Test_sens == "0.5")
df.ves0.1_lowTestSens_equalTest_Peak<-subset(df.ves0.1_testSens_equalTest,Label_epiPhase=="Peak" & Test_sens == "0.5")


#Efficacy against Susceptibility=0.55
#Vac
mean(df.ves0.55_lowTestSens_equalTest_Peak$FalseNeg_withTestSens_vac)/mean(df.ves0.55_lowTestSens_equalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_vac)
#Unvac
mean(df.ves0.55_lowTestSens_equalTest_Peak$FalseNeg_withTestSens_unvac)/mean(df.ves0.55_lowTestSens_equalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_unvac)

#Efficacy against Susceptibility=0.1
#Vac
mean(df.ves0.1_lowTestSens_equalTest_Peak$FalseNeg_withTestSens_vac)/mean(df.ves0.1_lowTestSens_equalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_vac)
#Unvac
mean(df.ves0.1_lowTestSens_equalTest_Peak$FalseNeg_withTestSens_unvac)/mean(df.ves0.1_lowTestSens_equalTest_Peak$NonCovid_cumSymptInfect_tested_withTestSens_unvac)

######################################
#SUPPLEMENTARY FIGURE 4
#MISCLASSIFICATION IN TEST-NEGATIVE DESIGN

##Evaluate using a higher and lower vaccine efficacy against susceptibility (0.55 & 0.1)

# Load in data
aggDF.ves0.1<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.1_app.csv",sep="/"))
aggDF.ves0.55<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.55_app.csv",sep="/"))

#Set-up for figure
ve.susc.0.1=0.1
ve.susc.0.55=0.55
t.start=30
t.end.fig4.0.1= 100
t.end.fig4.0.55=150
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

main.line.width=2.5
ribbon.line.width=1

aggDF.ves0.55<-subset(aggDF.ves0.55, t>=t.start & t<=t.end.fig4.0.55)
aggDF.ves0.1<-subset(aggDF.ves0.1, t>=t.start & t<=t.end.fig4.0.1)

aggDF.ves0.55$Label_testingDiff<-factor(aggDF.ves0.55$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
aggDF.ves0.1$Label_testingDiff<-factor(aggDF.ves0.1$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))


##Appendix Figure 4a
#Cumulative False Negative by vaccination status for vaccine efficacy against susceptibility=0.55
Cum.Prop.False.Neg.0.55<-ggplot(aggDF.ves0.55, aes(x=t, y=CumProp_falseNeg_unvac.50.)) +
  theme_classic() +
  geom_ribbon(aes(ymin=CumProp_falseNeg_unvac.2.5., ymax=CumProp_falseNeg_unvac.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig4.0.55),  breaks = seq(t.start,t.end.fig4.0.55,10)) + 
  scale_y_continuous(limits = c(0, 0.16), breaks= seq(0, 0.15, 0.05)) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_unvac.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  geom_ribbon(aes(ymin=CumProp_falseNeg_vac.2.5., ymax=CumProp_falseNeg_vac.97.5.),
              fill="lightgoldenrod4", color="lightgoldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_vac.50.), color="lightgoldenrod4",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="Cumulative Proportion of Misclassification\ndue to Missed Detection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")

##Appendix Figure 4b
#Cumulative False Negative by vaccination status for vaccine efficacy against susceptibility=0.1
Cum.Prop.False.Neg.0.1<-ggplot(aggDF.ves0.1, aes(x=t, y=CumProp_falseNeg_unvac.50.)) +
  theme_classic() +
  geom_ribbon(aes(ymin=CumProp_falseNeg_unvac.2.5., ymax=CumProp_falseNeg_unvac.97.5., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig4.0.1),  breaks = seq(t.start,t.end.fig4.0.1,10)) + 
  scale_y_continuous(limits = c(0, 0.15), breaks= seq(0, 0.15, 0.05)) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_unvac.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  geom_ribbon(aes(ymin=CumProp_falseNeg_vac.2.5., ymax=CumProp_falseNeg_vac.97.5.),
              fill="lightgoldenrod4", color="lightgoldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=CumProp_falseNeg_vac.50.), color="lightgoldenrod4",
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="Cumulative Proportion of Misclassification\ndue to Missed Detection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")

#Print Supplementary Figure 4
ggarrange(plotlist = list(Cum.Prop.False.Neg.0.55, Cum.Prop.False.Neg.0.1),
          ncol = 2, nrow = 1)

#set pathway to save figures
appfig4.pathway<-paste(pathway,"figures/appendices_figs/app_fig4&5",sep="/")

#Save Figures (warning occurs as we include only a certain time range)
# pdf(file=paste(appfig4.pathway,"AppFig.4a_CumPropFalseNeg.0.55.pdf",sep="/"), 
#     width=7.5, height=6)
# Cum.Prop.False.Neg.0.55
# dev.off()
# 
# pdf(file=paste(appfig4.pathway,"AppFig.4b_CumPropFalseNeg.0.1.pdf",sep="/"), 
#     width=7.5, height=6)
# Cum.Prop.False.Neg.0.1
# dev.off()

#####################################################
#SUPPLEMENTARY FIGURE 5
#MISCLASSIFICATION EXAMPLE 
#FIGURE CREATED OUTSIDE THE R ENVIRONMENT

####################################################
#REMINDER - SUPPLEMENTARY FIGURE 7, SUPPLEMENTARY FIGURE 8 &
#SUPPLEMENTARY 1: EFFECTS OF TESTING SENSITIVITY CODE
#CAN BE FOUND ABOVE NEAR THE CODE FOR APPENDIX FIGURE 2 

######################################################
#SUPPLEMENTARY FIGURE 9
#OTHER ETIOLOGIES SENSITIVITY ANALYSIS FOR TEST-NEGATIVE DESIGN

#Load in data for etiologies 
df.ve.susc0.1_other.etiologies.sens.high<-read.csv(paste(pathway,"data/appendices/df.ve.susc0.1_other.etiologies.sens.high.csv",sep="/"))
df.ve.susc0.55_other.etiologies.sens.high<-read.csv(paste(pathway,"data/appendices/df.ve.susc0.55_other.etiologies.sens.high.csv",sep="/"))

#Make sure an outbreak actually occurs in all scenarios (defined as >0.001)
unique(df.ve.susc0.1_other.etiologies.sens.high$Covid_cumPropSymptInfect_highChange >0.001)
unique(df.ve.susc0.55_other.etiologies.sens.high$Covid_cumPropSymptInfect_highChange >0.001)

#Make sure Testing Variable is a factor variable
df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff<-factor(df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
df.ve.susc0.55_other.etiologies.sens.high$Label_testingDiff<-factor(df.ve.susc0.55_other.etiologies.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

#Average timepoint at the highest epidemic growth rate
median(df.ve.susc0.1_other.etiologies.sens.high$t_highChange)
median(df.ve.susc0.55_other.etiologies.sens.high$t_highChange)

##Supplementary Figure 9a
#Other Etiologies Sensitivity Analysis for vaccine efficacy against susceptibility = 0.55
plot.ve.susc0.55_other.etiologies.sens<-ggplot(df.ve.susc0.55_other.etiologies.sens.high,aes(x=as.factor(NonCovid_propSympt) ,y=VE_symptOR_degUnderNeg_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01, color="grey70") +   
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
        legend.title = element_text(size=22)) + xlab("Prevalence of COVID-like Symptoms\n(due to Other Etiologies)") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Testing Scenarios"))  + theme(legend.position="none")


##Supplementary Figure 9b
#Other Etiologies Sensitivity Analysis for vaccine efficacy against susceptibility = 0.55
plot.ve.susc0.1_other.etiologies.sens<-ggplot(df.ve.susc0.1_other.etiologies.sens.high,aes(x=as.factor(NonCovid_propSympt) ,y=VE_symptOR_degUnderNeg_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01, color="grey70") +   
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
        legend.title = element_text(size=22)) + xlab("Prevalence of COVID-like Symptoms\n(due to Other Etiologies)") +
  ylab("Extent of Bias\nin VE_OR") +
  guides(fill=guide_legend(title="Testing Scenarios"))  + theme(legend.position="none")


#Print Supplementary Figure 9
ggarrange(plotlist = list(plot.ve.susc0.55_other.etiologies.sens, plot.ve.susc0.1_other.etiologies.sens),
          ncol = 2, nrow = 1) 

#set pathway to save figures
appfig9.pathway<-paste(pathway,"figures/appendices_figs/app_fig9",sep="/")

#Save Figures
# pdf(file=paste(appfig9.pathway,"AppFig.9a_VE.Susc0.55.OtherEtiologies.pdf",sep="/"),
#     width=8, height=6)
# plot.ve.susc0.55_other.etiologies.sens
# dev.off()
# 
# pdf(file=paste(appfig9.pathway,"AppFig.9b_VE.Susc0.1.OtherEtiologies.pdf",sep="/"), 
#     width=8, height=6)
# plot.ve.susc0.1_other.etiologies.sens
# dev.off()

###Measure Difference Across Prevalence of SARS-CoV-2 symptoms

low.prevalence<-mean(df.ve.susc0.1_other.etiologies.sens.high$VE_symptOR_degUnderNeg_highChange_trueVeff[df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="highUnequal" & df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.05])
high.prevalence<-mean(df.ve.susc0.1_other.etiologies.sens.high$VE_symptOR_degUnderNeg_highChange_trueVeff[df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="highUnequal" & df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.2])

low.prevalence - high.prevalence

#Adjusts the magnitude of bias due to testing differences by over 0.1 at the highest epidemic growth point

######################################################
#SUPPLEMENTARY 1: PROPORTIONAL INCREASE CALCULATIONS
#BY VACCINATION STATUS WITH COVID-LIKE SYMPTOMS DUE TO ALTERNATE ETIOLOGIES

#Uses same data as Supplementary Figure 9

#Mean cumulative numbers of those vaccinated and testing "negative" in the Highly Unequal Testing Scenario  given low and high prevalances
low.vac.unequal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_vac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.05 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="highUnequal"]))
low.vac.unequal
high.vac.unequal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_vac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.2 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="highUnequal"]))
high.vac.unequal

#Proportional Increase for Vaccinated
(high.vac.unequal - low.vac.unequal)/low.vac.unequal 

#Mean cumulative numbers of those unvaccinated and testing "negative" in the Highly Unequal Testing Scenario given low and high prevalances
low.unvac.unequal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_unvac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.05 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="highUnequal"]))
low.unvac.unequal
high.unvac.unequal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_unvac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.2 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="highUnequal"]))
high.unvac.unequal

#Proportional Increase for Unvaccinated
(high.unvac.unequal - low.unvac.unequal)/low.unvac.unequal

#Mean cumulative numbers of those unvaccinated and testing "negative" in the Equal Testing Scenario given low and high prevalances
low.vac.equal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_vac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.05 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="equal"]))
low.vac.equal
high.vac.equal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_vac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.2 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="equal"]))
high.vac.equal

#Proportional Increase for Vaccinated
(high.vac.equal - low.vac.equal)/low.vac.equal

#Mean cumulative numbers of those unvaccinated and testing "negative" in the Equal Testing Scenario given low and high prevalances
low.unvac.equal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_unvac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.05 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="equal"]))
low.unvac.equal
high.unvac.equal<-round(mean(df.ve.susc0.1_other.etiologies.sens.high$NonCovid_cumSymptInfect_tested_unvac[df.ve.susc0.1_other.etiologies.sens.high$NonCovid_propSympt==0.2 & df.ve.susc0.1_other.etiologies.sens.high$Label_testingDiff=="equal"]))
high.unvac.equal

#Proportional Increase for Unvaccinated
(high.unvac.equal - low.unvac.equal)/low.unvac.equal


##############################
#SUPPLEMENTARY FIGURE 10
#VE AGAINST SUSCEPTIBILITY SENSITIVITY ANALYSIS APPENDIX

#Load in Data
df.ve.susc.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.sens.csv",sep="/"))

#Separate by level of vaccine efficacy against susceptibility
#shows how many n (i.e. epidemic realizations are being used for each sub-plot)
df.ve.susc.sens.agg %>%
  select(VE_susc, totalNum_sims) %>%
  distinct() %>%
  arrange(VE_susc, totalNum_sims)

#Set-up for figures
t.start=30
t.end.fig10.0.1= 110
t.end.fig10.0.3= 130
t.end.fig10.0.5= 150
t.end.fig10.0.7= 190
t.end.fig10.0.9= 390
main.line.width=1
ribbon.line.width=0.5

#Separate by level of vaccine efficacy against susceptibility
df.ve.susc.sens.0.1<-subset(df.ve.susc.sens.agg, VE_susc==0.1 & t>=t.start & t<=t.end.fig10.0.1)
df.ve.susc.sens.0.3<-subset(df.ve.susc.sens.agg, VE_susc==0.3 & t>=t.start & t<=t.end.fig10.0.3)
df.ve.susc.sens.0.5<-subset(df.ve.susc.sens.agg, VE_susc==0.5 & t>=t.start & t<=t.end.fig10.0.5)
df.ve.susc.sens.0.7<-subset(df.ve.susc.sens.agg, VE_susc==0.7 & t>=t.start & t<=t.end.fig10.0.7)
df.ve.susc.sens.0.9<-subset(df.ve.susc.sens.agg, VE_susc==0.9 & t>=t.start & t<=t.end.fig10.0.9)

##Sub-figures for Supplementary Figure 10
#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against susceptibility=0.1
plot.cum.inf.ve.susc.0.1<-ggplot(df.ve.susc.sens.0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.1), breaks = seq(t.start,t.end.fig10.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.3), breaks = seq(t.start,t.end.fig10.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.5), breaks = seq(t.start,t.end.fig10.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.7), breaks = seq(t.start,t.end.fig10.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.9), breaks = seq(t.start,t.end.fig10.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2)  
#when vaccine efficacy against susceptibility = 0.1
plot.cum.uninf.ve.susc.0.1<-ggplot(df.ve.susc.sens.0.1, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.1), breaks = seq(t.start,t.end.fig10.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.3), breaks = seq(t.start,t.end.fig10.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.5), breaks = seq(t.start,t.end.fig10.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.7), breaks = seq(t.start,t.end.fig10.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig10.0.9), breaks = seq(t.start,t.end.fig10.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank()) +
  theme(legend.position = "none")

#Combine sub-figures
plotlist.ve.susc<- list(plot.cum.inf.ve.susc.0.1, plot.cum.inf.ve.susc.0.3, plot.cum.inf.ve.susc.0.5, 
                        plot.cum.inf.ve.susc.0.7, plot.cum.inf.ve.susc.0.9, 
                        plot.cum.uninf.ve.susc.0.1, plot.cum.uninf.ve.susc.0.3,plot.cum.uninf.ve.susc.0.5,
                        plot.cum.uninf.ve.susc.0.7,plot.cum.uninf.ve.susc.0.9)

ggarrange(plotlist = plotlist.ve.susc, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig10.pathway<-paste(pathway,"figures/appendices_figs/app_fig10",sep="/")

#Save Figure 10 (warnings are just because we are only plotting part of the time we have recorded; uncomment to save figures)
#ggarrange(plotlist = plotlist.ve.susc, ncol = 5, nrow = 2) %>%
#  ggexport(filename = paste(appfig10.pathway,"AppFig.10_VE.Susc.Infect.Dynamics.Fig.pdf",sep="/"))

###########################################
#SUPPLEMENTARY FIGURE 11
#VE AGAINST INFECTIOUSNESS SENSITIVITY ANALYSIS APPENDIX

##Higher vaccine efficacy against susceptibility (0.55)

#Load in data
df.ve.susc.0.55_ve.infect.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.55_ve.infect.sens.csv",sep="/"))

#shows how many n (i.e. epidemic realizations are being used for each sub-plot)
df.ve.susc.0.55_ve.infect.sens.agg %>%
  select(VE_infect, totalNum_sims) %>%
  distinct() %>%
  arrange(VE_infect, totalNum_sims)

#Set-up for figures
t.start=30
t.end.fig11.0.1= 150
t.end.fig11.0.3= 190
t.end.fig11.0.5= 230
t.end.fig11.0.7= 270
t.end.fig11.0.9= 630
main.line.width=1
ribbon.line.width=0.5

#Separate by level of vaccine efficacy against infectiousness
df.ve.susc.0.55_ve.infect.sens.0.1<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.1 & t>=t.start & t<=t.end.fig11.0.1)
df.ve.susc.0.55_ve.infect.sens.0.3<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.3 & t>=t.start & t<=t.end.fig11.0.3)
df.ve.susc.0.55_ve.infect.sens.0.5<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.5 & t>=t.start & t<=t.end.fig11.0.5)
df.ve.susc.0.55_ve.infect.sens.0.7<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.7 & t>=t.start & t<=t.end.fig11.0.7)
df.ve.susc.0.55_ve.infect.sens.0.9<-subset(df.ve.susc.0.55_ve.infect.sens.agg, VE_infect==0.9 & t>=t.start & t<=t.end.fig11.0.9)

##Sub-figures for Supplementary Figure 11
#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.1
plot.cum.inf.susc0.55.ve.infect.0.1<-ggplot(df.ve.susc.0.55_ve.infect.sens.0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.1), breaks = seq(t.start,t.end.fig11.0.1,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.3), breaks = seq(t.start,t.end.fig11.0.3,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.5), breaks = seq(t.start,t.end.fig11.0.5,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.7), breaks = seq(t.start,t.end.fig11.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.9), breaks = seq(t.start,t.end.fig11.0.9,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.1), breaks = seq(t.start,t.end.fig11.0.1,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.3), breaks = seq(t.start,t.end.fig11.0.3,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.5), breaks = seq(t.start,t.end.fig11.0.5,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.7), breaks = seq(t.start,t.end.fig11.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig11.0.9), breaks = seq(t.start,t.end.fig11.0.9,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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

ggarrange(plotlist = plotlist.ve.susc0.55.ve.infect, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig11.pathway<-paste(pathway,"figures/appendices_figs/app_fig11&12",sep="/")

#Save Figure 11 (warnings are just because we are only plotting part of the time we have recorded; uncomment to save figure)
# ggarrange(plotlist = plotlist.ve.susc0.55.ve.infect, ncol = 5, nrow = 2) %>%
#   ggexport(filename = paste(appfig11.pathway,"AppFig.11_VE.Susc0.55_VE.Infect.Combo.pdf",sep="/"))

###########################################
#SUPPLEMENTARTY FIGURE 12
#VE AGAINST INFECTIOUSNESS SENSITIVITY ANALYSIS APPENDIX

##Lower vaccine efficacy against susceptibility (0.1)

#Load data
df.ve.susc.0.1_ve.infect.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_ve.infect.sens.csv",sep="/"))


#shows how many n (i.e. epidemic realizations are being used for each sub-plot)
df.ve.susc.0.1_ve.infect.sens.agg %>%
  select(VE_infect, totalNum_sims) %>%
  distinct() %>%
  arrange(VE_infect, totalNum_sims)

#Set-up for figures
t.start=30
t.end.fig12.0.1= 110
t.end.fig12.0.3= 130
t.end.fig12.0.5= 150
t.end.fig12.0.7= 190
t.end.fig12.0.9= 390
main.line.width=1
ribbon.line.width=0.5

#Separate by level of vaccine efficacy against infectiousness
df.ve.susc.0.1_ve.infect.sens.0.1<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.1 & t>=t.start & t<=t.end.fig12.0.1)
df.ve.susc.0.1_ve.infect.sens.0.3<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.3 & t>=t.start & t<=t.end.fig12.0.3)
df.ve.susc.0.1_ve.infect.sens.0.5<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.5 & t>=t.start & t<=t.end.fig12.0.5)
df.ve.susc.0.1_ve.infect.sens.0.7<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.7 & t>=t.start & t<=t.end.fig12.0.7)
df.ve.susc.0.1_ve.infect.sens.0.9<-subset(df.ve.susc.0.1_ve.infect.sens.agg, VE_infect==0.9 & t>=t.start & t<=t.end.fig12.0.9)

##Sub-figures for Supplementary Figure 12
#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.1
plot.cum.inf.susc0.1.ve.infect.0.1<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.1), breaks = seq(t.start,t.end.fig12.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when vaccine efficacy against infectiousness=0.3
plot.cum.inf.susc0.1.ve.infect.0.3<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.3, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.3), breaks = seq(t.start,t.end.fig12.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.5), breaks = seq(t.start,t.end.fig12.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.7), breaks = seq(t.start,t.end.fig12.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.9), breaks = seq(t.start,t.end.fig12.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.1), breaks = seq(t.start,t.end.fig12.0.1,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when vaccine efficacy against infectiousness=0.3
plot.cum.uninf.susc0.1.ve.infect.0.3<-ggplot(df.ve.susc.0.1_ve.infect.sens.0.3, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.3), breaks = seq(t.start,t.end.fig12.0.3,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.5), breaks = seq(t.start,t.end.fig12.0.5,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.7), breaks = seq(t.start,t.end.fig12.0.7,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig12.0.9), breaks = seq(t.start,t.end.fig12.0.9,60)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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

ggarrange(plotlist = plotlist.ve.susc0.1.ve.infect, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig12.pathway<-paste(pathway,"figures/appendices_figs/app_fig11&12",sep="/")

#Save Figure 12 (warnings are just because we are only plotting part of the time we have recorded; uncomment to save figures)
# ggarrange(plotlist = plotlist.ve.susc0.1.ve.infect, ncol = 5, nrow = 2) %>%
#   ggexport(filename = paste(appfig12.pathway,"AppFig.12_VE.Susc0.1_VE.Infect.Combo.pdf",sep="/"))

###########################################
#CONTENT FOR SUPPLEMENTARY FIGURES 13 & 14
#ANALYSIS FOR CHANGING VACCINE EFFICACY AGAINST SUSCEPTIBILITY

#Load data
df.ve.susc.sens.agg_highChange<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.sens_highChange.csv",sep="/"))

#Create a factor variable

#Separate by level of vaccine efficacy against susceptibility
df.ve.susc.sens.highChange.0.1<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.1 & Label_testingDiff !="modUnequal")
df.ve.susc.sens.highChange.0.3<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.3 & Label_testingDiff !="modUnequal")
df.ve.susc.sens.highChange.0.5<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.5 & Label_testingDiff !="modUnequal")
df.ve.susc.sens.highChange.0.7<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.7 & Label_testingDiff !="modUnequal")
df.ve.susc.sens.highChange.0.9<-subset(df.ve.susc.sens.agg_highChange, VE_susc==0.9 & Label_testingDiff !="modUnequal")

#Point of highest positive epidemic growth and number of epidemic realizations
df.ve.susc.sens.agg_highChange %>%
  select(VE_susc, t_highChange.50., totalNum_sims) %>%
  arrange(VE_susc, t_highChange.50., totalNum_sims)

##Considering VE_RR (cumulative numbers of tested symptomatic SARS-CoV-2 infected)
#Vaccine efficacy against susceptibility=0.1
cbind(Label_test_ve.susc0.1=df.ve.susc.sens.highChange.0.1$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.3
cbind(Label_test_ve.susc0.3=df.ve.susc.sens.highChange.0.3$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.5
cbind(Label_test_ve.susc0.5=df.ve.susc.sens.highChange.0.5$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.7
cbind(Label_test_ve.susc0.7=df.ve.susc.sens.highChange.0.7$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.9
cbind(Label_test_ve.susc0.9=df.ve.susc.sens.highChange.0.9$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_unvac.50.)


#Considering VE_OR (cumulative numbers tested with other etiologies of Covid-like symptoms)
#Vaccine efficacy against susceptibility=0.1
cbind(Label_test_ve.susc0.1=df.ve.susc.sens.highChange.0.1$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.3
cbind(Label_test_ve.susc0.3=df.ve.susc.sens.highChange.0.3$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.5
cbind(Label_test_ve.susc0.5=df.ve.susc.sens.highChange.0.5$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.7
cbind(Label_test_ve.susc0.7=df.ve.susc.sens.highChange.0.7$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_unvac.50.)

#Vaccine efficacy against susceptibility=0.9
cbind(Label_test_ve.susc0.9=df.ve.susc.sens.highChange.0.9$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_unvac.50.)

###########################################
#CONTENT FOR SUPPLEMENTARY FIGURE 15 & 16
#ANALYSIS FOR CHANGING VACCINE EFFICACY AGAINST INFECTIOUSNESS

#Lower vaccine efficacy against susceptibility (0.1)

#Load data
df.ve.susc.0.1_ve.infect.sens.agg_highChange<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_ve.infect.sens_highChange.csv",sep="/"))

df.ve.susc.0.1_ve.infect.sens.agg_highChange %>%
  select(VE_infect, t_highChange.50.,totalNum_sims) %>%
  arrange(VE_infect, t_highChange.50.,totalNum_sims)

#Separate by level of vaccine efficacy against infectiousness
df.ve.susc.0.1_ve.infect.sens.highChange.0.1<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.1 & Label_testingDiff !="modUnequal")
df.ve.susc.0.1_ve.infect.sens.highChange.0.3<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.3 & Label_testingDiff !="modUnequal")
df.ve.susc.0.1_ve.infect.sens.highChange.0.5<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.5 & Label_testingDiff !="modUnequal")
df.ve.susc.0.1_ve.infect.sens.highChange.0.7<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.7 & Label_testingDiff !="modUnequal")
df.ve.susc.0.1_ve.infect.sens.highChange.0.9<-subset(df.ve.susc.0.1_ve.infect.sens.agg_highChange, VE_infect==0.9 & Label_testingDiff !="modUnequal")


##Considering VE_RR (cumulative numbers of tested symptomatic SARS-CoV-2 infected)
#vaccine efficacy against infectiousness=0.1
cbind(Label_test_ve.infect0.1=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Covid_cumSymptExpInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.3
cbind(Label_test_ve.infect0.3=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Covid_cumSymptExpInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.5
cbind(Label_test_ve.infect0.5=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Covid_cumSymptExpInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.7
cbind(Label_test_ve.infect.0.7=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Covid_cumSymptExpInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.9
cbind(Label_test_ve.infect0.9=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Covid_cumSymptExpInfect_tested_unvac.50.)


#Considering VE_OR (cumulative numbers tested with other etiologies of Covid-like symptoms)
#vaccine efficacy against infectiousness=0.1
cbind(Label_test_ve.infect0.1=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.1$NonCovid_cumSymptInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.3
cbind(Label_test_ve.infect0.3=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.3$NonCovid_cumSymptInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.5
cbind(Label_test_ve.infect0.5=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.5$NonCovid_cumSymptInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.7
cbind(Label_test_ve.infect0.7=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.7$NonCovid_cumSymptInfect_tested_unvac.50.)

#vaccine efficacy against infectiousness=0.9
cbind(Label_test_ve.infect0.9=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_ve.infect.sens.highChange.0.9$NonCovid_cumSymptInfect_tested_unvac.50.)

###########################################
#SUPPLEMENTARY FIGURE 17
#PROBABILITY OF TRANSMISSION ANALYSIS APPENDIX

##Higher vaccine efficacy against susceptibility (0.55) 

#Load Data
df.ve.susc.0.55_prob.trans.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.55_prob.trans.sens.csv",sep="/"))

#shows how many n (i.e. epidemic realizations are being used for each sub-plot)
df.ve.susc.0.55_prob.trans.sens.agg %>%
  select(Covid_probTrans, totalNum_sims) %>%
  distinct() %>%
  arrange(Covid_probTrans, totalNum_sims)

#Set-up for figures
t.start=30
t.end.fig17.0.05= 530
t.end.fig17.0.07= 270
t.end.fig17.0.09= 190
t.end.fig17.0.11= 150

#Separate by probability of transmission
df.ve.susc.0.55_prob.trans.sens.0.05<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.05 & t>=t.start & t<=t.end.fig17.0.05)
df.ve.susc.0.55_prob.trans.sens.0.07<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.07 & t>=t.start & t<=t.end.fig17.0.07)
df.ve.susc.0.55_prob.trans.sens.0.09<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.09 & t>=t.start & t<=t.end.fig17.0.09)
df.ve.susc.0.55_prob.trans.sens.0.11<-subset(df.ve.susc.0.55_prob.trans.sens.agg, Covid_probTrans==0.11 & t>=t.start & t<=t.end.fig17.0.11)

main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Supplementary Figure 17
#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.05
plot.cum.inf.susc0.55.prob.trans.0.05<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.05, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.05), breaks = seq(t.start,t.end.fig17.0.05,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))

#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.07
plot.cum.inf.susc0.55.prob.trans.0.07<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.07, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.07), breaks = seq(t.start,t.end.fig17.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.09), breaks = seq(t.start,t.end.fig17.0.09,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.11), breaks = seq(t.start,t.end.fig17.0.11,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.05), breaks = seq(t.start,t.end.fig17.0.05,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Uninfected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Vaccination Status"))



#Cum. Prop of other etiologies of Covid-like symptoms (who have never infected with symptomatic SARS-CoV-2) 
#when probability of transmission=0.07
plot.cum.uninf.susc0.55.prob.trans.0.07<-ggplot(df.ve.susc.0.55_prob.trans.sens.0.07, aes(x=t, y=NonCovid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.07), breaks = seq(t.start,t.end.fig17.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.09), breaks = seq(t.start,t.end.fig17.0.09,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig17.0.11), breaks = seq(t.start,t.end.fig17.0.11,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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

ggarrange(plotlist = plotlist.ve.susc0.55.prob.transmiss, ncol = 4, nrow = 2) #to view combined figure

#set pathway to save figures
appfig17.pathway<-paste(pathway,"figures/appendices_figs/app_fig17&18",sep="/")

#Save Figure 17 (warnings are just because we are only plotting part of the time we have recorded; uncomment to save figures)
# ggarrange(plotlist = plotlist.ve.susc0.55.prob.transmiss, ncol = 4, nrow = 2) %>%
#   ggexport(filename = paste(appfig17.pathway,"AppFig.17_VE.Susc0.55_Prob.Transmiss.Combo.pdf",sep="/"))


###########################################
#SUPPLEMENTARY FIGURE 18
#PROBABILITY OF TRANSMISSION ANALYSIS APPENDIX

##Lower vaccine efficacy against susceptibility (0.1)

#Load in data
df.ve.susc.0.1_prob.trans.sens.agg<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_prob.trans.sens.csv",sep="/"))

##Set-up for figures
t.start=30
t.end.fig18.0.03= 530
t.end.fig18.0.05= 270
t.end.fig18.0.07= 190
t.end.fig18.0.09= 150
t.end.fig18.0.11= 110

#shows how many n (i.e. epidemic realizations are being used for each sub-plot)
df.ve.susc.0.1_prob.trans.sens.agg %>%
  select(Covid_probTrans, totalNum_sims) %>%
  distinct() %>%
  arrange(Covid_probTrans, totalNum_sims)


#Separate by probability of transmission
df.ve.susc.0.1_prob.trans.sens.0.03<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.03 & t>=t.start & t<=t.end.fig18.0.03)
df.ve.susc.0.1_prob.trans.sens.0.05<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.05 & t>=t.start & t<=t.end.fig18.0.05)
df.ve.susc.0.1_prob.trans.sens.0.07<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.07 & t>=t.start & t<=t.end.fig18.0.07)
df.ve.susc.0.1_prob.trans.sens.0.09<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.09 & t>=t.start & t<=t.end.fig18.0.09)
df.ve.susc.0.1_prob.trans.sens.0.11<-subset(df.ve.susc.0.1_prob.trans.sens.agg, Covid_probTrans==0.11 & t>=t.start & t<=t.end.fig18.0.11)

main.line.width=1
ribbon.line.width=0.5

##Sub-figures for Supplementary Figure 18
#Cum. Prop of symptomatic SARS-CoV-2 when probability of transmission=0.03
plot.cum.inf.susc0.1.prob.trans.0.03<-ggplot(df.ve.susc.0.1_prob.trans.sens.0.03, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.03), breaks = seq(t.start,t.end.fig18.0.03,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.05), breaks = seq(t.start,t.end.fig18.0.05,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.07), breaks = seq(t.start,t.end.fig18.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.09), breaks = seq(t.start,t.end.fig18.0.09,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.2.5., ymax=Covid_cumPropSymptInfect_unvac.97.5.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.11), breaks = seq(t.start,t.end.fig18.0.11,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.2.5., ymax=Covid_cumPropSymptInfect_vac.97.5.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.03), breaks = seq(t.start,t.end.fig18.0.03,100)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.05), breaks = seq(t.start,t.end.fig18.0.05,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.07), breaks = seq(t.start,t.end.fig18.0.07,40)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.09), breaks = seq(t.start,t.end.fig18.0.09,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_unvac.2.5., ymax=NonCovid_cumPropSymptInfect_unvac.97.5.),
              color="bisque2", fill="bisque2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig18.0.11), breaks = seq(t.start,t.end.fig18.0.11,20)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=NonCovid_cumPropSymptInfect_vac.2.5., ymax=NonCovid_cumPropSymptInfect_vac.97.5.),
              color="bisque4", fill="bisque4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=NonCovid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Vaccination Status", values = c("Vaccinated" = "bisque4", "Unvaccinated" = "bisque2")) +
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

ggarrange(plotlist = plotlist.ve.susc0.1.prob.transmiss, ncol = 5, nrow = 2) #to view combined figure

#set pathway to save figures
appfig18.pathway<-paste(pathway,"figures/appendices_figs/app_fig17&18",sep="/")

#Save Figure 18 (warnings are just because we are only plotting part of the time we have recorded; uncomment to save figures)
# ggarrange(plotlist = plotlist.ve.susc0.1.prob.transmiss, ncol = 5, nrow = 2) %>%
#   ggexport(filename = paste(appfig18.pathway,"AppFig.18_VE.Susc0.1_Prob.Transmiss.Combo.pdf",sep="/"))

###########################################
#SUPPLEMENTARY 1: CONTENT FOR SUPPLEMENTARY FIGURES 19 & 20
#ANALYSIS FOR CHANGING PROBABILITY OF TRANSMISSION

#Load in data
df.ve.susc.0.1_prob.trans.sens.agg_highChange<-read.csv(paste(pathway,"data/appendices/sens.analyses/df.ve.susc.0.1_prob.trans.sens_highChange.csv",sep="/"))

df.ve.susc.0.1_prob.trans.sens.agg_highChange %>%
  select(Covid_probTrans, t_highChange.50.,totalNum_sims) %>%
  arrange(Covid_probTrans, t_highChange.50.,totalNum_sims)

#Separate by probability of transmission
df.ve.susc.0.1_prob.trans.sens.highChange.0.03<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.03 & Label_testingDiff!= "modUnequal")
df.ve.susc.0.1_prob.trans.sens.highChange.0.05<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.05 & Label_testingDiff!= "modUnequal")
df.ve.susc.0.1_prob.trans.sens.highChange.0.07<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.07 & Label_testingDiff!= "modUnequal")
df.ve.susc.0.1_prob.trans.sens.highChange.0.09<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.09 & Label_testingDiff!= "modUnequal")
df.ve.susc.0.1_prob.trans.sens.highChange.0.11<-subset(df.ve.susc.0.1_prob.trans.sens.agg_highChange, Covid_probTrans==0.11 & Label_testingDiff!= "modUnequal")


#Considering VE_RR (cumulative numbers of tested symptomatic SARS-CoV-2 infected)
#probability of transmission=0.03
cbind(Label_test_prob.trans0.03=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Covid_cumSymptExpInfect_tested_unvac.50.)

#probability of transmission=0.05
cbind(Label_test_prob.trans0.05=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Covid_cumSymptExpInfect_tested_unvac.50.)

#probability of transmission=0.07
cbind(Label_test_prob.trans0.07=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Covid_cumSymptExpInfect_tested_unvac.50.)

#probability of transmission=0.09
cbind(Label_test_prob.trans0.09=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Covid_cumSymptExpInfect_tested_unvac.50.)

#probability of transmission=0.11
cbind(Label_test_prob.trans0.11=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Label_testingDiff,
      Covid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Covid_cumSymptExpInfect_tested_vac.50.,
      Covid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Covid_cumSymptExpInfect_tested_unvac.50.)


#Considering VE_OR (cumulative numbers tested with other etiologies of Covid-like symptoms)
#probability of transmission=0.03
cbind(Label_test_prob.trans0.03=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.03$NonCovid_cumSymptInfect_tested_unvac.50.)

#probability of transmission=0.05
cbind(Label_test_prob.trans0.05=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.05$NonCovid_cumSymptInfect_tested_unvac.50.)

#probability of transmission=0.07
cbind(Label_test_prob.trans0.07=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.07$NonCovid_cumSymptInfect_tested_unvac.50.)

#probability of transmission=0.09
cbind(Label_test_prob.trans0.09=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.09$NonCovid_cumSymptInfect_tested_unvac.50.)

#probability of transmission=0.11
cbind(Label_test_prob.trans0.11=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$Label_testingDiff,
      NonCovid_tested_vac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$NonCovid_cumSymptInfect_tested_vac.50.,
      NonCovid_tested_unvac=df.ve.susc.0.1_prob.trans.sens.highChange.0.11$NonCovid_cumSymptInfect_tested_unvac.50.)

######################################################
#SUPPLEMENTARY 2: SUPPLEMENTARY FIGURES 21 & 22
#MODEL VERIFICATION (TARGET ESTIMATES & ESTIMATES BASED ON EQUAL TESTING)

##Evaluate using a higher and lower vaccine efficacy against susceptibility (0.55 & 0.1)

# Load in data
aggDF.ves0.1<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.1_app.csv",sep="/"))
aggDF.ves0.55<-read.csv(paste(pathway,"data/appendices/aggDF.ves0.55_app.csv",sep="/"))

#Set-up for figures
ve.susc.0.1=0.1
ve.susc.0.55=0.55
t.start=30
t.end.fig21.0.1= 100
t.end.fig21.0.55=150

main.line.width=2.5
ribbon.line.width=1
aggDF.ves0.1<-subset(aggDF.ves0.1, t>=t.start & t<=t.end.fig21.0.1)
aggDF.ves0.55<-subset(aggDF.ves0.55, t>=t.start & t<=t.end.fig21.0.55)

aggDF.ves0.55$Label_testingDiff<-factor(aggDF.ves0.55$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
aggDF.ves0.1$Label_testingDiff<-factor(aggDF.ves0.1$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

aggDF.ves0.55$LegendLabel_1 <- "VE Measurement (Everyone Eligible)"
aggDF.ves0.1$LegendLabel_1 <- "VE Measurement (Everyone Eligible)"

aggDF.ves0.55$LegendLabel_2 <- "VE Estimates (Equal Sampling)"
aggDF.ves0.1$LegendLabel_2 <- "VE Estimates (Equal Sampling)"

##Figures for vaccine efficacy against susceptibility = 0.55

##Supplementary Figure 21a 
#Relative Risk - Cohort Design Target Estimate (Everyone included)
VE.RR.ves0.55_target<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptRR.2.5., ymax=VE_symptRR.97.5., color=LegendLabel_1, fill=LegendLabel_1), 
                                alpha=0.3,linewidth=ribbon.line.width) +
  geom_borderline(aes(color = LegendLabel_1),linewidth=main.line.width, bordercolour="grey20", borderwidth=0.4) +
  scale_color_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "steelblue4")
  ) + scale_fill_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "skyblue3")) +
  scale_x_continuous(limits = c(t.start, t.end.fig21.0.55),  breaks = seq(t.start,t.end.fig21.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_blank(),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")

##Supplementary Figure 21b 
#Odds Ratio - Test-Negative Design Target Estimate (everyone included)
VE.OR.ves0.55_target<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptOR.2.5., ymax=VE_symptOR.97.5., color=LegendLabel_1, fill=LegendLabel_1), 
                                alpha=0.3,linewidth=ribbon.line.width) +
  geom_borderline(aes( color=LegendLabel_1),linewidth=main.line.width, bordercolour="grey20", borderwidth=0.4) +
  scale_color_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "steelblue4")
  ) + scale_fill_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "skyblue3")) +
  scale_x_continuous(limits = c(t.start, t.end.fig21.0.55),  breaks = seq(t.start,t.end.fig21.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Supplementary Figure 21c 
#Relative Risk - Cohort Design (equal testing only)
VE.RR.ves0.55.equal.testing<-ggplot(subset(aggDF.ves0.55, Label_testingDiff== "equal"), aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.2.5., ymax=VE_symptRR_observed.97.5., color=LegendLabel_2, fill=LegendLabel_2),
              alpha=0.3, linewidth=ribbon.line.width) +
  scale_color_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")
  ) + scale_fill_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")) +
  scale_x_continuous(limits = c(t.start, t.end.fig21.0.55),  breaks = seq(t.start,t.end.fig21.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50., color=LegendLabel_2),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_blank(),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")

##Supplementary Figure 21d 
#Odds Ratio - Test-Negative Design (equal testing only)
VE.OR.ves0.55.equal.testing<-ggplot(subset(aggDF.ves0.55, Label_testingDiff== "equal"), aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.2.5., ymax=VE_symptOR_observed.97.5.,
                  fill=LegendLabel_2, color=LegendLabel_2),alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig21.0.55),  breaks = seq(t.start,t.end.fig21.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=LegendLabel_2),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")
  ) + scale_fill_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

#Print Supplementary Figure 21 (uncomment to save figures)
ggarrange(plotlist = list(VE.RR.ves0.55_target, VE.OR.ves0.55_target, VE.RR.ves0.55.equal.testing, VE.OR.ves0.55.equal.testing),
          ncol = 2, nrow = 2)

#set pathway to save figures
appfig21.pathway<-paste(pathway,"figures/appendices_figs/app_fig21",sep="/")

#Save Figures

# pdf(file=paste(appfig21.pathway,"AppFig.21a_VE.RR.0.55.Target.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.RR.ves0.55_target
# dev.off()
# 
# pdf(file=paste(appfig21.pathway,"AppFig.21b_VE.OR.0.55.Target.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.OR.ves0.55_target
# dev.off()
# 
# pdf(file=paste(appfig21.pathway,"AppFig.21c_VE.RR.0.55.Equal.Testing.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.RR.ves0.55.equal.testing
# dev.off()
# 
# pdf(file=paste(appfig21.pathway,"AppFig.21d_VE.OR.0.55.Equal.Testing.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.OR.ves0.55.equal.testing
# dev.off()

#Set-up for figures
ve.susc.0.1=0.1
ve.susc.0.55=0.55
t.start=30
t.end.fig22.0.1= 100
t.end.fig22.0.55=150

main.line.width=2.5
ribbon.line.width=1
aggDF.ves0.1<-subset(aggDF.ves0.1, t>=t.start & t<=t.end.fig22.0.1)
aggDF.ves0.55<-subset(aggDF.ves0.55, t>=t.start & t<=t.end.fig22.0.55)

aggDF.ves0.55$Label_testingDiff<-factor(aggDF.ves0.55$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
aggDF.ves0.1$Label_testingDiff<-factor(aggDF.ves0.1$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

aggDF.ves0.55$LegendLabel_1 <- "VE Measurement (Everyone Eligible)"
aggDF.ves0.1$LegendLabel_1 <- "VE Measurement (Everyone Eligible)"

aggDF.ves0.55$LegendLabel_2 <- "VE Estimates (Equal Sampling)"
aggDF.ves0.1$LegendLabel_2 <- "VE Estimates (Equal Sampling)"

#Figures for vaccine efficacy against susceptibility = 0.1

##Supplementary Figure 22a 
#Relative Risk - Cohort Design Target Estimate (Everyone included)
VE.RR.ves0.1_target<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptRR.2.5., ymax=VE_symptRR.97.5., color=LegendLabel_1, fill=LegendLabel_1), 
                                alpha=0.3,linewidth=ribbon.line.width) +
  geom_borderline(linewidth=main.line.width, aes(color=LegendLabel_1), bordercolour="grey20", borderwidth=0.4) +
  scale_color_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "steelblue4")
  ) + scale_fill_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "skyblue3")) +
  scale_x_continuous(limits = c(t.start, t.end.fig22.0.1),  breaks = seq(t.start,t.end.fig22.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Supplementary Figure 22b 
#Odds Ratio - Test-Negative Design Target Estimate (everyone included)
VE.OR.ves0.1_target<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + geom_ribbon(aes(ymin=VE_symptOR.2.5., ymax=VE_symptOR.97.5., color=LegendLabel_1, fill=LegendLabel_1), 
                                alpha=0.3,linewidth=ribbon.line.width) +
  geom_borderline(linewidth=main.line.width, aes(color=LegendLabel_1), bordercolour="grey20", borderwidth=0.4) +
  scale_color_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "steelblue4")
  ) + scale_fill_manual(name = NULL,values = c("VE Measurement (Everyone Eligible)" = "skyblue3")) +
  scale_x_continuous(limits = c(t.start, t.end.fig22.0.1),  breaks = seq(t.start,t.end.fig22.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario")) 

##Supplementary Figure 22c 
#Relative Risk - Cohort Design (equal testing only)
VE.RR.ves0.1.equal.testing<-ggplot(subset(aggDF.ves0.1, Label_testingDiff== "equal"), aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.2.5., ymax=VE_symptRR_observed.97.5.,color=LegendLabel_2, fill=LegendLabel_2),
              alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig22.0.1),  breaks = seq(t.start,t.end.fig22.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50., color=LegendLabel_2),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")
  ) + scale_fill_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Supplementary Figure 22d 
#Odds Ratio - Test-Negative Design (equal testing only)
VE.OR.ves0.1.equal.testing<-ggplot(subset(aggDF.ves0.1, Label_testingDiff== "equal"), aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.2.5., ymax=VE_symptOR_observed.97.5., color=LegendLabel_2, fill=LegendLabel_2),
              alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.fig22.0.1),  breaks = seq(t.start,t.end.fig22.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=LegendLabel_2),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")
  ) + scale_fill_manual(name = NULL,values = c("VE Estimates (Equal Sampling)" = "darkgreen")) +
  labs(y="VE against\nSymptomatic Infection", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

ggarrange(plotlist = list(VE.RR.ves0.1_target, VE.OR.ves0.1_target, VE.RR.ves0.1.equal.testing, VE.OR.ves0.1.equal.testing),
          ncol = 2, nrow = 2)

#set pathway to save figures
appfig22.pathway<-paste(pathway,"figures/appendices_figs/app_fig22",sep="/")

#Save Figures (uncomment to save figures)
# pdf(file=paste(appfig22.pathway,"AppFig.22a_VE.RR.0.1.Target.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.RR.ves0.1_target
# dev.off()
# 
# pdf(file=paste(appfig22.pathway,"AppFig.22b_VE.OR.0.1.Target.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.OR.ves0.1_target
# dev.off()
# 
# pdf(file=paste(appfig22.pathway,"AppFig.22c_VE.RR.0.1.Equal.Testing.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.RR.ves0.1.equal.testing
# dev.off()
# 
# pdf(file=paste(appfig22.pathway,"AppFig.22d_VE.OR.0.1.Equal.Testing.pdf",sep="/"), 
#     width=7.5, height=6)
# VE.OR.ves0.1.equal.testing
# dev.off()
