######################################################

#Analyses and Figures

#####################################################
#Running the code below will produce all primary results,
#including producing Figures 3-6

#Load Packages
library(tidyverse)
library(ggborderline)
library(data.table)
library(ggpubr)

#Set pathway to retrieve data
setwd("..") #moves back a folder
pathway=getwd()

######################################################
#FIGURE 4 & ANALYSIS FOR OBJECTIVE 1 
#HOW TESTING BIAS AFFECTS VACCINE EFFECTIVENESS OVER TIME
#FOR COHORT AND TEST-NEGATIVE DESIGN

##Evaluate using a higher and lower vaccine efficacy against susceptibility (0.55 & 0.1)

#data pathways
pathway.ves.0.55<-paste(pathway,"/data/obj.1_ve.susc.0.55",sep="/")
pathway.ves.0.1<-paste(pathway,"/data/obj.1_ve.susc.0.1",sep="/")


#Load data for Ve.susc = 0.55
setwd(pathway.ves.0.55)

df.ves0.55 <-
  list.files(path = pathway.ves.0.55, pattern = "*.csv") %>%
  map_df(~fread(.))

#Load data for Ve.susc = 0.1
setwd(pathway.ves.0.1)

df.ves0.1 <-
  list.files(path = pathway.ves.0.1, pattern = "*.csv") %>%
  map_df(~fread(.))


#Ensure the average number of contacts is 6
unique(df.ves0.55$Avg_numContacts)
unique(df.ves0.1$Avg_numContacts)


# Generate aggregate dataframe (including 25 quantile, median, 75 quantile)
aggDF.ves0.55<- aggregate(cbind(VE_symptOR_observed, VE_symptOR, VE_symptRR_observed, VE_symptRR,
                                Covid_propSymptInfect, Covid_propSymptInfect_vac, Covid_propSymptInfect_unvac,
                                Covid_cumPropSymptInfect, Covid_cumPropSymptInfect_vac, Covid_cumPropSymptInfect_unvac,
                                NonCovid_propSymptInfect, NonCovid_propSymptInfect_vac, NonCovid_propSymptInfect_unvac,
                                Covid_cumPropSymptExpInfect_tested_unvac,Covid_cumPropSymptExpInfect_tested_vac, Avg_numContacts, t_highChange) ~ t + Label_testingDiff , df.ves0.55, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ves0.55<-do.call(data.frame, aggDF.ves0.55)

aggDF.ves0.1<- aggregate(cbind(VE_symptOR_observed, VE_symptOR, VE_symptRR_observed, VE_symptRR,
                               Covid_propSymptInfect, Covid_propSymptInfect_vac, Covid_propSymptInfect_unvac,
                               Covid_cumPropSymptInfect, Covid_cumPropSymptInfect_vac, Covid_cumPropSymptInfect_unvac,
                               NonCovid_propSymptInfect, NonCovid_propSymptInfect_vac, NonCovid_propSymptInfect_unvac,
                               Covid_cumPropSymptExpInfect_tested_unvac,Covid_cumPropSymptExpInfect_tested_vac, 
                               Avg_numContacts, t_highChange) ~ t + Label_testingDiff , df.ves0.1, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ves0.1<-do.call(data.frame, aggDF.ves0.1)


#Average timestep where there is the highest growth rate of SARS-CoV-2 
mean(aggDF.ves0.55$t_highChange.50.)
mean(aggDF.ves0.1$t_highChange.50.)

#Average observed VE at inflection point
aggDF.ves0.55$VE_symptOR_observed.50.[aggDF.ves0.55$Label_testingDiff==
                                        "highUnequal" & aggDF.ves0.55$t==73]
aggDF.ves0.55$VE_symptOR_observed.50.[aggDF.ves0.55$Label_testingDiff==
                                        "modUnequal" & aggDF.ves0.55$t==73]

aggDF.ves0.1$VE_symptOR_observed.50.[aggDF.ves0.1$Label_testingDiff==
                                       "highUnequal" & aggDF.ves0.1$t==50]
aggDF.ves0.1$VE_symptOR_observed.50.[aggDF.ves0.1$Label_testingDiff==
                                       "modUnequal" & aggDF.ves0.1$t==50]

#Average observed VE across time and testing scenarios
aggDF.ves0.55_subset_modUnequal<-subset(aggDF.ves0.55, Label_testingDiff==
                                        "modUnequal" & t >=20 & t <=150)
aggDF.ves0.55_subset_highUnequal<-subset(aggDF.ves0.55, Label_testingDiff==
                                      "highUnequal" & t >=20 & t <=150)

aggDF.ves0.1_subset_modUnequal<-subset(aggDF.ves0.1, Label_testingDiff==
                                         "modUnequal" & t >=20 & t <=100)
aggDF.ves0.1_subset_highUnequal<-subset(aggDF.ves0.1, Label_testingDiff==
                                          "highUnequal" & t >=20 & t <=100)

#Average VE across entire sampling period
mean(aggDF.ves0.55_subset_modUnequal$VE_symptOR_observed.50.)
mean(aggDF.ves0.55_subset_highUnequal$VE_symptOR_observed.50.)

mean(aggDF.ves0.1_subset_modUnequal$VE_symptOR_observed.50.)
mean(aggDF.ves0.1_subset_highUnequal$VE_symptOR_observed.50.)


#create factor variable for testing scenarios
aggDF.ves0.55$Label_testingDiff<-factor(aggDF.ves0.55$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))
aggDF.ves0.1$Label_testingDiff<-factor(aggDF.ves0.1$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))


#Set-up figure
ve.susc.0.55=0.55
ve.susc.0.1=0.1
t.start=20
t.end.0.55=150
t.end.0.1= 100
t.end.0.55.zoomed=75
t.end.0.1.zoomed=50
 group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

main.line.width=2.5
ribbon.line.width=1
aggDF.ves0.55<-subset(aggDF.ves0.55, t>=t.start & t<=t.end.0.55)
aggDF.ves0.1<-subset(aggDF.ves0.1, t>=t.start & t<=t.end.0.1)

##Figure 3a
#Relative Risk - Cohort Design
VE.RR.ves0.55<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptRR_observed.25., ymax=VE_symptRR_observed.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_RR Estimates", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Figure 3c
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
Cum.Pop.Sympt.Infect.ves0.55<-ggplot(aggDF.ves0.55, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() + geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
                                color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55), breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=3) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  #geom_line(aes(x=t, y=prop.sympt.infected.unvac.observed), color="goldenrod2",size=3,linetype="dashed") +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenario"))

##Figure 3e 
#Odds Ratio - Test-Negative Design 
VE.OR.ves0.55<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.25., ymax=VE_symptOR_observed.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55),  breaks = seq(t.start,t.end.0.55,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR Estimates", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

##Figure 3e (zoomed in)
#Odds Ratio - Test-Negative Design 
VE.OR.ves.0.55.zoomed<-ggplot(aggDF.ves0.55, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.55, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.25., ymax=VE_symptOR_observed.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.55.zoomed),  breaks = seq(t.start,t.end.0.55.zoomed,10)) + 
  scale_y_continuous(limits = c(0, 1),  breaks = seq(0,1,0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR Estimates", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

#set pathway to save figures
fig3.pathway<-paste(pathway,"figures/fig3",sep="/")

#Saving Figures
pdf(file=paste(fig3.pathway,"Fig.3a_VE.RR.0.55.pdf",sep="/"), 
    width=7.5, height=6)
VE.RR.ves0.55
dev.off()

pdf(file=paste(fig3.pathway,"Fig.3c_Cum.Sympt.Infect.0.55.pdf",sep="/"), 
    width=7.5, height=6)
Cum.Pop.Sympt.Infect.ves0.55
dev.off()

pdf(file=paste(fig3.pathway,"Fig.3e_VE.OR.0.55.pdf",sep="/"), 
    width=7.5, height=6)
VE.OR.ves0.55
dev.off()

pdf(file=paste(fig3.pathway,"Fig.3e_VE.OR.0.55.zoom.pdf",sep="/"), 
    width=7.5, height=3)
VE.OR.ves.0.55.zoomed
dev.off()


##Figure 3b
#Relative Risk - Cohort Design
VE.RR.ves0.1<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptRR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() + 
  geom_ribbon(aes(ymin=VE_symptRR_observed.25., ymax=VE_symptRR_observed.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptRR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_RR Estimates", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

##Figure 3d
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
Cum.Pop.Sympt.Infect.ves0.1<-ggplot(aggDF.ves0.1, aes(x=t, y=Covid_cumPropSymptInfect_unvac.50.,color="Unvaccinated")) +
  theme_classic() +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_unvac.25., ymax=Covid_cumPropSymptInfect_unvac.75.),
              color="goldenrod2", fill="goldenrod2", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1), breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(aes(ymin=Covid_cumPropSymptInfect_vac.25., ymax=Covid_cumPropSymptInfect_vac.75.),
              color="goldenrod4", fill="goldenrod4", alpha=0.3, linewidth=ribbon.line.width) +
  geom_borderline(aes(x=t, y=Covid_cumPropSymptInfect_vac.50., color="Vaccinated"),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width) +
  scale_color_manual(name = "Y series", values = c("Vaccinated" = "goldenrod4", "Unvaccinated" = "goldenrod2")) +
  labs(y="Cum. Prop. of Symptomatic Infected", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  theme(legend.position = "none")
#guides(color=guide_legend(title="Testing Scenarios"))

##Figure 3f
#Odds Ratio - Test-Negative Design
VE.OR.ves0.1<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=2) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.25., ymax=VE_symptOR_observed.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1),  breaks = seq(t.start,t.end.0.1,10)) + 
  scale_y_continuous(limits = c(-2.5, 1), breaks= seq(-2.5, 1, 0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=Label_testingDiff),
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR Estimates", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

##Figure 3f (zoomed in)
#Odds Ratio - Test-Negative Design
VE.OR.ves.0.1.zoomed<-ggplot(aggDF.ves0.1, aes(x=t, y=VE_symptOR.50.)) +
  geom_hline(yintercept=ve.susc.0.1, linetype="dashed", 
             color = "grey40", linewidth=main.line.width) +
  theme_classic() +
  geom_ribbon(aes(ymin=VE_symptOR_observed.25., ymax=VE_symptOR_observed.75., fill=Label_testingDiff, 
                  color=Label_testingDiff), alpha=0.3, linewidth=ribbon.line.width) +
  scale_x_continuous(limits = c(t.start, t.end.0.1.zoomed),  breaks = seq(t.start,t.end.0.1.zoomed,10)) + 
  scale_y_continuous(limits = c(-0.5, 0.5),  breaks = seq(-0.5,0.5,0.5)) +
  geom_borderline(aes(x=t, y=VE_symptOR_observed.50., color=Label_testingDiff), 
                  bordercolour="grey20",borderwidth=0.4,linewidth=main.line.width)+
  scale_color_manual(values=group.colors, labels=names) +
  scale_fill_manual(values=group.colors, labels=names) +
  labs(y="VE_OR Estimates", x = "Days") +
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20, margin = margin(t = 5)),
        legend.title = element_text(size=22),
        legend.spacing.y = unit(0.5, 'cm')) +
  theme(legend.position = "none")
#+ guides(color=guide_legend(title="Testing Scenario"))

#Save Figures
pdf(file=paste(fig3.pathway,"Fig.3b_VE.RR.0.1.pdf",sep="/"), 
    width=7.5, height=6)
VE.RR.ves0.1
dev.off()

pdf(file=paste(fig3.pathway,"Fig.3d_Cum.Sympt.Infect.0.1.pdf",sep="/"), 
    width=7.5, height=6)
Cum.Pop.Sympt.Infect.ves0.1
dev.off()

pdf(file=paste(fig3.pathway,"Fig.3f_VE.OR.0.1.pdf",sep="/"), 
    width=7.5, height=6)
VE.OR.ves0.1
dev.off()

pdf(file=paste(fig3.pathway,"Fig.3f_VE.OR.0.1.zoom.pdf",sep="/"), 
    width=7.5, height=4)
VE.OR.ves.0.1.zoomed
dev.off()


######################################################
#FIGURE 4 & ANALSIS FOR OBJECTIVE 2a: 
#HOW TESTING BIAS IS INFLUENCED BY VACCINE EFFICACY AGAINST SUSCEPTIBILITY
#ACROSS COHORT & TEST-NEGATIVE DESIGN

#Load in Data
df.susc.sens.high<-read.csv(paste(pathway,"data/obj.2&3_sens.analyses/ve.susc.sens.csv",sep="/")) 

#create factor variable for testing scenarios
df.susc.sens.high$Label_testingDiff<-factor(df.susc.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

#select simulations where SARS-CoV-2 cumulative symptomatic infections were above 0.1% 
df.susc.sens.high<-subset(df.susc.sens.high,Covid_cumPropSymptInfect_highChange >0.001) 

#Fixes direction of underestimation variable
df.susc.sens.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.susc.sens.high$VE_symptOR_degUnder_highChange_trueVeff
df.susc.sens.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.susc.sens.high$VE_symptRR_degUnder_highChange_trueVeff

#Set-up for figure
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

##Figure 4a
#Relative Risk - Cohort Design
VE.susc.RR<-ggplot(df.susc.sens.high,aes(x=as.factor(VE_susc) ,y=VE_symptRR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(1),
              width=1) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Susceptibility") +
  ylab("Magnitude of Bias in VE_RR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario")) + theme(legend.position="none")

##Figure 4c 
#Odds Ratio - Test-Negative Design
VE.susc.OR<-ggplot(df.susc.sens.high,aes(x=as.factor(VE_susc) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Susceptibility") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario"))  + theme(legend.position="none")
 
##Figure 4c (zoomed in)
#Odds Ratio - Test-Negative Design
VE.susc.OR.zoom<-ggplot(df.susc.sens.high,aes(x=as.factor(VE_susc) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-0.4, 0.2), breaks=round(seq(from=-0.4,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Susceptibility") +
  ylab("Magnitude of Bias\nin VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario"))  + theme(legend.position="none")


#Set-up for cumulative proportion of symptomatic SARS-CoV-2 figure
aggDF.ve.susc<- aggregate(cbind(Covid_cumPropSymptInfect_vac,Covid_cumPropSymptInfect_unvac, t_highChange) ~ VE_susc, df.susc.sens.high, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ve.susc<-do.call(data.frame, aggDF.ve.susc)

aggDF.ve.susc$VE_susc<-factor(aggDF.ve.susc$VE_susc)
aggDF.ve.susc$place_holder<-0.93 #to set-up placement for circles
aggDF.ve.susc$place_holder2<-1.13 #to set-up placement for circles

##Figure 4b
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
SymptInfect.susc<-ggplot(aggDF.ve.susc, aes(x = VE_susc)) + 
  geom_point(aes(y = place_holder,size = Covid_cumPropSymptInfect_vac.50.), alpha = 0.75, shape = 21, fill="goldenrod4")  +
  geom_point(aes(y= place_holder2, size = Covid_cumPropSymptInfect_unvac.50.), alpha = 0.75, shape = 21, fill="goldenrod2")  +
  scale_size_continuous(limits = c(0.001, 0.21), 
                        range = c(0,30), breaks = c(0.01,0.05,0.1,0.15,0.2)) + 
  scale_y_continuous(limits = c(0.85, 1.21)) +
  theme_classic()+
  labs( x= "", y = "", size = "Cum. Prop. \nSympt. Infected", fill = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.ticks.y=element_blank() ,
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Susceptibility")  + guides(fill="none") +
  theme(legend.position="none")

#set pathway to save figures
fig4.pathway<-paste(pathway,"figures/fig4",sep="/")

#Save Figures
pdf(file=paste(fig4.pathway,"Fig.4a_VE.susc.RR.pdf",sep="/"), 
    width=10, height=6)
VE.susc.RR
dev.off()

pdf(file=paste(fig4.pathway,"Fig.4b_SymptInfect.susc.pdf",sep="/"), 
    width=10, height=3)
SymptInfect.susc
dev.off()

pdf(file=paste(fig4.pathway,"Fig.4c_VE.susc.OR.pdf",sep="/"), 
    width=10, height=6)
VE.susc.OR
dev.off()

pdf(file=paste(fig4.pathway,"Fig.4c_VE.susc.OR.zoom.pdf",sep="/"), 
    width=8, height=3)
VE.susc.OR.zoom
dev.off()


##Descriptive summary (across levels of vaccine efficacy against susceptibility [ve.susc])

#Number of simulations used in analyses by ve.susc and testing scenarios
df.susc.sens.high %>%
  group_by(VE_susc,Label_testingDiff) %>%
  summarize(days = n())

#Median observed VE underestimate for cohort design by ve.susc and testing scenarios
df.susc.sens.high %>%
  group_by(VE_susc,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptRR_degUnder_highChange_trueVeff))

#Median observed VE underestimate for test-negative design by ve.susc and testing scenarios
df.susc.sens.high %>%
  group_by(VE_susc,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptOR_degUnder_highChange_trueVeff))

#Cumulative proportion of symptomatic SARS-CoV-2 by vaccination status and ve.susc 
#at timestep when SARS-CoV-2 at its highest epidemic growth point 
cbind(cum.prop.unvac=aggDF.ve.susc$Covid_cumPropSymptInfect_unvac.50.,
      cum.prop.vac=aggDF.ve.susc$Covid_cumPropSymptInfect_vac.50., t.at.highchange=aggDF.ve.susc$t_highChange.50.)

######################################################
#FIGURE 5 & ANALSIS FOR OBJECTIVE 2b (1 of 2): 
#HOW TESTING BIAS IS INFLUENCED BY VACCINE EFFICACY AGAINST INFECTIOUSNESS
#ACROSS COHORT & TEST-NEGATIVE DESIGN

#Using higher vaccine efficacy against susceptibility (0.55)

#Load data
df.ve.susc.0.55_ve.infect.sens.high<-read.csv(paste(pathway,"data/obj.2&3_sens.analyses/ve.infect.sens_ve.susc.0.55.csv",sep="/"))

#create factor variable for testing scenarios
df.ve.susc.0.55_ve.infect.sens.high$Label_testingDiff<-factor(df.ve.susc.0.55_ve.infect.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

#Fix direction of underestimation variable
df.ve.susc.0.55_ve.infect.sens.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.55_ve.infect.sens.high$VE_symptOR_degUnder_highChange_trueVeff
df.ve.susc.0.55_ve.infect.sens.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.55_ve.infect.sens.high$VE_symptRR_degUnder_highChange_trueVeff

#select simulations where SARS-CoV-2 cumulative symptomatic infections were above 0.1%
df.ve.susc.0.55_ve.infect.sens.high<-subset(df.ve.susc.0.55_ve.infect.sens.high,Covid_cumPropSymptInfect_highChange >0.001)

#Set-Up for Plot
group.colors <- c("darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

##Figure 5a
#Relative Risk - Cohort Design
VE.susc0.55_VE.infect.RR<-ggplot(df.ve.susc.0.55_ve.infect.sens.high,aes(x=as.factor(VE_infect) ,y=VE_symptRR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(1),
              width=1) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Infectiousness") +
  ylab("Magnitude of Bias in VE_RR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario")) + theme(legend.position="none")

##Figure 5e
#Odds Ratio - Test-Negative Design
VE.susc0.55_VE.infect.OR<-ggplot(df.ve.susc.0.55_ve.infect.sens.high,aes(x=as.factor(VE_infect) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Infectiousness") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario"))  + theme(legend.position="none")


#Set-up for cumulative proportion of symptomatic SARS-CoV-2 figure
aggDF.ve.susc0.55.ve.infect<- aggregate(cbind(Covid_cumPropSymptInfect_vac,Covid_cumPropSymptInfect_unvac, t_highChange) ~ VE_infect, df.ve.susc.0.55_ve.infect.sens.high, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ve.susc0.55.ve.infect<-do.call(data.frame, aggDF.ve.susc0.55.ve.infect)

aggDF.ve.susc0.55.ve.infect$VE_infect<-factor(aggDF.ve.susc0.55.ve.infect$VE_infect)
aggDF.ve.susc0.55.ve.infect$place_holder<-0.93 #set-up for circle placement
aggDF.ve.susc0.55.ve.infect$place_holder2<-1.13 #set-up for circle placement

##Figure 5c
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
VE.susc0.55_SymptInfect.susc<-ggplot(aggDF.ve.susc0.55.ve.infect, aes(x = VE_infect)) + 
  geom_point(aes(y = place_holder,size = Covid_cumPropSymptInfect_vac.50.), alpha = 0.75, shape = 21, fill="goldenrod4")  +
  geom_point(aes(y= place_holder2, size = Covid_cumPropSymptInfect_unvac.50.), alpha = 0.75, shape = 21, fill="goldenrod2")  +
  scale_size_continuous(limits = c(0.001, 0.21), 
                        range = c(0,30), breaks = c(0.01,0.05,0.1,0.15,0.2)) + 
  scale_y_continuous(limits = c(0.85, 1.21)) +
  theme_classic()+
  labs( x= "", y = "", size = "Cum. Prop. \nSympt. Infected", fill = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.ticks.y=element_blank() ,
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Infectiousness")  + guides(fill="none") +
  theme(legend.position="none")

#Set pathway to save figures
fig5.pathway<-paste(pathway,"figures/fig5",sep="/")

#Save Figures
pdf(file=paste(fig5.pathway,"Fig.5a_VE.susc.0.55_VE.infect.RR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.55_VE.infect.RR
dev.off()

pdf(file=paste(fig5.pathway,"Fig.5c_VE.susc.0.55_SymptInfect.infect.pdf", sep="/"), 
    width=10, height=3)
VE.susc0.55_SymptInfect.susc
dev.off()

pdf(file=paste(fig5.pathway,"Fig.5e_VE.susc0.55_VE.infect.OR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.55_VE.infect.OR
dev.off()

##Descriptive summary (across levels of vaccine efficacy against infectiousness [Ve.infect])

#Number of simulations used in analyses by ve.infect and testing scenarios
df.ve.susc.0.55_ve.infect.sens.high %>%
  group_by(VE_infect,Label_testingDiff) %>%
  summarize(days = n())

#Median observed VE underestimate for cohort design by ve.infect and testing scenarios
df.ve.susc.0.55_ve.infect.sens.high %>%
  group_by(VE_infect,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptRR_degUnder_highChange_trueVeff))

#Median observed VE underestimate for test-negative design by ve.infect and testing scenarios
df.ve.susc.0.55_ve.infect.sens.high %>%
  group_by(VE_infect,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptOR_degUnder_highChange_trueVeff))

#Cumulative proportion of symptomatic SARS-CoV-2 by ve.infect and vaccination status 
#at timestep when SARS-CoV-2 at its highest epidemic growth point 
cbind(cum.prop.unvac=aggDF.ve.susc0.55.ve.infect$Covid_cumPropSymptInfect_unvac.50.,
      cum.prop.vac=aggDF.ve.susc0.55.ve.infect$Covid_cumPropSymptInfect_vac.50., 
      t.at.highchange=aggDF.ve.susc0.55.ve.infect$t_highChange.50.)

######################################################
#FIGURE 5 & ANALSIS FOR OBJECTIVE 2b (2 of 2): 
#HOW TESTING BIAS IS INFLUENCED BY VACCINE EFFICACY AGAINST INFECTIOUSNESS
#ACROSS COHORT & TEST-NEGATIVE DESIGN

#Using lower vaccine efficacy against susceptibility (0.1)

#Load in data
df.ve.susc.0.1_ve.infect.sens.high<-read.csv(paste(pathway,"data/obj.2&3_sens.analyses/ve.infect.sens_ve.susc.0.1.csv",sep="/"))

#create factor variable for testing scenarios
df.ve.susc.0.1_ve.infect.sens.high$Label_testingDiff<-factor(df.ve.susc.0.1_ve.infect.sens.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

#Fix direction of underestimation variable
df.ve.susc.0.1_ve.infect.sens.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.1_ve.infect.sens.high$VE_symptOR_degUnder_highChange_trueVeff
df.ve.susc.0.1_ve.infect.sens.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.1_ve.infect.sens.high$VE_symptRR_degUnder_highChange_trueVeff

#select simulations where SARS-CoV-2 cumulative symptomatic infections were above 0.1%
df.ve.susc.0.1_ve.infect.sens.high<-subset(df.ve.susc.0.1_ve.infect.sens.high,Covid_cumPropSymptInfect_highChange >0.001)

#Set-Up for Plot
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

##Figure 5b
#Relative Risk - Cohort Design
VE.susc0.1_VE.infect.RR<-ggplot(df.ve.susc.0.1_ve.infect.sens.high,aes(x=as.factor(VE_infect) ,y=VE_symptRR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(1),
              width=1) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Infectiousness") +
  ylab("Magnitude of Bias in VE_RR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario")) + theme(legend.position="none")

#Figure 5f
#Odds Ratio - Test-Negative Design
VE.susc0.1_VE.infect.OR<-ggplot(df.ve.susc.0.1_ve.infect.sens.high,aes(x=as.factor(VE_infect) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Infectiousness") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario"))  + theme(legend.position="none")


#set-up for cumulative proportion of symptomatic SARS-CoV-2 figure 
aggDF.ve.susc0.1.ve.infect<- aggregate(cbind(Covid_cumPropSymptInfect_vac,Covid_cumPropSymptInfect_unvac, t_highChange) ~ VE_infect, df.ve.susc.0.1_ve.infect.sens.high, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ve.susc0.1.ve.infect<-do.call(data.frame, aggDF.ve.susc0.1.ve.infect)

aggDF.ve.susc0.1.ve.infect$VE_infect<-factor(aggDF.ve.susc0.1.ve.infect$VE_infect)
aggDF.ve.susc0.1.ve.infect$place_holder<-0.93 #set-up circle placement
aggDF.ve.susc0.1.ve.infect$place_holder2<-1.13 #set-up circle placement

##Figure 5d
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
VE.susc0.1_SymptInfect.susc<-ggplot(aggDF.ve.susc0.1.ve.infect, aes(x = VE_infect)) + 
  geom_point(aes(y = place_holder,size = Covid_cumPropSymptInfect_vac.50.), alpha = 0.75, shape = 21, fill="goldenrod4")  +
  geom_point(aes(y= place_holder2, size = Covid_cumPropSymptInfect_unvac.50.), alpha = 0.75, shape = 21, fill="goldenrod2")  +
  scale_size_continuous(limits = c(0.001, 0.21), 
                        range = c(0,30), breaks = c(0.01,0.05,0.1,0.15,0.2)) + 
  scale_y_continuous(limits = c(0.85, 1.21)) +
  theme_classic()+
  labs( x= "", y = "", size = "Cum. Prop. \nSympt. Infected", fill = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.ticks.y=element_blank() ,
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Vaccine Efficacy Against Infectiousness")  + guides(fill="none") +
  theme(legend.position="none")

#set pathway to save figures
fig5.pathway<-paste(pathway,"figures/fig5",sep="/")

#Save Figures
pdf(file=paste(fig5.pathway,"Fig.5b_VE.susc.0.1_VE.infect.RR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.1_VE.infect.RR
dev.off()

pdf(file=paste(fig5.pathway,"Fig.5d_VE.susc0.1_SymptInfect.infect.pdf", sep="/"), 
    width=10, height=3)
VE.susc0.1_SymptInfect.susc
dev.off()

pdf(file=paste(fig5.pathway,"Fig.5f_VE.susc.0.1_VE.infect.OR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.1_VE.infect.OR
dev.off()

##Descriptive summary (across levels of vaccine efficacy against infectiousness [ve.infect])

#Number of simulations used in analyses by ve.infect and testing scenarios
df.ve.susc.0.1_ve.infect.sens.high %>%
  group_by(VE_infect,Label_testingDiff) %>%
  summarize(days = n())

#Median observed VE underestimate for cohort design by ve.infect and testing scenarios
df.ve.susc.0.1_ve.infect.sens.high %>%
  group_by(VE_infect,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptRR_degUnder_highChange_trueVeff))

#Median observed VE underestimate for test-negative design by ve.infect and testing scenarios
df.ve.susc.0.1_ve.infect.sens.high %>%
  group_by(VE_infect,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptOR_degUnder_highChange_trueVeff))

#Cumulative proportion of symptomatic SARS-CoV-2 by vaccination status and ve.infect 
#at timestep when SARS-CoV-2 at its highest epidemic growth point 
cbind(cum.prop.unvac=aggDF.ve.susc0.1.ve.infect$Covid_cumPropSymptInfect_unvac.50.,
      cum.prop.vac= aggDF.ve.susc0.1.ve.infect$Covid_cumPropSymptInfect_vac.50.,
      t.at.highchange=aggDF.ve.susc0.1.ve.infect$t_highChange.50.)

######################################################
#FIGURE 6 & ANALSIS FOR OBJECTIVE 3 (1 of 2): 
#HOW TESTING BIAS IS INFLUENCED BY PROBABILIY OF TRANSMISSION
#ACROSS COHORT & TEST-NEGATIVE DESIGN

#Using higher vaccine efficacy against susceptibility (0.55)

#Load in data
df.ve.susc.0.55_prob.trans.high<-read.csv(paste(pathway,"data/obj.2&3_sens.analyses/prob.trans.sens_ve.susc.0.55.csv",sep="/")) 

#create factor variable for testing scenarios
df.ve.susc.0.55_prob.trans.high$Label_testingDiff<-factor(df.ve.susc.0.55_prob.trans.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

# Fix direction of underestimation variable
df.ve.susc.0.55_prob.trans.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.55_prob.trans.high$VE_symptOR_degUnder_highChange_trueVeff
df.ve.susc.0.55_prob.trans.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.55_prob.trans.high$VE_symptRR_degUnder_highChange_trueVeff

#select simulations where SARS-CoV-2 cumulative proportion was above 0.1 
df.ve.susc.0.55_prob.trans.high<-subset(df.ve.susc.0.55_prob.trans.high, Covid_cumPropSymptInfect_highChange>0.001)

#Set-up for figures
df.ve.susc.0.55_prob.trans.high$Covid_probTrans<-factor(df.ve.susc.0.55_prob.trans.high$Covid_probTrans,levels=c("0.03", levels(factor(df.ve.susc.0.55_prob.trans.high$Covid_probTrans))))
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

##Figure 6a
#Relative Risk - Cohort Design
VE.susc0.55_VE.probtrans.RR<-ggplot(df.ve.susc.0.55_prob.trans.high,aes(x=as.factor(Covid_probTrans) ,y=VE_symptRR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(1),
              width=1) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+ scale_x_discrete(drop=F) +
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Probability of Transmission") +
  ylab("Magnitude of Bias in VE_RR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario")) + theme(legend.position="none")

##Figure 6e
#Odds Ratio - Test-Negative Design
VE.susc0.55_VE.probtrans.OR<-ggplot(df.ve.susc.0.55_prob.trans.high,aes(x=as.factor(Covid_probTrans) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+ scale_x_discrete(drop=F) +
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Probability of Transmission") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario"))  + theme(legend.position="none")


#Set-up for cumulative symptomatic SARS-CoV-2 figure
aggDF.ve.susc0.55.prob.trans<- aggregate(cbind(Covid_cumPropSymptInfect_vac,Covid_cumPropSymptInfect_unvac, t_highChange) ~ Covid_probTrans, df.ve.susc.0.55_prob.trans.high, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ve.susc0.55.prob.trans<-do.call(data.frame, aggDF.ve.susc0.55.prob.trans)
aggDF.ve.susc0.55.prob.trans$Covid_probTrans<-factor(aggDF.ve.susc0.55.prob.trans$Covid_probTrans, levels=c("0.03",levels(factor(aggDF.ve.susc0.55.prob.trans$Covid_probTrans))))

aggDF.ve.susc0.55.prob.trans$place_holder<-0.93 #set-up circle placement
aggDF.ve.susc0.55.prob.trans$place_holder2<-1.13 #set-up circle placement

##Figure 6c
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
VE.susc0.55_SymptInfect.probtrans<-ggplot(aggDF.ve.susc0.55.prob.trans, aes(x = Covid_probTrans)) + 
  geom_point(aes(y = place_holder,size = Covid_cumPropSymptInfect_vac.50.), alpha = 0.75, shape = 21, fill="goldenrod4")  +
  geom_point(aes(y= place_holder2, size = Covid_cumPropSymptInfect_unvac.50.), alpha = 0.75, shape = 21, fill="goldenrod2")  +
  scale_size_continuous(limits = c(0.001, 0.21), 
                        range = c(0,30), breaks = c(0.01,0.05,0.1,0.15,0.2)) + 
  scale_y_continuous(limits = c(0.85, 1.21)) +
  theme_classic()+
    scale_x_discrete(limits = levels(aggDF.ve.susc0.55.prob.trans$Covid_probTrans)) +
  labs( x= "", y = "", size = "Cum. Prop. \nSympt. Infected", fill = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.ticks.y=element_blank() ,
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Probability of Transmission")  + guides(fill="none") +
  theme(legend.position="none")

#set pathway to save figures
fig6.pathway<-paste(pathway,"figures/fig6",sep="/")

#Save Figures
pdf(file=paste(fig6.pathway,"Fig.6a_VE.susc0.55_VE.probtrans.RR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.55_VE.probtrans.RR
dev.off()

pdf(file=paste(fig6.pathway,"Fig.6c_VE.susc0.55_SymptInfect.probtrans.pdf", sep="/"), 
    width=10, height=3)
VE.susc0.55_SymptInfect.probtrans
dev.off()

pdf(file=paste(fig6.pathway,"Fig.6e_VE.susc0.55_VE.probtrans.OR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.55_VE.probtrans.OR
dev.off()

##Descriptive summary (Across probabilities of transmission of SARS-CoV-2 [prob.trans])

df.ve.susc.0.55_prob.trans.high %>%
  group_by(Covid_probTrans,Label_testingDiff) %>%
  summarize(days = n())

#Median observed VE underestimate for cohort design by prob.trans and testing scenarios
df.ve.susc.0.55_prob.trans.high %>%
  group_by(Covid_probTrans,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptRR_degUnder_highChange_trueVeff))

#Median observed VE underestimate for test-negative design by prob.trans and testing scenarios
df.ve.susc.0.55_prob.trans.high %>%
  group_by(Covid_probTrans,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptOR_degUnder_highChange_trueVeff))

#Cumulative proportion of symptomatic SARS-CoV-2 by prob.trans and vaccination status 
#at timestep when SARS-CoV-2 at its highest epidemic growth point
cbind(cum.prop.unvac=aggDF.ve.susc0.55.prob.trans$Covid_cumPropSymptInfect_unvac.50.,
      cum.prop.vac=aggDF.ve.susc0.55.prob.trans$Covid_cumPropSymptInfect_vac.50., 
      t.at.highchange=aggDF.ve.susc0.55.prob.trans$t_highChange.50.)

######################################################
#FIGURE 6 & ANALSIS FOR OBJECTIVE 3 (2 of 2): 
#HOW TESTING BIAS IS INFLUENCED BY PROBABILIY OF TRANSMISSION
#ACROSS COHORT & TEST-NEGATIVE DESIGN

#Using lower vaccine efficacy against susceptibility (0.1)

#Load data
df.ve.susc.0.1_prob.trans.high<-read.csv(paste(pathway,"data/obj.2&3_sens.analyses/prob.trans.sens_ve.susc.0.1.csv",sep="/"))

#create factor variable for testing scenarios
df.ve.susc.0.1_prob.trans.high$Label_testingDiff<-factor(df.ve.susc.0.1_prob.trans.high$Label_testingDiff, levels=c("equal", "modUnequal", "highUnequal"))

#Fix direction of underestimation variable
df.ve.susc.0.1_prob.trans.high$VE_symptOR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.1_prob.trans.high$VE_symptOR_degUnder_highChange_trueVeff
df.ve.susc.0.1_prob.trans.high$VE_symptRR_degUnder_highChange_trueVeff<--1*df.ve.susc.0.1_prob.trans.high$VE_symptRR_degUnder_highChange_trueVeff

#select simulations where SARS-CoV-2 cumulative proportion was above 0.1 
df.ve.susc.0.1_prob.trans.high<-subset(df.ve.susc.0.1_prob.trans.high, Covid_cumPropSymptInfect_highChange>0.001)

#Set-up for Figure
group.colors <- c( "darkgreen", "springgreen2","darkseagreen2")
names<-c("Equal Testing", "Moderately Unequal Testing", "Highly Unequal Testing" )

##Figure 6b
#Relative Risk - Cohort Design
VE.susc0.1_VE.probtrans.RR<-ggplot(df.ve.susc.0.1_prob.trans.high,aes(x=as.factor(Covid_probTrans) ,y=VE_symptRR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(1),
              width=1) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+ scale_x_discrete(drop=F) +
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Probability of Transmission") +
  ylab("Magnitude of Bias in VE_RR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario")) + theme(legend.position="none")

##Figure 6f
#Odds Ratio - Test-Negative Design
VE.susc0.1_VE.probtrans.OR<-ggplot(df.ve.susc.0.1_prob.trans.high,aes(x=as.factor(Covid_probTrans) ,y=VE_symptOR_degUnder_highChange_trueVeff, fill=Label_testingDiff)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey40", linewidth=2)+
  geom_violin(aes(fill=Label_testingDiff), trim=FALSE, position = position_dodge(width=1),
              width=1.01) +   
  geom_boxplot(
    aes(fill=Label_testingDiff), width = 0.3,
    position = position_dodge(1)
  )+ scale_x_discrete(drop=F) +
  theme_classic()+
  scale_y_continuous(limits = c(-1.74, 0.4), breaks=round(seq(from=-1.6,to=0.4,by=0.4),3)) +
  scale_fill_manual(values=group.colors, labels=names) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Probability of Transmission") +
  ylab("Magnitude of Bias in VE_OR Estimates") +
  guides(fill=guide_legend(title="Testing Scenario"))  + theme(legend.position="none")


#Set-up for cumulative symptomatic SARS-CoV-2 figure
aggDF.ve.susc0.1.prob.trans<- aggregate(cbind(Covid_cumPropSymptInfect_vac,Covid_cumPropSymptInfect_unvac, t_highChange) ~ Covid_probTrans, df.ve.susc.0.1_prob.trans.high, function(x) quantile(x, probs = seq(0,1, 0.25))) #see if multiple variables work as an outcome
aggDF.ve.susc0.1.prob.trans<-do.call(data.frame, aggDF.ve.susc0.1.prob.trans)
aggDF.ve.susc0.1.prob.trans$Covid_probTrans<-factor(aggDF.ve.susc0.1.prob.trans$Covid_probTrans)

aggDF.ve.susc0.1.prob.trans$place_holder<-0.93 #set-up circle placement
aggDF.ve.susc0.1.prob.trans$place_holder2<-1.13 #set-up circle placement

##Figure 6d
#Cumulative proportion of symptomatic infection of SARS-CoV-2 by vaccination status
VE.susc0.1_SymptInfect.probtrans<-ggplot(aggDF.ve.susc0.1.prob.trans, aes(x = Covid_probTrans)) + 
  geom_point(aes(y = place_holder,size = Covid_cumPropSymptInfect_vac.50.), alpha = 0.75, shape = 21, fill="goldenrod4")  +
  geom_point(aes(y= place_holder2, size = Covid_cumPropSymptInfect_unvac.50.), alpha = 0.75, shape = 21, fill="goldenrod2")  +
  scale_size_continuous(limits = c(0.001, 0.21), 
                        range = c(0,30), breaks = c(0.01,0.05,0.1,0.15,0.2)) + 
  scale_y_continuous(limits = c(0.85, 1.21)) +
  theme_classic()+
  labs( x= "", y = "", size = "Cum. Prop. \nSympt. Infected", fill = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_blank(),  
        axis.ticks.y=element_blank() ,
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = 2, face = "plain"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22)) + xlab("Probability of Transmission")  + guides(fill="none") +
  theme(legend.position="none")

#set pathway to save figures
fig6.pathway<-paste(pathway,"figures/fig6",sep="/")

#Save Figures
pdf(file=paste(fig6.pathway,"Fig.6b_VE.susc0.1_VE.probtrans.RR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.1_VE.probtrans.RR
dev.off()

pdf(file=paste(fig6.pathway,"Fig.6d_VE.susc0.1_SymptInfect.probtrans.pdf", sep="/"), 
    width=10, height=3)
VE.susc0.1_SymptInfect.probtrans
dev.off()

pdf(file=paste(fig6.pathway,"Fig.6f_VE.susc0.1_VE.probtrans.OR.pdf", sep="/"), 
    width=10, height=6)
VE.susc0.1_VE.probtrans.OR
dev.off()


##Descriptive summary (Across probabilities of transmission of SARS-CoV-2 [prob.trans])

#Number of simulations used in analyses by prob.trans and testing scenarios
df.ve.susc.0.1_prob.trans.high %>%
  group_by(Covid_probTrans,Label_testingDiff) %>%
  summarize(days = n())

#Median observed VE underestimate for cohort design by prob.trans and testing scenarios
df.ve.susc.0.1_prob.trans.high %>%
  group_by(Covid_probTrans,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptRR_degUnder_highChange_trueVeff))

#Median observed VE underestimate for test-negative design by prob.trans and testing scenarios
df.ve.susc.0.1_prob.trans.high %>%
  group_by(Covid_probTrans,Label_testingDiff) %>%
  summarize(median.values = median(VE_symptOR_degUnder_highChange_trueVeff))

#Cumulative proportion of symptomatic SARS-CoV-2 by prob.trans and vaccination status 
#at timestep when SARS-CoV-2 at its highest epidemic growth point
cbind(cum.prop.unvac=aggDF.ve.susc0.1.prob.trans$Covid_cumPropSymptInfect_unvac.50.,
      cum.prop.vac=aggDF.ve.susc0.1.prob.trans$Covid_cumPropSymptInfect_vac.50., 
      t.at.highchange=aggDF.ve.susc0.1.prob.trans$t_highChange.50.)




