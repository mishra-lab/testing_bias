### Description:
All datasets listed in this folder are called in by AppendixAnalyses_and_Figures.R to generate the figures and analyses found in the appendix. 

### Objective 1 Datasets Overview:
- *aggDF.ves0.1.csv*: contains quantile summaries for selected variables (0%, 25%, 50%, 75%, 100%), aggregated from the 100 csv files when vaccine efficacy against susceptibility is lower (located in the *obj.1_ve.susc.0.1* folder)
- *aggDF.ves0.55.csv*: contains quantile summaries for selected variables (0%, 25%, 50%, 75%, 100%), aggregated from the 100 csv files when vaccine efficacy against susceptibility is higher (located in the *obj.1_ve.susc.0.55* folder)
- *df.vs.susc0.1_other.etiologies.sens.high.csv*: contains all variables sampled at the point of highest epidemic growth across different prevalances of COVID-like symptoms due to other etiologies (0.05, 0.1, 0.15, 0.2) when vaccine efficacy against susceptibility is lower. Sampled data comes from 100 epidemic realizations. 
- *df.vs.susc0.55_other.etiologies.sens.high.csv*:contains all variables sampled at the point of highest epidemic growth across different prevalances of COVID-like symptoms due to other etiologies (0.05, 0.1, 0.15, 0.2) when vaccine efficacy against susceptibility is higher. Sampled data comes from 100 epidemic realizations. 
- *df.ves0.1_highChange.peak.csv*: contains all variables sampled at the point of highest epidemic growth and at the epidemic peak, compiled from each of the 100 csv files when vaccine efficacy against susceptibility is lower (located in the *obj.1_ve.susc.0.1* folder)
- *df.ves0.55_highChange.peak.csv*: contains all variables sampled at the point of highest epidemic growth and at the epidemic peak, compiled from each of the 100 csv files when vaccine efficacy against susceptibility is higher (located in the *obj.1_ve.susc.0.55* folder)

### Objective 2 Datasets Overview (datasets in the *sens.analyses* folder):
- *df.ve.susc.0.1_prob.trans.sens* and *df.ve.susc.0.55_prob.trans.sens*: contains quantile summaries for selected variables (0%, 25%, 50%, 75%, 100%) across varying levels of probability of transmission (0.03, 0.05, 0.07, 0.09,0.11) when efficacy against susceptibility is lower and higher, respectively. 
- *df.ve.susc.0.1_prob.trans.sens_highchange*: contains the median value of selected variables at the point of highest epidemic growth across different probabilities of transmission (0.03, 0.05, 0.07, 0.09,0.11) when efficacy against susceptibility is lower. 
