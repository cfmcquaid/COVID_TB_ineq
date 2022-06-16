# Finn McQuaid & Marc Henrion 10/12/2021
# COVID impact on TB notifications by age/sex
setwd("C:/Users/eidefmcq/Documents/Simulations/COVID Sex")
rm(list=ls())
## ================ Load packages ==============================================
library("tidyverse")
library("data.table")
library("ggplot2")
library("dplyr")
library("ggrepel")
library("mgsub")
library("ggthemes")
library("meta")
library("kableExtra")
library("ggridges")
library("viridis")

## ================ Load data ==================================================
# High TB burden country lists
country_HBC<-c('Angola','Bangladesh','Brazil','Central African Republic','China','Congo','Democratic People\'s Republic of Korea','Democratic Republic of the Congo','Ethiopia','Gabon','India','Indonesia','Kenya','Lesotho','Liberia','Mongolia','Mozambique','Myanmar','Namibia','Nigeria','Pakistan','Papua New Guinea','Philippines','Sierra Leone','South Africa','Thailand','Uganda','United Republic of Tanzania','Viet Nam','Zambia')
country_MDR<-c('Angola','Azerbaijan','Bangladesh','Belarus','China','Democratic People\'s Republic of Korea','Democratic Republic of the Congo','India','Indonesia','Kazakhstan','Kyrgyzstan','Mongolia','Mozambique','Myanmar','Nepal','Nigeria','Pakistan','Papua New Guinea','Peru','Philippines','Republic of Moldova','Russian Federation','Somalia','South Africa','Tajikistan','Ukraine','Uzbekistan','Viet Nam','Zambia','Zimbabwe')
country_HIV<-c('Botswana','Brazil','Cameroon','Central African Republic','China','Congo','Democratic Republic of the Congo','Eswatini','Ethiopia','Gabon','Guinea','Guinea-Bissau','India','Indonesia','Kenya','Lesotho','Liberia','Malawi','Mozambique','Myanmar','Namibia','Nigeria','Philippines','Russian Federation','South Africa','Thailand','Uganda','United Republic of Tanzania','Zambia','Zimbabwe')
country_list<-unique(c(country_HBC,country_MDR,country_HIV))
# Remove Mozambique & Uganda as incomplete age data, Angola and Papua New Guinea as only two data points prior to 2020
country_list<-country_list[!country_list %in% c("Angola","Mozambique","Papua New Guinea","Uganda")]
# Read in WHO notification data from https://www.who.int/teams/global-tuberculosis-programme/data
WHO_not<-fread('TB_notifications_2022-06-15.csv')
# Use only HBC countries
WHO_not$g_whoregion<-mgsub(WHO_not$g_whoregion,c("AFR","AMR","EMR","EUR","SEA","WPR"),c("African Region","Region of the Americas","Eastern Mediterranean Region","European Region","South-East Asia Region","Western Pacific Region"))
WHO_not<-WHO_not[country%in%country_list]
# Remove years where new cases only (not new + relapse) are recorded
# Except Azerbaijan where this is always the case, and Mongolia where there is a blank for 2020
WHO_not[country=="Azerbaijan",rel_in_agesex_flg:=1]
WHO_not[country=="Mongolia",rel_in_agesex_flg:=1]
WHO_not<-WHO_not[rel_in_agesex_flg==1]   
WHO_not<-WHO_not[year%in%c(2013:2020)]
# Define categories
WHO_not[,men:=newrel_m15plus]
WHO_not[,women:=newrel_f15plus]
WHO_not[,children:=newrel_m014+newrel_f014]
WHO_not[,adults:=newrel_m1524+newrel_m2534+newrel_m3544+newrel_m4554+newrel_m5564+newrel_f1524+newrel_f2534+newrel_f3544+newrel_f4554+newrel_f5564]
WHO_not[,elderly:=newrel_m65+newrel_f65]
# Simplify data set
dat<-select(WHO_not,country,iso3,g_whoregion,year,men,women,children,adults,elderly)
options(knitr.kable.NA = '')
# Show data
dat %>%
  dplyr::select(!c(country)) %>%
  knitr::kable(col.names=c("Country code","Region","Year","Men (observed)","Women (observed)","Children (observed)","Adults (observed)","Elderly (observed)")) %>%
  kableExtra::kable_styling(full_width = FALSE)
# Total notifications
sum(WHO_not[year%in%c(2013:2019),men],na.rm=TRUE)
sum(WHO_not[year%in%c(2013:2019),women],na.rm=TRUE)
sum(WHO_not[year%in%c(2013:2019),children],na.rm=TRUE)
sum(WHO_not[year%in%c(2013:2019),adults],na.rm=TRUE)
sum(WHO_not[year%in%c(2013:2019),elderly],na.rm=TRUE)

sum(WHO_not[year%in%c(2020),men],na.rm=TRUE)
sum(WHO_not[year%in%c(2020),women],na.rm=TRUE)
sum(WHO_not[year%in%c(2020),children],na.rm=TRUE)
sum(WHO_not[year%in%c(2020),adults],na.rm=TRUE)
sum(WHO_not[year%in%c(2020),elderly],na.rm=TRUE)


## ================ Run linear models ==========================================
mod<-list()
countries<-unique(dat$iso3)
gr<-expand.grid(countries,2013:2020)
datPred<-data.frame(iso3=gr[,1],year=gr[,2],men=NA,women=NA,children=NA,adults=NA,elderly=NA,men_SE=NA,women_SE=NA,children_SE=NA,adults_SE=NA,elderly_SE=NA)
for(c in countries){
  for(var in c("men","women","children","adults","elderly")){
    mod[[paste(sep="_",c,var)]]<-glm(as.formula(paste(sep="",var," ~ year")), data=dat %>% dplyr::filter(iso3==c & year<2020),family=poisson)
    tmpFit<-predict(mod[[paste(sep="_",c,var)]],newdata=datPred %>% dplyr::filter(iso3==c),se.fit=TRUE,type="response")
    datPred[datPred$iso3==c,var]<-tmpFit$fit
    tmpFit<-predict(mod[[paste(sep="_",c,var)]],newdata=datPred %>% dplyr::filter(iso3==c),se.fit=TRUE,type="link")
    datPred[datPred$iso3==c,paste(sep="_",var,"SE")]<-tmpFit$se.fit
  }
}
# Show data
datPred %>%
  dplyr::select(!contains("SE")) %>%
  knitr::kable(col.names=c("Country code","Year","Men (Expected)","Women (expected)","Children (expected)","Adults (expected)","Elderly (expected)")) %>%
  kableExtra::kable_styling(full_width = FALSE)

## ================ Plot data points ===========================================
datObsLong<-dat %>%
  dplyr::select(iso3,year,men,women,adults,children,elderly) %>%
  tidyr::pivot_longer(cols=c(men,women,adults,children,elderly),names_to="group",values_to="observed")
datExpLong<-datPred %>%
  dplyr::select(iso3,year,men,women,adults,children,elderly) %>%
  tidyr::pivot_longer(cols=c(men,women,adults,children,elderly),names_to="group",values_to="expected")
datExpSELong<-datPred %>%
  dplyr::select(iso3,year,men_SE,women_SE,adults_SE,children_SE,elderly_SE) %>%
  tidyr::pivot_longer(cols=c(men_SE,women_SE,adults_SE,children_SE,elderly_SE),names_to="group",values_to="SE")
datExpSELong$group<-gsub(pattern="_SE",replacement="",datExpSELong$group)
datFullLong<-datObsLong %>%
  mutate(
    expected=datExpLong$expected[match(paste(sep="_",iso3,year,group),paste(sep="_",datExpLong$iso3,datExpLong$year,datExpLong$group))],
    expectedLow=exp(log(datExpLong$expected[match(paste(sep="_",iso3,year,group),paste(sep="_",datExpLong$iso3,datExpLong$year,datExpLong$group))])-qnorm(0.975)*datExpSELong$SE[match(paste(sep="_",iso3,year,group),paste(sep="_",datExpLong$iso3,datExpLong$year,datExpLong$group))]),
    expectedUpp=exp(log(datExpLong$expected[match(paste(sep="_",iso3,year,group),paste(sep="_",datExpLong$iso3,datExpLong$year,datExpLong$group))])+qnorm(0.975)*datExpSELong$SE[match(paste(sep="_",iso3,year,group),paste(sep="_",datExpLong$iso3,datExpLong$year,datExpLong$group))]),
  )
datFullLong %>%
  ggplot(mapping=aes(x=year)) +
  geom_ribbon(mapping=aes(ymin=expectedLow,ymax=expectedUpp),fill="orange",alpha=0.5) +
  geom_line(mapping=aes(y=expected),col="orange",lwd=1.25) +
  geom_point(mapping=aes(y=observed),col="steelblue",size=2) + 
  facet_wrap(~paste(sep=" ",iso3,group),ncol = 5,scales = "free_y")
ggsave("Plot_CovidTb_trends.png",device="png",width=20,height=92,units=c("cm"))
## ================ Reformat data ==============================================
datPredLong<-datPred %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(!contains("SE")) %>%
  tidyr::pivot_longer(cols=c(men,women,children,adults,elderly),names_to="group",values_to="expected")
datPredSELong<-datPred %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(c(iso3,year,contains("SE"))) %>%
  tidyr::pivot_longer(cols=c(men_SE,women_SE,children_SE,adults_SE,elderly_SE),names_to="group",values_to="expected")
datLong<-dat %>% 
  dplyr::filter(year==2020) %>%
  dplyr::select(iso3,men,women,children,adults,elderly) %>%
  tidyr::pivot_longer(cols=c(men,women,children,adults,elderly),names_to="group",values_to="observed") %>%
  dplyr::mutate(
    expected=datPredLong$expected[match(paste(sep="_",iso3,group),paste(sep="_",datPredLong$iso3,datPredLong$group))],
    expected_SE=datPredSELong$expected[match(paste(sep="_",iso3,group,"SE"),paste(sep="_",datPredSELong$iso3,datPredSELong$group))]
  )

## ================ Compute relative risks and SE ==============================
analysisFun<-function(refDat,compDat,B=1e4){
# refDat = data frame with observed, expected and expected SEs for the reference group for each country
# compDat = same dataframe but including all comparator groups
# B = number of parametric bootstrap replicates to sample
# Filter out countries with complete data
  allCountries<-intersect(refDat$iso3[!is.na(refDat$observed) & !is.na(refDat$expected) & !is.na(refDat$expected_SE)],compDat$iso3[!is.na(compDat$observed) & !is.na(compDat$expected) & !is.na(compDat$expected_SE)])
  refDat<-refDat %>% dplyr::filter(iso3 %in% allCountries) %>% dplyr::mutate(risk=NA)
  compDat<-compDat %>% dplyr::filter(iso3 %in% allCountries) %>% dplyr::mutate(risk=NA,RR=NA,RR_low=NA,RR_upp=NA,RR_logSE=NA)
# Compute RRs
  for(c in allCountries){
    R0<-((refDat$expected-refDat$observed)/refDat$expected)[refDat$iso3==c]
    R0[R0<0]<-0
    refDat$risk[refDat$iso3==c]<-R0
    idx<-which(compDat$iso3==c)
    compDat$risk[idx]<-((compDat$expected-compDat$observed)/compDat$expected)[idx]
    compDat$risk[idx][compDat$risk[idx]<0]<-0
    compDat$RR[idx]<-compDat$risk[idx]/R0
  }
# Parametric boostrapping using the estimated SEs
  bootRR<-compDat %>% dplyr::select(iso3,group)
  for(b in 1:B){
    refDat_BS<-refDat
    refDat_BS$expected<-exp(rnorm(n=nrow(refDat),mean=log(refDat$expected),sd=refDat$expected_SE)) 
    compDat_BS<-compDat
    compDat_BS$expected<-exp(rnorm(n=nrow(compDat),mean=log(compDat$expected),sd=compDat$expected_SE))
    for(c in allCountries){
      R0<-((refDat_BS$expected-refDat_BS$observed)/refDat_BS$expected)[refDat_BS$iso3==c]
      R0[R0<0]<-0
      refDat_BS$risk[refDat_BS$iso3==c]<-R0
      idx<-which(compDat_BS$iso3==c)
      compDat_BS$risk[idx]<-((compDat_BS$expected-compDat_BS$observed)/compDat_BS$expected)[idx]
      compDat_BS$risk[idx][compDat_BS$risk[idx]<0]<-0
      compDat_BS$RR[idx]<-compDat_BS$risk[idx]/R0
    }
    
    bootRR[,paste(sep="_","RR",b)]<-compDat_BS$RR
  }
  compDat[,c("RR_low","RR_upp")]<-t(apply(X=bootRR %>% dplyr::select(contains("RR_")),MARGIN=1,FUN=quantile,probs=c(0.025,0.975),na.rm=TRUE))
  logsd<-function(x){
    x<-x[!is.na(x)] # removes NaNs due to 0/0
    x[x<1e-4]<-1e-4 # avoids -Inf values when logged
    x[x>1e4]<-1e4 # avoids +Inf values when logged
    res<-sd(log(x))
    return(res)
  }
  compDat[,"RR_logSE"]<-apply(X=bootRR %>% dplyr::select(contains("RR_")),MARGIN=1,FUN=logsd)
  return(list=list(compDat=compDat,reference=refDat,bootRR=bootRR))
}
plotFun<-function(analysisObj,maxRR=25){
# analysisObj = output from analysisFun()
  g1<-analysisObj$compDat %>%
    mutate(iso3=paste(sep="",iso3," (",format(nsmall=2,round(digits=1,100*analysisObj$reference$risk[match(iso3,analysisObj$reference$iso3)])),"%)")) %>%
    ggplot() +
    geom_segment(mapping=aes(y=group,yend=group,x=RR_low,xend=RR_upp)) +
    geom_point(mapping=aes(x=RR,y=group)) +
    geom_vline(xintercept=1,lty=2,col="darkgrey") +
    facet_wrap(~iso3,scales="free_x") +
    labs(caption="Estimated risk ratios with 95% confidence intervals obtained from parametric bootstrapping from fitted linear models.\nRisks of being missed in the reference group are shown in brackets in the panel titles.",title=paste(sep="","Reference group: ",unique(analysisObj$reference$group,".")))
  tmp<-analysisObj$compDat %>%
    mutate(iso3=paste(sep="",iso3," (",format(nsmall=2,round(digits=1,100*analysisObj$reference$risk[match(iso3,analysisObj$reference$iso3)])),"%)"))
  g2<-analysisObj$bootRR %>%
    mutate(iso3=paste(sep="",iso3," (",format(nsmall=2,round(digits=1,100*analysisObj$reference$risk[match(iso3,analysisObj$reference$iso3)])),"%)")) %>%
    pivot_longer(cols=contains("RR_"),names_to = "bootNum",values_to="RR") %>%
    dplyr::filter(abs(RR)<maxRR) %>%
    ggplot(mapping=aes(x = RR, y = group, fill = group)) +
    geom_segment(data=tmp,mapping=aes(y=group,yend=group,x=RR_low,xend=RR_upp),col="grey50",size=1,position = position_nudge(y = -0.1)) +
    geom_point(data=tmp,mapping=aes(x=RR,y=group),col="grey50",size=1.5,position = position_nudge(y = -0.1)) +
    geom_density_ridges(alpha=0.75) +
    geom_vline(xintercept=1,lty=2,col="darkgrey") +
    scale_fill_manual(values=viridis(length(unique(analysisObj$bootRR$group)))) +
    theme_ridges() +
    theme(legend.position = "none",axis.title.x=element_text(hjust=0.5)) +
    ylab("") +
    xlab("relative risk") +
    facet_wrap(~iso3,scales="free_x") +
    labs(caption="Estimated risk ratios with 95% confidence intervals obtained from parametric bootstrapping from fitted linear models.\nDensity histograms show the empirical distributions for the risk ratio computed from the parametric boostrapped samples.\nGrey bars below each histogram show the derived 95% confidence intervals for the risk ratio estimates.\nRisks of being missed in the reference group are shown in brackets in the panel titles.",title=paste(sep="","Reference group: ",unique(analysisObj$reference$group,".")))
  return(list(forestPlot=g1,ridgePlot=g2))
}

## ================ Calculate missed cases ================
refDat<-datLong
refDat<-refDat %>%
  dplyr::mutate(
    missed=case_when(
      expected-observed>0~round(expected-observed),
      TRUE~0),
    missedLow=case_when(
      exp(log(expected)-qnorm(0.975)*expected_SE)-observed>0~round(exp(log(expected)-qnorm(0.975)*expected_SE)-observed),
      TRUE~0),
    missedUpp=case_when(
      exp(log(expected)+qnorm(0.975)*expected_SE)-observed>0~round(exp(log(expected)+qnorm(0.975)*expected_SE)-observed),
      TRUE~0)
  ) %>%
  mutate(
    missed_95CI=paste(sep="",format(big.mark=",",round(missed))," (",format(big.mark=",",missedLow),", ",format(big.mark=",",missedUpp),")")
  ) %>%
  mutate(
    observed=format(big.mark=",",observed,scientific=FALSE),
  )
refDat %>%
  dplyr::select(c(iso3,group,observed,expected,expected_SE,missed_95CI)) %>%
  knitr::kable(col.names=c("Country code","Group","Observed","Expected","SE for log(expected)","Missed (95% CI)"),caption = "Observed and expected counts for 2020 for different groups.\nExpected counts are derived from a Poisson regression model fitted to data from 2016-2019.\nThe 95% CIs for missed cases are derived using a normal approximation of the log(count) scale.") %>%
  kableExtra::kable_styling(full_width = FALSE)
# Total missed cases
sum(refDat[refDat$group=="men",6])
sum(refDat[refDat$group=="men",7])
sum(refDat[refDat$group=="men",8])
sum(refDat[refDat$group=="men",6])/sum(refDat[refDat$group=="men",4])
sum(refDat[refDat$group=="women",6])
sum(refDat[refDat$group=="women",7])
sum(refDat[refDat$group=="women",8])
sum(refDat[refDat$group=="women",6])/sum(refDat[refDat$group=="women",4])
sum(refDat[refDat$group=="children",6])
sum(refDat[refDat$group=="children",7])
sum(refDat[refDat$group=="children",8])
sum(refDat[refDat$group=="children",6])/sum(refDat[refDat$group=="children",4])
sum(refDat[refDat$group=="adults",6])
sum(refDat[refDat$group=="adults",7])
sum(refDat[refDat$group=="adults",8])
sum(refDat[refDat$group=="adults",6])/sum(refDat[refDat$group=="adults",4])
sum(refDat[refDat$group=="elderly",6])
sum(refDat[refDat$group=="elderly",7])
sum(refDat[refDat$group=="elderly",8])
sum(refDat[refDat$group=="elderly",6])/sum(refDat[refDat$group=="elderly",4])
## ================ Calculate risk ratio for reference case men ================
resm<-analysisFun(refDat=datLong %>% filter(group=="men"),compDat=datLong %>% filter(group!="men"))
# Show data
resm$compDat %>%
  dplyr::select(c(iso3,group,contains("RR"))) %>%
  knitr::kable(col.names=c("Country code","Group","RR","RR (95% CI lower bound)","RR (95% CI upper bound)","SE of log(RR)"),caption = "Relative risks of being missed using men as reference. 10,000 parametric bootstrap samples for the CI and SE.") %>%
  kableExtra::kable_styling(full_width = FALSE)
gm<-plotFun(resm)
print(gm$ridgePlot)
# Prep data for meta-analysis
resm1<-as.data.frame(resm$compDat)
resm1<-resm1[resm1$group=="women",]
resm1$Region<-dat[year=="2020",g_whoregion]
resm1$Country<-dat[year=="2020",country]
# Excess notifications countries
resm$reference[(resm$reference[,4]-resm$reference[,3]<0),1]
resm1[resm1$expected-resm1$observed<0,1]
# Remove RR of zero, infinity or NaN
resm1<-subset(resm1,!is.nan(RR))
resm1<-subset(resm1,RR!=Inf)
resm1<-subset(resm1,RR!=0)
# Conduct meta-analysis
Risk_sex<-metagen(studlab=Country,sm="RR",TE=log(RR),subgroup=Region,fixed=FALSE,data=resm1,backtransf=TRUE,lower=log(RR_low),upper=log(RR_upp))
# Countries with evidence of effect
MF_evidence_hi_strong<-sum(exp(Risk_sex$TE)>1.1&(Risk_sex$pval<0.01))
MF_evidence_hi_med<-sum(exp(Risk_sex$TE)>1.1&Risk_sex$pval>0.01&Risk_sex$pval<0.05)
MF_evidence_hi_weak<-sum(exp(Risk_sex$TE)>1.25&Risk_sex$pval>0.05&Risk_sex$pval<0.1)
MF_evidence_lo_strong<-sum(exp(Risk_sex$TE)<1/1.1&(Risk_sex$pval<0.01))
MF_evidence_lo_med<-sum(exp(Risk_sex$TE)<1/1.1&Risk_sex$pval>0.01&Risk_sex$pval<0.05)
MF_evidence_lo_weak<-sum(exp(Risk_sex$TE)<1/1.25&Risk_sex$pval>0.05&Risk_sex$pval<0.1)

## ================ Calculate risk ratio for reference case adults =============
resa<-analysisFun(refDat=datLong %>% filter(group=="adults"),compDat=datLong %>% filter(group!="adults"))
# Show data
resa$compDat %>%
  dplyr::select(c(iso3,group,contains("RR"))) %>%
  knitr::kable(col.names=c("Country code","Group","RR","RR (95% CI lower bound)","RR (95% CI upper bound)","SE of log(RR)"),caption = "Relative risks of being missed using adults as reference. 10,000 parametric bootstrap samples for the CI and SE.") %>%
  kableExtra::kable_styling(full_width = FALSE)
ga<-plotFun(resa)
print(ga$ridgePlot)
# Prep data for comparison to children meta-analysis
resa1<-as.data.frame(resa$compDat)
resac1<-resa1[resa1$group=="children",]
resac1$Region<-dat[year=="2020",g_whoregion]
resac1$Country<-dat[year=="2020",country]
# Excess notifications countries
resa$reference[(resa$reference[,4]-resa$reference[,3]<0),1]
resac1[resac1$expected-resac1$observed<0,1]
# Remove RR of zero, infinity or NaN
resac1<-subset(resac1,!is.nan(RR))
resac1<-subset(resac1,RR!=Inf)
resac1<-subset(resac1,RR!=0)
# Conduct meta-analysis
Risk_children<-metagen(studlab=Country,sm="RR",TE=log(RR),subgroup=Region,fixed=FALSE,data=resac1,backtransf=TRUE,lower=log(RR_low),upper=log(RR_upp))
# Countries with evidence of effect
AC_evidence_hi_strong<-sum(exp(Risk_children$TE)>1.1&(Risk_children$pval<0.01))
AC_evidence_hi_med<-sum(exp(Risk_children$TE)>1.1&Risk_children$pval>0.01&Risk_children$pval<0.05)
AC_evidence_hi_weak<-sum(exp(Risk_children$TE)>1.25&Risk_children$pval>0.05&Risk_children$pval<0.1)
AC_evidence_lo_strong<-sum(exp(Risk_children$TE)<1/1.1&(Risk_children$pval<0.01))
AC_evidence_lo_med<-sum(exp(Risk_children$TE)<1/1.1&Risk_children$pval>0.01&Risk_children$pval<0.05)
AC_evidence_lo_weak<-sum(exp(Risk_children$TE)<1/1.25&Risk_children$pval>0.05&Risk_children$pval<0.1)
# Prep data for comparison to elderly meta-analysis
resae1<-resa1[resa1$group=="elderly",]
resae1$Region<-dat[year=="2020",g_whoregion]
resae1$Country<-dat[year=="2020",country]
# Excess notifications countries
resae1[resae1$expected-resae1$observed<0,1]
# Remove RR of zero, infinity or NaN
resae1<-subset(resae1,!is.nan(RR))
resae1<-subset(resae1,RR!=Inf)
resae1<-subset(resae1,RR!=0)
# Conduct meta-analysis
Risk_elderly<-metagen(studlab=Country,sm="RR",TE=log(RR),subgroup=Region,fixed=FALSE,data=resae1,backtransf=TRUE,lower=log(RR_low),upper=log(RR_upp))
# Countries with evidence of effect
AE_evidence_hi_strong<-sum(exp(Risk_elderly$TE)>1.1&(Risk_elderly$pval<0.01))
AE_evidence_hi_med<-sum(exp(Risk_elderly$TE)>1.1&Risk_elderly$pval>0.01&Risk_elderly$pval<0.05)
AE_evidence_hi_weak<-sum(exp(Risk_elderly$TE)>1.25&Risk_elderly$pval>0.05&Risk_elderly$pval<0.1)
AE_evidence_lo_strong<-sum(exp(Risk_elderly$TE)<1/1.1&(Risk_elderly$pval<0.01))
AE_evidence_lo_med<-sum(exp(Risk_elderly$TE)<1/1.1&Risk_elderly$pval>0.01&Risk_elderly$pval<0.05)
AE_evidence_lo_weak<-sum(exp(Risk_elderly$TE)<1/1.25&Risk_elderly$pval>0.05&Risk_elderly$pval<0.1)

## ================ Plot forest plots ==========================================
# Men to women comparison
png("forest_MF.png", width = 1000, height = 1800, res=120) 
forest(x=Risk_sex, print.subgroup.name=FALSE,text.random="Overall summary",text.random.w="Regional summary",ref=1,at=(c(.1, 1, 10)),
       leftcols=c("studlab"),rightcols=c("effect","ci"),weight.study="same",weight.subgroup="same",test.subgroup.random=FALSE,xlim=c(0.1,10),
       lty.random = 0,hetstat=FALSE,sortvar=-TE,leftlabs = c("Country"),smlab=("Women:Men"),rightlabs = c("Risk ratio","95% CI"),
       col.study="black",col.diamond = "white",col.square="black",col.inside="black",squaresize=0.5,col.by="black",allstudies=FALSE)
dev.off() 
# Adults to children comparison
png("forest_AC.png", width = 1000, height = 1800, res=120) 
forest(x=Risk_children, backtransf=TRUE,print.subgroup.name=FALSE,text.random="Overall summary",text.random.w="Regional summary",ref=1,at=(c(.1, 1, 10)),
       leftcols=c("studlab"),rightcols=c("effect","ci"),weight.study="same",weight.subgroup="same",test.subgroup.random=FALSE,xlim=c(0.1,10),
       lty.random = 0,hetstat=FALSE,sortvar=-TE,leftlabs = c("Country"),smlab=("Children:Adults"),rightlabs = c("Risk ratio","95% CI"),
       col.study="black",col.diamond = "white",col.square="black",col.inside="black",squaresize=0.5,col.by="black",allstudies=FALSE)
dev.off() 
# Adults to elderly comparison
png("forest_AE.png", width = 1000, height = 1800, res=120) 
forest(x=Risk_elderly, backtransf=TRUE,print.subgroup.name=FALSE,text.random="Overall summary",text.random.w="Regional summary",ref=1,at=(c(.1, 1, 10)),
       leftcols=c("studlab"),rightcols=c("effect","ci"),weight.study="same",weight.subgroup="same",test.subgroup.random=FALSE,xlim=c(0.1,10),
       lty.random = 0,hetstat=FALSE,sortvar=-TE,leftlabs = c("Country"),smlab=("Elderly:Adults"),rightlabs = c("Risk ratio","95% CI"),
       col.study="black",col.diamond = "white",col.square="black",col.inside="black",squaresize=0.5,col.by="black",allstudies=FALSE)
dev.off() 
