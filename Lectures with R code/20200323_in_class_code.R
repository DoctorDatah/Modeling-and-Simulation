#-------------------------------------------#
# Importing data
#-------------------------------------------#
ncov_simple=read.csv(file.choose())
ncov_simple=ncov_simple$days
summary(ncov_simple)


#-------------------------------------------#
# Bootstrapping samples for the median
#-------------------------------------------#

#median of the original data is about 4 days
median(ncov_simple)  

#bootstrapping
bootsize=5000
bootresult=rep(NA,bootsize)
for(i in 1:bootsize){
  bootresult[i]=median(sample(ncov_simple,replace=T))
}

#estimate of the sd of the median; more variation than the mean
sd(bootresult)  

#95% confidence interval of the median is 3 to 4.5 days
quantile(bootresult,probs=c(0.025,0.975))  

#sampling distribution of the median is not normal, as the mean is
hist(bootresult,nclass=50)


#################################################################################
# The following are selected code from the incubation paper:
# Source: https://github.com/HopkinsIDD/ncov_incubation
# You can run the code if interested (need to install a few packages first),
#   but you don't have to go over this part for the class
#################################################################################
library(dplyr)
library(tidyverse)
library(lubridate)
library(coarseDataTools)
library(gridExtra)
library(ggpubr)
library(plotly)
library(MASS)
library(ggplot2)

#-------------------------------------------#
# Import data and data cleaning
#-------------------------------------------#
ncov_raw <- read_csv("../20200316/nCoV-IDD-traveler-data.csv") %>% 
  rename(EL_date=EL, ER_date=ER, SL_date=SL, SR_date=SR)
## change dates to restrict exposure to after 1 December 2019
## add other times where missing
ncov_ELSR <- ncov_raw %>% 
  # if EL is missing or before 1 Dec 2019, use 1 Dec 2019
  mutate(EL_date=ifelse(is.na(EL_date),"2019-12-01 00:00:00", EL_date) %>% 
           ymd_hms() %>% 
           if_else(. < ymd_hms("2019-12-01 00:00:00"), ymd_hms("2019-12-01 00:00:00"), .),
         # if SR is missing, use PR
         SR_date=ifelse(ymd_hms(SR_date) %>% is.na, PR, SR_date) %>% 
           ymd_hms(),
         # SR_fever is only for cases with confirmed fever dates
         SR_fever=ymd_hms(SR_fever))
ncov <- ncov_ELSR %>% 
  # if ER is missing, use SR; if SL is missing, use EL
  mutate(ER_date=if_else(is.na(ER_date), SR_date, ymd_hms(ER_date)),
         ER_date=if_else(ER_date>SR_date, SR_date, ER_date),
         SL_date=if_else(is.na(SL_date), EL_date, ymd_hms(SL_date)),
         SL_date=if_else(SL_date<EL_date, EL_date, SL_date),
         SL_fever=if_else(is.na(SL_fever) & !is.na(SR_fever),
                          SL_date, ymd_hms(SL_fever)),
         SL_fever=if_else(SL_fever<EL_date, EL_date, SL_fever)) %>% 
  # calculate days since 1 Dec 2019
  mutate(EL=difftime(EL_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         ER=difftime(ER_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SL=difftime(SL_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SR=difftime(SR_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SL_fever=difftime(SL_fever, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SR_fever=difftime(SR_fever, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         PL=difftime(PL, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         PR=difftime(PR, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         E_int=ER-EL,
         S_int=SR-SL,
         S_fever_int=SR_fever-SL_fever,
         max_inc_int=SR-EL,
         min_inc_int=SL-ER) %>% 
  # remove any entries missing EL, ER, SL, or SR
  filter(!is.na(EL_date), !is.na(ER_date), !is.na(SL_date), !is.na(SR_date)) %>% 
  # remove entries that haven't been reviewed by two people
  filter(!is.na(REVIEWER2), REVIEWER2!="NA") %>% 
  # remove entries with exposure/onset intervals less than 0
  # remove entries where ER greater than SR or EL greater than SL
  filter(E_int > 0, S_int > 0, ER<=SR, SL>=EL)
## Now lets divide data sets by observation type
## only fevers
ncov_fever <- ncov %>% filter(!is.na(SL_fever) | !is.na(SR_fever))
ncov_mild <- ncov %>% filter(is.na(SL_fever) & is.na(SR_fever))
## only travel outside of China
ncov_foreign <- ncov %>% filter(COUNTRY.DEST != "China" | PROVINCE.DEST %in% c("HongKong", "Macau"))
## only fevers outside of China
ncov_foreign_fever <- ncov_foreign %>% filter(!is.na(SL_fever) | !is.na(SR_fever))
## only cases within mainland China
ncov_mainland <- ncov %>% filter(COUNTRY.DEST == "China" & !(PROVINCE.DEST %in% c("HongKong", "Macau")))
## only cases with a defined EL
ncov_EL <- ncov_raw %>% 
  filter(!is.na(EL_date)) %>% 
  # if EL is missing or before 1 Dec 2019, use 1 Dec 2019
  mutate(EL_date=ymd_hms(EL_date),
         # if SR is missing, use PR
         SR_date=ifelse(ymd_hms(SR_date) %>% is.na, PR, SR_date) %>% 
           ymd_hms(),
         # SR_fever is only for cases with confirmed fever dates
         SR_fever=ymd_hms(SR_fever)) %>% 
  # if ER is missing, use SR; if SL is missing, use EL
  mutate(ER_date=if_else(is.na(ER_date), SR_date, ymd_hms(ER_date)),
         SL_date=if_else(is.na(SL_date), EL_date, ymd_hms(SL_date)),
         SL_fever=if_else(is.na(SL_fever) & !is.na(SR_fever),
                          SL_date, ymd_hms(SL_fever))) %>% 
  # calculate days since 1 Dec 2019
  mutate(EL=difftime(EL_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         ER=difftime(ER_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SL=difftime(SL_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SR=difftime(SR_date, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         SL_fever=difftime(SL_fever, ymd_hms("2019-12-01 00:00:00"),
                           units="days") %>% 
           as.numeric(),
         SR_fever=difftime(SR_fever, ymd_hms("2019-12-01 00:00:00"),
                           units="days") %>% 
           as.numeric(),
         PL=difftime(PL, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         PR=difftime(PR, ymd_hms("2019-12-01 00:00:00"), units="days") %>% 
           as.numeric(),
         E_int=ER-EL,
         S_int=SR-SL,
         S_fever_int=SR_fever-SL_fever,
         max_inc_int=SR-EL,
         min_inc_int=SL-ER) %>% 
  # remove any entries missing EL, ER, SL, or SR
  filter(!is.na(EL_date), !is.na(ER_date), !is.na(SL_date), !is.na(SR_date)) %>% 
  # remove entries that haven't been reviewed by two people
  filter(!is.na(REVIEWER2)) %>% 
  # remove entries with exposure/onset intervals less than 0
  # remove entries where ER greater than SR or EL greater than SL
  filter(E_int > 0, S_int > 0, ER<=SR, SL>=EL, EL>0)





#-------------------------------------------#
# EDA
#-------------------------------------------#

## understanding the data
names(ncov)
data.frame(ncov$SL,ncov$SL_date)

## sex distribution
table(ncov$SEX, useNA = 'a')
prop.table(table(ncov$SEX, useNA = 'a'))

## age distribution
ncov$age.mid <- (ncov$AGEL + ncov$AGER)/2
summary(ncov$age.mid)

## mainland China vs non mainland
ncov <- ncov %>% 
  mutate(mlChina=ifelse(COUNTRY.DEST=="China" &
                          !PROVINCE.DEST%in%c("Hong Kong", "Macau"),1,0))
table(ncov$mlChina)

## Regions within mainland China
ncov %>%
  filter(mlChina==1) %>%
  group_by(PROVINCE.DEST) %>%
  summarise(ncases=n()) %>%
  arrange(desc(ncases))

## Countries + regions outside of mainland China
ncov %>% filter(mlChina==0) %>%
  group_by(COUNTRY.DEST) %>%
  summarise(ncases=n()) %>%
  arrange(desc(ncases))
ncov %>%
  filter(mlChina==0 & COUNTRY.DEST=="China") %>%
  group_by(PROVINCE.DEST) %>%
  summarise(ncases=n()) %>%
  arrange(desc(ncases))

## Wuhan exposure
ncov$WUHAN_EXP <- if_else(ncov$WUHAN_RESIDENT%in%c("Yes", "Hubei") | ncov$WUHAN_VISITED=="Yes", 1, 0) 
table(ncov$WUHAN_EXP, useNA='a')
table(ncov$WUHAN_RESIDENT)
table(ncov$WUHAN_VISITED)




#-------------------------------------------#
# Visualizations
#-------------------------------------------#

## Median exposure interval
summary(ncov$E_int)
data.frame(ncov$EL,ncov$ER,ncov$E_int)

dat_sum <- ncov %>%
  mutate(ELnew = EL-ER,
         ERnew = ER-ER,
         Emid = (ELnew + ERnew)/2,
         SLnew = SL-ER,
         SRnew = SR-ER,
         Smid = (SLnew + SRnew)/2,
         PLnew = PL-ER,
         PRnew = PR-ER,
         Pmid = (PLnew + PRnew)/2,
         UID=reorder(UID, SR-EL))
p1=ggplot(dat_sum, aes(y=factor(UID))) + 
  geom_segment(aes(x=ELnew, xend=ERnew, yend=factor(UID)), 
               color="#0072B2", size=3, alpha=.25) +
  geom_segment(aes(x=SLnew, xend=SRnew, yend=factor(UID)), 
               size=1.5, color="#CC0000", alpha=.65) +
  #geom_segment(aes(x=PLnew, xend=PRnew, yend=factor(UID)), 
  #             size=2, color="#00a841", alpha=.25) +
  #geom_point(aes(x=Emid, y=factor(UID)), size=0.5, color="#0072B2") +
  #geom_point(aes(x=Smid, y=factor(UID)), size=0.5, color="#CC0000") +
  #geom_point(aes(x=Pmid, y=factor(UID)), size=0.5, color="#00a841") +
  #geom_segment(aes(x=Emid, xend=Smid, yend=factor(UID)), size=0.33, color="#999999") +
  #ggtitle("Exposure and symptom onset windows") +
  scale_x_continuous("Days from last possible exposure") +
  scale_y_discrete("Case") +
  theme_transparent() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.x=element_text(color="black"))

ggplotly(p1)





