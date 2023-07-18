##Script to code the models, figures and tables for Hutchison et al, 2023 - JMIR

#Relevant libraries
library(tidyverse)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)
library(likert)
library(scales)
library(ggrepel)
library(forcats)
library(scales)
library(rstan)
library(rethinking)
library(cowplot)
library(bayesplot)
library(tidybayes)
library(modelr) 
library(dagitty)

#for speed with mcmc
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#functions
HDILow<- function(x, HDI=0.9) {
  sortedPts = sort( x)
  ciIdxInc = ceiling( HDI * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc 
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ] }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ] 
  return( HDImin)
} #for 90% CI's w shortest credible diff in tables.
HDIHigh<- function(x, HDI=0.9) {
  sortedPts = sort( x)
  ciIdxInc = ceiling( HDI * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc 
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ] }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ] 
  return( HDImax)
}
z_score <- function(x) { 
  
  x <- as.data.frame(x)
  
  out <- as.data.frame(apply(x,2,function(x) (x - mean(x,na.rm=TRUE))/sd(x,na.rm = TRUE)))
  
} #quick z_score func

#create function for calculating amount of time elapsed during exercise session
time_func <- 
  function(x){ sapply(1:nrow(x),function(i) x[i,3]-x[1,3])}
#function that finds first aerobic exercise session, and then the next session after it
pre_post_func <- function(x){
  x$time <- NA
  x$time[min(which(x$plan_type_id=="Aerobic Resistance Exercise"))] <- 'pre'
  
  if (nrow(x)>=3) {
    x$time[which(x$time=='pre')+1] <- 'post'}
  
  else if (nrow(x)==2 & which(x$time=='pre')==1){
    x$time[which(x$time=='pre')+1] <- 'post'
  }  
  else{x}
  return(x)}

#load relevant files 
demos <- read.csv('demos.csv',stringsAsFactors = FALSE)
plans <- read.csv('plans.csv',stringsAsFactors = FALSE)
assessments <- read.csv('assessments.csv',stringsAsFactors = FALSE)
sessions <- read.csv('sessions.csv',stringsAsFactors = FALSE)
hr <- read.csv('heart_rate.csv',stringsAsFactors = FALSE)
diff_df <- read.csv('diff_df.csv',stringsAsFactors = FALSE) #symp sev diffs
symp_diff_df <- read.csv('symp_diff_df.csv',stringsAsFactors = FALSE) #all symp diffs
pre_post_symp_tbl_df <-read.csv('pre_post_symp_tbl.csv',
                                stringsAsFactors = FALSE) # for symp pre post table


###Assorted data cleaning for tables/figures and modelling
#remove NA because it gets rid of the last row, which has an inappropriate date
#anyways.
hr <-na.omit(hr)
hr$timestamp<-  
  strptime(hr$timestamp ,'%Y-%m-%d %H:%M',tz = "GMT") 
hr$timestamp <- as.POSIXct(hr$timestamp)
hr2 <- hr[hr$plan_id%in%plans$plan_id,]

#merge hr ids to ages from demos
plans_for_merge <- plans[c(1,2)]
colnames(plans_for_merge) <- c("plan_id",'user_id')
users_for_merge <- demos[c(1,2)]
colnames(users_for_merge)[1] <- 'user_id'
merge_df <- merge(users_for_merge,plans_for_merge,by = 'user_id')
merge_df2 <- merge(hr2,merge_df,by = 'plan_id')
unique(merge_df2$plan_id)
hr_df <- merge_df2
hr_df$hr_per <- NA
hr_df$hr_per <- round(hr_df$bpm/(220-hr_df$Age)*100,digits = 2)
unique(hr_df$session_index)
hr_df$Session <- NA
hr_df$Session[hr_df$session_index==0] <- 'Session 1'
hr_df$Session[hr_df$session_index==1] <- 'Session 2'
hr_df$Session[hr_df$session_index==2] <- 'Session 3'
length(unique(hr_df$user_id))
length(unique(hr_df$plan_id))
table(hr_df$user_id,hr_df$plan_id)

#remove plans that users did twice
unique(hr_df$plan_id)
hr_df <-hr_df[!(hr_df$plan_id==42|hr_df$plan_id==64|
                  hr_df$plan_id==148),]
unique(hr_df$plan_id)

#combining plans and hr
#put hr in table format to merge a single mean value to plans
hr_table = as.table(by(hr_df$bpm,hr_df$plan_id,mean))
hr_table = as.data.frame(hr_table)

#Identifying the amount of time in each session
#sessions df
colnames(sessions)
#replace NULL with NA
sessions1 <- sessions
sessions1[sessions1=="NULL"] <- NA
#remove empty columns
sessions2 <- sessions1[-c(6,9)] 
#fix dates
sessions2$start_time <-  
  strptime(sessions2$start_time ,'%Y-%m-%d %H:%M:%S',tz = "GMT") 
sessions2$start_time <- as.POSIXct(sessions2$start_time)
sessions2$end_time <-  
  strptime(sessions2$end_time ,'%Y-%m-%d %H:%M:%S',tz = "GMT") 
sessions2$end_time <- as.POSIXct(sessions2$end_time)
sessions2$feedback_delayed_timestamp <-  
  strptime(sessions2$feedback_delayed_timestamp ,'%Y-%m-%d %H:%M:%S',tz = "GMT") 
sessions2$feedback_delayed_timestamp <- 
  as.POSIXct(sessions2$feedback_delayed_timestamp)

#feedback for only the 18 participants included in the study
plans_feedback <- plans[!(plans$plan_id==42|plans$plan_id==64|
                             plans$plan_id==148),]
sessions_feedback <- sessions2[sessions2$plan_id%in%plans_feedback$plan_id,]
#better/same/worse prep
colnames(sessions_feedback)
bsw <- sessions_feedback[c(3,6)]
bsw1 <- bsw[bsw$session_index==0,]
bsw2 <- bsw[bsw$session_index==1,]
bsw3 <- bsw[bsw$session_index==2,]
colnames(bsw1)[2] <- 'Session 1'
colnames(bsw2)[2] <- 'Session 2'
colnames(bsw3)[2] <- 'Session 3'

s1 <- data.frame(table(bsw1$`Session 1`))
s2 <- data.frame(table(bsw2$`Session 2`))
s3 <- data.frame(table(bsw3$`Session 3`))
bsw_plot <- merge(s1,s2,by = "Var1")
bsw_plot <- merge(bsw_plot,s3,by = "Var1")
bsw_plot[-c(1)] <- apply(bsw_plot[-c(1)],2,function(x)x/sum(x))
colnames(bsw_plot) <- c("Feedback",'Session 1','Session 2','Session 3')
bsw_plot$Feedback <- NULL
bsw_plot <- gather(bsw_plot)
bsw_plot$Feedback <- rep(c('Same','Better','Worse'),3)
bsw_plot$key <- factor(bsw_plot$key)
bsw_plot$Feedback <- factor(bsw_plot$Feedback)
bsw_plot$value <- bsw_plot$value*100
bsw_plot$value <- round(bsw_plot$value,digits = 0)

#stacked bar plot used in results (only data was used, but figure is nice to have)
feedback_plot <-ggplot(bsw_plot, aes(fill=Feedback, y=value, x=key,label = value)) + 
  theme_classic()+
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 6, position = position_stack(vjust = 0.4))+
  ggtitle("Participant Feedback") + ylab('Percent (%)')+
  theme(axis.title.x = element_blank(),axis.text= element_text(size = 12),
        axis.text.x = element_text(angle = 45,vjust = 0.9,hjust = 0.9,face = 'bold'),
        axis.title.y = element_text(face = 'bold'),title = element_text(face = 'bold'),
        plot.background=element_rect(fill = "papayawhip"),
        panel.background = element_rect(fill = 'dodgerblue4'),
        legend.background = element_rect(fill='papayawhip'))+
  scale_fill_manual(values = c("salmon1","yellow","turquoise") )
feedback_plot

ggsave('feedback_plot.pdf',feedback_plot,dpi = 600)

#calculating average exercise time for each session
test <- sessions2[sessions2$plan_id%in%plans$plan_id,]
test$end_time <- as.POSIXct(test$end_time)
test$start_time <- as.POSIXct(test$start_time)
test$exercise_time <- difftime(test$end_time,test$start_time,units = 'min')

by(test$exercise_time,test$session_index,mean)
test2<-test[test$exercise_time<60,]
by(test2$exercise_time,test2$session_index,mean)

#exercise time for heart rate
hr_df$link <- (paste0(hr_df$plan_id,hr_df$session_index))
hr_df <- hr_df[!(hr_df$link=='440'|hr_df$link=='442'),]
hr_df$link <- as.factor(hr_df$link)
hr_df_list <- split(hr_df,hr_df$link)
hr_df_list <- lapply(hr_df_list,function(x)x <- x[order(x$timestamp),])
times <- lapply(hr_df_list,function(x)difftime(x[nrow(x),3],x[1,3]))
times <- as.data.frame(do.call(rbind,times))
times$link <- rownames(times)
colnames(times)[1] <- 'Session Time'
table(hr_df$plan_id,hr_df$user_id)

#remove 440 and 442 because they don't meet a 5 min cut point
hr_df_list <- split(hr_df,hr_df$link)
hr_df_list <- lapply(hr_df_list,function(x)x <- x[order(x$timestamp),])
hr_df_list[[1]]         

#apply time function
time_list <- lapply(hr_df_list,time_func)

#putting together 2 lists to create a time metric for each session
test <- do.call(rbind.data.frame, hr_df_list)
test2 <- data.frame(unlist(time_list))
hr_df3 <- cbind.data.frame(test,test2)
hr_df4 <- hr_df3[hr_df3$unlist.time_list.<=25,]
by(hr_df4$unlist.time_list.,hr_df4$link,max)
levels(hr_df4$link)

#Create Table 1
t1_hr <- 
  tbl_summary(demos[c(2,3,9,4:8,11:14,15)],missing = 'no',
              statistic = all_continuous() ~ "{median} ({p25},{p75})",
              digits = all_continuous() ~ 1)%>%
  bold_labels()%>%
  modify_caption("**Table 1. Participant Demographics and Concussion Characteristics**") %>%
  as_gt() %>%
  tab_source_note('Percentages not always calculated on 21 participants due to missing data.')%>%
  gt::gtsave(filename = "t1_hr.html") 

#how many participants completed 3 session plan
plans_dnf <- plans[plans$is_finished==0,]
dnf <- demos[demos$user_id%in%plans_dnf$userId,]

###Plot for Figure 1
hr_plot <- ggplot(data = hr_df4,aes(x = as.factor(user_id),y = hr_per)) +
  annotate('rect',xmin = -Inf,xmax=Inf,ymin=55,ymax=65,alpha = 0.2,fill = 'purple')+
  geom_text(x = 4.5, y = 90,label = "Desired Range: 55 - 65%",fontface = 'bold')+
  geom_boxplot() + 
  ylab('% of HR max (age adjusted)') + 
  xlab("User") + theme_bw()+
  scale_x_discrete(labels = c('1','2','3','4','5','6','7','8','9','10','11',
                              '12','13','14','15','16','17','18'))+
  theme(plot.title = element_text(face = 'bold',hjust = 0.5),
        strip.text = element_text(face='bold',size = 11),
        strip.background = element_rect(fill = "white",colour = 'white'),
        axis.text.x = element_text(face='bold',size = 11),
        axis.text.y = element_text(face='bold',size = 11),
        axis.title.y=element_text(face='bold',size = 13),
        axis.title.x=element_text(face='bold',size = 13),
        legend.position = 'none')


hr_plot2 <- ggplot(data = na.omit(hr_df4),aes(x = as.factor(user_id),y = hr_per)) +
  annotate('rect',xmin = -Inf,xmax=Inf,ymin=55,ymax=65,alpha = 0.2,fill = 'purple')+
  geom_boxplot() + 
  xlab("User") + theme_bw()+
  scale_x_discrete(labels = c('1','2','3','4','5','6','7','8','9','10','11',
                              '12','13','14','15','16','17','18'))+
  theme(plot.title = element_text(face = 'bold',hjust = 0.5),
        strip.text = element_text(face='bold',size = 11),
        strip.background = element_rect(fill = "white",colour = 'white'),
        axis.text.x = element_text(face='bold',size = 11),
        axis.text.y = element_text(face='bold',size = 11),
        axis.title.y=element_blank(),
        axis.title.x=element_text(face='bold',size = 13),
        legend.position = 'none')+
  facet_wrap(Session~.,ncol = 1,strip.position = 'top')
hr_plot2

fig1 <- plot_grid(hr_plot,hr_plot2,labels = 'AUTO')
fig1

ggsave('fig1.png',fig1,dpi = 600, width = 14)

#########MODELLING########
#remove some superfluous NA's that were created
hr_df4 <- na.omit(hr_df4)
#wrangle data list
hr_z <- z_score(hr_df4$hr_per)
hr_df4$session_index[hr_df4$session_index==2] <- 3
hr_df4$session_index[hr_df4$session_index==1] <- 2
hr_df4$session_index[hr_df4$session_index==0] <- 1


data_hr <- list(hr = hr_z$x,
                session = factor(hr_df4$session_index),
                user = factor(hr_df4$user_id))
str(data_hr)

#stability across sessions?
m_sesh <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a1[session],
    a1[session] ~ dnorm(0,1), #doesnt change between 0.5 and 1 for SD prior
    sig ~ dexp(1)
  ), data = data_hr, iter = 2000,  chains = 4, log_lik = TRUE  )

precis(m_sesh,2,prob = 0.9)

#extract posterior
pm_msesh <- data.frame(extract.samples(m_sesh))
pm_msesh[c(1:3)] <- data.frame(apply(pm_msesh[c(1:3)],2,function(x)
  mean(hr_df4$hr_per) + 
    (x*sd(hr_df4$hr_per))))
pm_msesh$sig <- sd(hr_df4$hr_per)*pm_msesh$sig
pm_msesh$diff1 <- pm_msesh$a1.1-pm_msesh$a1.2
pm_msesh$diff2 <- pm_msesh$a1.1-pm_msesh$a1.3
pm_msesh$diff3 <- pm_msesh$a1.2-pm_msesh$a1.3
precis(pm_msesh,2,prob = 0.9)
mean(pm_msesh$diff1>0)
str(data_hr)

#posterior predictive contrasts
sesh1 <-rnorm(1000,pm_msesh$a1.1,pm_msesh$sig)
sesh2 <- rnorm(1000,pm_msesh$a1.2,pm_msesh$sig)
sesh3 <- rnorm(1000,pm_msesh$a1.3,pm_msesh$sig)
sesh1_2 <- sesh1 - sesh2
sesh1_3 <- sesh1 - sesh3
sesh2_3 <- sesh2 - sesh3

#plot session differences in HR at the group level
a1 <- data.frame(value = sesh1_2,group = 'Session 1 vs. Session 2')
a2 <- data.frame(value = sesh1_3,group = 'Session 1 vs. Session 3')
a3 <- data.frame(value = sesh2_3,group = 'Session 2 vs. Session 3')

sesh_df <-rbind.data.frame(a1,a2,a3)
sesh_plot <- sesh_df%>%
  ggplot(aes(x = value,y = fct_rev(group),fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_bw()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_text(face = 'bold'))+
  scale_fill_manual(values = c("pink",'grey')) +
  xlab('Group Difference in Mean HR% Between Sessions')
sesh_plot

ggsave('sup_fig1.jpg',sesh_plot,dpi = 600)

#hr icc model
#users as adaptive prior
m_icc <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a1[user],
    a1[user] ~ dnorm(mua,tau),
    mua ~ dnorm(0,1),
    sig ~ dexp(1),
    tau ~ dexp(1)
  ), data = data_hr, iter = 2000, chains = 4, log_lik = TRUE)

#extract posterior
pm_icc <- data.frame(extract.samples(m_icc))
#recover raw values
pm_icc[c(1:19)] <- data.frame(apply(pm_icc[c(1:19)],2,function(x)
  mean(hr_df4$hr_per) + 
    (x*sd(hr_df4$hr_per))))
#recover sd
pm_icc[c(20:21)] <- 
  data.frame(apply(pm_icc[c(20:21)],2,function(x)x*sd(hr_df4$hr_per)))
pm_icc$between_user_icc <- pm_icc$tau/(pm_icc$tau+pm_icc$sig)
pm_icc$within_user_icc <- pm_icc$sig/(pm_icc$tau+pm_icc$sig)
precis(pm_icc,2)

#checking w raw data
by(hr_df4$hr_per,factor(hr_df4$session_index),mean,na.rm=TRUE)
raw_user_means <- 
  as.vector(by(hr_df4$hr_per,factor(hr_df4$user_id),mean,na.rm=TRUE))
raw_user_sd <- 
  as.vector(by(hr_df4$hr_per,factor(hr_df4$user_id),sd,na.rm=TRUE))

summary(raw_user_means) 
sd(raw_user_means)
summary(raw_user_sd) 

#Summative data for results
#hr means by session
t3 <- 
  tbl_summary(hr_df4[c(7,8)],by = 'Session',missing = 'no',
              statistic = all_continuous() ~ "{median} ({p25},{p75})",
              digits = all_continuous() ~ 1,
              label = hr_per ~ "% of HR Max",)%>%
  bold_labels()%>%
  modify_caption("**Age Adjusted % of Heart Rate Max by Session**") %>%
  as_gt() %>%
  tab_source_note('HR, heartrate.')%>%
  tab_source_note('Max taken as 220-age.')%>%
  gt::gtsave(filename = "t3.html") 

#sessions completed
session_completed_vec <- 
  as.vector(by(hr_df4$session_index,factor(hr_df4$user_id),
               function(x) length(unique(x))))
table(session_completed_vec)

#average session length
session_length_vec <- as.vector(by(hr_df4$unlist.time_list.,hr_df4$link,max))
mean(session_length_vec)
summary(session_length_vec)

#number of subjects within range
user_hr <- as.vector(by(hr_df4$hr_per,factor(hr_df4$user_id),mean))
#less conservative (allow rounding)
length(user_hr[user_hr<=65.5 & user_hr>=54.5])
#stringent
length(user_hr[user_hr<=65 & user_hr>55])

sort(user_hr)
summary(user_hr)

#number of sessions within range
session_hr <- as.vector(by(hr_df4$hr_per,factor(hr_df4$link),mean))
length(session_hr[session_hr<=65.5 & session_hr>=54.5])
19/46
length(session_hr[session_hr>=65.5])
8/46
length(session_hr[session_hr<=54.5])
19/46

sort(session_hr)

#model hr and characteristics
hr <- z_score(diff_df$avg_hr_per)
exercise <- ifelse(diff_df$Follow.Exercise.Program=='Yes',2,1)
age <- z_score(diff_df$Age)
data_hr2 <- list(hr = hr$x,
                 exercise_program = exercise,
                 age = age$x
)
str(data_hr2)

#model age
m_age <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a + b*age,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = data_hr2, iter = 2000, chains = 4, log_lik = TRUE)

#extract posterior
precis(m_age,2,prob = 0.9)

#posterior sims - need counterfactual ages within study sample limits
age.seq <- seq(from = -1.5, to = 2.5, by = 0.1)  

#recover values: linpred and predictions
link.hr <- link(m_age,data=data.frame(age = age.seq))*sd(diff_df$avg_hr_per)+
  mean(diff_df$avg_hr_per)
sim.hr <- sim(m_age,data = list(age = age.seq))*sd(diff_df$avg_hr_per)+
  mean(diff_df$avg_hr_per)
mu.mean <- apply(link.hr,2,mean)
hdi_low <- apply(link.hr,2,HDILow,HDI = 0.9)
hdi_high <- apply(link.hr,2,HDIHigh,HDI = 0.9)
hdi_sim_low <- apply(sim.hr,2,HDILow,HDI= 0.9)
hdi_sim_high <- apply(sim.hr,2,HDIHigh,HDI = 0.9)

#Plotting HR v AGE
hr_age_df <- data.frame(age = age.seq*sd(diff_df$Age)+
                          mean(diff_df$Age),mu = mu.mean)
hr_age_plot <- ggplot(hr_age_df,aes(x = age, y = mu)) + theme_bw()+
  geom_ribbon(data = hr_age_df,aes(x = age , ymin = hdi_sim_low,
                                   ymax = hdi_sim_high), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = hr_age_df,aes(x = age , ymin = hdi_low,
                                   ymax = hdi_high), alpha = 0.3,
              show.legend = FALSE,fill = 'purple') +
  geom_line(data = hr_age_df, aes(x = age, y = mu),
            size = 1.5)+
  xlab('Age (years)') + ylab('Mean HR (% of age-predicted max)')+
  geom_point(data = diff_df,aes(x = Age,y = avg_hr_per))+
  theme(axis.title = element_text(face = "bold"),
        axis.title.y=element_text(face='bold',size = 13),
        axis.title.x=element_text(face='bold',size = 13),)
hr_age_plot

#hypothetical counterfactual comparison of two people
#get zscores for 20 and 56 year old
low_age <- (20 - mean(diff_df$Age))/sd(diff_df$Age)
high_age <- (56 - mean(diff_df$Age))/sd(diff_df$Age)

#with sim
age_20 <- 
  sim(m_age, data = data.frame(age = low_age))*sd(diff_df$avg_hr_per)+
  mean(diff_df$avg_hr_per)
age_56 <- 
  sim(m_age, data = data.frame(age = high_age))*sd(diff_df$avg_hr_per)+
  mean(diff_df$avg_hr_per)
age_contrast <- age_56 - age_20

dens(age_contrast)
mean(age_contrast>0)
mean(age_contrast)
PI(age_contrast,prob = 0.9)
HDIHigh(age_contrast,HDI = 0.9)
HDILow(age_contrast,HDI = 0.9)

#plot contrast
age_contrast <- data.frame(contrast=age_contrast)
hist1 <- age_contrast%>%
  ggplot(aes(x = contrast,fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_minimal()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold',size = 13),
        axis.text.y = element_blank())+
  scale_fill_manual(values = c("purple",'grey')) +
  xlab('Difference in HR% in older vs younger individual')
hist1 

#Exercise
str(data_hr2)
m_ex<- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a[exercise_program],
    a[exercise_program]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = data_hr2, iter = 2000, chains = 4, log_lik = TRUE)

#extract posterior
precis(m_ex,2,prob = 0.9)

pm_ex <- data.frame(extract.samples(m_ex))
pm_ex[c(1:2)] <- data.frame(apply(pm_ex[c(1:2)],2,function(x)
  mean(diff_df$avg_hr_per) + (x*sd(diff_df$avg_hr_per))))
pm_ex$sig <- sd(diff_df$avg_hr_per)*pm_ex$sig
pm_ex$contrast <- pm_ex$a.1-pm_ex$a.2
precis(pm_ex,2,prob = 0.9)

#post pred contrast
no_ex <- rnorm(1000,pm_ex$a.1,pm_ex$sig)
ex <- rnorm(1000,pm_ex$a.2,pm_ex$sig)
ex_contrast <- ex - no_ex

#Data list for depression and concussion history with n = 17
diff_df2 <- diff_df[!(is.na(diff_df$History.of.Depression)),]
hr <- z_score(diff_df2$avg_hr_per)
depression <- ifelse(diff_df2$History.of.Depression=='Yes',2,1)
conc_hx <- ifelse(diff_df2$History.of.Concussion=='Yes',2,1)
headaches <- ifelse(diff_df2$History.of.Headaches=='Yes',2,1)
data_hr3 <- list(hr = hr$x,
                 dep = depression,
                 chx = conc_hx,
                 hdache = headaches
)
str(data_hr3)

#Depression
m_dep<- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a[dep],
    a[dep]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = data_hr3, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_dep,2,prob = 0.9)

#extract posterior
pm_dep <- data.frame(extract.samples(m_dep))
pm_dep[c(1:2)] <- data.frame(apply(pm_dep[c(1:2)],2,function(x)
  mean(diff_df2$avg_hr_per) + (x*sd(diff_df2$avg_hr_per))))
pm_dep$sig <- sd(diff_df2$avg_hr_per)*pm_dep$sig
pm_dep$contrast <- pm_dep$a.1-pm_dep$a.2
precis(pm_dep,2,prob = 0.9)

#post pred contrast
no_dep <- rnorm(1000,pm_dep$a.1,pm_dep$sig)
dep <- rnorm(1000,pm_dep$a.2,pm_dep$sig)
dep_contrast <- dep - no_dep

#Conchx
m_chx<- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a[chx],
    a[chx]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = data_hr3, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_chx,2,prob = 0.9)

#extract posterior
pm_chx <- data.frame(extract.samples(m_chx))
pm_chx[c(1:2)] <- data.frame(apply(pm_chx[c(1:2)],2,function(x)
  mean(diff_df2$avg_hr_per) + (x*sd(diff_df2$avg_hr_per))))
pm_chx$sig <- sd(diff_df2$avg_hr_per)*pm_chx$sig
pm_chx$contrast <- pm_chx$a.1-pm_chx$a.2
precis(pm_chx,2,prob = 0.9)
mean(pm_chx$contrast>0)

#post pred contrast
no_chx <- rnorm(1000,pm_chx$a.1,pm_chx$sig)
chx <- rnorm(1000,pm_chx$a.2,pm_chx$sig)
chx_contrast <- chx - no_chx

#headaches
m_hdache<- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a[hdache],
    a[hdache]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = data_hr3, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_hdache,2,prob = 0.9)

#extract posterior
pm_hdache <- data.frame(extract.samples(m_hdache))
pm_hdache[c(1:2)] <- data.frame(apply(pm_hdache[c(1:2)],2,function(x)
  mean(diff_df2$avg_hr_per) + (x*sd(diff_df2$avg_hr_per))))
pm_hdache$sig <- sd(diff_df2$avg_hr_per)*pm_hdache$sig
pm_hdache$contrast <- pm_hdache$a.1-pm_hdache$a.2
precis(pm_hdache,2,prob = 0.9)
mean(pm_hdache$contrast>0)

#post pred contrast
no_hdache <- rnorm(1000,pm_hdache$a.1,pm_hdache$sig)
hdache <- rnorm(1000,pm_hdache$a.2,pm_hdache$sig)
hdache_contrast <- hdache - no_hdache

#combined supp figure for assorted comparisons
b1 <- data.frame(value = ex_contrast,group = 'Excercise Program vs. No Exercise Program')
b2 <- data.frame(value = chx_contrast,group = 'Concussion History vs. No Concussion History')
b3 <- data.frame(value = dep_contrast,group = 'Depression History vs. No Depression History')
b4 <- data.frame(value = hdache_contrast,group = 'Headache History vs. No Headache History')

contrast_df <- rbind.data.frame(b1,b2,b3,b4)

#plot
contrast_plot <- contrast_df%>%
  ggplot(aes(x = value,y = fct_rev(group),fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_bw()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_text(face = 'bold',size = 10))+
  scale_fill_manual(values = c("orange",'grey')) +
  xlab('Difference in HR% Between Groups')
contrast_plot

#extract posterior
ggsave('sup_fig2.jpg',contrast_plot,dpi = 600,width =10)

#MULTILEVEL HR MODELLING
#prep dataframe
colnames(diff_df)
prep_ml_df <- diff_df
prep_ml_df$Age <- (prep_ml_df$Age - mean(prep_ml_df$Age))/sd(prep_ml_df$Age)
prep_ml_df$Follow.Exercise.Program <- ifelse(prep_ml_df$Follow.Exercise.Program  =='Yes',2,1)
prep_ml_df$Time.of.Symptom.Onset <- ifelse(prep_ml_df$Time.of.Symptom.Onset=='Immediate',1,2)
ml_df <- merge(hr_df4,prep_ml_df,by = 'user_id',all = TRUE)

dat_ml <- list(hr = (ml_df$hr_per - mean(ml_df$hr_per))/sd(ml_df$hr_per),
               user = as.factor(ml_df$user_id),
               age = ml_df$Age.y,
               ex = as.integer(ml_df$Follow.Exercise.Program),
               onset = as.integer(ml_df$Time.of.Symptom.Onset))
str(dat_ml) 

#model 2 (multilevel)
ml_age <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <- a[user] + b*age,
    a[user] ~ dnorm(mu_u, sig_u),
    b ~ dnorm(0,1),
    mu_u ~ dnorm(0,1),
    sig_u ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)

#extract posterior
precis(ml_age,2,prob = 0.9)

pml_age <- data.frame(extract.samples(ml_age))
precis(pml_age,2) 

#posterior sims
age.seq <- seq(from = -1.5, to = 2.5, by = 0.1)  

#multi_level posterior of the link function
p_link_hyper <- function(x) {
  mu <- with(pml_age,mu_u + b*(x))
  return(mu)
}

#sim function
p_raw <- (sapply(age.seq, function(i)p_link_hyper(i)))*sd(hr_df4$hr_per)+
  mean(hr_df4$hr_per)

p_sim_hyper <- with(pml_age,rnorm(length(mu_u),mu_u,sig_u))
p_sim_hyper_2 <- sapply(age.seq, function(i)rnorm(nrow(pml_age),
                                                  p_sim_hyper+pml_age$b*(i),
                                                  pml_age$sig))*sd(hr_df4$hr_per)+
  mean(hr_df4$hr_per)

mu.mean <- apply(p_raw,2,mean)
hdi_high <- apply(p_raw,2,HDIHigh)
hdi_low <- apply(p_raw,2,HDILow)
hdi_sim_high <- apply(p_sim_hyper_2,2,HDIHigh)
hdi_sim_low <- apply(p_sim_hyper_2,2,HDILow)

#Plotting HR v AGE #2 (multilevel)
hr_age_df <- data.frame(age = age.seq*sd(diff_df$Age)+
                          mean(diff_df$Age),mu = mu.mean)
hr_age_plot <- ggplot(hr_age_df,aes(x = age, y = mu)) + theme_bw()+
  geom_ribbon(data = hr_age_df,aes(x = age , ymin = hdi_sim_low,
                                   ymax = hdi_sim_high), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = hr_age_df,aes(x = age , ymin = hdi_low,
                                   ymax = hdi_high), alpha = 0.3,
              show.legend = FALSE,fill = 'purple') +
  geom_line(data = hr_age_df, aes(x = age, y = mu),
            size = 1.5)+
  xlab('Age (years)') + ylab('HR (% of age-predicted max)')+
  geom_point(data = diff_df,aes(x = Age,y = avg_hr_per))+
  theme(axis.title = element_text(face = "bold"))
hr_age_plot

#hypothetical counterfactual comparison of two people #2
#get z scores for 20 and 56 year old
low_age <- (20 - mean(diff_df$Age))/sd(diff_df$Age)
high_age <- (56 - mean(diff_df$Age))/sd(diff_df$Age)

#with sim
age_20 <- sapply(low_age, function(i)rnorm(nrow(pml_age),
                                           p_sim_hyper+pml_age$b*(i),
                                           pml_age$sig))*sd(hr_df4$hr_per)+ mean(hr_df4$hr_per)
age_56 <- sapply(high_age, function(i)rnorm(nrow(pml_age),
                                            p_sim_hyper+pml_age$b*(i),
                                            pml_age$sig))*sd(hr_df4$hr_per)+ mean(hr_df4$hr_per)
age_contrast <- age_56 - age_20

dens(age_contrast)
mean(age_contrast>0)
mean(age_contrast)
PI(age_contrast,prob = 0.9)
HDIHigh(age_contrast,HDI = 0.9)
HDILow(age_contrast,HDI = 0.9)

#plot contrast
age_contrast <- data.frame(contrast=age_contrast)
hist1 <- age_contrast%>%
  ggplot(aes(x = contrast,fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_minimal()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_blank())+
  scale_fill_manual(values = c("purple",'grey')) +
  xlab('Difference in mean HR% in older vs younger individual')
hist1 

#explanation of beta coefficient
sd(hr_df4$hr_per)
sd(diff_df$Age)
9.8*0.22 #0.22*SD of HR, which has an SD of 9.8
#SD of Age is 10.4, therefore a 10.4 year increase in age = 2.2% average in hr%

#combine figure
fig2 <- plot_grid(hr_age_plot,hist1,labels = "AUTO")
fig2
ggsave('fig2.jpg',fig2,dpi = 600, width = 10)

#exercise with all hr datapoints
ml_ex <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b[ex], 
    a[user] ~ dnorm(a_bar,a_sig),
    b[ex] ~ dnorm(0,0.5),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_ex,2)

#extract posterior
pml_ex <- data.frame(extract.samples(ml_ex))

#sim function
sim_bar <- rnorm(nrow(pml_ex),pml_ex$a_bar,pml_ex$a_sig)

p_sim_ex1<- (rnorm(nrow(pml_ex),sim_bar+pml_ex$b.1,pml_ex$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
p_sim_ex2<- (rnorm(nrow(pml_ex),sim_bar+pml_ex$b.2,pml_ex$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
ml_ex_contrast <- p_sim_ex2 - p_sim_ex1

#symptom onset ml
#with all hr datapoints
ml_onset <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b[onset], 
    a[user] ~ dnorm(a_bar,a_sig),
    b[onset] ~ dnorm(0,0.5),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_onset,2)

#extract posterior
pml_onset <- data.frame(extract.samples(ml_onset))

#sim function
sim_bar <- rnorm(nrow(pml_onset),pml_onset$a_bar,pml_onset$a_sig)
p_sim_onset1<- (rnorm(nrow(pml_onset),sim_bar+pml_onset$b.1,pml_onset$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
p_sim_onset2<- (rnorm(nrow(pml_onset),sim_bar+pml_onset$b.2,pml_onset$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
ml_onset_contrast <- p_sim_onset2 - p_sim_onset1

#mlm contrasts w missing values (headaches, depression, conchx)
colnames(diff_df)
prep_ml_df2 <- diff_df
prep_ml_df2$History.of.Headaches <- ifelse(prep_ml_df2$History.of.Headaches=='Yes',2,1)
prep_ml_df2$History.of.Depression<- ifelse(prep_ml_df2$History.of.Depression=='Yes',2,1)
prep_ml_df2$History.of.Concussion <- ifelse(prep_ml_df2$History.of.Concussion=='Yes',2,1)

#merge df
ml_df2 <- merge(hr_df4,prep_ml_df2,by = 'user_id',all = TRUE)

#run complete cases (everything is MCAR, we can sacrifice some precision here)
ml_df2 <- ml_df2[!(is.na(ml_df2$History.of.Headaches)),]

dat_ml2 <- list(hr = (ml_df2$hr_per - mean(ml_df2$hr_per))/sd(ml_df2$hr_per),
                user = factor(ml_df2$user_id),
                depression = as.integer(ml_df2$History.of.Depression),
                headache = as.integer(ml_df2$History.of.Headaches),
                concussion = as.integer(ml_df2$History.of.Concussion))

#headache ml
#with all hr datapoints
ml_headache <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b[headache], 
    a[user] ~ dnorm(a_bar,a_sig),
    b[headache] ~ dnorm(0,0.5),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml2, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_headache,2)

#extract posterior
pml_headache <- data.frame(extract.samples(ml_headache))

#sim function
sim_bar <- rnorm(nrow(pml_headache),pml_headache$a_bar,pml_headache$a_sig)
p_sim_no_headache<- (rnorm(nrow(pml_headache),sim_bar+pml_headache$b.1,pml_headache$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
p_sim_headache<- (rnorm(nrow(pml_headache),sim_bar+pml_headache$b.2,pml_headache$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
ml_headache_contrast <- p_sim_headache- p_sim_no_headache
precis(ml_headache_contrast)

#depression ml
#with all hr datapoints
ml_depression <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b[depression], 
    a[user] ~ dnorm(a_bar,a_sig),
    b[depression] ~ dnorm(0,0.5),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml2, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_depression,2)

#extract posterior
pml_depression <- data.frame(extract.samples(ml_depression))

#sim function
sim_bar <- rnorm(nrow(pml_depression),pml_depression$a_bar,pml_depression$a_sig)
p_sim_no_depression<- (rnorm(nrow(pml_depression),sim_bar+pml_depression$b.1,pml_depression$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
p_sim_depression<- (rnorm(nrow(pml_depression),sim_bar+pml_depression$b.2,pml_depression$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
ml_depression_contrast <- p_sim_depression- p_sim_no_depression

#concussion history ml
#with all hr datapoints
ml_concussion <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b[concussion], 
    a[user] ~ dnorm(a_bar,a_sig),
    b[concussion] ~ dnorm(0,0.5),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml2, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_concussion,2)

#extract posterior
pml_concussion <- data.frame(extract.samples(ml_concussion))

#sim function
sim_bar <- rnorm(nrow(pml_concussion),pml_concussion$a_bar,pml_concussion$a_sig)
p_sim_no_concussion<- (rnorm(nrow(pml_concussion),sim_bar+pml_concussion$b.1,pml_concussion$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
p_sim_concussion<- (rnorm(nrow(pml_concussion),sim_bar+pml_concussion$b.2,pml_concussion$sig))*sd(ml_df$hr_per)+
  mean(ml_df$hr_per)
ml_concussion_contrast <- p_sim_concussion- p_sim_no_concussion

#ml contrast figure
#combined supp figure for assorted comparisons
b1 <- data.frame(value = ml_ex_contrast,group = 'Excercise Program vs. No Exercise Program')
b2 <- data.frame(value = ml_concussion_contrast,group = 'Concussion History vs. No Concussion History')
b3 <- data.frame(value = ml_depression_contrast,group = 'Depression History vs. No Depression History')
b4 <- data.frame(value = ml_headache_contrast,group = 'Headache History vs. No Headache History')
b5 <- data.frame(value = ml_onset_contrast,group = 'Immediate Onset vs. Delayed Onset')

contrast_ml_df <- rbind.data.frame(b1,b2,b3,b4,b5)

#plot
contrast_plot_ml <- contrast_ml_df%>%
  ggplot(aes(x = value,y = fct_rev(group),fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_bw()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_text(face = 'bold',size = 10))+
  scale_fill_manual(values = c("orange",'grey')) +
  xlab('Group Difference in HR% Between Sessions')
contrast_plot_ml

ggsave('sup_fig2_ml.jpg',contrast_plot_ml,dpi = 600,width =10)

#MLM symptom HR modelling
colnames(diff_df)
prep_ml_df3 <- diff_df
prep_ml_df3$symp_diff <- (prep_ml_df3$symp_diff - mean(prep_ml_df3$symp_diff,na.rm = TRUE))/
  sd(prep_ml_df3$symp_diff,na.rm = TRUE)
prep_ml_df3$Age <- (prep_ml_df3$Age - mean(prep_ml_df3$Age,na.rm = TRUE))/
  sd(prep_ml_df3$Age,na.rm = TRUE)
ml_df3 <- merge(hr_df4,prep_ml_df3,by = 'user_id',all = TRUE)
ml_df3 <- ml_df3[!(is.na(ml_df3$symp_diff)),]

dat_ml3 <- list(hr = (ml_df3$hr_per - mean(ml_df3$hr_per))/sd(ml_df3$hr_per),
                user = factor(ml_df3$user_id),
                symp_diff = ml_df3$symp_diff,
                age = ml_df3$Age.y)

str(dat_ml3)

#model
ml_hr_symp <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b*symp_diff, 
    a[user] ~ dnorm(a_bar,a_sig),
    b ~ dnorm(0,1),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml3, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_hr_symp,2)

#extract posterior
pml_hr_symp <- data.frame(extract.samples(ml_hr_symp))
precis(pml_hr_symp,2, prob = 0.9) 

#simulate for posterior contrasts
min(dat_ml3$symp_diff)
max(dat_ml3$symp_diff)
#posterior sims
symp.seq <- seq(from = -2, to = 2, by = 0.1)  

#multi_level posterior of the link function
p_link_hyper <- function(x) {
  mu <- with(pml_hr_symp,a_bar + b*(x))
  return(mu)
}

#sim function
p_raw <- (sapply(symp.seq, function(i)p_link_hyper(i)))*sd(ml_df3$hr_per)+
  mean(ml_df3$hr_per)

p_sim_hyper <- with(pml_hr_symp,rnorm(length(a_bar),a_bar,a_sig))
p_sim_hyper_2 <- 
  sapply(symp.seq, function(i)rnorm(nrow(pml_hr_symp), 
                                    p_sim_hyper+pml_hr_symp$b*(i),
                                    pml_hr_symp$sig))*sd(ml_df3$hr_per) + mean(ml_df3$hr_per)

mu.mean <- apply(p_raw,2,mean)
hdi_high <- apply(p_raw,2,HDIHigh)
hdi_low <- apply(p_raw,2,HDILow)
hdi_sim_high <- apply(p_sim_hyper_2,2,HDIHigh)
hdi_sim_low <- apply(p_sim_hyper_2,2,HDILow)

#Plotting HR v AGE #2 (multilevel)
hr_symp_df <- data.frame(symp = symp.seq*sd(diff_df$symp_diff,na.rm = TRUE)+
                           mean(diff_df$symp_diff,na.rm = TRUE),mu = mu.mean)
hr_symp_plot <- ggplot(hr_symp_df,aes(x = symp, y = mu)) + theme_bw()+
  geom_ribbon(data = hr_symp_df,aes(x = symp, ymin = hdi_sim_low,
                                    ymax = hdi_sim_high), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = hr_symp_df,aes(x = symp , ymin = hdi_low,
                                    ymax = hdi_high), alpha = 0.3,
              show.legend = FALSE,fill = 'red') +
  geom_line(data = hr_symp_df, aes(x = symp, y = mu),
            size = 1.5)+
  xlab('Symptom Difference (Pre vs. Post)') + ylab('HR (% of Age-Predicted Max)')+
  geom_point(data = diff_df,aes(x = symp_diff,y = avg_hr_per))+
  theme(axis.title = element_text(face = "bold"))
hr_symp_plot

ggsave('sup_fig5.jpg', hr_symp_plot,dpi = 600)

#age adjusted
ml_hr_symp_age <- ulam(
  alist(
    hr ~ dnorm(mu,sig),
    mu <-a[user]+ b1*symp_diff + b2*age, 
    a[user] ~ dnorm(a_bar,a_sig),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    a_bar ~ dnorm(0,1),
    a_sig ~dexp(1),
    sig ~ dexp(1)
  ), data = dat_ml3, iter = 3000, chains = 4,cores = 10, log_lik = TRUE)
precis(ml_hr_symp_age,2)

#extract posterior
pml_hr_symp_age <- data.frame(extract.samples(ml_hr_symp_age))
precis(pml_hr_symp_age,2,prob = 0.9)

#simulate for posterior contrasts
min(dat_ml3$symp_diff)
max(dat_ml3$symp_diff)

#posterior sims
symp.seq <- seq(from = -2, to = 2, by = 0.1)  

#multi_level posterior of the link function
p_link_hyper <- function(x) {
  mu <- with(pml_hr_symp_age,a_bar+ b1*(x))
  return(mu)
}
#sim function
p_raw <- (sapply(symp.seq, function(i)p_link_hyper(i)))*sd(ml_df3$hr_per,na.rm = TRUE)+
  mean(ml_df3$hr_per,na.rm = TRUE)

p_sim_hyper <- with(pml_hr_symp_age,rnorm(length(a_bar),a_bar,a_sig))
p_sim_hyper_2 <- sapply(symp.seq, function(i)rnorm(nrow(pml_hr_symp_age),
                                                   p_sim_hyper + pml_hr_symp_age$b1*(i),
                                                   pml_hr_symp_age$sig))*sd(ml_df3$hr_per,na.rm = TRUE)+
  mean(ml_df3$hr_per,na.rm = TRUE)

mu.mean <- apply(p_raw,2,mean)
hdi_high <- apply(p_raw,2,HDIHigh)
hdi_low <- apply(p_raw,2,HDILow)
hdi_sim_high <- apply(p_sim_hyper_2,2,HDIHigh)
hdi_sim_low <- apply(p_sim_hyper_2,2,HDILow)

#Plotting HR v AGE #2 (multilevel)
hr_symp_df_adj <- data.frame(symp = symp.seq*sd(diff_df$symp_diff,na.rm = TRUE)+
                               mean(diff_df$symp_diff,na.rm = TRUE),mu = mu.mean)
hr_symp_adj_plot <- ggplot(hr_symp_df_adj,aes(x = symp, y = mu)) + theme_bw()+
  geom_ribbon(data = hr_symp_df_adj,aes(x = symp, ymin = hdi_sim_low,
                                        ymax = hdi_sim_high), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = hr_symp_df_adj,aes(x = symp , ymin = hdi_low,
                                        ymax = hdi_high), alpha = 0.3,
              show.legend = FALSE,fill = 'red') +
  geom_line(data = hr_symp_df_adj, aes(x = symp, y = mu),
            size = 1.5)+
  xlab('Symptom Severity (Difference Pre - Post)') + ylab('HR (% of Age-Predicted Max)')+
  geom_point(data = diff_df,aes(y = avg_hr_per,x = symp_diff))+
  theme(axis.title = element_text(face = "bold"))
hr_symp_adj_plot

ggsave('fig3.jpg',hr_symp_adj_plot,dpi = 600)

####Symptom Modelling
diff_df5 <- diff_df[!(is.na(diff_df$symp_diff)),]

#create data list
dat_hr4 <- 
  list(hr = (diff_df5$avg_hr_per - mean(diff_df5$avg_hr_per))/
         sd(diff_df5$avg_hr_per),
       symp_diff = (diff_df5$symp_diff - mean(diff_df5$symp_diff))/
         sd(diff_df5$symp_diff), 
       age = (diff_df5$Age - mean(diff_df5$Age))/
         sd(diff_df5$Age), 
       exercise = ifelse(diff_df5$Follow.Exercise.Program=="Yes",2,1),
       onset = ifelse(diff_df5$Time.of.Symptom.Onset=="Immediate",1,2)) 

str(dat_hr4)

#symp vs hr model (not for paper but for reference)
m_symp_hr <- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a + b*hr,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr4, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_symp_hr,2,prob = 0.9)

pm_symp_hr <- data.frame(extract.samples(m_symp_hr))
precis(pm_symp_hr,2)

#create df for plots
hr.seq <- seq(from = -2, to = 1.5, by = 0.1)

#recover values: linpred and predictions
link.symp <- link(m_symp_hr,data=data.frame(hr =hr.seq))*sd(diff_df5$symp_diff)+
  mean(diff_df5$symp_diff)
sim.symp <- sim(m_symp_hr,data = list(hr= hr.seq))*sd(diff_df5$symp_diff)+
  mean(diff_df5$symp_diff)
mu.mean <- apply(link.symp,2,mean)
hdi_low <- apply(link.symp,2,HDILow,HDI = 0.9)
hdi_high <- apply(link.symp,2,HDIHigh,HDI = 0.9)
hdi_sim_low <- apply(sim.symp,2,HDILow,HDI= 0.9)
hdi_sim_high <- apply(sim.symp,2,HDIHigh,HDI = 0.9)

#Plotting HR v symp
hr_symp_df <- data.frame(hr=hr.seq*sd(diff_df5$avg_hr_per)+
                           mean(diff_df5$avg_hr_per),mu = mu.mean)
#Plot
hr_symp_plot <- ggplot(hr_symp_df,aes(x = hr, y = mu)) + theme_bw()+
  geom_ribbon(data = hr_symp_df,aes(x = hr , ymin = hdi_sim_low,
                                    ymax = hdi_sim_high), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = hr_symp_df,aes(x = hr , ymin = hdi_low,
                                    ymax = hdi_high), alpha = 0.3,
              show.legend = FALSE,fill = 'green') +
  geom_line(data = hr_symp_df, aes(x = hr, y = mu),
            size = 1.5)+
  geom_point(data=diff_df5,aes(y = symp_diff,x = avg_hr_per))+
  ylab('Difference in Symptom Score Pre vs. Post Plan') +xlab('HR (% of Age-Predicted Max)')
hr_symp_plot

#symp vs hr model -age adjusted
m_symp_hr_adj <- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a + b1*hr + b2*age,
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr4, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_symp_hr_adj,2,prob = 0.9)

hr.seq2 <- seq(from = -2, to = 1.5, by = 0.1)

#recover values: linpred and predictions
link.symp2 <- link(m_symp_hr_adj,data=data.frame(hr =hr.seq2,age = 0))*sd(diff_df5$symp_diff)+
  mean(diff_df5$symp_diff)
sim.symp2 <- sim(m_symp_hr,data = list(hr= hr.seq2,age = 0))*sd(diff_df5$symp_diff)+
  mean(diff_df5$symp_diff)
mu.mean2 <- apply(link.symp2,2,mean)
hdi_low2 <- apply(link.symp2,2,HDILow,HDI = 0.9)
hdi_high2 <- apply(link.symp2,2,HDIHigh,HDI = 0.9)
hdi_sim_low2 <- apply(sim.symp2,2,HDILow,HDI= 0.9)
hdi_sim_high2<- apply(sim.symp2,2,HDIHigh,HDI = 0.9)

#Plotting HR v symp
hr_symp_df_adj <- data.frame(hr=hr.seq2*sd(diff_df5$avg_hr_per)+
                               mean(diff_df5$avg_hr_per),mu = mu.mean2)
#Plot
hr_symp_adj_plot <- ggplot(hr_symp_df_adj,aes(x = hr, y = mu)) + theme_bw()+
  geom_ribbon(data = hr_symp_df_adj,aes(x = hr , ymin = hdi_sim_low2,
                                        ymax = hdi_sim_high2), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = hr_symp_df_adj,aes(x = hr , ymin = hdi_low2,
                                        ymax = hdi_high2), alpha = 0.3,
              show.legend = FALSE,fill = 'orange') +
  geom_line(data = hr_symp_df_adj, aes(x = hr, y = mu),
            size = 1.5)+
  geom_point(data = diff_df,aes(x = avg_hr_per,y = symp_diff))+
  ylab('Difference in Symptom Score Pre vs. Post Plan') +xlab('HR (% of Age-Predicted Max)')
hr_symp_adj_plot

#counterfactual for symptom and hr, age adjusted
sort(dat_hr4$hr)
sort(diff_df5$avg_hr_per)
low_hr <- (47 - mean(diff_df5$avg_hr_per))/sd(diff_df5$avg_hr_per)
high_hr<- (67 - mean(diff_df5$avg_hr_per))/sd(diff_df5$avg_hr_per)

hr_47 <- 
  sim(m_symp_hr_adj, data = data.frame(age = 0,hr = low_hr))*
  sd(diff_df5$symp_diff)+mean(diff_df5$symp_diff)
hr_67 <- 
  sim(m_symp_hr_adj, data = data.frame(age = 0, hr = high_hr))*
  sd(diff_df5$symp_diff)+mean(diff_df5$symp_diff)
hr_contrast <- hr_47 - hr_67
dens(hr_contrast)
mean(hr_contrast<0)
mean(hr_contrast)
PI(hr_contrast,prob = 0.9)
HDIHigh(hr_contrast,HDI = 0.9)
HDILow(hr_contrast,HDI = 0.9)

#plot contrast
hr_contrast <- data.frame(contrast=hr_contrast)
hist3 <- hr_contrast %>%
  ggplot(aes(x = contrast,fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_minimal()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_blank())+
  scale_fill_manual(values = c("grey",'orange')) +
  xlab('Symptom Reduction in High vs. Low Avg HR%')
hist3

#symptom_diff and age
m_symp_age <- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a + b*age,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr4, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_symp_age,2,prob = 0.9)

#extract posterior
pm_symp_age <- data.frame(extract.samples(m_symp_age))
precis(pm_symp_age,2)

#explanation of beta coefficient
sd(diff_df5$symp_diff)*-0.29*2.5
sd(diff_df5$Age) 

#beta explanation
#1SD change in age has a 0.29SD change in symp diff
#10.5 years = 1.6

#create df for plots
age.seq <- seq(from = -1.5, to = 2, by = 0.1)

#recover values: linpred and predictions
link.age <- link(m_symp_age,data=data.frame(age =age.seq))*sd(diff_df5$symp_diff)+
  mean(diff_df5$symp_diff)
sim.age <- sim(m_symp_age,data = list(age= age.seq))*sd(diff_df5$symp_diff)+
  mean(diff_df5$symp_diff)
mu.mean <- apply(link.age,2,mean)
hdi_low <- apply(link.age,2,HDILow,HDI = 0.9)
hdi_high <- apply(link.age,2,HDIHigh,HDI = 0.9)
hdi_sim_low <- apply(sim.age,2,HDILow,HDI= 0.9)
hdi_sim_high <- apply(sim.age,2,HDIHigh,HDI = 0.9)

#Plotting HR v symp
age_symp_df <- data.frame(age=age.seq*sd(diff_df5$Age)+
                            mean(diff_df5$Age),mu = mu.mean)

#Plot
age_symp_plot <- ggplot(age_symp_df,aes(x = age, y = mu)) + theme_bw()+
  geom_ribbon(data = age_symp_df,aes(x = age , ymin = hdi_sim_low,
                                     ymax = hdi_sim_high), alpha = 0.6,
              show.legend = FALSE,fill = 'grey') +
  geom_ribbon(data = age_symp_df,aes(x = age , ymin = hdi_low,
                                     ymax = hdi_high), alpha = 0.3,
              show.legend = FALSE,fill = 'yellow') +
  geom_line(data = age_symp_df, aes(x = age, y = mu),
            size = 1.5)+
  theme(axis.title = element_text(face = 'bold'))+
  ylab('Symptom Reduction') +xlab('Age (Years)')+
  geom_point(data = diff_df5,aes(x = Age,y = symp_diff))
age_symp_plot 

#counterfactual for symptom in age
low_age_b <- (20 - mean(diff_df5$Age))/sd(diff_df5$Age)
high_age_b <- (56 - mean(diff_df5$Age))/sd(diff_df5$Age)

age_20_b <- 
  sim(m_symp_age, data = data.frame(age = low_age_b))*
  sd(diff_df5$symp_diff)+mean(diff_df5$symp_diff)
age_56_b <- 
  sim(m_symp_age, data = data.frame(age = high_age_b))*
  sd(diff_df5$symp_diff)+mean(diff_df5$symp_diff)
age_contrast_b <- age_56_b - age_20_b
dens(age_contrast_b)
mean(age_contrast_b<0)
mean(age_contrast_b)
PI(age_contrast_b,prob = 0.9)
HDIHigh(age_contrast_b,HDI = 0.9)
HDILow(age_contrast_b,HDI = 0.9)

#plot contrast
age_contrast2 <- data.frame(contrast=age_contrast_b)
hist2 <- age_contrast2%>%
  ggplot(aes(x = contrast,fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_minimal()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_blank())+
  scale_fill_manual(values = c("grey",'yellow')) +
  xlab('Symptom Reduction in Older vs. Younger Individuals')
hist2

#combine figure
fig3 <- plot_grid(age_symp_plot,hist2,labels = "AUTO")
fig3

ggsave('fig3.jpg',fig3,dpi = 600, width = 8)

#time of symptom onset
m_onset <- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a[onset],
    a[onset]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr4, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_onset,2,prob = 0.9)

#extract posterior
pm_onset <- data.frame(extract.samples(m_onset))
pm_onset[c(1:2)] <- data.frame(apply(pm_onset[c(1:2)],2,function(x)
  mean(diff_df5$symp_diff) + (x*sd(diff_df5$symp_diff))))
pm_onset$sig <- sd(diff_df5$symp_diff)*pm_onset$sig
pm_onset$contrast <- pm_onset$a.1-pm_onset$a.2
precis(pm_onset,2,prob = 0.9)
mean(pm_onset$contrast>0)

#post pred contrast
immediate <- rnorm(1000,pm_onset$a.1,pm_onset$sig)
delayed <- rnorm(1000,pm_onset$a.2,pm_onset$sig)
onset_contrast <- immediate - delayed
mean(immediate)
c(HDIHigh(immediate),HDILow(immediate))
mean(delayed)
c(HDIHigh(delayed),HDILow(delayed))
c(HDIHigh(onset_contrast),HDILow(onset_contrast))

#exercise program
m_ex2 <- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a[exercise],
    a[exercise]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr4, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_ex2,2,prob = 0.9)

#extract posterior
pm_ex2 <- data.frame(extract.samples(m_ex2))
pm_ex2[c(1:2)] <- data.frame(apply(pm_ex2[c(1:2)],2,function(x)
  mean(diff_df5$symp_diff) + (x*sd(diff_df5$symp_diff))))
pm_ex2$sig <- sd(diff_df5$symp_diff)*pm_ex2$sig
pm_ex2$contrast <- pm_ex2$a.1-pm_ex2$a.2
precis(pm_ex2,2,prob = 0.9)
mean(pm_ex2$contrast>0)

#post pred contrast
no_ex2 <- rnorm(1000,pm_ex2$a.1,pm_ex2$sig)
ex2 <- rnorm(1000,pm_ex2$a.2,pm_ex2$sig)
ex2_contrast <- no_ex2 - ex2

#reduced dataset for missing values on headaches, depression etc.
diff_df6 <- diff_df5[!(is.na(diff_df5$History.of.Headaches)),]

#create data list
dat_hr5 <- 
  list(symp_diff = (diff_df6$symp_diff - mean(diff_df6$symp_diff))/
         sd(diff_df6$symp_diff), 
       chx = ifelse(diff_df6$History.of.Concussion=='Yes',2,1),
       depression = ifelse(diff_df6$History.of.Depression=='Yes',2,1),
       headaches = ifelse(diff_df6$History.of.Headaches=='Yes',2,1)
  ) 

str(dat_hr5)

#Depression
m_dep2<- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a[depression],
    a[depression]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr5, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_dep2,2,prob = 0.9)

#extract posterior
pm_dep2 <- data.frame(extract.samples(m_dep2))
pm_dep2[c(1:2)] <- data.frame(apply(pm_dep2[c(1:2)],2,function(x)
  mean(diff_df6$symp_diff) + (x*sd(diff_df6$symp_diff))))
pm_dep2$sig <- sd(diff_df6$symp_diff)*pm_dep2$sig
pm_dep2$contrast <- pm_dep2$a.1-pm_dep2$a.2
precis(pm_dep2,2,prob = 0.9)

#post pred contrast
no_dep2 <- rnorm(1000,pm_dep2$a.1,pm_dep2$sig)
dep2 <- rnorm(1000,pm_dep2$a.2,pm_dep2$sig)
dep2_contrast <- dep2 - no_dep2

#Conchx
m_chx2 <- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a[chx],
    a[chx]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr5, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_chx2,2,prob = 0.9)

#extract posterior
pm_chx2 <- data.frame(extract.samples(m_chx2))
pm_chx2[c(1:2)] <- data.frame(apply(pm_chx2[c(1:2)],2,function(x)
  mean(diff_df6$symp_diff) + (x*sd(diff_df6$symp_diff))))
pm_chx2$sig <- sd(diff_df6$symp_diff)*pm_chx2$sig
pm_chx2$contrast <- pm_chx2$a.1-pm_chx2$a.2
precis(pm_chx2,2,prob = 0.9)
mean(pm_chx2$contrast>0)

#post pred contrast
no_chx2 <- rnorm(1000,pm_chx2$a.1,pm_chx2$sig)
chx2 <- rnorm(1000,pm_chx2$a.2,pm_chx2$sig)
chx_contrast2 <- chx2 - no_chx2

#headaches
m_hdache2<- ulam(
  alist(
    symp_diff ~ dnorm(mu,sig),
    mu <- a[headaches],
    a[headaches]~ dnorm(0,1),
    sig ~ dexp(1)
  ), data = dat_hr5, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_hdache2,2,prob = 0.9)

#extract posterior
pm_hdache2 <- data.frame(extract.samples(m_hdache2))
pm_hdache2[c(1:2)] <- data.frame(apply(pm_hdache2[c(1:2)],2,function(x)
  mean(diff_df6$symp_diff) + (x*sd(diff_df6$symp_diff))))
pm_hdache2$sig <- sd(diff_df6$symp_diff)*pm_hdache2$sig
pm_hdache2$contrast <- pm_hdache2$a.1-pm_hdache2$a.2
precis(pm_hdache2,2,prob = 0.9)
mean(pm_hdache2$contrast>0)

#post pred contrast
no_hdache2 <- rnorm(1000,pm_hdache2$a.1,pm_hdache2$sig)
hdache2 <- rnorm(1000,pm_hdache2$a.2,pm_hdache2$sig)
hdache_contrast2 <- hdache2 - no_hdache2
mean(no_hdache2)
HDIHigh(no_hdache2)
HDILow(no_hdache2)
mean(hdache2)
HDIHigh(hdache2)
HDILow(hdache2)
mean(hdache2)
mean(hdache_contrast2)
HDIHigh(hdache_contrast2)
HDILow(hdache_contrast2)

#combined supp figure for assorted comparisons
b1.1 <- data.frame(value = ex2_contrast,group = 'Excercise Program vs. No Exercise Program')
b2.1 <- data.frame(value = chx_contrast2,group = 'Concussion History vs. No Concussion History')
b3.1 <- data.frame(value = dep2_contrast,group = 'Depression History vs. No Depression History')
b4.1 <- data.frame(value = hdache_contrast2,group = 'Headache History vs. No Headache History')
#b5.1 <- data.frame(value = onset_contrast,group = 'Immediate Symptom Onset vs. Delayed Symptom Onset')
contrast_df2 <- rbind.data.frame(b1.1,b2.1,b3.1,b4.1)

pm_1 <- as.list(sort(by(contrast_df2$value,factor(contrast_df2$group),
                        function(x) mean(x>0)),decreasing = TRUE))
pm_1 <- lapply(pm_1,function(x) round(x*100,digits = 2))

#reorder by descending pm
contrast_df2$group <- factor(contrast_df2$group,
                             level = c(
                               'Excercise Program vs. No Exercise Program',
                               'Depression History vs. No Depression History',
                               'Concussion History vs. No Concussion History',
                               'Headache History vs. No Headache History'))


#plot
contrast_plot2 <- contrast_df2%>%
  ggplot(aes(x = value,y = fct_rev(group),fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_bw()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_text(face = 'bold',size = 10))+
  scale_fill_manual(values = c("green",'grey')) +
  xlab('Difference in Symptom Reduction Following CARE Protocol')+
  annotate("text", x = 3.5, y = 0.4+(4:1), size = 2.5,label = (unlist(pm_1)))
contrast_plot2

ggsave('sup_fig4.jpg',contrast_plot2,dpi = 600,width = 10)


###Pre vs. post symptom differences.
#z score first
symp_diff_df2 <- symp_diff_df
symp_diff_df2[-c(1,2)] <- data.frame(apply(symp_diff_df[-c(1,2)],2,z_score))

symp_dat <- list(total = symp_diff_df2$pointsTota ,
                 somatic = symp_diff_df2$pointsSomatic,
                 cognitive = symp_diff_df2$pointsCognitive,
                 fatigue = symp_diff_df2$pointsFatigue,
                 sleep = symp_diff_df2$pointsSleep,
                 neck = symp_diff_df2$pointsNeck,
                 balance = symp_diff_df2$pointsBalance,
                 vision = symp_diff_df2$pointsVision)

str(symp_dat)

#models
m_multi_symp <- ulam(
  alist(
    total ~ dnorm(mu_t,sig_t),
    mu_t <- t,
    t ~ dnorm(-1.9,1),
    sig_t~dexp(1),
    
    somatic ~ dnorm(mu_s,sig_s),
    mu_s <- s,
    s ~ dnorm(-0.85,1),
    sig_s~dexp(1),
    
    cognitive ~ dnorm(mu_c,sig_c),
    mu_c <- cog,
    cog ~ dnorm(-1.2,1),
    sig_c~dexp(1),
    
    fatigue ~ dnorm(mu_f,sig_f),
    mu_f <- f,
    f ~ dnorm(-0.25,1),
    sig_f~dexp(1),
    
    sleep ~ dnorm(mu_sl,sig_sl),
    mu_sl <- sl,
    sl ~ dnorm(-0.25,1),
    sig_sl~dexp(1),
    
    neck ~ dnorm(mu_n, sig_n),
    mu_n <- n,
    n ~ dnorm(-0.5,1),
    sig_n ~ dexp(1),
    
    balance ~ dnorm(mu_b,sig_b),
    mu_b <- b,
    b ~ dnorm(-0.14,1),
    sig_b~dexp(1),
    
    vision ~ dnorm(mu_v,sig_v),
    mu_v <- v,
    v ~ dnorm(-0.7,1),
    sig_v~dexp(1)
  ), data = symp_dat, iter = 2000, chains = 4, log_lik = TRUE)

precis(m_multi_symp,2)

#extract posterior
pm_ms <- data.frame(extract.samples(m_multi_symp))

#recover values
colnames(pm_ms)
pm_ms <- pm_ms[c(1,3,5,7,9,11,13,15,2,4,6,8,10,12,14,16)]
pm_ms[-c(9:16)] <- sapply(1:8,function(i)pm_ms[-c(9:16)][,i]*sd(symp_diff_df[,i+2])+
                            mean(symp_diff_df[,i+2]))
pm_ms[-c(1:8)] <- sapply(1:8,function(i)pm_ms[-c(1:8)][,i]*sd(symp_diff_df[,i+2]))
precis(pm_ms,2)

mean(pm_ms>0)

#post pred contrasts
total_diff <- rnorm(1000,pm_ms$t,pm_ms$sig_t)
som_diff <- rnorm(1000,pm_ms$s,pm_ms$sig_s)
cog_dff <- rnorm(1000,pm_ms$cog,pm_ms$sig_c)
fat_diff <- rnorm(1000,pm_ms$f,pm_ms$sig_f)
sl_diff <- rnorm(1000,pm_ms$sl,pm_ms$sig_sl)
neck_diff <- rnorm(1000,pm_ms$n,pm_ms$sig_n)
bal_diff <- rnorm(1000,pm_ms$b,pm_ms$sig_b)
vis_diff <- rnorm(1000,pm_ms$v,pm_ms$sig_v)

#combined supp figure for assorted comparisons
s1 <- data.frame(value = total_diff,group = 'Total Symptom Severity')
mean(total_diff>0)
HDIHigh(total_diff)
HDILow(total_diff)
s2 <- data.frame(value = som_diff,group = 'Somatic')
s3 <- data.frame(value = cog_dff,group = 'Cognitive')
s4 <- data.frame(value = fat_diff,group = 'Fatigue')
s5 <- data.frame(value = sl_diff,group = 'Sleep')
s6 <- data.frame(value = neck_diff,group = 'Neck')
s7 <- data.frame(value = bal_diff,group = 'Balance')
s8 <- data.frame(value = vis_diff,group = 'Vision')

symp_contrast_df <- rbind.data.frame(s1,s2,s3,s4,s5,s6,s7,s8)

#organize by pm > 0
pm_0 <- as.list(sort(by(symp_contrast_df$value,factor(symp_contrast_df$group),
                        function(x) mean(x>0)),decreasing = TRUE))
pm_0 <- lapply(pm_0,function(x) round(x*100,digits = 2))

#reorder by descending pm
symp_contrast_df$group <- factor(symp_contrast_df$group,
                                 level = c("Total Symptom Severity",
                                           'Cognitive','Somatic','Vision',
                                           'Neck','Sleep','Fatigue','Balance'))

#plot
contrast_plot <- symp_contrast_df%>%
  ggplot(aes(x = value,y = fct_rev(group),fill = stat(x<0))) +
  stat_halfeye(.width = c(.50, .90))+
  theme_bw()+
  theme(legend.position ='none', axis.title.y = element_blank(),
        axis.title.x = element_text(face='bold'),
        axis.text.y = element_text(face = 'bold',size = 10))+
  scale_fill_manual(values = c("red",'grey')) +
  xlab('Estimated Average Symptom Change Following CARE Protocol')+
  annotate("text", x = 3, y = 0.4+(8:1), size = 2.5,label = (unlist(pm_0)))
contrast_plot

ggsave('sup_fig3.jpg',contrast_plot,dpi = 600)

####table for symptom differences (Table 3 in manuscript)

#make 'before' and 'after' in the right order for table
pre_post_symp_tbl_df$Time <- 
  factor(pre_post_symp_tbl_df$Time,
         levels = c("Before",'After'))

#create first part
raw_symp_tbl <- 
  tbl_summary(pre_post_symp_tbl_df,by = Time,
              statistic = all_continuous() ~ "{median} ({p25},{p75})",
              type = list(Somatic ~ 'continuous', Cognitive ~ 'continuous',
                          Fatigue ~ 'continuous', Sleep ~ 'continuous',
                          Balance ~ 'continuous', Vision ~ 'continuous',
                          Neck ~ 'continuous'),
              digits = all_continuous() ~ 1)%>%
  bold_labels()%>%
  modify_caption("**Table 3. Symptoms Before-and-After Aerobic Exercise Plan**") 

#table of % of ppl who had decrease in pre post symp
colnames(symp_diff_df)
perc_dec_df <- data.frame(apply(symp_diff_df[-c(1,2)],2,
                                function(x) ifelse(x>0,1,0)))
colnames(perc_dec_df) <- colnames(pre_post_symp_tbl_df[-c(1)])

#perc improved tbl
perc_dec_tbl <-
  tbl_summary(perc_dec_df)%>%
  bold_labels()

#estimated differences w HDI
#reshape df
colnames(symp_contrast_df)
symp_contrast_df$id <- rep(1:1000,times=8)
est_diff_df <- data.frame(reshape(symp_contrast_df,timevar = 'group',
                                  direction = 'wide'))
est_diff_df$id <- NULL
colnames(est_diff_df) <- colnames(perc_dec_df)
est_diff_df <- as.data.frame(est_diff_df)
#tbl
est_diff_tbl <-
  tbl_summary(est_diff_df,
              digits = all_continuous() ~1,
              statistic = all_continuous()~c("{mean} ({HDILow},{HDIHigh})"))%>%
  modify_footnote(all_stat_cols() ~ "Mean (90% Compatibility Interval)")%>%
  modify_header(label ~ '**Estimated Difference**')%>%
  bold_labels()

#merge 3 sections into one table
t3_hr <-
  tbl_merge(list(raw_symp_tbl,est_diff_tbl,perc_dec_tbl),
            tab_spanner = c("**Raw Symptom Scores**", 
                            "**Estimated Difference**",
                            "**Participants Reporting Decrease**"))%>%
  modify_header(stat_0_2 = '**Posterior Samples**, N = 1000')%>%
  as_gt() %>%
  gt::gtsave(filename = 't3_hr.html')



