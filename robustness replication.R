# load packages
rm(list=ls(all=T))
require('xtable')
library('lmtest')
library('multiwayvcov')
library('tidyverse')
library('estimatr')

# load data
nyc<-read.csv('nyc_data.csv',as.is=T)
nyc_data <- nyc



######## Original Results #######
#set seed
set.seed(70984)

temp_table<-rep(0,10)

#prepare table
temp_results<-c('location','pedestrians','controls','df','fit','t_value','Std. Error','p_value','lower90','upper90','lower95','upper95')
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)

mod0<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Original'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'Y'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                           round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)

mod0<-lm(distance_diff~black_confederates, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Original'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'N'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                           round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)





######## Robustnesss Check 1 #######
# show prevalence of outliers
black <- nyc_data %>% filter(black_confederates == "Y")

nonblack <- nyc_data %>% filter(black_confederates == "N")

hist(nyc_data$distance_diff, breaks = seq(-.8, .8, by = 0.01))
hist(black$distance_diff, breaks = seq(-.8, .8, by = 0.01))
hist(nonblack$distance_diff, breaks = seq(-.8, .8, by = 0.01))


#set seed
set.seed(70984)

# exclude extreme outliers as described in the PRAP
nyc_cut <- nyc %>% 
  filter(distance_diff > -0.4 & distance_diff < 0.4)

small_nyc_cut <- small_nyc %>% 
  filter(distance_diff > -0.4 & distance_diff < 0.4)

mod0<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=nyc_cut)
small_nyc_cut<-nyc_cut[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=small_nyc_cut)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc_cut[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Excluding Outliers'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'Y'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)

mod0<-lm(distance_diff~black_confederates, data=nyc_cut)
small_nyc_cut<-nyc_cut[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates, data=small_nyc_cut)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc_cut[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Excluding Outliers'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'N'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)







######## Robustnesss Check 2a #######
#set alternative seed 1
set.seed(111)

mod0<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Alternative Seed 1'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'Y'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)

mod0<-lm(distance_diff~black_confederates, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Alternative Seed 1'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'N'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)







######## Robustnesss Check 2b #######
#set alternative seed 2
set.seed(123)

mod0<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Alternative Seed 2'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'Y'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)

mod0<-lm(distance_diff~black_confederates, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Alternative Seed 2'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'N'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)







######## Robustnesss Check 2c #######
#set alternative seed 3
set.seed(2023)

mod0<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates+race+block_id+direction+group_id, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Alternative Seed 3'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'Y'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)

mod0<-lm(distance_diff~black_confederates, data=nyc)
small_nyc<-nyc[names(residuals(mod0)),]
mod3<-lm(distance_diff~black_confederates, data=small_nyc)
mod3_df<-mod3$df.residual
boot_se <- cluster.boot(mod3, boot_type = "wild", cluster = small_nyc[,c('cluster')], parallel = TRUE, R=1000)

temp_model<-'Alternative Seed 3'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'N'
temp_row<-c(temp_model,temp_location,temp_ped,temp_controls,round(coeftest(mod3, boot_se)['black_confederatesY',c('Estimate')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('t value')],3),
            round(coeftest(mod3, boot_se)['black_confederatesY',c('Std. Error')],3),
            mod3_df,
            ifelse(0.001>coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],'<0.001',
                   round(coeftest(mod3, boot_se)['black_confederatesY',c('Pr(>|t|)')],3)),
            paste('[',round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[1],3),', ',
                  round(coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95)[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,temp_location,temp_ped,temp_controls,mod3_df,coeftest(mod3, boot_se)['black_confederatesY',c('Estimate','t value','Std. Error','Pr(>|t|)')],coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.90),coefci(mod3, parm='black_confederatesY',vcov=boot_se,level=.95))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)







######## Robustnesss Check 3 #######
#set seed
set.seed(70984)


mod3 <- lm_robust(distance_diff~black_confederates+race+block_id+direction+group_id,
                                      se_type = "stata",
                                      clusters = cluster,
                                      data = nyc)

mod3



temp_model<-'Non-Bootstrapped SE'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'Y'
temp_row<-c(temp_model,
            temp_location,
            temp_ped,
            temp_controls,
            round(mod3$coefficients[2],3),
            round(mod3$statistic[2],3),
            round(mod3$std.error[2],3),
            mod3$df.residual,
            ifelse(0.001>mod3$p.value[2],'<0.001',
                   round(mod3$p.value[2],3)),
            paste('[',round(mod3$conf.low[2],3),', ',
                  round(mod3$conf.high[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,
                temp_location,
                temp_ped,
                temp_controls,
                round(mod3$coefficients[2],3),
                round(mod3$statistic[2],3),
                round(mod3$std.error[2],3),
                mod3$df.residual,
                ifelse(0.001>mod3$p.value[2],'<0.001',
                       round(mod3$p.value[2],3)),
                paste('[',round(mod3$conf.low[2],3),', ',
                      round(mod3$conf.high[2],3),']',sep=''))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)



mod3 <- lm_robust(distance_diff~black_confederates,
                  se_type = "stata",
                  clusters = cluster,
                  data = nyc)

mod3



temp_model<-'Non-Bootstrapped SE'
temp_location<-'Both Locations'
temp_ped<-'All'
temp_controls<-'N'
temp_row<-c(temp_model,
            temp_location,
            temp_ped,
            temp_controls,
            round(mod3$coefficients[2],3),
            round(mod3$statistic[2],3),
            round(mod3$std.error[2],3),
            mod3$df.residual,
            ifelse(0.001>mod3$p.value[2],'<0.001',
                   round(mod3$p.value[2],3)),
            paste('[',round(mod3$conf.low[2],3),', ',
                  round(mod3$conf.high[2],3),']',sep=''))
temp_table<-rbind(temp_table,temp_row)

temp_results<-c(temp_model,
                temp_location,
                temp_ped,
                temp_controls,
                round(mod3$coefficients[2],3),
                round(mod3$statistic[2],3),
                round(mod3$std.error[2],3),
                mod3$df.residual,
                ifelse(0.001>mod3$p.value[2],'<0.001',
                       round(mod3$p.value[2],3)),
                paste('[',round(mod3$conf.low[2],3),', ',
                      round(mod3$conf.high[2],3),']',sep=''))
write.table(matrix(temp_results,nrow=1),'data/closest_point_results.csv',row.names=F,col.names=F,sep=',',append=T)





# create table
colnames(temp_table)<-c('Model','Location','Pedestrians',"Controls",'Beta','T-Statistic','Standard Error','DF','P-Value','CI')
print(xtable(temp_table[-1,]),type='latex',file='table_1.html',include.colnames =TRUE,include.rownames =FALSE)
