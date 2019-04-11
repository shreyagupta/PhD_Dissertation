setwd("C:/Users/Shreya/Dropbox/Shreya/Research/Samsung1/Final Results 2/Heterogen on new graph/Paper code and results")
library(xlsx)
library(stats)
statisticdf_names <- c('avgWaitTime','avgPickupTime','avgDeliveryTime','avgServiceTime','avgCycleTime',
'sdevWaitTime','sdevPickupTime','sdevDeliveryTime','sdevServiceTime','sdevCycleTime','vehUtil')

algo_hypo_test <- function(file_name,sheet_name,dataframe,sample_start_colNum,sample_end_colNum){
  algos <- as.vector(dataframe$algorithm)
  pneq <- c()
  pgeq <- c()
  pleq <- c()
  algo1 <- c()
  algo2 <- c()
  mean_difference <- c()
  lb_difference <- c()
  ub_difference <- c()
  for (i in 1:length(algos)){
    for (j in 1:length(algos)){
      if (i<j){
        algo1[length(algo1)+1] <- algos[i]
        algo2[length(algo2)+1] <- algos[j]
        # browser()
        mui <- as.numeric(dataframe[i,sample_start_colNum:sample_end_colNum])
        muj <- as.numeric(dataframe[j,sample_start_colNum:sample_end_colNum])
        alternate_neq <- t.test(mui,muj,alternative="t",mu=0,
                                paired=TRUE,var.equal=FALSE,
                                conf.level=0.95)
        alternate_geq <- t.test(mui,muj,alternative="g",mu=0,
                                paired=TRUE,var.equal=FALSE,
                                conf.level=0.95)
        alternate_leq <- t.test(mui,muj,alternative="l",mu=0,
                                paired=TRUE,var.equal=FALSE,
                                conf.level=0.95)
        if((is.na(alternate_neq$p.value<0.05))){
          pneq[length(pneq)+1] <- 0.00
          pgeq[length(pgeq)+1] <- 999999.99
          pleq[length(pleq)+1] <- 999999.99
          lb_difference[length(lb_difference)+1] <- 0.00
          ub_difference[length(ub_difference)+1] <- 0.00
          mean_difference[length(mean_difference)+1] <- 0.00
        }
        else{
          pneq[length(pneq)+1] <- round(alternate_neq$p.value,4)
          pgeq[length(pgeq)+1] <- round(alternate_geq$p.value,4)
          pleq[length(pleq)+1] <- round(alternate_leq$p.value,4)
          if(pneq[length(pneq)]>=0.05){
            lb_difference[length(lb_difference)+1] <- alternate_neq$conf.int[1]
            ub_difference[length(ub_difference)+1] <- alternate_neq$conf.int[2]
            mean_difference[length(mean_difference)+1] <- alternate_neq$estimate
          }
          else if(pgeq[length(pgeq)]<0.05){
            lb_difference[length(lb_difference)+1] <- alternate_geq$conf.int[1]
            ub_difference[length(ub_difference)+1] <- alternate_geq$conf.int[2]
            mean_difference[length(mean_difference)+1] <- alternate_geq$estimate
          }
          else{
            lb_difference[length(lb_difference)+1] <- alternate_leq$conf.int[1]
            ub_difference[length(ub_difference)+1] <- alternate_leq$conf.int[2]
            mean_difference[length(mean_difference)+1] <- alternate_leq$estimate
          }
        }
      }
    }
  }
  statistic_df <- data.frame(algo1,algo2,lb_difference,ub_difference,mean_difference,pneq,pgeq,pleq)
  write.xlsx(statistic_df,file=file_name,sheetName = sheet_name,append = TRUE, row.names = TRUE,col.names = TRUE,showNA = TRUE)
}

recursive_rank<- function(lowr_algo2,betr_algo2,rank,hypo_grtr,hypo_smllr){
  if(!(is.element(lowr_algo2,(names(rank))))){
    rank[lowr_algo2]=rank[betr_algo2]+1
  }
  else if(rank[lowr_algo2]<=rank[betr_algo2]){
    # if(as.character(lowr_algo2)=="FabNetwork_PFIFO_RBigM2_heterogen" && rank["FabNetwork_PFIFO_RBigM2_heterogen"]==6){browser()}
    rank[lowr_algo2]=rank[betr_algo2]+1 
    betr_algo2 = lowr_algo2
    # lowr_algos2 = relation_df$algo2[which(hypo_grtr$algo1==as.character(lowr_algo2))]
    
    lowr_algos2 <- unique(c(as.character(hypo_smllr$algo2[which(hypo_smllr$algo1==as.character(betr_algo2))]),
                           as.character(hypo_grtr$algo1[which(hypo_grtr$algo2==as.character(betr_algo2))])))
    if(length(lowr_algos2)>0){
      for(lowr_algo2 in lowr_algos2){
        rank = recursive_rank(lowr_algo2,betr_algo2,rank,hypo_grtr,hypo_smllr)
      }
    }
  }
  return(rank)
}

file_name <- "Hypo_Test_Results3.xlsx"
for(i in seq(11)){#seq(length(statisticdf_names))){
  sheet_name=statisticdf_names[i]
  # file_sheet_name <- paste('Hypo_Test_',statisticdf_names[i],'.xlsx',sep='')
  # browser()
  sim_stat_df=read.xlsx("simulation_samples.xlsx",i,colClasses = c(rep('numeric',1),"character",rep('numeric',10)))
  algo_hypo_test(file_name,sheet_name,sim_stat_df,4,13)
  #Reading hypo test results
  hypo_results=read.xlsx(file_name,i)
  # colnames(hypo_results)
  # browser()
  hypo_results$NA.<-NULL
  hypo_results$lb_difference[!(is.finite(hypo_results$lb_difference))] <- -Inf
  hypo_results$Result[hypo_results$pneq<0.05 & hypo_results$pleq<0.05]<-"significantly smaller"
  hypo_results$Result[hypo_results$pneq<0.05 & hypo_results$pgeq<0.05]<-"significantly bigger"
  hypo_results$Result[is.na(hypo_results$Result)]<-"no significant difference"
  # browser()
  hypo_results$Result<-as.character(hypo_results$Result)
  hypo_grtr <- subset(hypo_results,Result=="significantly bigger")
  hypo_smllr <- subset(hypo_results,Result=="significantly smaller")
  better_algos = {}
  algos_grtr = unique(hypo_smllr$algo1)
  rank = {}
  for(betr_algo in algos_grtr){
    if(!(is.element(betr_algo,(names(rank))))){
      rank[betr_algo]=0
    }
    rank[betr_algo]=rank[betr_algo]+1
    # lowr_algos = hypo_grtr$algo2[which(hypo_grtr$algo1==as.character(betr_algo))]
    # lowr_algos = c(lowr_algos,hypo_smllr$algo1[which(hypo_smllr$algo2==as.character(betr_algo))])
    # lowr_algos = unique(lowr_algos)
    lowr_algos <- unique(c(as.character(hypo_smllr$algo2[which(hypo_smllr$algo1==as.character(betr_algo))]),
                           as.character(hypo_grtr$algo1[which(hypo_grtr$algo2==as.character(betr_algo))])))
    # if(length(lowr_algos)>0){browser()}
    for(lowr_algo in lowr_algos){
      rank = recursive_rank(lowr_algo,betr_algo,rank,hypo_grtr,hypo_smllr)
    }
  }
  sim_stat_df$Rank <- NA
  for(algo in names(rank)){
    sim_stat_df$Rank[sim_stat_df$algorithm==algo] <- as.integer(rank[algo])
  }
  # browser()
  # write.xlsx(sim_stat_df,file='Algorithm Ranks3.xlsx',sheetName=statisticdf_names[i],append = TRUE, row.names = TRUE,col.names = TRUE,showNA = TRUE)
}

  
#thsd
i=5 #cycle time
file_name<-"Final Results 2/Heterogen on new graph/Paper code and results/simulation_samples.xlsx"#"simulation_samples.xlsx"#"Algorithm Ranks3.xlsx" #
sim_stat_df=read.xlsx(file_name,i,colClasses = c(rep('numeric',1),"character",rep('numeric',10)))
algos<-c()
for(k in seq(10)){
  algos<-c(algos,as.character(sim_stat_df[,2]))
  ctimes<-c(ctimes,as.numeric(sim_stat_df[,k+2]))
}
for(k in seq(length(statisticdf_names))){
  sim_stat_df=read.xlsx(file_name,k,colClasses = c(rep('numeric',1),"character",rep('numeric',10)))
  stats<-c()
  for(j in seq(10)){
    stats<-c(stats,as.numeric(sim_stat_df[,j+2]))
  }
  # browser()
  algo_df <- cbind.data.frame(algos,stats)
  
  #plot(stats ~ algos, data=algo_df, main=statisticdf_names[i],las=2,font.axis=0.5)
  results = aov(formula=stats~algos,data=algo_df)
  pair_wise_test_thsd <- TukeyHSD(results,conf.level = 0.95)
  difference_interval_data<-pair_wise_test_thsd$algos
  write.xlsx(difference_interval_data,file='Final Results 2/Heterogen on new graph/Paper code and results/TukeyHSD pairwise differences_2.xlsx',sheetName=statisticdf_names[k],append = TRUE, row.names = TRUE,col.names = TRUE,showNA = TRUE)
}

sim_stat_df=read.xlsx(file_name,i,colClasses = c(rep('numeric',1),"character",rep('numeric',10)))
stats<-c()
for(j in seq(10)){
  stats<-c(stats,as.numeric(sim_stat_df[,j+2]))
}
# browser()
algo_df <- cbind.data.frame(algos,stats)
#plot(stats ~ algos, data=algo_df, main=statisticdf_names[i],las=2,font.axis=0.5)
results = aov(formula=stats~algos,data=algo_df)
pair_wise_test_thsd <- TukeyHSD(results,conf.level = 0.95)
difference_interval_data<-pair_wise_test_thsd$algos
write.xlsx(difference_interval_data,file='Final Results 2/Heterogen on new graph/Paper code and results/TukeyHSD pairwise differences_2.xlsx',sheetName=statisticdf_names[i],append = TRUE, row.names = TRUE,col.names = TRUE,showNA = TRUE)

