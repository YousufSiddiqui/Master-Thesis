
setwd("C:/Master Thesis/Preprocessed_Data")

data_final_train = read.csv("preproc_data_train.csv")


### Creating samples using methodology of cross validation

##creating 5 folds manually (instead of using caret) out of training data set

dat_dv=data_final_train[,c("Kauf","sessionID")]



###Creating folds manually with control on dv

set.seed(243)
rand_num=c(sample(c(1:length(which(dat_dv$Kauf==0)))),sample(c(1:length(which(dat_dv$Kauf==1)))))  
dat_dv2=cbind(dat_dv[order(dat_dv[,'Kauf']),],rand_num)

##separating data on value of target variable
dat_dv2_0=dat_dv2[dat_dv2$Kauf==0,'rand_num']
dat_dv2_0=as.data.frame(dat_dv2_0)
dat_dv2_1=dat_dv2[dat_dv2$Kauf==1,'rand_num']
dat_dv2_1=as.data.frame(dat_dv2_1)


##Creating bins or folds with division or rows in 5 equal parts
#For dataset with 0 value of target variable
breaks=unique(quantile(dat_dv2_0[,1], probs = seq(0, 1, by= 0.2)))
dat_dv2_0[,paste(colnames(dat_dv2_0[1]),"bin",sep="_")] = cut(dat_dv2_0[,1], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
colnames(dat_dv2_0)[1]=paste("rand_num")
colnames(dat_dv2_0)[2]=paste("bin")

#For dataset with 1 value of target variable
breaks=unique(quantile(dat_dv2_1[,1], probs = seq(0, 1, by= 0.2)))
dat_dv2_1[,paste(colnames(dat_dv2_1[1]),"bin",sep="_")] = cut(dat_dv2_1[,1], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
colnames(dat_dv2_1)[1]=paste("rand_num")
colnames(dat_dv2_1)[2]=paste("bin")

##Recombining
dat_dv2_bin=rbind(dat_dv2_0,dat_dv2_1)


dat_dv2=cbind(dat_dv2,dat_dv2_bin)
dat_dv2=dat_dv2[,c(1,2,5)]
dat_dv2=dat_dv2[order(dat_dv2[,'sessionID']),]

##creating 5 different datasets using the combination of the folds with one left out
for(j in 1:5)
{
  assign(paste("data_fold_",j,sep=''), dat_dv2[ dat_dv2$bin!=j, c("sessionID","Kauf")])
}


##creating a list of all the datasets
dflist = list(data_fold_1,data_fold_2,data_fold_3,data_fold_4,data_fold_5) 

max_row_num=as.integer(max(as.character(lapply(dflist,function(x)nrow(x)))))


###Creating 5 bootstrap samples

setwd("C:/Master Thesis/Preprocessed_Data/Folds")

for(k in 1:5) 
{
  for(l in 1:5)
  {
    set.seed(243)
    ##taking 67 to 70% of original data and rest of the samples will be repeated in each bag
    a=dflist[[k]][sample(1:nrow(dflist[[k]]),floor(runif(1, 67, 70)*nrow(dflist[[k]])/100)),]
    set.seed(243)
    b=rbind(a,a[sample(1:nrow(a),max_row_num-nrow(a)),])
    b=merge(data_final_train,b,by=c('sessionID','Kauf'))
    b=b[order(b[,'sessionID']),]
    write.csv(b, paste("preproc_data_dfold",k,"_bag",l,".csv",sep=''),row.names=F)
    
  }
  ##also saving each fold created from cross validation method
  write.csv(merge(data_final_train,dflist[[k]],by=c('sessionID','Kauf')),paste("preproc_data_dfold",k,".csv",sep=''),row.names=F)
}

