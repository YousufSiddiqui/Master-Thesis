rm(list=ls())

setwd("C:/Users/Yousuf/OneDrive/Master Thesis/Data_from_network_drive/Model Ready Data/Metric Split Data")

training_data= read.csv("data_metric_train.csv",as.is=T)

### Creating samples using methodology of cross validation

##creating 5 folds manually (instead of using caret) out of training data set

dat_Kauf=training_data[,c("Kauf","sessionID")]



###Creating folds manually with control on Kauf

set.seed(243)
rand_num=c(sample(c(1:length(which(dat_Kauf$Kauf==0)))),sample(c(1:length(which(dat_Kauf$Kauf==1)))))  
dat_Kauf2=cbind(dat_Kauf[order(dat_Kauf[,'Kauf']),],rand_num)

##separating data on value of target variable
dat_Kauf2_0=dat_Kauf2[dat_Kauf2$Kauf==0,'rand_num']
dat_Kauf2_0=as.data.frame(dat_Kauf2_0)
dat_Kauf2_1=dat_Kauf2[dat_Kauf2$Kauf==1,'rand_num']
dat_Kauf2_1=as.data.frame(dat_Kauf2_1)


##Creating bins or folds with division or rows in 5 equal parts
#For dataset with 0 value of target variable
breaks=unique(quantile(dat_Kauf2_0[,1], probs = seq(0, 1, by= 0.2)))
dat_Kauf2_0[,paste(colnames(dat_Kauf2_0[1]),"bin",sep="_")] = cut(dat_Kauf2_0[,1], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
colnames(dat_Kauf2_0)[1]=paste("rand_num")
colnames(dat_Kauf2_0)[2]=paste("bin")

#For dataset with 1 value of target variable
breaks=unique(quantile(dat_Kauf2_1[,1], probs = seq(0, 1, by= 0.2)))
dat_Kauf2_1[,paste(colnames(dat_Kauf2_1[1]),"bin",sep="_")] = cut(dat_Kauf2_1[,1], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
colnames(dat_Kauf2_1)[1]=paste("rand_num")
colnames(dat_Kauf2_1)[2]=paste("bin")

##Recombining
dat_Kauf2_bin=rbind(dat_Kauf2_0,dat_Kauf2_1)


dat_Kauf2=cbind(dat_Kauf2,dat_Kauf2_bin)
dat_Kauf2=dat_Kauf2[,c(1,2,5)]
dat_Kauf2=dat_Kauf2[order(dat_Kauf2[,'sessionID']),]

##creating 5 different datasets using the combination of the folds with one left out
	for(j in 1:5)
	{
	assign(paste("data_fold_",j,sep=''), dat_Kauf2[ dat_Kauf2$bin!=j, c("sessionID","Kauf")])
	}


##creating a list of all the datasets
dflist = list(data_fold_1,data_fold_2,data_fold_3,data_fold_4,data_fold_5) 

max_row_num=as.integer(max(as.character(lapply(dflist,function(x)nrow(x)))))


###Creating 5 bootstrap samples


	for(k in 1:5) 
	{
		for(l in 1:5)
		{
			set.seed(243)
			##taking 67 to 70% of original data and rest of the samples will be repeated in each bag
			a=dflist[[k]][sample(1:nrow(dflist[[k]]),floor(runif(1, 67, 70)*nrow(dflist[[k]])/100)),]
			set.seed(243)
			b=rbind(a,a[sample(1:nrow(a),max_row_num-nrow(a)),])
			b=merge(training_data,b,by=c('sessionID','Kauf'))
			b=b[order(b[,'sessionID']),]
			write.csv(b, paste("data_metric_dfold",k,"_bag",l,".csv",sep=''),row.names=F)
			
		}
		##also saving each fold created from cross validation method
		write.csv(merge(training_data,dflist[[k]],by=c('sessionID','Kauf')),paste("data_metric_dfold",k,".csv",sep=''),row.names=F)
	}
