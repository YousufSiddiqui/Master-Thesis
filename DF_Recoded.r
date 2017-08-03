setwd("C:/Master Thesis")

#Reading Csv to a DataFrame

feature_data1 = read.csv("variables_cross_session_tablet_1.csv",header=TRUE)


#saving all unique sessions

sessions=(unique(feature_data1$sessionId))

feat_data2=data.frame()

for(i in 1:length(sessions)){
  
  feature_data2= subset(feature_data1, sessionId== sessions[i])
  feature_data3=feature_data2[!feature_data2$category=="-1",]
  feature_data=feature_data3
  
  sessionID=as.character(sessions[i])
  
  #variables where aggregation of 1's gives relevant information for the session
  
  revisitedPagesInSession = sum(feature_data$pageVisitedBeforeSession==1)
  revisitedPagesBetweenSession = sum(feature_data$currentPageVisitedLastTime==1)
  
  

  #df
  feat_data2 = rbind(feat_data2,data.frame(sessionID,
                                           revisitedPagesInSession,
                                           revisitedPagesBetweenSession
                                       
  ))
}


