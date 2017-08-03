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
  
  #variables where only maximum values are required
  
  countPagesRevisitedLastSession=max(feature_data$countPagesRevisitedLastSession)
  sessionCart=max(feature_data$sessionCart)
  sessionOverview=max(feature_data$sessionOverview)
  sessionProduct=max(feature_data$sessionProduct)
  sessionSale=max(feature_data$sessionSale)
  sessionSearch=max(feature_data$sessionSearch)
  sessionTime=max(feature_data$sessionTime)
  viewCountSession=max(feature_data$viewCountSession)
  clickEventsSession=max(feature_data$clickEventsSession)
  scrollEventSession =max(feature_data$scrollEventSession) 
  tabSwitchSession =max(feature_data$tabSwitchSession)
  timeSinceAddToBasket  =max(feature_data$timeSinceAddToBasket)
  countPagesRevisitedLastSession =max(feature_data$countPagesRevisitedLastSession)
  purchasesrVisit=max(feature_data$purchasesrVisit)
  totPurchasesAmount=max(feature_data$totPurchasesAmount)    
  totPurchasesItems=max(feature_data$totPurchasesItems)
  totViewCount=max(feature_data$totViewCount)                   
  totVisitTime=max(feature_data$totVisitTime)
  currentViewCountVsPreviousAvg=max(feature_data$currentViewCountVsPreviousAvg)
  currentVisitLengthVsAvg=max(feature_data$currentVisitLengthVsAvg)

  #df
  feat_data2 = rbind(feat_data2,data.frame(sessionID,
                                           countPagesRevisitedLastSession,
                                           sessionCart,
                                           sessionOverview,
                                           sessionProduct,
                                           sessionSale,
                                           sessionSearch,
                                           sessionTime,
                                           viewCountSession,
                                           clickEventsSession,
                                           scrollEventSession, 
                                           tabSwitchSession,
                                           timeSinceAddToBasket,
                                           countPagesRevisitedLastSession,
                                           purchasesrVisit,
                                           totPurchasesAmount,   
                                           totPurchasesItems,
                                           totViewCount,                  
                                           totVisitTime,
                                           currentViewCountVsPreviousAvg,
                                           currentVisitLengthVsAvg
                                       
                                       
                                       
  ))
}


