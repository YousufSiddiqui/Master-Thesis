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
  
  #variables where only the last value in the column is required
  
  
  amountBasketSession=tail(feature_data$amountBasketSession,n=1)
  clickEventProduct=tail(feature_data$clickEventProduct,n=1)             
  clickEventSearch=tail(feature_data$clickEventSearch,n=1)              
  clickEventOverview=tail(feature_data$clickEventOverview,n=1)            
  clickEventSale=tail(feature_data$clickEventSale,n=1)                
  clickEventCart=tail(feature_data$clickEventCart,n=1)
  scrollEventProduct=tail(feature_data$scrollEventProduct,n=1)                          
  scrollEventSearch=tail(feature_data$scrollEventSearch,n=1)                         
  scrollEventOverview=tail(feature_data$scrollEventOverview,n=1)                            
  scrollEventSale=tail(feature_data$scrollEventSale,n=1)                                
  scrollEventCart=tail(feature_data$scrollEventCart,n=1)
  tabSwitchProduct=tail(feature_data$tabSwitchProduct,n=1)                           
  tabSwitchSearch=tail(feature_data$tabSwitchSearch,n=1)                            
  tabSwitchOverview=tail(feature_data$tabSwitchOverview,n=1)                              
  tabswitchSale=tail(feature_data$tabswitchSale,n=1)                
  tabswitchCart=tail(feature_data$tabswitchCart,n=1)
  timeSpentOnCart=tail(feature_data$timeSpentOnCart,n=1) 
  timeSpentOnProduct=tail(feature_data$timeSpentOnProduct,n=1)
  totPercPageOverview=tail(feature_data$totPercPageOverview,n=1)            
  totPercPageProduct=tail(feature_data$totPercPageProduct,n=1)             
  totPercPageSearch=tail(feature_data$totPercPageSearch,n=1)             
  totPercPageSale=tail(feature_data$totPercPageSale,n=1)                
  totPercPageCart=tail(feature_data$totPercPageCart,n=1)
  percPageProduct=tail(feature_data$percPageProduct,n=1)                
  percPageSearch=tail(feature_data$percPageSearch,n=1)                 
  percPageOverview=tail(feature_data$percPageOverview,n=1)              
  percPageSale=tail(feature_data$percPageSale,n=1)                   
  percPageCart=tail(feature_data$percPageCart,n=1)
  currentViewCountVsLV=tail(feature_data$currentViewCountVsLV,n=1)
  currentVisittimeVsLV=tail(feature_data$currentVisittimeVsLV,n=1)
  Kauf=tail(feature_data$Kauf,n=1)
  pageCartrVisit=tail(feature_data$pageCartrVisit,n=1)
  pageOverviewrVisit=tail(feature_data$pageOverviewrVisit,n=1)
  pageProductrVisit=tail(feature_data$pageProductrVisit,n=1)
  pageSalerVisit=tail(feature_data$pageSalerVisit,n=1)
  pageSearchrVisit=tail(feature_data$pageSearchrVisit,n=1)
  totCart=tail(feature_data$totCart,n=1)
  totOverview=tail(feature_data$totOverview,n=1)
  totProduct=tail(feature_data$totProduct,n=1)
  totSale=tail(feature_data$totSale,n=1)
  totalItemValueBasketSession=tail(feature_data$otalItemValueBasketSession,n=1)
  
  

  #df
  feat_data2 = rbind(feat_data2,data.frame(sessionID,
                                           amountBasketSession,
                                           percPageProduct,               
                                           percPageSearch,       
                                           percPageOverview,              
                                           percPageSale,                   
                                           percPageCart,
                                           clickEventProduct,             
                                           clickEventSearch,              
                                           clickEventOverview,            
                                           clickEventSale,                
                                           clickEventCart,
                                           scrollEventProduct,                          
                                           scrollEventSearch,                         
                                           scrollEventOverview,                            
                                           scrollEventSale,                                
                                           scrollEventCart,
                                           tabSwitchProduct,                           
                                           tabSwitchSearch,                            
                                           tabSwitchOverview,                              
                                           tabswitchSale,                
                                           tabswitchCart,
                                           timeSpentOnCart, 
                                           timeSpentOnProduct,
                                           totPercPageOverview,            
                                           totPercPageProduct,             
                                           totPercPageSearch,             
                                           totPercPageSale,                
                                           totPercPageCart,
                                           currentViewCountVsLV,
                                            currentVisittimeVsLV,
                                            Kauf,
                                            pageCartrVisit,
                                            pageOverviewrVisit,
                                            pageProductrVisit,
                                            pageSalerVisit,
                                            pageSearchrVisit,
                                            totCart,
                                            totOverview,
                                            totProduct,
                                            totSale,
                                            totalItemValueBasketSession
                                       
                                       
                                       
  ))
}


