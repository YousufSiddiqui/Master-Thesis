setwd("C:/Master Thesis")

#Reading Csv to a DataFrame

feature_data1 = read.csv("variables_cross_session_tablet_1.csv",header=TRUE)


#saving all unique sessions

sessions=(unique(feature_data1$sessionId))

feat_data_max=data.frame()

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
  feat_data_max = rbind(feat_data_max,data.frame(sessionID,
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

feat_data_last=data.frame()

for(i in 1:length(sessions)){
  
  feature_data2= subset(feature_data1, sessionId== sessions[i])
  feature_data3=feature_data2[!feature_data2$category=="-1",]
  feature_data=feature_data3
  
  sessionID=as.character(sessions[i])
  
  #variables where only the last value in the column is required
  
  amountBasketSession=tail(feature_data$amountBasketSession,n=1)
  percPageProduct=tail(feature_data$percPageProduct,n=1)               
  percPageSearch=tail(feature_data$percPageSearch,n=1)       
  percPageOverview=tail(feature_data$percPageOverview,n=1)              
  percPageSale=tail(feature_data$percPageSale,n=1)                   
  percPageCart=tail(feature_data$percPageCart,n=1)
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
  feat_data_last = rbind(feat_data_last,data.frame(sessionID,
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
                                           percPageProduct,                
                                           percPageSearch,                 
                                           percPageOverview,              
                                           percPageSale,                   
                                           percPageCart,
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

feat_data_recode=data.frame()

for(i in 1:length(sessions)){
  
  feature_data2= subset(feature_data1, sessionId== sessions[i])
  feature_data3=feature_data2[!feature_data2$category=="-1",]
  feature_data=feature_data3
  
  sessionID=as.character(sessions[i])
  
  #variables where aggregation of 1's gives relevant information for the session
  
  revisitedPagesInSession = sum(feature_data$pageVisitedBeforeSession==1)
  revisitedPagesBetweenSession = sum(feature_data$currentPageVisitedLastTime==1)
  
  

  #df
  feat_data_recode = rbind(feat_data_recode,data.frame(sessionID,
                                           revisitedPagesInSession,
                                           revisitedPagesBetweenSession
                                       
  ))
}

feat_data_unique=data.frame()

for(i in 1:length(sessions)){
  
  feature_data2= subset(feature_data1, sessionId== sessions[i])
  feature_data3=feature_data2[!feature_data2$category=="-1",]
  feature_data=feature_data3
  
  sessionID=as.character(sessions[i])
  
  #variables where all values are identical so only one is required
  
  dayOfMonth = as.character(unique(feature_data$dayOfMonth))
  sessionStartHour = as.character(unique(feature_data$sessionStartHour))
  weekday = as.character(unique(feature_data$weekday))
  convertedBefore = as.character(unique(feature_data$convertedBefore))
  exitEqualLanding = as.character(unique(feature_data$exitEqualLanding))
  frequencyVisit = as.character(unique(feature_data$frequencyVisit))
  hurry = as.character(unique(feature_data$hurry))
  meanRecencyVisit = as.character(unique(feature_data$meanRecencyVisit))
  browser  =  as.character(unique(feature_data$browser))
  browserVersion = as.character(unique(feature_data$browserVersion))
  device_comp = as.numeric(unique(feature_data$device_comp))
  device_tab = as.numeric(unique(feature_data$device_tab))
  operatingSystem = as.character(unique(feature_data$operatingSystem))
  operatingSystemVersion = as.numeric(unique(feature_data$operatingSystemVersion))
  screenHeight = as.character(unique(feature_data$screenHeight))
  screenWidth = as.character(unique(feature_data$screenWidth))
  tabVisible = as.character(unique(feature_data$tabVisible))
  visitorKnown = as.character(unique(feature_data$visitorKnown))
  recencyVisit = as.character(unique(feature_data$recencyVisit))
  month= as.character(unique(feature_data$month))
  pageCartLV= as.character(unique(feature_data$pageCartLV))
  pageOverviewLV= as.character(unique(feature_data$pageOverviewLV))
  pageProductLV= as.character(unique(feature_data$pageProductLV))
  pageSaleLV= as.character(unique(feature_data$pageSaleLV))
  pageSearchLV= as.character(unique(feature_data$pageSearchLV))
  productBasketLV= as.character(unique(feature_data$productBasketLV))
  purchaseLV= as.character(unique(feature_data$purchaseLV))
  purchaseRecency= as.character(unique(feature_data$purchaseRecency))
  visitTimeLV= as.character(unique(feature_data$visitTimeLV))    

  #df
  feat_data_unique = rbind(feat_data_unique,data.frame(sessionID,
                                       browser,
                                       browserVersion,
                                       convertedBefore,
                                       dayOfMonth,
                                       device_comp,
                                       device_tab,
                                       exitEqualLanding,
                                       frequencyVisit,
                                       meanRecencyVisit,
                                       operatingSystem,
                                       operatingSystemVersion,
                                       screenHeight,
                                       screenWidth,
                                       sessionStartHour,
                                       visitorKnown,
                                       weekday,
                                       hurry,
                                       tabVisible,
                                       recencyVisit,
                                       month,
                                        pageCartLV,
                                        pageOverviewLV,
                                        pageProductLV,
                                        pageSaleLV,
                                        pageSearchLV,
                                        productBasketLV,
                                        purchaseLV,
                                        purchaseRecency,
                                        visitTimeLV      
                                       
                                       
                                       
  ))
}
