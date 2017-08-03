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
  
  amountBasketSession=tail(feature_data2$amountBasketSession)
  percPageProduct=tail(feature_data2$percPageProduct)               
  percPageSearch=tail(feature_data2$percPageSearch)       
  percPageOverview=tail(feature_data2$percPageOverview)              
  percPageSale=tail(feature_data2$percPageSale)                   
  percPageCart=tail(feature_data2$percPageCart)
  clickEventProduct=tail(feature_data2$clickEventProduct)             
  clickEventSearch=tail(feature_data2$clickEventSearch)              
  clickEventOverview=tail(feature_data2$clickEventOverview)            
  clickEventSale=tail(feature_data2$clickEventSale)                
  clickEventCart=tail(feature_data2$clickEventCart)
  scrollEventProduct=tail(feature_data2$scrollEventProduct)                          
  scrollEventSearch=tail(feature_data2$scrollEventSearch)                         
  scrollEventOverview=tail(feature_data2$scrollEventOverview)                            
  scrollEventSale=tail(feature_data2$scrollEventSale)                                
  scrollEventCart=tail(feature_data2$scrollEventCart)
  tabSwitchProduct=tail(feature_data2$tabSwitchProduct)                           
  tabSwitchSearch=tail(feature_data2$tabSwitchSearch)                            
  tabSwitchOverview=tail(feature_data2$tabSwitchOverview)                              
  tabswitchSale=tail(feature_data2$tabswitchSale)                
  tabswitchCart=tail(feature_data2$tabswitchCart)
  timeSpentOnCart=tail(feature_data2$timeSpentOnCart) 
  timeSpentOnProduct=tail(feature_data2$timeSpentOnProduct)
  totPercPageOverview=tail(feature_data2$totPercPageOverview)            
  totPercPageProduct=tail(feature_data2$totPercPageProduct)             
  totPercPageSearch=tail(feature_data2$totPercPageSearch)             
  totPercPageSale=tail(feature_data2$totPercPageSale)                
  totPercPageCart=tail(feature_data2$totPercPageCart)
  percPageProduct=tail(feature_data2$percPageProduct)                
  percPageSearch=tail(feature_data2$percPageSearch)                 
  percPageOverview=tail(feature_data2$percPageOverview)              
  percPageSale=tail(feature_data2$percPageSale)                   
  percPageCart=tail(feature_data2$percPageCart)  
  

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
                                           percPageProduct,                
                                           percPageSearch,                 
                                           percPageOverview,              
                                           percPageSale,                   
                                           percPageCart  
                                       
                                       
                                       
  ))
}


