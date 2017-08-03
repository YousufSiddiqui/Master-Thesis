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

  #df
  feat_data2 = rbind(feat_data2,data.frame(sessionID,
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
                                       weekday
                                       
                                       
                                       
  ))
}


