source("STOMPTBMainCasesCont_R_2019-10-02_1156.r")
require(dplyr)

# make a single enrollment date varaible across arms
data$date_enrolled <- as.character(data$date_enrolled_case)
data$date_enrolled[data$date_enrolled==""] <- as.character(data$date_enrolled_ctrl[data$date_enrolled==""])
data$date_enrolled[data$date_enrolled==""] <- as.character(data$com_date_enrolled_case[data$date_enrolled==""])
data$date_enrolled[data$date_enrolled==""] <- as.character(data$com_date_enrolled_ctrl[data$date_enrolled==""])
data$date_enrolled[data$date_enrolled==""] <- as.character(data$ct_date_enrolled[data$date_enrolled==""])

