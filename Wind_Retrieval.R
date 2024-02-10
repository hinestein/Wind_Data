library(RCurl)
library(stringi)
library(stringr)
library(zeallot)
a1 = getURL('http://www.bom.gov.au/vic/observations/vicall.shtml')

gregexpr("February", a1)

b1 = gregexpr("</a>", a1)[[1]]
b1 = b1[b1 > gregexpr("MALLEE", a1)[[1]][1]]
b1 = b1[b1 < gregexpr("Weather station information", a1)[[1]][1]]
b1 = c(gregexpr("MALLEE", a1)[[1]][1], b1)
VIC_Locations = NULL
for(i in 2:length(b1)){
  sub_string1 = substr(a1, b1[i-1], b1[i])
  Ind_1 = gregexpr(">", sub_string1)[[1]]
  VIC_Locations = c(VIC_Locations, substr(sub_string1, tail(Ind_1, 1) + 1, nchar(sub_string1) - 1))
}

VIC_Locations1 = strsplit(VIC_Locations, "\\(")
VIC_Locations2 = NULL
for(i in 1:length(VIC_Locations1)){
  VIC_Locations2 = c(VIC_Locations2, paste0(VIC_Locations1[[i]], collapse = '\\('))
}
VIC_Locations3 = strsplit(VIC_Locations2, "\\)")
VIC_Locations4 = NULL
for(i in 1:length(VIC_Locations3)){
  VIC_Locations4 = c(VIC_Locations4, paste0(VIC_Locations3[[i]], collapse = '\\)'))
}

for(i in 1:length(VIC_Locations4)){
  Ind_1 = gregexpr("\\(", VIC_Locations4[i])[[1]]
  Ind_2 = gregexpr("\\)", VIC_Locations4[i])[[1]]
  if(Ind_1[1] > 0 & Ind_2[1] < 0){
    VIC_Locations4[i] = paste0(VIC_Locations4[i], "\\)")
  }
  VIC_Locations4[i] = paste0(">", VIC_Locations4[i], "<")
}


VIC_Data = NULL
for(i in 1:(length(VIC_Locations4) - 1)){
  Ind_1 = gregexpr(VIC_Locations4[i], a1)[[1]]
  Ind_2 = gregexpr(VIC_Locations4[i + 1], a1)[[1]]
  sub_string1 = substr(a1, Ind_1[1], Ind_2[1])
  b1 = gregexpr('">', sub_string1)[[1]]
  b2 = gregexpr('</td>', sub_string1)[[1]]
  b3 = gregexpr("<br />", sub_string1)[[1]]
  b4 = sort(c(b2, b3))
  Temp_Vec = NULL
  for(j in 1:length(b2)){
    sub_string2 = substr(sub_string1, b1[j], b2[j])
    Ind_3 = gregexpr("<", sub_string2)[[1]]
    sub_string3 = substr(sub_string2, 3, Ind_3[[1]][1] - 1)
    Temp_Vec = c(Temp_Vec, sub_string3)
  }
  Time = strsplit(Temp_Vec[1], "/")[[1]][2]
  Ind_4 = gregexpr("pm", Time)[[1]]
  if(Ind_4 > 0){
    Time = paste0(as.numeric(substr(Time, 0, 2)) + 12, substr(Time, 3, 5), ":00")
  }else{
    Time = paste0(substr(Time, 0, 5), ":00")
  }
  Temp_Vec = Temp_Vec[-1]
  Temp_Vec = c(VIC_Locations[i], as.character(Sys.Date()), Time, Temp_Vec)
  VIC_Data = rbind(VIC_Data, Temp_Vec)
}

VIC_Data = as.data.frame(VIC_Data)
row.names(VIC_Data) = NULL
colnames(VIC_Data) = c("Locations", "Date", "Time", "Temp", "Apparent_Temp", "Dew_Point", "Relative_Humidity", "Delta_T",
                       "Wind_Direction", "Wind_Speed(km/h)", "Wind_Gust(km/h)", "Wind_Speed(Knots)", "Wind_Gust(Knots)",
                       "Pressure", "Rain_Since_9am", "Low_Temp", "High_Temp", "Highest_Wind_Gust_Direction",
                       "Highest_Wind_Gust_Speed(km/h)", "Highest_Wind_Gust_Speed(Knots)")

while(TRUE){
  print("hi")
}

