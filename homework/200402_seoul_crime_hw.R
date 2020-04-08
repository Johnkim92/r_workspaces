googleAPIkey <-  "AIzaSyA2UeDNgg4KUaq5ANuZI-A_fU-2JnqcdOA"

library(devtools)
library(ggmap)
library(dplyr)

crime_data <- read.csv("../datas/crime_in_Seoul.csv")
head(crime_data)
str(station_data)

register_google(googleAPIkey)

copy_crime_data <- crime_data
# 절도.발생 절도.검거 폭력.발생 폭력.검거 열이 factor로 되어있다. 변환해주어야한다.
copy_crime_data$절도.발생 <- as.integer(gsub(",","", copy_crime_data$절도.발생))
copy_crime_data$폭력.발생 <- as.integer(gsub(",","", copy_crime_data$폭력.발생))
copy_crime_data$절도.검거 <- as.integer(gsub(",","", copy_crime_data$절도.검거))
copy_crime_data$폭력.검거 <- as.integer(gsub(",","", copy_crime_data$폭력.검거))

# 관서별 발생율만 분할
crime_occur <- cbind(copy_crime_data[,2],copy_crime_data[,4],copy_crime_data[,6],copy_crime_data[,8],copy_crime_data[,10])
head(crime_occur)
crime_occur_tot <- rowSums(crime_occur)
class(crime_occur_tot)
copy_crime_data <- transform(copy_crime_data, 총발생건수=crime_occur_tot)
head(copy_crime_data)
arrest_occur <- cbind(copy_crime_data[,3],copy_crime_data[,5],copy_crime_data[,7],copy_crime_data[,9],copy_crime_data[,11])
arrest_occur_tot <- rowSums(arrest_occur)
copy_crime_data <- transform(copy_crime_data, 총검거건수=arrest_occur_tot)
copy_crime_data <- transform(copy_crime_data, 발생율 = round(crime_occur_tot/(crime_occur_tot+arrest_occur_tot)*100,1))
copy_crime_data <- transform(copy_crime_data, 검거율 = round(arrest_occur_tot/(crime_occur_tot+arrest_occur_tot)*100,1))

crime_data_final <- aggregate(cbind(copy_crime_data$발생율, copy_crime_data$검거율) ~ copy_crime_data$관서명, crime_data_final, sum)
crime_data_final <- rename(crime_data_final,"관서명"="copy_crime_data$관서명","발생율"="V1", "검거율"="V2")
head(crime_data_final)

police_address <- read.csv("../datas/crime_in_Seoul_address.csv")
head(police_address)

police_address_code <- as.character(police_address$"주소") %>% enc2utf8() %>% geocode()
head(police_address_code)
police_final <- cbind(police_address,
                      police_address_code) %>% select("주소",lon,lat)
head(police_final)

crime_code_final <- cbind(crime_data_final,
                          police_final) %>% select("관서명","발생율","검거율","주소",lon,lat)
head(crime_code_final)
crime_code_final$발생율 <- paste("발생율:",crime_code_final$발생율)
crime_code_final$검거율 <- paste("검거율:",crime_code_final$검거율)
seoul_map <- get_googlemap("seoul", maptype = "roadmap", zoom = 11)

ggmap(seoul_map)+
  geom_point(data= crime_code_final,
             aes(x=lon, y=lat),
             colour = "red",
             size = 3)+
  geom_text(data = crime_code_final,
            aes(label=관서명, vjust=-1), colour="black", size=8,             
            label=crime_code_final$관서명)+
  geom_text(data = crime_code_final,
            aes(label = 발생율, vjust=2), colour="blue", size=5)+
  geom_text(data = crime_code_final,
            aes(label = 검거율, vjust=4), colour="blue",size=5)

  