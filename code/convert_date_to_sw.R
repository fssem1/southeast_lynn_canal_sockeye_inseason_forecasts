#notes----
#author: Sara E Miller 
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: April 2018

# load ----
source("code/helper.R")
source("code/functions.R")

#data----
read.csv('data/Chilkat_weir_by_date.csv') -> data

#data clean----
mdata <- melt(data, id=c("Date"))
names(mdata) <- c('date','year','count')
mdata$year <- substring(mdata$year, 2) #remove X from year
mdata$date2 <- paste(mdata$year,"-",mdata$date) 
mdata$date2 <- (str_replace_all(mdata$date2, fixed(" "), "")) #delete spaces in date2
mdata$date2 <- (str_replace_all(mdata$date2, fixed("/"), "-")) #delete spaces in date2
mdata$date3 <- as.Date(mdata$date2)
mdata$SW <- ifelse(format(as.Date(format(mdata$date3, "%Y-01-01")), "%a")=="Sun",
                      as.integer(strftime(as.Date(mdata$date3, "%m/%d/%Y"), format = "%U")),
                      as.integer(strftime(as.Date(mdata$date3, "%m/%d/%Y"), format = "%U")) + 1) #convert to SW



mdata %>% 
  mutate(date=as.Date(date3),
         year=factor(year),
         count=as.numeric(count),
         SW=as.numeric(SW))-> mdata
write.csv(mdata, file = "output/test1.csv")#SW (ADF&G version)
mdata <- mdata[ -c(1,4,5) ]
mdata <- dcast(mdata, SW~year, value.var="count", fun.aggregate=sum, na.rm=TRUE) #aggregate counts by year and SW (ADF&G version)
write.csv(mdata, file = "output/Chilkat_weir.csv")



#data----
read.csv('data/Chilkat_weir_by_date.csv') -> data

#data clean----
glimpse(data)
data %>% 
    gather(year, count, -Date) %>% 
    mutate(year = substring(year, 2), # remove x from year
         date =ymd(paste0(year, "/", Date)),
         week = week(date),
          Year = factor(year)) -> data # returns ISO week starting on Sunday (SW)

write.csv(data, file = "output/test2.csv")