rm(list=ls(all=TRUE)) 
library(rmarkdown)
library(ggplot2)
library(knitr)
library(lubridate)
library(stringr)
library(dplyr)
library(DT)
library(jsonlite)
library(reshape2)
library(scales)
library(ggrepel)
library(jcolors)
library(sparkline)
library(hrbrthemes)
library(htmlwidgets)

setwd('/Users/mart/Desktop/production/llvll.ch/')

# FUNCTIONS

toperson <- function(personnum, colourstr){
  unlucky<-sample(1:personnum, 1, replace=T)
  people<-paste(strrep(paste("<i class=\"fas fa-user\" style=\"color:",colourstr,"\"></i> ", sep=""), unlucky-1),
                strrep("<i class=\"fas fa-user\"  style=\"color:red\"></i> ",1),
                strrep(paste("<i class=\"fas fa-user\" style=\"color:",colourstr,"\"></i> ", sep=""), personnum-unlucky),
                strrep(" ",2+personnum), sep="")
  return(people)
}

double.every.n <- function(length, n){
  # Returns a vector of length that doubles every n 
  seed<-1
  increase<-seq(1,length)
  increase<-2^((increase-1)/n)
  return(increase)
}

percent.increase.in.last.n <- function(df, n){
  # Returns a vector of length that doubles every n 
  df<-data.frame("count"<-df)
  percent.increase= df %>% 
    mutate("increase"=ifelse(is.na(lag(count, n)),100,100*(count-lag(count, n))/count))
  return(percent.increase )
}


###################################### End of Functions

# Parameters

topcount<-10
confirmed.cutoff<-30
coi<-sort(c("Italy", "United Kingdom","Switzerland", "Germany","South Africa","USA","Austria","Peru", "Brazil" ,"Canada","Belarus", "Ghana", "India", "Sweden","Nigeria", "Russia"))
minimum.pop=5e6
urlcases<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master"
casespercapita.origin<-1e-6
casespercapita.cutoff<-0
doublingbreaks<-data.frame("days"=c(3,7,14,30,90))




###################################### End of Parameters

# Get data

# Time series data (Source reliability seems to be deteriorating. Consider changing if this is not rectified.)

if (as.numeric(difftime(Sys.time(),file.info("./data/time_series_covid19_deaths_global.csv")$mtime, units="hours"))>1){
#  if(TRUE){
  file.copy("./data/time_series_covid19_deaths_global.csv",paste("./data/time_series_covid19_deaths_global",Sys.Date(),".csv",sep=""))
  death.wide <- read.csv(paste(urlcases,"/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",sep=""),stringsAsFactors = FALSE)
  write.csv(death.wide,"./data/time_series_covid19_deaths_global.csv",row.names = FALSE)
  
} else {
  death.wide <- read.csv("./data/time_series_covid19_deaths_global.csv",stringsAsFactors = FALSE)
}


# if (as.numeric(difftime(Sys.time(),file.info("./time_series_19-covid-Recovered.csv")$mtime, units="hours"))>1){
# #if(TRUE){
#   file.copy("time_series_19-covid-Recovered.csv",paste("time_series_19-covid-Recovered.csv",Sys.Date(),".csv",sep=""))
#   recovered.wide <- read.csv(paste(urlcases,"/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",sep=""),stringsAsFactors = FALSE)
#   write.csv(recovered.wide,"./time_series_19-covid-Recovered.csv",append=FALSE,row.names = FALSE)
#   
# } else {
#   
#   recovered.wide <- read.csv("./time_series_19-covid-Recovered.csv",stringsAsFactors = FALSE)
# 
# } 

if (as.numeric(difftime(Sys.time(),file.info("./data/time_series_covid19_confirmed_global.csv")$mtime, units="hours"))>1){
#  if(TRUE){
  file.copy("./data/time_series_covid19_confirmed_global.csv",paste("./data/time_series_covid19_confirmed_global.csv",Sys.Date(),".csv",sep=""))
  confirmed.wide <- read.csv(paste(urlcases,"/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", sep=""),stringsAsFactors = FALSE)
  write.csv(confirmed.wide ,"./data/time_series_covid19_confirmed_global.csv",row.names = FALSE)
  
} else {
  
  confirmed.wide <- read.csv("./data/time_series_covid19_confirmed_global.csv",stringsAsFactors = FALSE)
}
# csv from high frequency worldometer data. 

worldometer<- read.csv("./data/input_file.csv",stringsAsFactors = FALSE)
worldometer$NewCases<-as.integer(gsub(",","",worldometer$NewCases))
worldometer$NewDeaths<-as.integer(gsub(",","",worldometer$NewDeaths))
worldometer$Tests..1M.pop.<-as.integer(gsub(",","",worldometer$Tests..1M.pop.))

# Get population data from the World Bank API
url <- 'http://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?date=2018&format=json&per_page=300'

# read url and convert to data.frame
#population.doc <- fromJSON(txt=url)
#write_json(population.doc,"./population.json")
population.doc <- fromJSON("./data/population.json")

# Continent -> Country dataset downloaded from Kaggle
country2continent <- read.csv(
  "https://llvll.ch/countryContinent.csv")

####################################################### End of getting data

# Tidy and rearrange data

# Turn date column titles into something I can work with Currently XM.DD.YY
# Convert to DD.MM.YY

total.cases.worldometer<-as.numeric(gsub(",", "",as.character(worldometer[worldometer$Country.Other=="World",]$TotalCases)), fixed=T)
total.deaths.worldometer<-as.numeric(gsub(",", "",as.character(worldometer[worldometer$Country.Other=="World",]$TotalDeaths)), fixed=T)
total.recovered.worldometer<-as.numeric(gsub(",", "",as.character(worldometer[worldometer$Country.Other=="World",]$TotalRecovered)), fixed=T)

names(death.wide)[4:length(names(death.wide))]<-str_replace(names(death.wide[4:length(names(death.wide))]), "X(\\d\\d?)+.(\\d\\d?).(\\d\\d)", "\\2.\\1.\\3")
#names(recovered.wide)[4:length(names(recovered.wide))]<-str_replace(names(recovered.wide[4:length(names(recovered.wide))]), "X(\\d\\d?)+.(\\d\\d?).(\\d\\d)", "\\2.\\1.\\3")
names(confirmed.wide)[4:length(names(confirmed.wide))]<-str_replace(names(confirmed.wide[4:length(names(confirmed.wide))]), "X(\\d\\d?)+.(\\d\\d?).(\\d\\d)", "\\2.\\1.\\3")

# Now melt them

death.long <- melt(death.wide, id=c("Province.State","Country.Region","Lat","Long" ))
names(death.long)[5]<-"Date"
names(death.long)[6]<-"number.death"
#recovered.long <- melt(recovered.wide, id=c("Province.State","Country.Region","Lat","Long" ))
#names(recovered.long)[5]<-"Date"
#names(recovered.long)[6]<-"number.recovered"
confirmed.long <- melt(confirmed.wide, id=c("Province.State","Country.Region","Lat","Long" ))
names(confirmed.long)[5]<-"Date"
names(confirmed.long)[6]<-"number.confirmed"

population.data<-as.data.frame(population.doc[[2]])

names(population.data)[3]<-"code_3"
population.data<-merge(population.data,country2continent, by=c("code_3"))

population<-as.data.frame(cbind("Country.Region"=population.data$country.x$value,"population"=population.data$value,"year"=population.data$date,"continent"=as.character(population.data$continent)),stringsAsFactors = FALSE)


################################################## Repair documented data errors on the github dataset
taiwan<-c("Taiwan*","32780000","2018","Asia")
population<-rbind (population, taiwan)

# Get counts before the merges so that disappearing countries will not deplete totals

df <-merge(death.long, confirmed.long, by=c("Lat","Long","Date","Country.Region"))
df$number.recovered=0      # We're getting rid of recovered from the JH dataset                 ADD IT BACK IN
#df <-merge(df, recovered.long, by=c("Lat","Long","Date","Country.Region"))
df$Date<-dmy(df$Date)
df$Country.Region[df$Country.Region=="US"]<-"USA"


################ Where multiple regions for the same country are listed on a date, sum and summarise 

df$Country.Region[df$Province.State.x=="Greenland"]<-"Greenland"
df$Country.Region[df$Province.State.x=="Hong Kong"]<-"Hong Kong"


df<- df %>%
  group_by(Country.Region, Date) %>%
  arrange(Date) %>%
  summarise(number.confirmed=sum(number.confirmed,na.rm=TRUE),number.death=sum(number.death,na.rm=TRUE),number.recovered=sum(number.recovered,na.rm=TRUE)) %>%
  ungroup()

###################################################################### End of region merge

# Fix of Some countrynames for alignment between data sets

population$Country.Region[population$Country.Region=="Korea, Rep."]<-"Korea, South"

population$Country.Region[population$Country.Region=="United States"]<-"USA"


population$Country.Region[population$Country.Region=="Iran, Islamic Rep."]<-"Iran"
population$Country.Region[population$Country.Region=="Hong Kong SAR, China"]<-"Hong Kong"
population$Country.Region[population$Country.Region=="Viet Nam"]<-"Vietnam"
population$Country.Region[population$Country.Region=="Slovak Republic"]<-"Slovakia"
population$Country.Region[population$Country.Region=="Russian Federation"]<-"Russia"
population$Country.Region[population$Country.Region=="Egypt, Arab Rep."]<-"Egypt"
population$Country.Region[population$Country.Region=="Czech Republic"]<-"Czechia"
# unique(df$Country.Region[!((df$Country.Region) %in% population$Country.Region)]) # Countries and data get lost in the population merge. Fix this

worldometer$Country.Other<-str_trim(as.character(worldometer$Country.Other))
worldometer$Country.Other[worldometer$Country.Other=="UK"]<-"United Kingdom"
worldometer$Country.Other[worldometer$Country.Other=="USA"]<-"USA"
worldometer$Country.Other[worldometer$Country.Other=="S. Korea"]<-"Korea, South"
#worldometer$Country.Other[worldometer$Country.Other=="Hong Kong"]<-"Hong Kong SAR, China"
worldometer$Country.Other[worldometer$Country.Other=="UAE"]<-"United Arab Emirates"

######################

# Merge death recovered and confirmed
df.pre.pop.merge<-df
df<- merge(df, population, by=c("Country.Region"))

df$Country.Region<-as.character(df$Country.Region)
df$population<-as.integer(df$population)

df$number.death.per.capita<-df$number.death/df$population
df$number.recovered.per.capita<-df$number.recovered/df$population
df$number.confirmed.per.capita<-df$number.confirmed/df$population

# Add a days of data column

mindate<-min(df$Date)
maxdate<-max(df$Date)
df<-df %>% 
  group_by(Date) %>%
  mutate("days.of.data"=difftime(Date, mindate,unit="days")) %>%
  ungroup()

df$continent<-as.character(df$continent)

#for normalised plots

growth<-df

growth<-growth %>%
  group_by(Date)%>%
  summarise("continent"=first(continent),"confirmed"=sum(number.confirmed,na.rm=TRUE),"death"=sum(number.death,na.rm=TRUE),"recovered" = sum(number.recovered,na.rm=TRUE)) %>%
  mutate("newconfirmed"=confirmed-lag(confirmed,1),"newdeath"=death-lag(death,1),"newrecovered"=recovered-lag(recovered,1)) %>%
  ungroup()

growth.by.country<-df %>%
  group_by(Date,Country.Region) %>%
  summarise("continent"=first(continent),"confirmed"=sum(number.confirmed,na.rm=TRUE),"death"=sum(number.death,na.rm=TRUE),"recovered" = sum(number.recovered,na.rm=TRUE)) %>%
  ungroup()

growth.by.country<-growth.by.country %>%
  group_by(Country.Region) %>%
  mutate("newconfirmed"=confirmed-lag(confirmed,1),"newdeath"=death-lag(death,1),"newrecovered"=recovered-lag(recovered,1),"active"=confirmed-death-recovered)%>%
  ungroup()

###################################################################### End of get counts


# Add a days of data column

mindate<-min(df$Date)
maxdate<-max(df$Date)
df<-df %>% 
  group_by(Date) %>%
  mutate("days.of.data"=difftime(Date, mindate,unit="days")) %>%
  ungroup()

df$continent<-as.factor(df$continent)

# For normalised plots

onsetday<-subset(df,population>casespercapita.cutoff) %>% 
  group_by(Country.Region) %>%
  mutate("day.of.onset"= if_else(number.confirmed.per.capita>casespercapita.origin,1,0),"day.of.first.10death"=if_else(number.death>10,1,0)) %>%  
  filter(number.confirmed>0) %>%
  ungroup()

onsetday<-onsetday %>% 
  group_by(Country.Region) %>%
  arrange(Date) %>%
  mutate("day.since.onset"= cumsum(day.of.onset),"day.since.onset.10death"= cumsum(day.of.first.10death)) %>%
  filter(number.confirmed>0) %>%
  ungroup()

onsetday<- merge(onsetday, population, by=c("Country.Region"))

####################################################################### End of onset data

df.today<-df %>%
  group_by(Country.Region) %>%
  summarise(total.death=max(number.death),
            percent.mortality=format(100*max(number.death)/max(number.confirmed),digits=3),
            total.recovered=max(number.recovered),
            total.confirmed=max(number.confirmed),
            death.per.capita=as.numeric(format(max(number.death/population),digits=3)),
            recovered.per.capita=as.numeric(format(max(number.recovered/population),digits=3)),
            confirmed.per.capita=as.numeric(format(max(number.confirmed/population),digits=3)),
            cases.per.capita=format((max(number.confirmed)-max(number.recovered)-max(number.death))/unique(population),digits=3),
            population= unique(population),
            continent=unique(continent))


df.today.above.cutoff <- df.today %>% filter(total.confirmed>confirmed.cutoff)

continental<-df %>%
  group_by(continent,Date) %>%
  summarise(number.confirmed=max(number.confirmed),
            number.death=max(number.death),
            number.recovered=max(number.recovered),
            population=max(population)
  ) 


### WORLDOMETHER TABLE 


df.table<-worldometer %>% 
  filter(!(Country.Other %in% c("Total:","World","Asia","Europe","Africa","North America","South America","Oceania")))


df.table$percent.mortality<-NA
df.table<-df.table[,c("Country.Other","TotalCases","TotalDeaths","percent.mortality","TotalTests","Tests..1M.pop.","Tot.Cases.1M.pop")]

cols = c(2:7);    
df.table[,cols] = apply(df.table[,cols], 2, function(x) as.numeric(gsub(",", "",as.character(x)),fixed=T));


df.table$TotalDeaths[is.na(df.table$TotalDeaths)]<-0
df.table$TotalCases[is.na(df.table$TotalCases)]<-0
df.table$TotalTests[is.na(df.table$TotalTests)]<-0
df.table$Tests..1M.pop.[is.na(df.table$Tests..1M.pop.)]<-0
df.table$percent.mortality<-df.table$TotalDeaths/df.table$TotalCases

country.population<-population %>%
  select(Country.Region, population)
names(country.population)[1]<-"Country.Other"

# Countries without a population following this are most likely due to naming mismatch
df.table<-merge(df.table, country.population, by="Country.Other", all.x=TRUE)

df.table<- df.table %>%
  select(-Tot.Cases.1M.pop) %>%
  filter(Country.Other!="")

df.table$Country.Other<-paste0("<a href=\"http://llvll.ch/country/",df.table$Country.Other,".html\">",df.table$Country.Other,"</a>")
df.table$TotalCases<-prettyNum(df.table$TotalCases, big.mark=",")
df.table$TotalDeaths<-prettyNum(df.table$TotalDeaths, big.mark=",")
df.table$TotalTests<-prettyNum(df.table$TotalTests, big.mark=",")
df.table$Tests..1M.pop.<-prettyNum(df.table$Tests..1M.pop., big.mark=",")
df.table$percent.mortality<-percent(as.numeric(df.table$percent.mortality), accuracy=0.1, suffix="%")
df.table$population<-prettyNum(df.table$population, big.mark=",")

df.today.above.cutoff$total.recovered.per.case<-df.today.above.cutoff$total.recovered/df.today.above.cutoff$total.confirmed
df.today.above.cutoff$total.death.per.case<-df.today.above.cutoff$total.death/df.today.above.cutoff$total.confirmed



# Get a list of highload countries

highload  <- subset(df.today.above.cutoff,population>minimum.pop) %>%
  arrange(-confirmed.per.capita) 

highload<- head(highload$Country.Region,topcount)


######################################################### End of tidy                                                            
maxcases<-max(onsetday$number.confirmed, na.rm=T)

######## Onsetday Cases=1/Million
onsetdaycopy<-onsetday           # THIS IS A HACK TO PREVENT A SHIFT TO TIME SINCE 10 DEATHS

onsetday<-onsetday %>%
  group_by(Country.Region) %>% 
  arrange(Date) %>%
  mutate("new.cases.in.last.7.days"=ifelse(is.na(lag(number.confirmed, 7)),100,100*(number.confirmed-lag(number.confirmed, 7))/number.confirmed),"darkness"=max(number.confirmed, na.rm=T)/maxcases) %>% 
  ungroup()

onsetday<-onsetday %>% 
  group_by(Country.Region, day.since.onset) %>%
  arrange(Date) %>%
  filter(Date == max(Date))%>%
  ungroup()


onsetdaycoi<-onsetday %>% 
  filter(Country.Region %in% coi)

maxdeaths<-max(onsetdaycoi$number.death, na.rm=T)

######## Onsetday Deaths = 10


onsetdaydeath<-onsetdaycopy %>%
  group_by(Country.Region) %>% 
  arrange(Date) %>%
  mutate("new.deaths.in.last.7.days"=ifelse((is.na(lag(number.death, 7))) | (number.death==0),100,100*(number.death-lag(number.death, 7))/number.death),"darkness"=max(number.confirmed, na.rm=T)/maxcases) %>% 
  ungroup()

onsetdaydeath<-onsetdaydeath %>% 
  group_by(Country.Region, day.since.onset.10death) %>%
  arrange(Date) %>%
  filter(Date == max(Date)) %>%
  ungroup()

onsetdaydeathcoi<-onsetdaydeath %>% 
  filter(Country.Region %in% coi)


##########


labelplace<-1.05*max(onsetdaycoi$day.since.onset)
x_limits <- c(labelplace, NA)
y_limits <- c(0, 90) 


################################# End of setup


# This is the loop that produces all the country pages
doublingbreaks$new.in.last10<-100*(1-2^(-10/doublingbreaks$days))
notationdf<-data.frame(value="GOAL: ZERO GROWTH",title=paste("Data up to:",max(df$Date)))
mindate<-ymd(maxdate) - ddays(28)
worldometer<-worldometer %>%
             filter(!(Country.Other %in% c("Total:","North America","Africa","Asia","Europe","")))
for (id in unique(worldometer$Country.Other)){
  subgroup <- worldometer[worldometer$Country.Other== id,]
  print(id)
  growthloop<- growth.by.country[growth.by.country$Country.Region==id,]
  render("./countrypagetemplate.Rmd",output_file = paste0("country/",subgroup$Country.Other, '.html'))
}

library(plotly)

#Produce Index PageSS
render("./index.Rmd",output_file = 'index.html')

#Produce countrylist page
render("./countrylist.Rmd",output_file = 'countrylist.html')
#Produce Google page
#render("./google+JH.Rmd",output_file = 'google+JH.html')



df<-df %>%
  group_by(Country.Region, Date) %>%
  mutate("percent.mortality"= 100*number.death/number.confirmed)%>%
  ungroup()

timeplots<-subset(growth.by.country, (Country.Region %in% coi))


doublingbreaks$new.in.last10<-100*(1-2^(-10/doublingbreaks$days))

library(tidyr)

onsetdayani<-onsetday %>% complete(Country.Region, nesting(Date),fill = list(new.cases.in.last.7.days = 100, new.deaths.in.last.7.days = 100, number.confirmed=0,number.death=0, day.since.onset = 0))

onsetdayani<- onsetdayani %>%
  group_by(Country.Region) %>%
  mutate(continent.y=last(continent.y))

onsetdayanideath<-onsetdaydeath %>% complete(Country.Region, nesting(Date),fill = list(new.cases.in.last.7.days = 100, new.deaths.in.last.7.days = 100, number.confirmed=0,number.death=0, day.since.onset.10death=0))

onsetdayanideath<- onsetdayanideath %>%
  group_by(Country.Region) %>%
  mutate(continent.y=last(continent.y))


######## Days to 100K deaths

daysto100K<-onsetdaydeath %>%
  group_by(Country.Region) %>%
  filter(Date == max(Date)) %>%
  summarise("deaths"=max(number.death),"days.to.double"=7/(log2(1/(1-(new.deaths.in.last.7.days/100))))) %>%
  filter(!is.infinite(days.to.double),days.to.double>0) %>%  
  ungroup()

daysto100K<- daysto100K %>% 
  group_by(Country.Region) %>%
  mutate("days.to.million"=days.to.double*log2(1e5/deaths))

daysto100K$Country.Region<-paste0("<a href=\"http://llvll.ch/country/",daysto100K$Country.Region,".html\">",daysto100K$Country.Region,"</a>")
daysto100K$days.to.double<- round(daysto100K$days.to.double,digits =2)
daysto100K$days.to.million<-round(daysto100K$days.to.million,digits= 2)
daysto100K$deaths<-prettyNum(daysto100K$deaths, big.mark=",")
####### Tinkering for 1st frame of propogation animation

df.firstframe<- df %>% 
  group_by(continent,Date) %>%
  arrange(Date) %>% 
  filter(row_number()==n())

df.firstframe$Country.Region<-""
#df.firstframe$days.of.data<-0
df.firstframe$percent.mortality<-0
df.firstframe$number.confirmed<-1
df.firstframe$number.death<-0
df.firstframe$population<-1

df.scatter<-subset(df, (number.death>=100) & (percent.mortality)<=20)

df.scatter<-bind_rows(df.firstframe,df.scatter)

df.scatter<-df.scatter %>%
            group_by(Country.Region) %>%
            arrange(Date) %>%
            ungroup


########################## ADD ISO 2 to DF.SCATTER
# countrycodes<- read.csv(paste(urlcases,"/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv",sep=""),stringsAsFactors = FALSE)
# justcountrycodes<-countrycodes[,c("iso2","Country_Region","Province_State")]
# names(justcountrycodes)[names(justcountrycodes)=="Country_Region"]<-"Country.Region"
# names(justcountrycodes)[names(justcountrycodes)=="Province_State"]<-"Province.State"
# # Drop all country codes with a non blank Province.State
# justcountrycodes<-justcountrycodes %>%
#                   filter(Province.State=="") %>%
#                   select(-Province.State)
# justcountrycodes<-rbind(justcountrycodes,c(".",""))
# df.scatter<-merge(df.scatter, justcountrycodes, by=c("Country.Region"))
countrycodes<- read.csv(paste(urlcases,"/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv",sep=""),stringsAsFactors = FALSE)

justcountrycodes<-countrycodes[,c("iso2","Country_Region","Province_State")]
names(justcountrycodes)[names(justcountrycodes)=="Country_Region"]<-"Country.Region"
names(justcountrycodes)[names(justcountrycodes)=="Province_State"]<-"Province.State"

death.wide<-merge(death.wide, justcountrycodes, by=c("Country.Region","Province.State"))
names(death.wide)[4:length(names(death.wide))]<-str_replace(names(death.wide[4:length(names(death.wide))]), "X(\\d\\d?)+.(\\d\\d?).(\\d\\d)", "\\2.\\1.\\3")

death.long <- melt(death.wide, id=c("Province.State","Country.Region","Lat","Long","iso2" ))
names(death.long)[names(death.long)=="variable"]<-"Date"
names(death.long)[names(death.long)=="value"]<-"number.death"
death.long<- death.long %>%
  group_by(Country.Region, Date) %>%
  arrange(Date) %>%
  summarise(iso2=first(iso2),number.death=sum(number.death,na.rm=TRUE)) %>%
  ungroup()
death.long$Date<-dmy(death.long$Date)


dfgoogle<-read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",stringsAsFactors = FALSE)
dfgoogle<-dfgoogle %>% 
  filter(sub_region_1=="")
dfgoogle$date<-ymd(dfgoogle$date)
names(dfgoogle)[names(dfgoogle)=="date"]<-"Date"
names(dfgoogle)[names(dfgoogle)=="country_region_code"]<-"iso2"
dfgoogle$iso2<-as.character(dfgoogle$iso2)

dfgoogle<-merge(dfgoogle, death.long,by=c("Date","iso2"), all=T)

dfgoogle<-dfgoogle %>% 
  group_by(iso2) %>%
  mutate("day.of.first.10death"=if_else(number.death>10,1,0)) %>%  
  ungroup()

dfgoogle<-dfgoogle%>% 
  group_by(iso2) %>%
  arrange(Date) %>%
  mutate("day.since.onset.10death"= cumsum(day.of.first.10death),"number.deaths.today"=(number.death-lag(number.death,7))/7) %>%
  filter(day.since.onset.10death>0) %>%
  ungroup()

# Bit of a hack. For any country code with some NA values in country_region, add them back in 
# this can be removed if the merge above is handled better

dfgoogle<-dfgoogle%>% 
  group_by(iso2) %>%
  arrange(Date) %>%
  mutate(country_region=lag(country_region,10)) %>%
  ungroup()

scatter<-dfgoogle %>%
  group_by(iso2) %>%
  arrange(Date) %>%
  summarise("at10deaths"=nth(residential_percent_change_from_baseline,1),"meanres"=mean(residential_percent_change_from_baseline),"totaldeaths"=last(number.death))



#Produce detailed page
render("./indexfull.Rmd",output_file = 'indexfull.html')



