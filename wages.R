#Preparing for analysis
install.packages('data.table')
install.packages('sqldf')
install.packages('dplyr')
install.packages('stringr')
install.packages('ggplot2')
install.packages('maps')
install.packages('bit64')
install.packages('RColorBrewer')
install.packages('choroplethr')
install.packages('rbenchmark')
install.packages('microbenchmark')

library(bit64)
library(data.table)
library(sqldf)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(choroplethr)
library(rbenchmark)
library(microbenchmark)



#Importing employment data into R

ann2012 <- read.csv('data/2012.annual.singlefile.csv', stringsAsFactors=F)


#Obtaining additional data for the codes

for(u in c('agglevel','area','industry',
           'ownership','size')){
  assign(u,read.csv(paste('data/',u,'_titles.csv',sep=''), stringsAsFactors=F))
}

#left join the values of the codes into a new dataframe

codes <- c('agglevel','industry','ownership','size')
ann2012full <- ann2012
for(i in 1:length(codes)){
  eval(parse(text=paste('ann2012full <- left_join(ann2012full, ',codes[i],')', sep='')))
}


#Adding geographical information

#capitalise first letter
simpleCap <-function(x){
  if(!is.na(x)){
    s <- strsplit(x,' ')[[1]]
    paste(toupper(substring(s,1,1)), substring(s,2), 
          sep='', collapse=' ')
  } else {NA}
}

#data from the maps package
data(county.fips)

#regex out the county name and add in standardised form
county.fips$fips <- str_pad(county.fips$fips, width=5, pad="0")

county.fips$polyname <- as.character(county.fips$polyname)
county.fips$county <- sapply(
  gsub('[a-z\ ]+,([a-z\ ]+)','\\1',county.fips$polyname),
  simpleCap)
county.fips <- unique(county.fips)


#same as county data
data(state.fips)


state.fips$fips <- str_pad(state.fips$fips, width=2, pad="0", side='left')
state.fips$state <- as.character(state.fips$polyname)
state.fips$state <- 
  gsub("([a-z\ ]+):[a-z\ \\']+",'\\1',state.fips$state)
state.fips$state <- sapply(state.fips$state, simpleCap)

mystatefips <-unique(state.fips[,c('fips','abb','state')])

lower48 <- setdiff(unique(state.fips$state),c('Hawaii','Alaska'))

myarea <- merge(area, county.fips, by.x='area_fips',by.y='fips', all.x=T)
myarea$state_fips <- substr(myarea$area_fips, 1,2)
myarea <- merge(myarea, mystatefips,by.x='state_fips',by.y='fips', all.x=T)

ann2012full <- left_join(ann2012full, myarea)
ann2012full <- filter(ann2012full, state %in% lower48)



#Extracting state- and county-level wage and employment information
d.state <- filter(ann2012full, agglvl_code==50)
d.state <- select(d.state, state, avg_annual_pay, annual_avg_emplvl)

d.state$wage <- cut(d.state$avg_annual_pay, quantile(d.state$avg_annual_pay, c(seq(0,.8, by=.2), .9, .95, .99, 1)))
d.state$empquantile <- cut(d.state$annual_avg_emplvl, quantile(d.state$annual_avg_emplvl, c(seq(0,.8,by=.2),.9,.95,.99,1)))

x <- quantile(d.state$avg_annual_pay, c(seq(0,.8,by=.2),.9, .95, .99, 1))
xx <- paste(round(x/1000),'K',sep='')
Labs <- paste(xx[-length(xx)],xx[-1],sep='-')
levels(d.state$wage) <- Labs

x <- quantile(d.state$annual_avg_emplvl, c(seq(0,.8,by=.2),.9, .95, .99, 1))
xx <- ifelse(x>1000, paste(round(x/1000),'K',sep=''),round(x))

Labs <- paste(xx[-length(xx)],xx[-1],sep='-')
levels(d.state$empquantile) <- Labs

Discretize <- function(x, breaks=NULL){
  if(is.null(breaks)){
    breaks <- quantile(x, c(seq(0,.8,by=.2),.9, .95, .99, 1))
    if (sum(breaks==0)>1) { 
      temp <- which(breaks==0, arr.ind=TRUE)
      breaks <- breaks[max(temp):length(breaks)]
    }
  }
  x.discrete <- cut(x, breaks, include.lowest=TRUE)
  breaks.eng <- ifelse(breaks > 1000,
                       paste0(round(breaks/1000),'K'),
                       round(breaks))
  Labs <- paste(breaks.eng[-length(breaks.eng)], breaks.eng[-1],
                sep='-')
  levels(x.discrete) <- Labs
  return(x.discrete)
}

d.cty <- filter(ann2012full, agglvl_code==70)%.%
  select(state,county,abb, avg_annual_pay, annual_avg_emplvl)%.%
  mutate(wage=Discretize(avg_annual_pay),
         empquantile=Discretize(annual_avg_emplvl))


#Visualizing geographical distributions of pay
#map_data crom ggplot2 package
state_df <- map_data('state')
county_df <- map_data('county')
#conform to our data
transform_mapdata <- function(x){
  names(x)[5:6] <- c('state','county')
  for(u in c('state','county')){
    x[,u] <- sapply(x[,u],simpleCap)
  }
  return(x)
}
state_df <- transform_mapdata(state_df)
county_df <- transform_mapdata(county_df)



chor <- left_join(county_df, d.cty)
ggplot(chor, aes(long,lat, group=group))+
  geom_polygon(aes(fill=wage))+
  geom_path( color='white',alpha=0.5,size=0.2)+
  geom_polygon(data=state_df, color='black',fill=NA)+
  scale_fill_brewer(palette='PuRd')+
  labs(x='',y='', fill='Avg Annual Pay')+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())





