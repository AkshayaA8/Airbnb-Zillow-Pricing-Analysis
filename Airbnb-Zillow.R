library(forecast)
library(sqldf)
library(plotly)
library(RColorBrewer)
library(zipcode)
library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(ggalt)
library(ggthemes)
library(ggrepel)

#-------------------------- Zillow data ----------------------------
#reading the zillow data
#data table is 'zillow'

#extracting only new york
zillow$City = tolower(trimws(zillow$City,which = "both"))
zillow = zillow[zillow$City %in% "new york",]
rownames(zillow) = zillow$RegionName
zillow = zillow[,c(-1:-7)]

zillow_T = data.frame(t(zillow))
colnames(zillow_T) = substring(colnames(zillow_T),2)

#to convert to date format
years = substring(rownames(zillow_T),2,5)
months = substring(rownames(zillow_T),7,8)
date = as.data.frame(as.Date(paste(paste(months,"01",sep="/"),years,sep = "/"),"%m/%d/%Y"))
zillow_T = cbind(date,zillow_T)
rownames(zillow_T) <- NULL
colnames(zillow_T)[1] = "date"

zillow_T = zillow_T[complete.cases(zillow_T),]
plot(zillow_T$date,zillow_T$`10025`,type = "line")

#using data from jan 2013
zillow_T = zillow_T[zillow_T$date>='2013-01-01',]

#time series for every zip code
zillow_zip = as.data.frame(zillow_T[,c(1)])
colnames(zillow_zip) = "date"
zillow_zip1 = as.data.frame(seq(as.Date("2017-07-01"),as.Date("2019-06-01"),by="months"))
colnames(zillow_zip1) = "date"
zillow_zip = rbind(zillow_zip,zillow_zip1)
View(zillow_zip)

#building Seasonal arima model
for(i in 2:ncol(zillow_T)){
   name = paste("zillow",(i-1))
   name = zillow_T[,c(1,i)]
   colnames(name)[2] = "pricing"
   name$new_pricing = log(name$pricing)
   name$new_pricing_diff[2:nrow(name)] = diff(name$new_pricing,differences = 1, lag = 1)  
   
   data_ts = ts(data = name$new_pricing_diff[2:nrow(name)],start = name$date[1], 
                end = name$date[nrow(name)],frequency = 12)
   
   #arima model
   data_auto = auto.arima(data_ts)
   p = arimaorder(data_auto)[1]
   d = arimaorder(data_auto)[2]
   q = arimaorder(data_auto)[3]
   P = arimaorder(data_auto)[4]
   D = arimaorder(data_auto)[5]
   Q = arimaorder(data_auto)[6]
   f = arimaorder(data_auto)[7]
   
   if(is.na(P) || is.na(Q) || is.na(D) || is.na(f)){
     model = arima(data_ts, order = c(p,d,q))
   }else{
     model = arima(data_ts, order = c(p,d,q), seas=list(order=c(P,D,Q),f))
   }
   
   #forecasts
   fore = predict(model,n.ahead =24)
   pred_df = data.frame(fore$pred)
   pred_df$date = seq(as.Date('2017-07-01'),as.Date('2019-06-01'),"months")
   colnames(pred_df)[1] = "new_pricing_diff"
   pred_df$pricing = 0
   pred_df$new_pricing = 0
   
   pred_df = pred_df[,c(2,3,4,1)]

   name = rbind(name,pred_df)
   name$new_pricing_undiff = diffinv(name$new_pricing_diff[2:nrow(name)],xi=name$new_pricing[1])
   name$pricing_unlog = exp(name$new_pricing_undiff)

   zillow_zip[,paste("col",(i-1))] = name$pricing_unlog
  
}


cols = colnames(zillow_zip)[2:26]
zillow_zip[cols] = lapply(zillow_zip[cols], as.integer)
colnames(zillow_zip) = colnames(zillow_T)
zillow_zip_final = as.data.frame(t(zillow_zip))

for(i in 1:ncol(zillow_zip_final)){
  colnames(zillow_zip_final)[[i]] = as.character(zillow_zip_final[1,i])
}

zillow_zip_final = zillow_zip_final[c(-1),]
zip_cols = as.data.frame(rownames(zillow_zip_final))
colnames(zip_cols) = "zipcode"
zillow_zip_final = cbind(zip_cols,zillow_zip_final)
rownames(zillow_zip_final) <- NULL


#-------------------------------listings - AirBnB data----------------------------------

#read listings data
#data table is 'listings'

listings = listings[,c(1,4,42,43,44,49,50,56,61,72,73,74,75)]
listings$city = trimws(tolower(listings$city),which = "both")

#choosing new york city data, 2 bedrooms
cities = c("bronx","brooklyn","new york","queens","manhattan","nyc","staten island","ny","new york city")
listings = listings[listings$city %in% cities,]
listings = listings[listings$bedrooms==2,]
listings = listings[complete.cases(listings),]
listings = listings[listings$zipcode!='',]
listings = listings[order(listings$zipcode),]

#to calculate the occupancy rate per zipcode
listings$occupancy_30 = round((1 - (listings$availability_30)/30),3)
listings$occupancy_60 = round((1 - (listings$availability_60)/60),3)
listings$occupancy_90 = round((1 - (listings$availability_90)/90),3)
listings$occupancy_365 = round((1 - (listings$availability_365)/365),3)
listings$aggr_occupancy_rate = round((0.4*(listings$occupancy_30) + 0.3*(listings$occupancy_60) + 0.25*(listings$occupancy_90) + 0.05*(listings$occupancy_365)),3)   

#daily rent price according to the occupancy rate
listings$price = as.character(listings$price)
listings$price = as.numeric(gsub("[\\$,]", "", listings$price))
listings$new_price = round((listings$price*listings$aggr_occupancy_rate),0)

#aggregating by zip code to calculate the median daily rental price 
listings$zipcode = as.factor(substr(as.character(listings$zipcode),1,5))
aggr_listings = aggregate(new_price ~ zipcode, data=listings, median)
aggr_listings$new_price = as.integer(aggr_listings$new_price)

#combining airbnb data and zillow data for common zip codes
aggr_listings = sqldf('SELECT * FROM aggr_listings a INNER JOIN zillow_zip_final z
                      ON a.zipcode==z.zipcode')

#calculating break even point in years
aggr_listings = aggr_listings[,c(1,2,56)]
colnames(aggr_listings)[3] = "cost"
aggr_listings$cost = as.integer(as.character(aggr_listings$cost))
aggr_listings$breakeven_pt = round((aggr_listings$cost/aggr_listings$new_price)/365,1)

#visualizing quantiles
summary(aggr_listings$breakeven_pt)
boxplot(aggr_listings$breakeven_pt, ylim = c(10,45), xlab = "Break even point", col = "#b98980")


#---------- Visualizations -------------------

#visulize profitability metrices
#Price
a1 = aggregate(price ~ zipcode, data=listings, median)
a2 = aggregate(occupancy_30 ~ zipcode, data=listings, median)
a2 = a2[,-1]
a3 = aggregate(occupancy_60 ~ zipcode, data=listings, median)
a3 = a3[,-1]
a4 = aggregate(occupancy_90 ~ zipcode, data=listings, median)
a4 = a4[,-1]
a5 = aggregate(occupancy_365 ~ zipcode, data=listings, median)
a5 = a5[,-1]
l = cbind(a1,a2,a3,a4,a5)
colnames(l)[2:6] = c("price","occupancy_30","occupancy_60","occupancy_90","occupancy_365")
l$occupancy_rate = 0.4*l$occupancy_30+0.3*l$occupancy_60+0.25*l$occupancy_90+0.05*l$occupancy_365
l$new_price = l$price * l$occupancy_rate

data("zipcode")
z <- zipcode
colnames(z)[colnames(z)=="zip"] <- "zipcode"

a <- merge(x = l, y = z, by = "zipcode", all.x = TRUE)

usa <- readOGR(dsn = "C:/Users/Akshaya/Desktop/Capital One/cb_2017_us_zcta510_500k", layer = "cb_2017_us_zcta510_500k")
poly <- subset(usa, usa$ZCTA5CE10 %in% a$zipcode)

poly$group <- substr(poly$ZCTA5CE10, 1, 5)
poly$ZCTA5CE10 <- droplevels(poly$ZCTA5CE10)
set.seed(111)

poly$value <- sample.int(n = l$new_price, size = nrow(poly), replace = FALSE)
poly.union <- unionSpatialPolygons(poly, poly$group)

map <- fortify(poly.union)

plot(poly)
plot(poly.union, add = T, border = "red", lwd = 1)

poly.df <- as(poly, "data.frame") %>%
  group_by(group) %>%
  summarise(value = value)

centers <- data.frame(gCentroid(spgeom = poly.union, byid = TRUE))
centers$zip <- rownames(centers)
ggplot() +
  geom_cartogram(data = map, aes(x = long, y = lat, map_id = id), map = map) +
  geom_cartogram(data = poly.df, aes(fill = value, map_id = group), map = map) +
  scale_fill_gradientn(colours = brewer.pal(10, "Spectral")) +
  coord_map() +
  labs(fill = "Property Cost")
theme_map()  




#least 50% of the break even points
list1 = aggr_listings[aggr_listings$breakeven_pt>=11.6 & aggr_listings$breakeven_pt<=25.95,]
list1 = list1[order(list1$breakeven_pt),]

zillow_zip_final = zillow_zip_final[,c(1,56:79)]

zillow_zip_final = sqldf('SELECT z.* FROM zillow_zip_final z INNER JOIN list1 l ON z.zipcode==l.zipcode')
colnames(zillow_zip_final)[2:25] = substring(colnames(zillow_zip_final)[2:25],2)

viz = t(zillow_zip_final)
colnames(viz)[1:11] = as.vector(viz[1,])
viz = viz[-1,]
viz = as.data.frame(viz)

v = data.frame()
v1 = data.frame()
for(i in 1:11){
  viz$date = seq(as.Date('2017-07-01'),as.Date('2019-06-01'),by="months")
  viz$zipcode = colnames(viz)[i]
  viz$cost = viz[,i]
  v = viz[,c(12:14)]
  rownames(v) <- NULL
  v1 = rbind(v1,v)
}

zipcodes = as.data.frame(unique(v1$zipcode))
zipcodes[2,]
my_color = data.frame(list(color = brewer.pal(11,"Paired")))
v1$cost = as.integer(as.character(v1$cost))

my_lines = list(
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[1,]],
       color = my_color[1,],name=zipcodes[1,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[2,]],
       color = my_color[2,],name=zipcodes[2,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[3,]],
       color = my_color[3,],name=zipcodes[3,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[4,]],
       color = my_color[4,],name=zipcodes[4,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[5,]],
       color = my_color[5,],name=zipcodes[5,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[6,]],
       color = my_color[6,],name=zipcodes[6,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[7,]],
       color = my_color[7,],name=zipcodes[7,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[8,]],
       color = my_color[8,],name=zipcodes[8,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[9,]],
       color = my_color[9,],name=zipcodes[9,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[10,]],
       color = my_color[10,],name=zipcodes[10,]),
  list(x=unique(v1$date), y=v1$cost[v1$zipcode==zipcodes[11,]],
       color = my_color[11,],name=zipcodes[11,])
)

p = plot_ly()
for(lines in my_lines) {
  p <- add_trace(p, y=lines[['y']], x=lines[['x']], type='scatter', mode = 'lines',
                 name = lines[['name']], line = list(color = lines[['color']]))
}

