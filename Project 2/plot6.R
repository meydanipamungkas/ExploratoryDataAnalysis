#packages library
library(readRDS)
library(ggplot2)
library(scales)
library(data.table)

#input data
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

##Q6
#answer Q6.1
scc.vehicles<-SCC[grep("Mobile.*Vehicles",SCC$EI.Sector), ] # Pattern match mobile vehicles in SCC description
scc.vehicles.list<-unique(scc.vehicles$SCC) # Create motor vehicle lookup list by SCC
nei.vehicles<-subset(NEI,SCC %in% scc.vehicles.list) # Filter for motor vehicle sources
nei.vehicles<-nei.vehicles%>%filter(fips=="24510"| fips=="06037") # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips=="24510"]<-"Baltimore"
nei.vehicles$fips[nei.vehicles$fips=="06037"]<-"Los Angeles"
nei.vehicles<-merge(x=nei.vehicles,y=scc.vehicles[,c("SCC","SCC.Level.Two")],by="SCC") # Join in descriptive data on SCC codes
nei.vehicles<-nei.vehicles%>%group_by(fips,year,SCC.Level.Two)%>%summarize(Annual.Total=sum(Emissions))
nei.vehicles.total<-nei.vehicles%>%group_by(fips,year)%>%summarize(Annual.Total=sum(Annual.Total))%>%mutate(SCC.Level.Two="Total")
nei.vehicles<-bind_rows(nei.vehicles,nei.vehicles.total)
nei.vehicles$SCC.Level.Two<-factor(nei.vehicles$SCC.Level.Two,levels=c("Total","Highway Vehicles-Diesel","Highway Vehicles-Gasoline"))
ggplot2(nei.vehicles,aes(x=factor(year),y=Annual.Total,fill=SCC.Level.Two)) +
  geom_bar(stat="identity") +
  facet_grid(fips ~ SCC.Level.Two) + 
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources",paste("in Baltimore City,MD and Los Angeles County,CA")))) +
  theme(plot.title=element_text(hjust=0.5)) + # Center the plot title
  theme(plot.margin=unit(c(1,1,1,1),"cm")) + # Adjust plot margins
  scale_fill_brewer(palette="Set1") +
  guides(fill=FALSE)

#answer Q6.2
scc.vehicles<-SCC[grep("Mobile.*Vehicles",SCC$EI.Sector), ] # Pattern match mobile vehicles in SCC description
scc.vehicles.list<-unique(scc.vehicles$SCC) # Create motor vehicle lookup list by SCC
nei.vehicles<-subset(NEI,SCC %in% scc.vehicles.list) # Filter for motor vehicle sources
nei.vehicles<-nei.vehicles%>%filter(fips=="24510"| fips=="06037") # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips=="24510"]<-"Baltimore"
nei.vehicles$fips[nei.vehicles$fips=="06037"]<-"Los Angeles"
nei.vehicles<-merge(x=nei.vehicles,y=scc.vehicles[,c("SCC","SCC.Level.Two")],by="SCC") # Join in descriptive data on SCC codes
nei.vehicles<-nei.vehicles%>%group_by(fips,year,SCC.Level.Two)%>%summarize(Annual.Total=sum(Emissions))
nei.vehicles.total<-nei.vehicles%>%group_by(fips,year)%>%summarize(Annual.Total=sum(Annual.Total))%>%mutate(SCC.Level.Two="Total")
nei.vehicles<-bind_rows(nei.vehicles,nei.vehicles.total)
nei.vehicles$SCC.Level.Two<-factor(nei.vehicles$SCC.Level.Two,levels=c("Total","Highway Vehicles-Diesel","Highway Vehicles-Gasoline"))
ggplot2(nei.vehicles,aes(x=factor(year),y=Annual.Total,fill=SCC.Level.Two)) +
  geom_bar(stat="identity") +
  facet_grid(fips ~ SCC.Level.Two,scales="free") + # Setup facets and allow scales to adjust to data in each location
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources",paste("in Baltimore City,MD and Los Angeles County,CA")))) +
  theme(plot.title=element_text(hjust=0.5)) + # Center the plot title
  theme(plot.margin=unit(c(1,1,1,1),"cm")) + # Adjust plot margins
  scale_fill_brewer(palette="Set1") +
  guides(fill=FALSE)

#answer Q6.3
nei.vehicles.DT<-data.table(nei.vehicles)
yoyFunc<-function(x) {x/shift(x)}
yoy.cols<-c("Annual.Total")
nei.vehicles.DT<-nei.vehicles.DT[,paste0("Percent.Change.",yoy.cols) := lapply(.SD,yoyFunc),by="fips,SCC.Level.Two",.SDcols=yoy.cols]
nei.vehicles.DT<-mutate(nei.vehicles.DT,Percent.Change.Annual.Total=Percent.Change.Annual.Total-1)
ggplot2(nei.vehicles.DT,aes(x=factor(year),y=Percent.Change.Annual.Total,fill=SCC.Level.Two)) +
  geom_bar(stat="identity") +
  facet_grid(fips ~ SCC.Level.Two) +
  xlab("Year") +
  ylab(expression("% Change From Prior Measurement")) + 
  ggtitle(expression(atop("Percentage Change in Total Tons of PM"[2.5]*" Emissions from Motor Vehicle",paste("Sources in Baltimore City,MD and Los Angeles County,CA")))) +
  theme(plot.title=element_text(hjust=0.5)) + # Center the plot title
  theme(plot.margin=unit(c(1,1,1,1),"cm")) + # Adjust plot margins
  scale_fill_brewer(palette="Set1") +
  guides(fill=FALSE)

##ending
CAGR.df<-nei.vehicles.DT%>%
  group_by(fips,SCC.Level.Two)%>%
  summarize(N.Years=max(year)-min(year),
            Beginning.Qty=Annual.Total[which(year==min(year))],
            Ending.Qty=Annual.Total[which(year==max(year))],
            CAGR=((Ending.Qty-Beginning.Qty)/N.Years)/Beginning.Qty)
CAGR.df
summary(nei.vehicles.DT$Percent.Change.Annual.Total[nei.vehicles.DT$fips=="Baltimore"])
summary(nei.vehicles.DT$Percent.Change.Annual.Total[nei.vehicles.DT$fips=="Los Angeles"])
