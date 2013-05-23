#####################################################################
## Hyo Byun
## Mayberg Lab - Emory University
## 11-2012
#####################################################################

###################################################
# Velocity Histogram
velocityHistogram<-function(save, velocity){
	if (save)
		png(paste(dir, "output/",subj,"/",timepoint, "/velhisto.png",sep=""),width=1000,height=800)
	else 
		X11()
	
	hist(velocity, 
			breaks=15,  
			main="Histogram: Velocity in Miles/Hour",
			xlab="mph")
	
	if(save)
		dev.off()
}

###################################################
# Acceleration Histogram
accelerationHistogram<-function(save, accl){
	if (save)
		png(paste(dir, "output/",subj,"/",timepoint, "/acclhisto.png",sep=""),width=1000,height=800)
	else 
		X11()
	
	hist(accl, 
			breaks=15,
			xlab="Acceleration in Miles per Second^2",
			main="Acceleration Frequency")
	
	if(save)
		dev.off()
}


###################################################
# Google GPS map Convex Hull
googleGPSmapHull<-function(save, lats,lons,velocity,hullNodes){
	if (save)
		png(paste(dir, "output/",subj,"/",timepoint, "/mapHull.png",sep=""),width=640,height=640)
	else 
		X11()
	
	print(lats)
	cols<-rgb(velocity/(max(velocity)+1),0,0,.1)
	plot.new()
	bb <- qbbox(lat=range(lats), lon=range(lons))
	m <- c(mean(lats), mean(lons))
	zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
	Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="roadmap", NEWMAP=TRUE)
	
	theplot <- PlotOnStaticMap(lats[hullNodes],lons[hullNodes],
			cex=3, pch=20, 
			col="black", 
			MyMap=Map,FUN=lines, NEWMAP=FALSE)
	
	if(save)
		dev.off()
}

###################################################
# Google GPS map
googleGPSmap<-function(save, lats,lons,velocity){
	if (save)
		png(paste(dir, "output/",subj,"/",timepoint, "/map.png",sep=""),width=640,height=640)
	else 
		X11()
	
	
	cols<-rgb(abs(velocity/(max(velocity)+1)),0,0,.1)
	#plot.new()
	bb <- qbbox(lat=range(lats), lon=range(lons))
	m <- c(mean(lats), mean(lons))
	zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
	Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="roadmap", NEWMAP=TRUE)
	
	theplot <- PlotOnStaticMap(lats,lons,
			cex=3, pch=20, 
			col=cols, 
			MyMap=Map, NEWMAP=FALSE)
	
	
	if(save)
		dev.off()
}



###################################################
# Google GPS map
googleGPSmapraw<-function(save, lats,lons,velocity){
  if (save)
    png(paste(dir, "output/",subj,"/",timepoint, "/mapRAW.png",sep=""),width=640,height=640)
  else 
    X11()
  
  
  #plot.new()
  bb <- qbbox(lat=range(lats), lon=range(lons))
  m <- c(mean(lats), mean(lons))
  zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
  Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="roadmap", NEWMAP=TRUE)
  
  theplot <- PlotOnStaticMap(lats,lons,
                             cex=1,
                             pch=20, 
                             col="black", 
                             MyMap=Map, NEWMAP=FALSE)
  
  
  if(save)
    dev.off()
}



###################################################
# dist/steps plot
velocStepsPlot<-function(save, time,velocity,steps, dateStart_f,dateEnd_f){
	if (save)
		png(paste(dir, "output/",subj,"/",timepoint, "/points.png",sep=""),width=1000,height=800)
	else 
		X11()
	
	
	
	par(mar=c(5,4,4,5) +1)
	plot.new()
	plot(time,velocity,
			pch=20,
			col="#71BA51", 
			main="Steps and Instantaneous Velocity",
			cex.main=2,
			xlab="Days",
			xlim=c( as.numeric(as.POSIXlt(dateStart_f)), as.numeric(as.POSIXlt(dateEnd_f))),
			xaxt="n",
			cex=2,
			axes=FALSE,
			ylab="Pedometer Steps",
			cex.lab=1.6) #Stringn to date to number (epoch time)
	
	axis(4, 
			col="#4E8736",
			lwd=3,
			ylim=c(0,max(velocity)),
			col.axis="#4E8736",
			cex.axis=1.4)
	
	par(new=TRUE)
	
	plot(time,
			steps,
			pch=20,
			xaxt="n",
			col="#3D8EB9",
			cex=2,
			axes=FALSE,
			ylab="",
			xlab="")
	
	axis(2, 
			col="#2A617E",
			lwd=3,
			col.axis="#2A617E",
			cex.axis=1.4)
	
	axis.POSIXct(1, at=seq(time[1],time[length(time)], by="day"), 
			format="%D",
			xlim=c(time[1],time[length(merged.df)]),
			cex.axis=1.4,
			lwd=3)
	
	axis.POSIXct(1, at=seq(time[1],time[length(time)], by="6 hours"), 
			labels = FALSE,
			lty=3,
			cex.axis=1.4)
	
	axis.POSIXct(1, at=seq(time[1],time[length(time)], by="1 hours"), 
			labels = FALSE,
			lwd=3,
			tck=0)
#ma <- function(x,n=10){filter(x,rep(1/n,n), sides=2)}
	
	mtext("Instantaneous Velocity (mph)",  
			EAST <-4, 
			at=max(velocity)/2, 
			line=4.5, 
			col="black",
			cex=1.6)
	legend("topright",
			pch=c(15,15),
			col=c("#3D8EB9","#71BA51"),c("Steps (Pedometer)","Instantaneous Velocity (GPS)"),
			cex=1.4,
			pt.cex=1.6,
			bty="n")
	
	
	
	
	if(save)
		dev.off()
}



###################################################
# Group Bar Plot
stepsMetersBarPlot<-function(save, steps,meters,time1, time2){
	if (save)
		png(paste(dir, "output/",subj,"/",timepoint, "/stepsDistancePlot.png",sep=""),width=1000,height=800)
	else 
		X11()
	
	
	
	temp<-data.frame("Steps"=steps,"Velocity"=meters)
	
	par(mar=c(5,4,4,5) +1)   
	#aveVelocityPerDay<- getAverageVeloctyPerDay(dateStart,dateEnd,gpspoints.df)
	##Make other thing transparent
	barplot(t(temp), 
			main="Steps and Distance Traveled by Day",
			cex.main=2,xlab="Days",
			col=c("#3D8EB9","#71BA5100"),
			border=NA,
			beside=TRUE,
			ylim=c(0,max(steps)),
			axes=FALSE,ylab="Pedometer Steps",cex.lab=1.6)
	axis(2,
			col="#2A617E",
			lwd=3,
			col.axis="#2A617E",
			cex.axis=1.4)
	par(new=TRUE)
	barplot(t(temp),
			col=c("#3D8EB900","#71BA51"),
			border=NA,
			beside=TRUE,
			ylim=c(0,max(meters)),
			axes=FALSE,side=4)
	axis(4, 
			col="#4E8736",
			lwd=3,
			ylim=c(0,max(meters)),
			col.axis="#4E8736",
			cex.axis=1.4)
	mtext("GPS Distance (Meters)", 
			EAST <-4, at=max(meters)/2, 
			line=4.5, 
			col="black",
			cex=1.6)
	
	##Make Axis
	tics<-NA
	label<-c("")
	j<-1
	for(i in seq(time1,time2, by="day")) {
		tics[j]<-2+j*3-3
		label[j]<-  format(as.POSIXlt(i, origin="1970-01-01"),format="%D")
	#  print( format(as.POSIXlt(i, origin="1970-01-01"),format="%D"))
		j<-j+1
	}
	par(new=TRUE)
	axis(1, 
			at=c(0,length(tics)*3+1),
			labels=c("",""),
			lwd.ticks=0,
			lwd=3)
	axis(1,
			at=tics,
			labels=label, 
			col="#000000",
			tck=0, 
			lwd=0,
			cex.axis=1.4)
	legend("topright",
			pch=c(15,15),
			col=c("#3D8EB9","#71BA51"),
			c("Steps (Pedometer)","Distance Traveled (GPS)"),
			cex=1.4,
			pt.cex=1.6,
			bty="n")
	
	
	
	
	if(save)
		dev.off()
}




###################################################
# Google GPS map
convexHullGraph<-function(save, lats,lons,vertecies){
    if (save)
        png(paste(dir, "output/",subj,"/",timepoint, "/convexHull.png",sep=""),width=1000,height=800)
    else 
        X11()
    
    ##CHULL RED
    plot(lons[vertecies],lats[vertecies],
         main="Minimum Convex Polygon (inaccurate projection)", 
         cex=3, 
         pch=20, 
         col="#FF0000BB",
         na.rm=TRUE)
    par(new=TRUE)
    
    polygon(lons[vertecies],lats[vertecies],
            col="#FF000022",
            na.rm=TRUE,
            axes=FALSE,
            xlab="",
            ylab="",
            border="#FF000055",
            lwd=2)
    par(new=TRUE)
    
    plot(lons,lats,
         cex=.5,
         axes=FALSE,
         xlab="",
         ylab="",
         pch=20)
    

    if(save)
        dev.off()
}

