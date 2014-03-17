#Code to build age length key and use it to estimate proportions at age for Pacific cod
#Author: Robyn Forrest PBS, November 2013
#Data provided by Kate Rutherford, November 2013

#IN THIS VERSION AGES FROM 2009 AND 2011 ARE COMBINED TO MAKE THE AGE LENGTH KEY - INCREASES SAMPLE SIZE AND NUMBER OF AGES
#2007 DATA NOT USED - TOO LONG AGO AND TOO FEW SAMPLES

#Uses package FSA by Derek Ogle http://www.rforge.net/FSA/Installation.html
#Based on analysis by: Isermann, D. A. and C. T. Knight. 2005. A computer program for age length keys incorporating age assignment to individual fish. North American Journal of Fisheries Management 25:1153{1160.
#Code written following instructions in vignette by D. Ogle: AgeLengthKey.pdf
graphics.off()
rm(list=ls(all=TRUE))
library(FSA)
library(FSAdata)
require(PBSmodelling) #for bubble plot

plotLengths<-1 #switch for plotting length-frequency

#A. Load data
#All queries performed by Kate Rutherford November 6 2013
xxq <- as.data.frame(read.csv("Pcod_lens_QCS_synoptic_survey.csv")) 
xxh <- as.data.frame(read.csv("Pcod_lens_HS_synoptic_assemblage_comb.csv")) 
xxqc <- as.data.frame(read.csv("Pcod_commercial_specimens_5AB.csv"))
xxhc <- as.data.frame(read.csv("Pcod_commercial_specimens_5CD.csv"))  
xx<-rbind(xxh, xxq) #combine the HS and QCS Synoptic Length Age datasets for making the age length key

#Set year or years to use to make ALK :: Set width of length bins (mm)
#In this version years are combined to make the ALK
alkyr <- c(2009,2011) # the ALK years - i.e. years with observed ages
yrs <-  c(2009,2011) #Years for which ages will be assigned - should be the same years as for the ALK
comyrs <-2009:2011
Nyr<-length(yrs)
survey_exclude <- "HECATE STRAIT MULTISPECIES TRAWL SURVEY" #c("Hecate Strait Pacific Cod Monitoring Survey","Queen Charlotte Sound Synoptic Survey","Joint Can/US Hake Acoustic Survey", "PHMA Rockfish Longline Survey - Outside North")#exclude datasets not being used
binwidth <- 5


 #############################################################
#Get a summary of the age observations for each area

#This is the data for the age length key (i.e., the AGE_SAMPLE)
pcod.agesQCS <- xxq[,c("YEAR","ACTIVITY_DESC","SPECIMEN_AGE")]
pcod.agesHS <- xxh[,c("YEAR","ACTIVITY_DESC","SPECIMEN_AGE")] 
pcod.agesHS <- Subset( pcod.agesHS, !is.element(pcod.agesHS$ACTIVITY_DESC,survey_exclude)) #remove all surveys except synoptic (for 5CD)
#write.table(pcod.data_unsorted, "test.csv", sep=",", row.names=F)

pcod.agesQCS <- pcod.agesQCS[,c("YEAR","SPECIMEN_AGE")]
pcod.agesHS <- pcod.agesHS[,c("YEAR","SPECIMEN_AGE")]

#SORT the data by age - this is very important for the application of the ageing error matrix
oq <- order(pcod.agesQCS$SPECIMEN_AGE)
oh <- order(pcod.agesHS$SPECIMEN_AGE)
PQ <- as.data.frame(cbind(pcod.agesQCS$YEAR[oq],pcod.agesQCS$Length_cm[oq],pcod.agesQCS$SPECIMEN_AGE[oq]))
PH <- as.data.frame(cbind(pcod.agesHS$YEAR[oh],pcod.agesHS$Length_cm[oh],pcod.agesHS$SPECIMEN_AGE[oh]))
colnames(PQ) <- c("YEAR","SPECIMEN_AGE")
colnames(PH) <- c("YEAR","SPECIMEN_AGE")

sum.ageQ<-t(table(PQ))
sum.ageH<-t(table(PH))
write.table(sum.ageQ,"Summary_Age_QCS.csv", sep=",", row.names=T, col.names=TRUE)
write.table(sum.ageH,"Summary_Age_HS.csv", sep=",", row.names=T, col.names=TRUE)
#############################################################
 #The AGE SAMPLE is used to construct the age-length key and contains all records with both age and length :: A SUBSET OF YEARS
 #The LENGTH SAMPLE contains lengths that will be used in conjunction with the age length key to get proportions at age. 
 #The LENGTH SAMPLE consists of all length data that are NOT associated with an age. Should include all years from the area and survey of interest.
 

#This is the data for the age length key (i.e., the AGE_SAMPLE) - doesn't need to be in a loop as it is always from the same data source (surveys)
 pcod.data_unsorted <- xx[,c("YEAR","ACTIVITY_DESC","Length_cm","SPECIMEN_AGE")] #remove columns that contain location info
 pcod.data_unsorted <- Subset( pcod.data_unsorted, !is.element(pcod.data_unsorted$ACTIVITY_DESC,survey_exclude)) #remove all surveys except synoptic (for 5CD)
 pcod.data_unsorted <- Subset( pcod.data_unsorted, is.element(pcod.data_unsorted$YEAR,yrs))

 P <- pcod.data_unsorted
#REDUCE DATA FRAME TO ONLY THREE COLUMNS: YEAR, LENGTH AND AGE
#ALSO SUBSET THE YEARS TO USE
#Columns used in this preliminary test: 2,5 - May later decide col 3 should be used instead of 5
#pcod.data <- pcod.data[,c(1,3,6)] # use when using FINAL_READING
P <- P[,c("YEAR","Length_cm","SPECIMEN_AGE")] 
cat("\n Checking  columns of final age sample: \n")
print(P[1:20,])

#SORT the data by age - this is very important for the application of the ageing error matrix
o <- order(P$SPECIMEN_AGE)
pcod.data <- as.data.frame(cbind(P$YEAR[o],P$Length_cm[o],P$SPECIMEN_AGE[o]))
colnames(pcod.data) <- c("YEAR","Length_cm","SPECIMEN_AGE")
  
#loop over datasets to use as the length sample for assigning age to length
#All analyses use a combined 5ABCD dataset for the ALK 
for(Ls in 1:4){
			#Area and gear combos for length sample. 
			#1.  5AB_survey
			#2.  5CD_survey
			#3.  5AB_commercial 
			#4.  5CD_commercial
			Lsamp <-Ls

			#Set Area for length sample 
			if(Lsamp==1) pcod.area<- "5AB_survey"
			if(Lsamp==2) pcod.area<- "5CD_survey"
			if(Lsamp==3) pcod.area<- "5AB_comm"
			if(Lsamp==4) pcod.area<- "5CD_comm"

			#############################################################################

			
			#This is the data for the length data to be assigned to ages using the ALK (i.e., the LENGTH_SAMPLE)
			if(Lsamp==1) LL<- xxq
			if(Lsamp==2) LL<- xxh
			if(Lsamp==3) LL<- xxqc
			if(Lsamp==4) LL<- xxhc
			#survey data
			if(Lsamp<=2){
				pcod.Ldata <- LL[,c("YEAR","ACTIVITY_DESC","Length_cm","SPECIMEN_AGE")] #remove columns that contain location info
				pcod.Ldata <- Subset( pcod.Ldata, !is.element(pcod.Ldata$ACTIVITY_DESC,survey_exclude)) #remove all surveys except synoptic (for 5CD)
				 pcod.Ldata <- Subset( pcod.Ldata,is.element(pcod.Ldata$YEAR,yrs))
			}else {
				#comm data
				pcod.Ldata <- LL[,c("YEAR","Length_cm","SPECIMEN_AGE")]
				pcod.Ldata <- Subset( pcod.Ldata,is.element(pcod.Ldata$YEAR,comyrs))
			}
			
			
			#Columns of pdata
			#1. YEAR
			#2. Length_cm
			#3. SPECIMEN_AGE

			#Make a matrix for summarising number of samples in each year
			SampSizeOut <- matrix(nrow=1, ncol=4)
			colnames(SampSizeOut) <- c("Num Age Samples", "Num Length Samples", "Max Age", "Max Length")

			 PL <- pcod.Ldata
			
			#############################################################################
			#B. Make age length key
			#B1. Get AGE SAMPLE
			pcod.age <- Subset(pcod.data,!is.na(SPECIMEN_AGE))
			
			#Get sample size
			Asamp <- nrow(pcod.age)
			
			# ARK (10-Dec-12) Dangerous to use "==" when comparing vectors. Sometimes gives
			# desired result for specfic patterns, but does not work in general.

			#pcod.age <- Subset(pcod.age, YEAR==alkyr)
			pcod.age <- Subset( pcod.age, is.element(pcod.age$YEAR,alkyr) )

			#cat("\n Checking the AGE SAMPLE: \n")
			#print(pcod.age[1:10,])
			#cat("\n Length of the AGE SAMPLE: \n")
			#print(Asamp)
			str(pcod.age)
          			
			#B2. Get LENGTH SAMPLE
			pcod.len <- Subset(PL,is.na(SPECIMEN_AGE))
			#cat("\n Checking the LENGTH SAMPLE: \n")
			#print(pcod.len[1:10,])

			Lsamp <- nrow(pcod.len)

			#cat("\n Length of the LENGTH SAMPLE: \n")
			#print(Lsamp)
			str(pcod.len)

			#B3. Get the smallest length in the AGE SAMPLE and the LENGTH SAMPLE
			#There should be no length samples in the LENGTH SAMPLE that are smaller than the smallest length in the AGE SAMPLE
			min.AGE.len <- min(pcod.age$Length_cm, na.rm=T)
			min.LEN.len <- min(pcod.len$Length_cm, na.rm=T)
			maxlen <- max(pcod.len$Length_cm, na.rm=T)

			#print(min.AGE.len)
			#print(min.LEN.len)

			if(min.AGE.len > min.LEN.len)  pcod.len <- Subset(pcod.len, Length_cm >= min.AGE.len)

			#set smallest length for the age length key
			psmall <- min(pcod.age$Length_cm, na.rm=T)

			#B4. Bin the lengths and assign ages to length bins
			#The next step is to create a variable in the AGE SAMPLE that identifies the length category to which each fish belongs. 
			#This variable is constructed, with default name LCat, and is appended to the data frame containing the AGE SAMPLE using the FSA function lencat()
			#Arguments:
				#1. The column of the age sample containing length data (~Length_cm)
				#2. The AGE SAMPLE
				#3. The first length bin (mm) - psmall, defined above  
				#4. The size of bins (mm) - binwidth defined above
			pcod.age1 <- lencat(~Length_cm, data=pcod.age, startcat=psmall, w=binwidth) #"Length_cm"
			#cat("\n Checking the LENGTH categories to be used in ALK: \n")

			#B5. Make the raw age length key - numbers of fish in each combined age and length category
			pcod.raw <- table(pcod.age1$LCat, pcod.age1$SPECIMEN_AGE)
			#cat("\n Checking the raw age length key: \n")
			#print(pcod.raw)

			#B6. Convert the raw age length key into proportions to get age-length key
			pcod.key <- prop.table(pcod.raw,margin=1)
			pcod.key <- round(pcod.key,3)
			#cat("\n Checking the proportional age length key: \n")
			#print(pcod.key)
			#cat("\n Sum rows of the proportional age length key: \n")
			#print(rowSums(pcod.key))

			#############################################################################
			#C. Assign ages to individuals using the age length key

			#C1. Age Assignment
			#The completely-random age assignment method is implemented with FSA function ageKey().
			#Arguments:
				#1. The age length key
				#2. The LENGTH SAMPLE
				#3. The data containing the length sample (
				#The column of the length sample containing the lengths
				#4. The name of the column to be appended to the length sample that will contain the assigned ages

			pcod.len1 <- ageKey(pcod.key,~Length_cm,pcod.len,type="CR")
			#cat("\n Checking the assigned ages from the age length key: \n")
			#print(pcod.len1[1:10,])

			#Note: RF has skipped the step that recombines the lengths with assigned ages with the lengths used to make ALK - not a good idea to mix these up imo

			#C2. Summarize the results
			pcod.summary <- Summarize(Length_cm~age,data=pcod.len1,digits=2)
			#cat("\n Summarise the results: \n")
			#print(pcod.summary)
			#cat("\n In this summary, note that the age and n columns represent an age-frequency and the age and \n Mean columns represent the mean length-at-age for ALL individuals in the entire sample\n")

			#D. Get proportions at age
			#D1. Get raw numbers at assigned age in each year
			num.age.raw <- table(pcod.len1$YEAR, pcod.len1$age)

			#D2. Get proportions at assigned age  - this was before the code was built into Yrs loop but still works so leave as legacy
			nyr<-length(num.age.raw[,1])
			tots<-rowSums(num.age.raw)
			pcod.prop.age <- num.age.raw #create new matrix with same dimensions as num.age.raw
			maxage<-max(pcod.len1$age)

			for(i in 1:nyr) pcod.prop.age[i,] <- num.age.raw[i,]/tots[i]
			Pyrs<-as.numeric(unique(row.names(pcod.prop.age)))
						                        
			#cat("\n Checking the raw numbers at assigned age and proportions at assigned age: \n")
			#print(num.age.raw)
			#print(pcod.prop.age)

			#Get sample size and maxage ready for writing out
			SampSizeOut[1,]<-c(Asamp,Lsamp,maxage,maxlen)

       			#############################################################################
			#E. Write out the results
			#E1. Make four panel plot
			#win.metafile(paste("Pcod_Area", pcod.area, ".wmf"))
			jpeg(paste("Pcod_Area", pcod.area, ".jpg", sep=""), quality=100)
			par(mfrow=c(2,2),mai=c(0.7,0.75,0.3,0.1), omi=c(0.1,0.1,0.25,0.05), cex.lab=1.2, cex.axis=1.1) 

			plot(Length_cm~jitter(age),data=pcod.len1,ylab="Fork Length (cm)",xlab="Assigned Age (jittered)", ylim=c(0,1.1*max(Length_cm)), las=1)
			lines(mean~fact2num(age),data=pcod.summary,col="blue",lwd=2)

			hist(age~1,data=pcod.len1,freq=F,right=FALSE,breaks="Sturges", xlim=c(1,11),xlab="Age (yrs)",ylab="Proportion",main="", las=1)

			histStack(pcod.len1$Length_cm, pcod.len1$age, xlab="Fork Length (cm)", las=1, main="")

			#if(nyr>3) {
			#	pcod.prop.age.syn <- pcod.prop.age[(nyr-3):length(yrs),] 
			#	}else pcod.prop.age.syn <- pcod.prop.age
			pcod.prop.age.syn <- pcod.prop.age
			plotBubbles(t(pcod.prop.age.syn),dnam=T,rpro=F,hide0=T,size=0.12,xlab="Survey Year",ylab="Assigned age proportion",
				    ylim=c(0,max(pcod.len1$age)),axes=T,clrs="blue", las=1, cex.axis=0.8, xlim=c(2009,2011)) #, xlim=c(2005,2011)
			mtext(paste("Area", pcod.area), side=3, line=-1., outer=T, cex=1.6)
			dev.off()

				#2. Synoptic survey
				#prop.age<-pcod.prop.age[12:15,]
				prop.age<-pcod.prop.age
				win.metafile(paste("Pcod_Synoptic_Survey_Bubbleplot_Area_", pcod.area, ".wmf", sep=""))
				plotBubbles(t(prop.age),dnam=T,rpro=F,hide0=T,size=0.175,xlab="Survey Year",ylab="Assigned age proportion",
					    ylim=c(1,max(pcod.len1$age)),clrs="blue", las=2,cex.axis=1, cex.lab=1.3, xlim=c(2009,2011)) #, xaxt="n"   , xlim=c(2007,2011)
						  abline(a=2-1996,b=1,lty=2,col=3)
					       abline(a=2-1997,b=1,lty=2,col=3)
						abline(a=2-1999,b=1,lty=2,col=3)
						abline(a=2-2001,b=1,lty=2,col=3)
						abline(a=2-2003,b=1,lty=2,col=3)
						abline(a=2-2004,b=1,lty=2,col=3)
						abline(a=2-2006,b=1,lty=2,col=3)
						abline(a=2-2008,b=1,lty=2,col=3)
						abline(a=2-2010,b=1,lty=2,col=3)
						abline(a=2-2012,b=1,lty=2,col=3)
				#axis(1,at = c(1,3,5,7,9,11,12,14,16,18,19,20,21,23,25,27), labels = yrs, tick = TRUE, outer = F, font = NA, lty = "solid",col = 1, col.ticks = 1, xlim=c(1984,2011)) #,  
				dev.off() #

			#Make annual stacked bar plots
			win.metafile(paste("Pcod_Stacked_Hist_Area", pcod.area, ".wmf", sep=""))
			par(mfrow=c(2,2), cex.lab=1.5, cex.axis=1.5,mai=c(0.3,0.4,0.3,0.1), omi=c(0.55,0.55,0.5,0.05)) 
			Lyrs<-unique(pcod.len1$YEAR)
			for(i in 1:length(Lyrs)){  
				pcod.len.yr <- Subset( pcod.len1, is.element(pcod.len1$YEAR,Lyrs[i]) )
				histStack(pcod.len.yr$Length_cm, pcod.len.yr$age,xlab="", ylab="",las=1, main=Lyrs[i], cex.axis=1)
			}
			mtext("Fork length (cm)", side=1, line=1., outer=T, cex=1.6)
			mtext("Frequency", side=2, line=1.2, outer=T, cex=1.6)
			dev.off()

			#make barplot of age proportions 
			win.metafile(paste("Pcod_Barplot_Area", pcod.area, ".wmf", sep=""))
			par(mfrow=c(2,2), cex.lab=1.5, cex.axis=1.5,mai=c(0.3,0.4,0.3,0.1), omi=c(0.55,0.55,0.5,0.05)) 
			
			for(i in 1:length(Pyrs)){
				#a.props <- pcod.prop.age[i,2:length(pcod.prop.age[i,])]
				a.props <- pcod.prop.age[i,]
				Yr <- Pyrs[i]
				barplot(a.props, names.arg=names(a.props), xlab="", ylab="", main=paste(Yr), las=1, col="gray", ylim=c(0,0.5), cex.names=1, cex.axis=1)
			}
			mtext(paste("Assigned proportions at age: Area", pcod.area), side=3, line=1., outer=T, cex=1.6)
			mtext("Age (years)", side=1, line=1., outer=T, cex=1.6)
			mtext("Assigned proportions at age", side=2, line=1.2, outer=T, cex=1.6)
			dev.off()

		##############################################	
		#make plots of L-F data. Use LL as dataset as it contains all length obs
		#NOTE THAT QUARTERS REPRESENT FISHING YEARS, THEREFORE THE 4TH QUARTER ASSOCIATED WITH A YEAR IS ACTUALLY THE FOLLOWING CALENDAR YEAR
		#E.G. FOR FISHING YEAR 1975: 1=APRIL-JUNE 1975, 2=JULY-SEPT 1975, 3=OCT-DEC 1975, 4=JAN-MAR 1976
		#NOTE that RF has assigned all survey observations Quarter code = 1
	 #only plot comm data after 1996
	 if(Ls>2) {
		if(plotLengths>0)
		{
		       	#counters for tracking how many graphs per plot and opening new plot when necessary (max is 16)
			lcount<-0 #counts number of plots in file
			graphcount<-1 #counts number of files
			
			LFyrs<-unique(LL$YEAR)
			
			#LFyrs<-LFyrs[which(LFyrs>1995)]
			sortLF<-order(LFyrs)
			LFyrs<-LFyrs[sortLF]
						
			for(i in 1:length(LFyrs)){  	   
				 if(LFyrs[i]>1995){
					lcount<-lcount+1
					if(lcount==1){
						#open new graphing file for multi-panel plot
						win.metafile(paste("Pcod_LenFreq_Area", pcod.area, graphcount, ".wmf", sep=""))
						par(mfcol=c(4,5), cex.lab=1.5, cex.axis=1.5,mai=c(0.3,0.4,0.3,0.1), omi=c(0.55,0.55,0.5,0.05)) 
					}
					#get lengths for this year
					pcod.len.yr <- Subset( LL, is.element(LL$YEAR,LFyrs[i]) )
					nL<-length(pcod.len.yr$Length_cm)

					#Get lengths for first quarter	(i.e., April-June)
					pcod.len.qtr <-	 Subset( pcod.len.yr, is.element(pcod.len.yr$Quarter_code,1))
					pcod.len.qtr <- pcod.len.qtr$Length_cm

					openplot=0
					#Get density for first quarter
					if(length(pcod.len.qtr)>1){
						openplot=1
						dqtr <- density(pcod.len.qtr, na.rm=T)
						plot(dqtr$x, dqtr$y, type="l", lty=1, xlab="", ylab="",las=1, main=paste(LFyrs[i], " n=",nL,sep=""), cex.axis=1, col=1, ylim=c(0,2.*max(dqtr$y)), xlim=c(0,100), lwd=2)
					}#end if	

					#do other quarters for comm samples
					for(iq in 2:4){
						pcod.len.qtr <-	 Subset( pcod.len.yr, is.element(pcod.len.yr$Quarter_code,iq))
						pcod.len.qtr <- pcod.len.qtr$Length_cm

						 #Get density
						if(length(pcod.len.qtr)>1){
							dqtr <- density(pcod.len.qtr, na.rm=T)
							if(openplot<1) plot(dqtr$x, dqtr$y, type="l", lty=1, xlab="", ylab="",las=1, main=paste(LFyrs[i], " n=",nL,sep=""), cex.axis=1, col=1, ylim=c(0,2.*max(dqtr$y)), xlim=c(0,100), lwd=2)
							else lines(dqtr$x, dqtr$y, lty=iq, col=iq, lwd=2)
							openplot<-1
						}
						if(openplot==1) {
							 if(lcount==1) legend("topright",c("Q1: Apr-Jun","Q2: Jul-Sep", "Q3: Oct-Dec","Q4:Jan-Mar"), lty=1:4, lwd=2,col=1:4, bty="n")
						 }
					  }#end iq

						if(i==length(LFyrs)||lcount==20){
							#close graphing window and reset counters
							mtext("Fork length (cm)", side=1, line=1., outer=T, cex=1.6)
							mtext("Frequency", side=2, line=1.2, outer=T, cex=1.6)
							mtext(paste(pcod.area), side=3, line=1.2, outer=T, cex=1.6)
							dev.off()
							lcount<-0
							graphcount<-graphcount+1
						}
				   } #end if>1995
				   }	#end for
			}#end L-F plots
		}#end if Ls>2
			
		 if(Ls<=2) {
	       		if(plotLengths>0)
	       		{
	       		       	#counters for tracking how many graphs per plot and opening new plot when necessary (max is 16)
	       			lcount<-0 #counts number of plots in file
	       			graphcount<-1 #counts number of files
	       			
	       			LFyrs<-unique(LL$YEAR)
	       			
	       			#LFyrs<-LFyrs[which(LFyrs>1995)]
	       			sortLF<-order(LFyrs)
	       			LFyrs<-LFyrs[sortLF]
	       						
	       			for(i in 1:length(LFyrs)){  	   
	       				
	       					lcount<-lcount+1
	       					if(lcount==1){
	       						#open new graphing file for multi-panel plot
	       						win.metafile(paste("Pcod_LenFreq_Area", pcod.area, graphcount, ".wmf", sep=""))
	       						par(mfcol=c(4,4), cex.lab=1.5, cex.axis=1.5,mai=c(0.3,0.4,0.3,0.1), omi=c(0.55,0.55,0.5,0.05)) 
	       					}
	       					#get lengths for this year
	       					pcod.len.yr <- Subset( LL, is.element(LL$YEAR,LFyrs[i]) )
	       					nL<-length(pcod.len.yr$Length_cm)
	       
	       					#Get lengths for first quarter	(i.e., April-June)
	       					pcod.len.qtr <-	 Subset( pcod.len.yr, is.element(pcod.len.yr$Quarter_code,1))
	       					pcod.len.qtr <- pcod.len.qtr$Length_cm
	       
	       					openplot=0
	       					#Get density for first quarter
	       					if(length(pcod.len.qtr)>1){
	       						openplot=1
	       						dqtr <- density(pcod.len.qtr, na.rm=T)
	       						plot(dqtr$x, dqtr$y, type="l", lty=1, xlab="", ylab="",las=1, main=paste(LFyrs[i], " n=",nL,sep=""), cex.axis=1, col=1, ylim=c(0,2.*max(dqtr$y)), xlim=c(0,100), lwd=2)
	       					}#end if	
	       
	       					#do other quarters for comm samples
	       					for(iq in 2:4){
	       						pcod.len.qtr <-	 Subset( pcod.len.yr, is.element(pcod.len.yr$Quarter_code,iq))
	       						pcod.len.qtr <- pcod.len.qtr$Length_cm
	       
	       						 #Get density
	       						if(length(pcod.len.qtr)>1){
	       							dqtr <- density(pcod.len.qtr, na.rm=T)
	       							if(openplot<1) plot(dqtr$x, dqtr$y, type="l", lty=1, xlab="", ylab="",las=1, main=paste(LFyrs[i], " n=",nL,sep=""), cex.axis=1, col=1, ylim=c(0,2.*max(dqtr$y)), xlim=c(0,100), lwd=2)
	       							else lines(dqtr$x, dqtr$y, lty=iq, col=iq, lwd=2)
	       							openplot<-1
	       						}
	       						if(openplot==1) {
	       							 if(lcount==1) legend("topright",c("Q1: Apr-Jun","Q2: Jul-Sep", "Q3: Oct-Dec","Q4:Jan-Mar"), lty=1:4, lwd=2,col=1:4, bty="n")
	       						 }
	       					  }#end iq
	       
	       						if(i==length(LFyrs)||lcount==16){
	       							#close graphing window and reset counters
	       							mtext("Fork length (cm)", side=1, line=1., outer=T, cex=1.6)
	       							mtext("Frequency", side=2, line=1.2, outer=T, cex=1.6)
	       							mtext(paste(pcod.area), side=3, line=1.2, outer=T, cex=1.6)
	       							dev.off()
	       							lcount<-0
	       							graphcount<-graphcount+1
	       						}
	       				  
	       				   }	#end for
	       			}#end L-F plots
		}#end if Ls<2
		##############################################	
			#proportions at age
			#pcod.prop.age<-cbind(Pyrs,pcod.prop.age)
			#colnames(pcod.prop.age) <- c("Year",0:maxage)

			#E2. Write out tables
			write.table(pcod.key, file=paste("Pcod_ALK_Area_", pcod.area,".csv"), sep=",", row.names=T, col.names=T)
			write.table(pcod.len1, file=paste("Pcod_AssignedAges_Area_", pcod.area,".csv"), sep=",", row.names=F, col.names=T)
			write.table(pcod.summary, file=paste("Pcod_Summary_Area_", pcod.area, ".csv"), sep=",", row.names=F, col.names=T)
			write.table(pcod.prop.age, file=paste("Pcod_Proportions_at_Age_", pcod.area, ".csv"), sep=",", row.names=F, col.names=T)

			###
			#IGNORE EVERYTHING BELOW. THE ONLY "CONVERSION TO TRUE AGE" IS READING THE DIAGONAL OF THE AGEING ERROR A
			#See Richards et al 1992, p 1806.
			#We are dealing with their Question 4. "Thus the estimate can be obtained by inspecting row c_k of the matrix Q" where c_k is the observed age of fish k (note we have observed ages in columns)
			#See Appendix B of P cod assessment document (2013). It is misleading to try to correct the observations directly because of the assumptions
			#inherent in developing the ageing error matrix (P(obs|true)) - circular. Richards et al assumed the mode was the true age, hence the above statement

			#################################################################################################
			#################################################################################################
			#RF TO MOVE THIS CODE TO AGEING ERROR SECTION

			#F. Use ageing error matrix from Thorson/Punt software to convert 'observed' ages to 'true' ages 
			#Read ageing misclassification matrix and plot for fig
			mis <- as.matrix(read.csv("Pcod_ageing_error.csv", header=F))
			colnames(mis)<-0:10
			rownames(mis)<-0:10
			#print(round(mis,4))
			numcat <- length(mis[,1])

			win.metafile(paste("Pcod_Ageing_Misclassification_Area", pcod.area, ".wmf", sep=""))
			par(mfrow=c(3,4),mai=c(0.3,0.35,0.3,0.1), omi=c(0.3,0.3,0.5,0.1))
			for(i in 1:numcat){
				age.probs<-mis[i,]
				barplot(age.probs, names.arg=1:numcat, main=paste("True Age =",i), ylim=c(0,1),las=1,cex=1.4, cex.axis=1.1, cex.names=1.)

			}
			mtext("Ageing error: P(Observed age|True age, parameters)", side=3, line=1,outer=T, cex=1.4)
			mtext("Observed age (Primary Reader)", side=1, line=1,outer=T, cex=1.3)
			mtext("Probability of observed age (Primary Reader)", side=2, line=0.5,outer=T, cex=1.3)
			dev.off()
	
	write.table(SampSizeOut, file=paste("Pcod_SampSizes_Area", pcod.area,".csv"), sep=",", row.names=F, col.names=T)	
}#end length sample loop