#**************************************************************************************************************************************************
# Author : Robyn Forrest (RF), Pacific Biological Station, Nanaimo
# Development Date  : March, 2014
# Description: #Code to generate commercial mean weight data for Pacific Cod
# Follows steps listed in Appendix D:
#Forrest, R.E., Rutherford, K.L, Lacko, L., Kronlund, A.R., Starr, P.J., McClelland, E.K. 2013. 
#	Assessment of Pacific Cod (Gadus macrocephalus) for Hecate Strait (5CD) and Queen Charlotte Sound (5AB) in 2013. 
#	DFO Can. Sci. Advis. Sec. Res. Doc. 2013/nnn
#PSEUDO-CODE
# Step 1: Read in raw length data and quarterly commercial catch data for Hecate Strait (HS) and Queen Charlotte Sound (QCS)
# Step 2: Extract areas, years and sequential quarters from the data
# Step 3: Set growth parameters
# Step 4: Convert individual length observations to weight observations
# Step 5: 
# Step 6: 
# Step 7: 
# Step 8: Calculate Mean Weight Wf for the fishing year f using two alternative approaches
#		7a. Average the quarterly Mean Weights SampWeights, weighted by the quarterly Commercial Catch Cs
#		7b. Average the quarterly Mean Weights SampWeights by dividing by 4
# Step 9:  Plot results
#
#	**Indented print statements can be uncommented to show how code is working**
#
#************************************************************************************************************************************************
#Turn off any graphics devices and remove all objects in R space
graphics.off()
rm(list=ls(all=TRUE))

#Set folders for data and figures
fdData <- "Data/"
fdFigs <- "Outputs/"

#Define Approach for converting Length to Weight  (Approach 2 is a bit quicker so use 2 while testing code)
Approach <- 2

#Define Areas for plotting legends
Areas <- c("Hecate Strait", "Queen Charlotte Sound")
#Statistical areas for extracting catch data from Catch table
StatAreas <- c("5CD", "5AB")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
# Step 1: Read in raw length data and quarterly commercial catch data for Hecate Strait (HS) and Queen Charlotte Sound (QCS)
#Length data
xxq <- read.csv(paste(fdData,"Pcod_commercial_specimens_5AB.csv", sep=""), header=T)
xxh <- read.csv(paste(fdData,"Pcod_commercial_specimens_5CD.csv", sep=""), header=T)
#Quarterly catch data
xxc   <- read.csv(paste(fdData,"Pcod_Catch_all_by_major_area_FY_Q.csv", sep=""), header=T)
pjs <- read.csv(paste(fdData,"Pcod_PJS_MeanWeights.csv", sep=""), header=T)

#Combine the length data into a single table to allow looping (requires that the csv files have exactly the same fields in the same order)
xxl <- rbind(xxq, xxh)

#Test the files have been read in correctly 
	#print(xxq[1:3,1:7])
	#print(xxh[1:3,1:7])
	#print(xxc[1:3,])

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
# Step 2: Extract years and sequential quarters from the data

#Remove all records older than 1956
xxl <- xxl[which(xxl$FYear>=1956 & xxl$FYear<2013) ,]
xxc <- xxc[which(xxc$FYear>=1956 & xxc$FYear<2013) ,]
nrecords <- nrow(xxl)
nrecordsc <- nrow(xxc)

#Get Years :: RF checked, all databases have the same sequence of years so can use the same vector of years for all of them
yrs <- unique(xxl$FYear)
yrsc <- unique(xxc$FYear)
o <- order(yrs) #gets the index numbers
yrs <- yrs[o]
o <- order(yrsc) #gets the index numbers
yrsc <- yrsc[o]
	#cat("Years in Length dataset\n")
	#print(yrs)
	#cat("Years in Catch dataset\n")
	#print(yrsc)
nyrs<-length(yrs)

###############################################################################
#Get Sequential Quarters (i.e., every quarter in dataset has unique identifier) from the LENGTH and CATCH data
#Re-label quarters as 1-4 then give each quarter in each year a sequential number
qtrs_txt <- xxl$Quarter
qtrs_txt <- as.character(qtrs_txt) #This is a vector of the field xxl$Quarter, re-typed to character so it can be searched on below 
qtrs_txtc <- xxc$Quarter
qtrs_txtc <- as.character(qtrs_txtc) #This is a vector of the field xxl$Quarter, re-typed to character so it can be searched on below 

#get the row numbers of each named quarter and put the four vectors of row numbers into a list
qrow_list<-list()
qrow_list$q1 <- which(is.element(qtrs_txt,"Apr-Jun"))
qrow_list$q2 <- which(is.element(qtrs_txt,"Jul-Sep"))
qrow_list$q3 <- which(is.element(qtrs_txt,"Oct-Dec"))
qrow_list$q4 <- which(is.element(qtrs_txt,"Jan-Mar"))

qrow_listc<-list()
qrow_listc$q1 <- which(is.element(qtrs_txtc,"Apr-Jun"))
qrow_listc$q2 <- which(is.element(qtrs_txtc,"Jul-Sep"))
qrow_listc$q3 <- which(is.element(qtrs_txtc,"Oct-Dec"))
qrow_listc$q4 <- which(is.element(qtrs_txtc,"Jan-Mar"))

#Create a new vector for numbered quarters (i.e., 4,1:3) and fill the appropriate rows with the number that matches the quarter name
NumQuarter <-vector(length=nrecords)
NumQuarterc <-vector(length=nrecordsc)
for(i in 1:4) NumQuarter[qrow_list[[i]]] <- i
for(i in 1:4) NumQuarterc[qrow_listc[[i]]] <- i	

#bind the new vector of quarter numbers to the length or catch database
xxl <- cbind(xxl,NumQuarter)
xxc <- cbind(xxc,NumQuarterc)
	#print(xxl[1:500,c("Quarter","NumQuarter")])     #check that the right quarter numbers have been matched with the right quarter name

#Assign each quarter a unique identifier based on year and quarter number
QuarterID<- xxl$FYear*10 + xxl$NumQuarter
QuarterIDc<- xxc$FYear*10 + xxc$NumQuarterc
xxl <- cbind(xxl,QuarterID)
xxc <- cbind(xxc,QuarterIDc)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
# Step 3: Set growth parameters (Westrheim 1996)
a1 <- 7.377e-06	      #lwa parameter for first three quarters
b1 <- 3.0963		      #lwb parameter for first three quarters
a4 <- 4.988e-06	      #lwa parameter for fourth quarter (January - March)
b4 <- 3.2117		      #lwb parameter for fourth quarter (January - March)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#NOW LOOP OVER THE TWO AREAS, 5CD then 5AB
for(i in 1:2){
	#Get Paul's mean weights for this area (will not be identical because length query was different
	pjs_mean_weight <- pjs[,i]
		#print(pjs_mean_weight)
	
	#Extract length data from area i
	#"which" command returns row numbers of table that satisfy the criteria
	xx <- xxl[which(is.element(xxl$Area,StatAreas[i])),]
	nrecordsa <- nrow(xx)
		#print(xx[1:5,])
		#print(unique(xx$Area))
	
	#Extract quarterly catch data from area i
	cc <- xxc[which(is.element(xxc$Area,StatAreas[i])),]
		#print(cc[1:5,])
		#print(unique(cc$Area))
		
	#**Assign each quarter a sequential number**
	#First get the row numbers of the length table ordered by FYear then Quarter then sample ID
	order_lrows <- order(xx$FYear, xx$NumQuarter, xx$SAMPLE_ID)  
	order_crows <- order(cc$FYear, cc$NumQuarter)  
	
	#Then re-order the length and catch tables in order of year and quarter
	xx <- xx[order_lrows,]
	cc <- cc[order_crows,]
		#print(cc)
	TotalCatch<-vector(length=nrow(cc))
	TotalCatch <-cc$Landed_kg+cc$Released_kg
	cc <- cbind(cc,TotalCatch)

	#Get unique identifiers for quarters then sequential quarter numbers (i.e., total number of quarters in this area) for LENGTH data
	Seq_Quarter<-vector(length=nrecordsa)
	numq<- unique(xx$QuarterID)
		#print(numq)
	
	#Get a sequential quarter number for each unique quarter
	for(j in 1:nrecordsa) {
		qid <- xx$QuarterID[j]
		Seq_Quarter[j] <- which(numq==qid)
	}
       
       #Add the sequential quarter number to the xx table
       xx<-cbind(xx,Seq_Quarter)
  
	#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
	# Step 4: Convert individual length observations to weight observations
	Weight <- vector(length=nrecordsa)
	#Do Approach 2 first same L-W relationship for every record
	Weight <- a1*xx$Length_cm^b1
	
	#If Approach 1, over-write all weights for the fourth quarter
	if(Approach==1){
		for(j in 1:nrecordsa){
		       	quart <- xx$NumQuarter[j] #Get the quarter for that record
			if(is.element(quart,4)) Weight[j] <- a4*xx$Length_cm[j]^b4 #If the quarter is 4, over-write the Weight
		 }#end for j
	}#end if 
	
	xx <- cbind(xx,Weight) #add the weights to the xx table
	
        #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
	# Step 6: Get the total Weight SampWeights from each quarter and the Sample Weight of each Sample - this is Eq. D3 in the 2013 Res Doc
	# Step 7: Weight the sample weights by the quarter weights	 	
	#Make a table for the Mean Weight in each sequential quarter
	#Get the total number of unique samples from the dataset
	
	samples <- unique(xx$SAMPLE_ID)
	SampWeights <- matrix(nrow=length(samples), ncol=7) #Matrix for calculating total sample weight for each sample
	colnames(SampWeights) <- c("FYear","QuarterID","Seq_Quarter", "SAMPLE_ID",  "Mean_Sample_Weight","Sample_Weight" ,"MeanXSample")
	
	#Get Mean Weight  and Sample Weight for each SAMPLE :: Wjs and Sjs in Eq D3
	for(j in 1:length(samples))
	{
		  tmp <-xx[which(xx$SAMPLE_ID==samples[j]),]  #Extract data for each sample
		  #Fill table with quarter and sample ID and total weights for samples
		  quarter <- as.numeric(tmp$Seq_Quarter[1])
		  SampWeights[j,1] <- tmp$FYear[1]				#sequential quarter
		  SampWeights[j,2] <-  tmp$QuarterID[1]				#sequential quarter 
		  SampWeights[j,3] <- quarter				#sequential quarter
		  SampWeights[j,4] <- samples[j]    		#sampleID
		  SampWeights[j,5] <- mean(tmp$Weight)	#Mean weight in sample
		  SampWeights[j,6] <- sum(tmp$Weight)	#Sum of weight in sample
		  SampWeights[j,7] <- SampWeights[j,5]*SampWeights[j,6]	#Sum of weight in sample
	 }#end for j
		
	#Get weighted Mean Weight for each QUARTER :: SampWeights in Eq D3
	QMeanWeight<-matrix(nrow=length(numq), ncol=8)
	colnames(QMeanWeight)<-c("FYear","QuarterID", "Seq_Quarter", "SumMeanXSample" ,"Quarter_Weight" ,"QuarterMean", "QuarterCatch", "QuarterMeanXCatch")
	for(j in 1:length(numq)){
			#get data for quarter j
			nq<-which(SampWeights[,3]==j) 
			qdat<-SampWeights[nq,] 
				#print(qdat)
			if(length(nq)>1) {
				QMeanWeight[j,1] <- qdat[1,1]	#FYear
				QMeanWeight[j,2] <- qdat[1,2]	#QuarterID
				QMeanWeight[j,3] <- qdat[1,3]	#Seq_Quarter
				QMeanWeight[j,4] <- sum(qdat[,7])	#SumMeanXSample
				QMeanWeight[j,5] <- sum(qdat[,6])	#Quarter_Weight
				QMeanWeight[j,6] <- QMeanWeight[j,4]/QMeanWeight[j,5]	#QuarterMean
				QMeanWeight[j,7] <- cc[which(cc$QuarterID== QMeanWeight[j,2]),"TotalCatch"] 	#QuarterCatch
				QMeanWeight[j,8] <- QMeanWeight[j,6] * QMeanWeight[j,7]	#QuarterMeanXCatch
			}
			if(length(nq)==1) {
				QMeanWeight[j,1] <- qdat[1]
				QMeanWeight[j,2] <- qdat[2]
				QMeanWeight[j,3] <- qdat[3]
				QMeanWeight[j,4] <- qdat[7]
				QMeanWeight[j,5] <-qdat[6]
				QMeanWeight[j,6] <- QMeanWeight[j,4]/QMeanWeight[j,5]
				QMeanWeight[j,7] <- cc[which(cc$QuarterID== QMeanWeight[j,2]),"TotalCatch"]
				QMeanWeight[j,8] <- QMeanWeight[j,6] * QMeanWeight[j,7]
			}	
	}
	
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
	# Step 8: Calculate Mean Weight Wf for the fishing year f using two alternative approaches
	#		7a. Average the quarterly Mean Weights SampWeights, weighted by the quarterly Commercial Catch Cs
	#		7b. Average the quarterly Mean Weights SampWeights by dividing by n = number of quartes sampled in each year
	AnnualMeanWeight<-matrix(nrow=nyrs, ncol=7)
	colnames(AnnualMeanWeight) <- c("FYear", "SumQuarterMean", "SumQMeanXCatch" ,"Annual_Catch", "NumQuarters", "AnnualMeanWeight_A", "AnnualMeanWeight_B")
	for(j in 1:nyrs){
		
		ny <- which(QMeanWeight[,1]==yrs[j]) 
		ydat<- QMeanWeight[ny,]
		if(length(ny)>1) {
			AnnualMeanWeight[j,1] <- yrs[j]	    #FYear
			AnnualMeanWeight[j,2] <- sum(ydat[,6])   #SumQuarterMean
			AnnualMeanWeight[j,3] <- sum(ydat[,8])   #SumQMeanXCatch
			AnnualMeanWeight[j,4] <- sum(ydat[,7])   #Annual_Catch
			AnnualMeanWeight[j,5] <- nrow(ydat)   #NumQuarters
			AnnualMeanWeight[j,6] <- AnnualMeanWeight[j,3] /AnnualMeanWeight[j,4] #AnnualMeanWeight :: Approach A
			AnnualMeanWeight[j,7] <- AnnualMeanWeight[j,2] /AnnualMeanWeight[j,5] 	#AnnualMeanWeight :: Approach B
		}
		if(length(ny)==1) {
			AnnualMeanWeight[j,1] <- yrs[j]	    #FYear
			AnnualMeanWeight[j,2] <- ydat[6]   #SumQuarterMean
			AnnualMeanWeight[j,3] <- ydat[8]   #SumQMeanXCatch
			AnnualMeanWeight[j,4] <- ydat[7]   #Annual_Catch
			AnnualMeanWeight[j,5] <- 1   #NumQuarters
			AnnualMeanWeight[j,6] <- AnnualMeanWeight[j,3] /AnnualMeanWeight[j,4] #AnnualMeanWeight :: Approach A
			AnnualMeanWeight[j,7] <- AnnualMeanWeight[j,2] /AnnualMeanWeight[j,5] 	#AnnualMeanWeight :: Approach B
		}
	}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
	#Step 9:  Write out tables and Plot results
	write.table(SampWeights, paste(fdFigs,"Area_",StatAreas[i],"_Approach_",Approach, "_Sample_Weights.csv", sep=""), sep=",", row.names=F)	
	write.table(QMeanWeight, paste(fdFigs,"Area_",StatAreas[i],"_Approach_",Approach, "_QuarterlyMeanWeight.csv", sep=""), sep=",", row.names=F)	
	write.table(AnnualMeanWeight, paste(fdFigs,"Area_",StatAreas[i],"_Approach_",Approach, "_AnnualMeanWeight.csv", sep=""), sep=",", row.names=F)	
	write.table(xx,paste(fdFigs,"Area_",StatAreas[i],"_Approach_",Approach, "_AllWeights.csv",sep=""),row.names=F, sep=",")
     	write.table(cc, paste(fdFigs,"Area_",StatAreas[i],"_Approach_",Approach, "_QuarterlyCatches.csv", sep=""), sep=",", row.names=F)
     	
     	win.metafile(paste(fdFigs,"Area_",StatAreas[i],"_Approach_",Approach,"_Annual_Mean_Weights.wmf", sep=""))
     	matplot(yrs, AnnualMeanWeight[,6:7], type="l", col=1:2, lwd=2, lty=1:2, xlab="Fishing Year", ylab="Mean Weight (Kg)", main=paste("Area",StatAreas[i],"Approach",Approach),ylim=c(0,1.1*max(AnnualMeanWeight[,6:7], na.rm=T)), las=1, cex=1.2, cex.lab=1.2, cex.axis=1.2)
     	lines(yrs,pjs_mean_weight, col=3, lty=3,lwd=2)
     	legend("topleft", legend=c("Weighted by commercial catch Eq. D4A", "Not weighted by commercial catch Eq. D4B","PJS weighted by commercial catch Eq. D4A"), bty="n", lty=1:3, lwd=2, col=1:3)
    	dev.off()

}
