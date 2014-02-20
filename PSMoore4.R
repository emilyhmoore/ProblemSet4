
setwd("C:/Users/emily m/Journal Articles/Applied Statistical Programming")
Jacob.data <- "NetLogo.csv"

read.nlogo<-function(file=Jacob.data){
  ##Gives the name of the file. 
  name<- scan(Jacob.data, skip=1, nlines=1, what=" ", sep=",", n=1)
  ##Gives the date/time info
  datetime<-scan(file, skip=2, nlines=1, what=" ", sep=",", n=1)
  ##Remove the information on the end. 
  datetime<-gsub(" -0400", "", datetime)
  ##Take out slashes and colons
  datetime<-gsub("/|:", ".", datetime)
  datetime<-gsub(" ", "_", datetime)
  ##Make the directory name include the name and date/time
  dir_name<-paste(name, datetime, sep="_")
  ##Creates the main folder
  dir.create(path=dir_name)
  ##Creates Globals Subfolder
  dir.create(paste(dir_name, "/", "Globals", sep=""))
  ##Creates Turtles Subfolder
  dir.create(paste(dir_name, "/", "Turtles", sep=""))
  ##Creats Plots subfolder
  dir.create(paste(dir_name, "/", "Plots", sep=""))
  ##Next four create the subsubdirectories for the plots,
  dir.create(paste(dir_name, "/Plots/PositionPlot", sep=""))
  dir.create(paste(dir_name, "/Plots/WinnersPlot", sep=""))
  dir.create(paste(dir_name, "/Plots/PolarizationPlot", sep=""))
  dir.create(paste(dir_name, "/Plots/IncumbentPercentagePlot", sep=""))
  
  ###Globals
  ##Read in names
  globals.names <- scan(file=file, what=" ",sep=",",skip=8,nlines=1)
  ##Read in values
  globals<-scan(file=file, what=" ",sep=",",skip=9,nlines=1)
  ##Remove odd characters
  globals<- gsub("\\[|\\]","",globals)
  ##Remove extra quote
  globals<- gsub("\"","",globals)
  ##put names on the globals BEFORE splitting else names will get screwed up
  names(globals)<-globals.names
  ##Now when you split the names will be right. 
  globals<- unlist(strsplit(globals, split=" "))
  
  dump("globals", file=paste(dir_name, "/Globals/Globals.R", sep=""))
  
  ##Turtles
  ##Names
  turtles.names<-scan(file=file, what=" ",sep=",",skip=12,nlines=1, n=38)
  ##Scan turtles
  turtles<-scan(file=file, what=" ", sep=",", skip=13, nlines=4786)
  ##make into a matrix
  turt<-matrix(turtles, nrow=4786, byrow=TRUE)
  ##Delete completely empty columns
  turt<-turt[,-c(39:84)]
  ##Get rid of brackets
  turt<- gsub("\\[|\\]","",turt)
  ##Remove extra quote
  turt<- gsub("\"","",turt)
  ##Remove curly brackets
  turt<-gsub("\\{|\\}", "", turt)
  turt<-gsub("turtles |breed ", "", turt)
  ##set column names. Must do before string splitting. 
  colnames(turt)<-turtles.names
  turt<-turt[,-7] ##Delete label as it is empty
  
  which(turt[,"breed"]=="districts") ##Figure out which ones are districts
  turt[209:4786,"district-prefs"]<-"NA NA NA" ##For non-districts, fill in NA
  ##When split, it will give NA for all three columns
  ##splits district preferences
  dist<-unlist(strsplit(turt[,"district-prefs"], " "))
  ##matrix of prefs
  distmat<-matrix(dist, ncol=3, byrow=TRUE)
  ##new column names
  colnames(distmat)<-c("district-prefs-1", "district-prefs-2", "district-prefs-3")
  ##remove old district prefs
  turt<-turt[,-35]
  ##Cbind turt with the split district preferences
  turt<-cbind(turt, distmat)
  head(turt)
  
  
  ##Divide into districts
  distr<-which(turt[,"breed"]=="districts") ##finding districts
  districts<-turt[distr,] ##subsetting districts
  cand<-districts[,"my-cands-district"]
  cand<-unlist(strsplit(cand, " "))
  cand.mat<-matrix(cand,ncol=2, byrow=TRUE)
  colnames(cand.mat)<-c("my-cands-district-1","my-cands-district-2")  
  districts<-districts[,-35]
  districts<-cbind(districts, cand.mat)
  districts<-as.data.frame(districts) ##making data.frame
  uniq<-function(x){length(unique(x))==1} ##If unique is 1 all the elements are equal
  uniques<-apply(districts, 2, uniq)##Findings rows with all equal
  uniques<-which(uniques==TRUE) ##finding which specific columns
  districts<-districts[,-uniques] ##trimming down data. 
  ##NEED TO SAVE AS CSV!
}
read.nlogo()

##Chapter 4 JMR Probs 3 and 4
###Prob 3:
squarecube<-function(n){
  number<-1:n
  square<-number^2
  cube<-number^3
  tab<-cbind(square, cube)
  tab<-as.table(tab)
  row.names(tab)<-1:n
  return(tab)
}
squarecube(7)

##Prob 4:
##Function creates multiplication table for any value of n. 
mult<-function(n){
vect<-seq(1,n) ##Create first vector from 1 to n
vect2<-seq(1,n) ##Do it again
mat<-matrix(rep (0, n*n), nrow=n) ##create a matrix of 0s to fill in
for(i in 1:n){
mat[i,]<-vect[i]*vect2  ##fill in matrix row by row multiplying each element
##of vector times entire vector
}
return(mat)
}

mult(9) ##Try it out

##Chapter 7 JMR Probs 3 and 4

