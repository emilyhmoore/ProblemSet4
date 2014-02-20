
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
  dir.create(paste(dir_name, "/", "Plots", "/", "PositionPlot", sep=""))
  dir.create(paste(dir_name, "/", "Plots", "/", "WinnersPlot", sep=""))
  dir.create(paste(dir_name, "/", "Plots", "/", "PolarizationPlot", sep=""))
  dir.create(paste(dir_name, "/", "Plots", "/", "IncumbentPercentagePlot", sep=""))
  
  ###Globals
  globals.names <- scan(file=file, what=" ",sep=",",skip=8,nlines=1)
  globals<-scan(file=file, what=" ",sep=",",skip=9,nlines=1)
  
  ##Turtles
  turtles.names<-scan(file=file, what=" ",sep=",",skip=12,nlines=1)
  turtles<-scan(file=file, what=" ", sep=",", skip=13, n=4786)
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


##Previous stuff from class to incorporate later. 


globals.values <- scan(file=Jacob.data,what=c(0,""),quote="",sep=" ",skip=9,nlines=1)
globals.values <- gsub("\\[|\\]","",globals.values)
globals.values <- gsub("","",globals.values)
head(globals.values)
globals.list <- as.list(globals.values)
head(globals.list)
list(globals.names,globals.values,name)
globals.values <- strsplit(globals.values,",")
globals.values <- unlist(globals.values)