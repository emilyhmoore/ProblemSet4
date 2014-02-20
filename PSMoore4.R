
setwd("C:/Users/emily m/Journal Articles/Applied Statistical Programming")
Jacob.data <- "NetLogo.csv"


##Gives the name of the file. 
name<- scan(Jacob.data, skip=1, nlines=1, what=" ", sep=",", n=1)

##Gives the date/time info
datetime<-scan(Jacob.data, skip=2, nlines=1, what=" ", sep=",", n=1)
##Remove the information on the end. 
datetime<-gsub(" -0400", "", datetime)
##Take out slashes and colons
datetime<-gsub("/|:", ".", datetime)
##Make the directory name include the name and date/time
dir_name<-paste(name, datetime)

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

globals.names <- scan(file=Jacob.data,what=" ",sep=",",skip=8,nlines=1)
globals.names

globals.values <- scan(file=Jacob.data,what=c(0,""),quote="",sep=" ",skip=9,nlines=1)
globals.values <- gsub("\\[|\\]","",globals.values)
globals.values <- gsub("","",globals.values)
head(globals.values)
globals.list <- as.list(globals.values)
head(globals.list)
list(globals.names,globals.values,name)
globals.values <- strsplit(globals.values,",")
globals.values <- unlist(globals.values)