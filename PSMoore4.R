
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
  head(districts)
  
  ##Parties
  party<-which(turt[,"breed"]=="parties") ##finding parties
  parties<-turt[party,] ##subsetting parties

  mpos<-parties[,c("mean-position")]
  mpos<-unlist(strsplit(mpos, " "))
  mpos.mat<-matrix(mpos,ncol=3, byrow=TRUE)
  colnames(mpos.mat)<-c("mean-pos-1","mean-pos-2", "mean-pos-3")  
  parties<-cbind(parties, mpos.mat)
  
  enf<-parties[,"enforcement-point"]
  enf<-unlist(strsplit(enf, " "))
  enf.mat<-matrix(enf, ncol=3, byrow=TRUE)
  colnames(enf.mat)<-c("enforcement-point-1",
                       "enforcement-point-2",
                       "enforcement-point-3")
  parties<-cbind(parties,enf.mat)
  
  cand.part<-parties[,"my-cands-party"]
  cand.part<-unlist(strsplit(cand.part, " "))
  cand.part.mat<-matrix(cand.part, nrow=2, byrow=TRUE)
  colnames(cand.part.mat)<-paste("my-cands-party", c(1:208))
  parties<-(cbind(parties,cand.part.mat))
  
  uniques_p<-apply(parties, 2, uniq)##Findings rows with all equal
  uniques_p<-which(uniques_p==TRUE) ##finding which specific columns
  parties<-parties[,-uniques_p] ##trimming down data.
  colnames(parties)
  parties<-parties[,-c(7,9,10)]
  head(parties)
  parties<-as.data.frame(parties) ##making data.frame
  
  ##Function to split from now on
  splitter<-function(x){
    unlist(strsplit(x, " "))
  }
  
  ##Voters
  voter<-which(turt[,"breed"]=="voters") 
  voters<-turt[voter,]
  
  voter.sal<-voters[,"this-voter-sal"]
  voter.sal<-splitter(voter.sal)
  sal.mat<-matrix(voter.sal, ncol=3, byrow=TRUE)
  voters<-cbind(voters, sal.mat)
  prefs<-voters[,"prefs"]
  prefs<-splitter(prefs)
  pref.mat<-matrix(prefs, ncol=3, byrow=TRUE)
  voters<-cbind(voters, pref.mat)
  
  uniques_v<-apply(voters, 2, uniq)##Findings rows with all equal
  uniques_v<-which(uniques_v==TRUE) ##finding which specific columns
  voters<-voters[,-uniques_v]
  voters<-voters[,-c(7,9)]
  colnames(voters)[8:13]<-c(paste("this-voter-sal", 1:3, sep=""), 
                            paste("prefs", 1:3,sep=""))
  head(voters)
  voters<-as.data.frame(voters)
 
 ##Activisits
 activ<-which(turt[,"breed"]=="activists") 
 activs<-turt[activ,]
 
 a.sal<-activs[,"this-act-sal"]
 a.sal<-splitter(a.sal)
 a.mat<-matrix(a.sal, ncol=3, byrow=TRUE)
 activs<-cbind(activs, a.mat)
 prefs1<-activs[,"prefs"]
 prefs1<-splitter(prefs1)
 pref.mat1<-matrix(prefs1, ncol=3, byrow=TRUE)
 activs<-cbind(activs, pref.mat1)
 
 uniques_a<-apply(activs, 2, uniq)##Findings rows with all equal
 uniques_a<-which(uniques_a==TRUE) ##finding which specific columns
 activs<-activs[,-uniques_a]
 activs<-activs[,-c(8,9)]
 colnames(activs)[9:14]<-c(paste("this-act-sal", 1:3, sep=""), 
                           paste("prefs", 1:3,sep=""))
 activs<-as.data.frame(activs)
 
 ##Candidates
 cand<-which(turt[,"breed"]=="cands") 
 cands<-turt[cand,]
 
 pos.ob<-cands[,"positions-obs"]
 pos.ob<-splitter(pos.ob)
 cand.mat<-matrix(pos.ob, ncol=3, byrow=TRUE)
 cands<-cbind(cands, cand.mat)
 
 pos.ob1<-cands[,"positions-obs-last"]
 pos.ob1<-splitter(pos.ob1)
 cand.mat1<-matrix(pos.ob1, ncol=3, byrow=TRUE)
 cands<-cbind(cands, cand.mat1)
 
 uniques_c<-apply(cands, 2, uniq)##Findings rows with all equal
 uniques_c<-which(uniques_c==TRUE) ##finding which specific columns
 cands<-cands[,-uniques_c]
 cands<-cands[,-c(7,12)]
 colnames(cands)[11:16]<-c(paste("pos-obs", 1:3, sep=""), 
                           paste("pos-obs-last", 1:3,sep=""))
 cands<-as.data.frame(cands)
 
 ##Write the CSV files 
 write.csv(districts, file=paste(dir_name, "/Turtles/Districts.csv", sep=""))
 write.csv(voters, file=paste(dir_name, "/Turtles/Voters.csv", sep=""))
 write.csv(parties, file=paste(dir_name, "/Turtles/Parties.csv", sep=""))
 write.csv(activs, file=paste(dir_name, "/Turtles/Activists.csv", sep=""))
 write.csv(cands, file=paste(dir_name, "/Turtles/Candidates.csv", sep=""))
 
 ##Plots
 plotcol<-scan(file=file, skip=8545, nlines=1, what=" ", sep=",", n=4)
 d1<-scan(file=file, skip=8546, nlines=169, what=" ", sep=",")
 d1<-matrix(d1, nrow=169, byrow=TRUE) 
 d1<-d1[,-c(25:84)]
 head(d1)
 reds<-d1[,1:4]
 blues<-d1[,5:8]
 red.activs<-d1[,9:12]
 red.voters<-d1[,13:16]
 blue.voters<-d1[,17:20]
 blue.activs<-d1[,21:24]
 D1<-rbind(reds,red.activs, red.voters, blues, blue.activs, blue.voters)
 colnames(D1)<-c("x", "y", "color", "pendown")
 rownames(D1)<-c(rep("reds", 169), 
                 rep("red.activs", 169), 
                 rep("red.voters", 169), 
                 rep("blues", 169),
                 rep("blue.activs", 169),
                 rep("blue.voters", 169))
 
 D1<-as.data.frame(D1)
 write.csv(D1, file=paste(dir_name, "/Plots/PositionPlot/D1.csv", sep=""))

 ##D2
 d2<-scan(file=file, skip=8730, nlines=169, what=" ", sep=",")
 d2<-matrix(d2, nrow=169, byrow=TRUE) 
 d2<-d2[,-c(25:84)]
 head(d2)
 reds<-d2[,1:4]
 blues<-d2[,5:8]
 red.activs<-d2[,9:12]
 red.voters<-d2[,13:16]
 blue.voters<-d2[,17:20]
 blue.activs<-d2[,21:24]
 d2<-rbind(reds,red.activs, red.voters, blues, blue.activs, blue.voters)
 colnames(d2)<-c("x", "y", "color", "pendown")
 rownames(d2)<-c(rep("reds", 169), 
                 rep("red.activs", 169), 
                 rep("red.voters", 169), 
                 rep("blues", 169),
                 rep("blue.activs", 169),
                 rep("blue.voters", 169))
 
 d2<-as.data.frame(d2)
 write.csv(d2, file=paste(dir_name, "/Plots/PositionPlot/D2.csv", sep=""))
 
 ##D3
 d3<-scan(file=file, skip=8914, nlines=169, what=" ", sep=",")
 d3<-matrix(d3, nrow=169, byrow=TRUE) 
 d3<-d3[,-c(25:84)]
 head(d3)
 reds<-d3[,1:4]
 blues<-d3[,5:8]
 red.activs<-d3[,9:12]
 red.voters<-d3[,13:16]
 blue.voters<-d3[,17:20]
 blue.activs<-d3[,21:24]
 d3<-rbind(reds,red.activs, red.voters, blues, blue.activs, blue.voters)
 colnames(d3)<-c("x", "y", "color", "pendown")
 rownames(d3)<-c(rep("reds", 169), 
                 rep("red.activs", 169), 
                 rep("red.voters", 169), 
                 rep("blues", 169),
                 rep("blue.activs", 169),
                 rep("blue.voters", 169))
 
 d3<-as.data.frame(d3)
 write.csv(d3, file=paste(dir_name, "/Plots/PositionPlot/D3.csv", sep=""))
 
 
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

