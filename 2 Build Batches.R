#Project: Zillow Tracker
#Code: 2 Build Batches
#Author: Scott Onestak
#Last Executed: 1/25/2022

#Pulling individual addresses is finicky...separate into batches so not all 3K+ have to compute in the same run
theListDedup = read.csv("Data/theListDedup.csv",stringsAsFactors = F, header = T)

#Get number of batches
batches = ceiling(dim(theListDedup)[1]/100)

#Loop through to create batches
for(i in seq(from=1,to=batches,by=1)){
  start = (i-1)*100 + 1
  end = min(i*100,dim(theListDedup)[1])
  
  theBatch = theListDedup[c(start:end),]
  write.csv(theBatch,paste("Data/batches/batch_",i,".csv",sep=""),row.names=FALSE)
}