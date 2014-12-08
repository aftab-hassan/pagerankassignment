#reading data
mydata = read.table("assignmentdataset")
count = NULL;
largest = max(mydata);
for(i  in 1:largest)
{
 count[i] = length(which(mydata[,1] == i))
}

#Computing A
A = matrix(0,largest,largest)
for(i in 1:nrow(mydata))
{
 #cat(paste("A[",mydata[i,1],"][",mydata[i,2],"]==",(1/count[mydata[i,1]]),"\n"))
 A[mydata[i,2],mydata[i,1]] = round((1/count[mydata[i,1]]),digits=2)
}

#Assigning V
V = matrix( (1/largest), largest, 1)

#Computing where you get a constant stochastic
M = A
matrixatthisstep = V
for(i in 1:100)
{ 
 matrixatlaststep = matrixatthisstep
 matrixatthisstep = round(M %*% matrixatthisstep,digits=2)
 #matrixatthisstep = M %*% matrixatthisstep
 cat(paste("at i == ",i,"\n"));
 #cat(paste("matrix at last step ","\n"))
 #print(matrixatlaststep)
 #cat("\n")
 cat(paste("matrix at this step ","\n"))
 print(matrixatthisstep)
 if(identical(matrixatlaststep,matrixatthisstep) == "TRUE") 
 { 
  cat(paste("identical at i == ",i," , we have reached a matrix which does not change over iterations"))
  break;
 }
}

cat(paste("Upon pagerank, the nodes are ranked in the following order : DocID "))
rev(order(matrixatlaststep))

cat(paste("The page rank scores for the nodes are as follows"))
print(matrixatthisstep)
