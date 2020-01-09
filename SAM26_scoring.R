
##load csv files with scoring weights csv files (may need to change path)
Episodic<-read.csv("~/Desktop/LBC/IndividualDifferences/SAM-26/Test4WeightJan2013SSam_Episodic_Reverse.csv")
Semantic<-read.csv("~/Desktop/LBC/IndividualDifferences/SAM-26/Test4WeightJan2013SSam_Semantic_Reverse.csv")
Spatial<-read.csv("~/Desktop/LBC/IndividualDifferences/SAM-26/Test4WeightJan2013SSam_Spatial_Reverse.csv")
Future<-read.csv("~/Desktop/LBC/IndividualDifferences/SAM-26/Test4WeightJan2013SSam_Future_Reverse.csv")
All<-read.csv("~/Desktop/LBC/IndividualDifferences/SAM-26/Test4WeightJan2013ShortSam_All_Reverse.csv")

##load data to be scored
Samtest<-read.csv("~/Desktop/LBC/IndividualDifferences/SAM-26/Samtest.csv", skip=1, blank.lines.skip=TRUE)
#rename first column
colnames(Samtest)[1]<-"ID"

##cleaning
##if subject number empty, remove row
Samtest<-Samtest[!(is.na(Samtest$ID)|Samtest$ID==""), ] 

##if all data cols 2-27 missing, remove row
Samtest<-Samtest[rowSums(is.na(Samtest))!=26, ]

##write clean data to excel file
write.xlsx(Samtest, "SAM_results_clean.xlsx")

#for each question, extract the weight from the corresponding csv file based on subject response, add to the sum, then store sum in variable
#will give NA when: full subscale is missing (or part of all is missing) OR when at least one question is missing from subscale
#n=question counter, sum=sum of weights, score_all=dataframe of all participants' scores, count=participant counter


##Episodic
#subset into own data frame
Episodic_sub<-subset(Samtest[2:9])
Episodic_df<-cbind(Samtest$ID,Episodic_sub)
#participant list
participantsE<-as.numeric(Episodic_df$`Samtest$ID`)
#for each participant, loop through and apply corresponding weight to each question
countE=0
Episodic_score_all=NULL
for(p in participantsE){
  nE=1 #question count
  sumE<-0 #sum 
  countE=countE+1 #participant count
  respE<-as.numeric(Episodic_df[countE,2:9])
  for(i in respE){
    wE<-Episodic[nE,i+1] #go to question row, response column
    sumE=sumE+wE
    nE=nE+1
    }
  Episodic_score<-ifelse(is.null(wE),"NA",(100+sumE))
  Episodic_score_all<-rbind(Episodic_score_all,Episodic_score)
}


##Semantic
#subset into own data frame and cbind participant column
Semantic_sub<-subset(Samtest[10:15])
Semantic_df<-cbind(Samtest$ID,Semantic_sub)
participantsS<-as.numeric(Semantic_df$`Samtest$ID`)
countS=0
Semantic_score_all=NULL
for(p in participantsS){
  nS=1 
  sumS<-0 
  countS=countS+1
  respS<-as.numeric(Semantic_df[countS,2:7])
  for(i in respS){
    wS<-Semantic[nS,i+1]
    sumS=sumS+wS
    nS=nS+1
  }
  Semantic_score<-ifelse(is.null(wS),"NA",(100+sumS))
  Semantic_score_all<-rbind(Semantic_score_all,Semantic_score)
}


##Spatial
Spatial_sub<-subset(Samtest[16:21])
Spatial_df<-cbind(Samtest$ID,Spatial_sub)
participantsSp<-as.numeric(Spatial_df$`Samtest$ID`)
countSp=0
Spatial_score_all=NULL
for(p in participantsSp){
  nSp=1 
  sumSp<-0 
  countSp=countSp+1
  respSp<-as.numeric(Spatial_df[countSp,2:7])
  for(i in respSp){
    wSp<-Spatial[nSp,i+1]
    sumSp=sumSp+wSp
    nSp=nSp+1
  }
  Spatial_score<-ifelse(is.null(wSp),"NA",(100+sumSp))
  Spatial_score_all<-rbind(Spatial_score_all,Spatial_score)
}


##Future
Future_sub<-subset(Samtest[22:27])
Future_df<-cbind(Samtest$ID,Future_sub)
participantsF<-as.numeric(Future_df$`Samtest$ID`)
countF=0
Future_score_all=NULL
for(p in participantsF){
  nF=1 
  sumF<-0 
  countF=countF+1
  respF<-as.numeric(Future_df[countF,2:7])
  for(i in respF){
    wF<-Future[nF,i+1]
    sumF=sumF+wF
    nF=nF+1
  }
  Future_score<-ifelse(is.null(wF),"NA",(100+sumF))
  Future_score_all<-rbind(Future_score_all,Future_score)
}

  
##Alltogethernow
participantsA<-as.numeric(Samtest$ID)
countA=0
All_score_all=NULL
for(p in participantsA){
  nA=1 
  sumA<-0 
  countA=countA+1
  respA<-as.numeric(Samtest[countA,2:27])
  for(i in respA){
    wA<-All[nA,i+1]
    sumA=sumA+wA
    nA=nA+1
  }
  All_score<-ifelse(is.null(wA),"NA",(100+sumA))
  All_score_all<-rbind(All_score_all,All_score)
}


##put all five scores in single dataframe and write to file
Sam26_scores<-cbind(Samtest[1],Episodic_score_all,Semantic_score_all,Spatial_score_all,Future_score_all,All_score_all)
colnames(Sam26_scores)<-c("ID","Episodic","Semantic","Spatial","Future","All")
write.xlsx(Sam26_scores, "Sam26_scores.xlsx")


