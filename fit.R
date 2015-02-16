irtfit<-function(model, data)
{require(MCMCpack);require(mirt);require(separationplot)
 if(mean(data==0|data==1|is.na(data))<1){
   cat("Error: Non-binary input data")
   stop("Please respecify and call irtfit() again.\n", 
        call. = FALSE)
 }
 
 if(class(model)=="SingleGroupClass"){
   holder<-matrix(NA, nrow=nrow(data),ncol=ncol(data))
   colnames(holder)<-colnames(data)
   fscores_holder<-fscores(model,full.scores=T, scores.only=T)
   for (i in 1:ncol(data)){
     holder[,i]<-plogis(coef(model)[[i]][2]+coef(model)[[i]][1]*fscores_holder)
   }
   return(separationplot(as.vector(holder)[which(!is.na(as.vector(data)))],as.vector(data)[which(!is.na(as.vector(data)))],type='rect'))
 }
 else if(class(model)=="mcmc"){
   if(ncol(as.data.frame(model))==nrow(data))
   {
     cat("Error: No item parameters specified")
     stop("Please respecify and call irtfit() again.\n", 
          call. = FALSE)
   }
   holder<-matrix(NA, nrow=nrow(data),ncol=ncol(data))
   colnames(holder)<-colnames(data)
   for (i in 1:ncol(data)){
     holder[,i]<-plogis(-mean(as.data.frame(model)[,nrow(data)-1+(i*2)])+mean(as.data.frame(model)[,nrow(data)+(i*2)])*colMeans(as.data.frame(model)[,1:nrow(data)]))  
   }
   return(separationplot(as.vector(holder)[which(!is.na(as.vector(data)))],as.vector(as.matrix(data))[which(!is.na(as.vector(as.matrix(data))))],type='rect'))
 }    
 else {
   cat("Error: Non-recognizable model")
   stop("Please respecify and call irtfit() again.\n", 
        call. = FALSE)
 }}
