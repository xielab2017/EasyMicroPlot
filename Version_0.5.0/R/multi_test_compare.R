duncan.test <-
  function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL,console=FALSE)
  {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
      if(is.null(main))main<-y$call
      A<-y$model
      DFerror<-df.residual(y)
      MSerror<-deviance(y)/DFerror
      y<-A[,1]
      ipch<-pmatch(trt,names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")	
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)
    ntr<-nrow(means)
    Tprob<-NULL
    k<-0
    for(i in 2:ntr){
      k<-k+1
      x <- suppressWarnings(warning(qtukey((1-alpha)^(i-1), i, DFerror)))
      if(x=="NaN")break
      else Tprob[k]<-x
    }
    if(k<(ntr-1)){
      for(i in k:(ntr-1)){
        f <- Vectorize(function(x)ptukey(x,i+1,DFerror)-(1-alpha)^i)
        Tprob[i]<-uniroot(f, c(0,100))$root
      }
    }
    Tprob<-as.numeric(Tprob)
    nr <- unique(nn[,2])
    # Critical Value of Studentized Range
    if(console){
      cat("\nStudy:", main)
      cat("\n\nDuncan's new multiple range test\nfor",name.y,"\n")
      cat("\nMean Square Error: ",MSerror,"\n\n")
      cat(paste(name.t,",",sep="")," means\n\n")
      print(data.frame(row.names = means[,1], means[,2:6]))
    }
    if(length(nr) == 1 ) sdtdif <- sqrt(MSerror/nr)
    else {
      nr1 <-  1/mean(1/nn[,2])
      sdtdif <- sqrt(MSerror/nr1)
    }
    DUNCAN <- Tprob * sdtdif
    names(DUNCAN)<-2:ntr
    duncan<-data.frame(Table=Tprob,CriticalRange=DUNCAN)
    if ( group & length(nr) == 1 & console){
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("\nCritical Range\n")
      print(DUNCAN)
    }
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    if ( length(nr) != 1) duncan<-NULL    
    Omeans<-order(means[,2],decreasing = TRUE) #correccion 2019, 1 abril.
    Ordindex<-order(Omeans)
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    DIF<-dif
    LCL<-dif
    UCL<-dif
    pvalue<-dif
    odif<-dif
    sig<-NULL
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      dif[k]<-means[i,2]-means[j,2]
      DIF[k]<-abs(dif[k])
      nx<-abs(i-j)+1
      odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
      pvalue[k]<- round(1-ptukey(DIF[k]/sdtdif,odif[k],DFerror)^(1/(odif[k]-1)),4)
      LCL[k] <- dif[k] - DUNCAN[odif[k]-1]
      UCL[k] <- dif[k] + DUNCAN[odif[k]-1]
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    if(!group){  
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console){cat("\nComparison between treatments means\n\n")
        print(comparison)}
      groups=NULL
    }
    if (group) {
      comparison=NULL
      # The probability matrix
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nMeans with the same letter are not significantly different.\n\n")
        print(groups)
      }      
    }
    parameters<-data.frame(test="Duncan",name.t=name.t,ntr = ntr,alpha=alpha)
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters, duncan=duncan,
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }

HSD.test <-
  function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL,unbalanced=FALSE,console=FALSE)
  {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
      if(is.null(main))main<-y$call
      A<-y$model
      DFerror<-df.residual(y)
      MSerror<-deviance(y)/DFerror
      y<-A[,1]
      ipch<-pmatch(trt,names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)
    #   row.names(means)<-means[,1]
    ntr<-nrow(means)
    Tprob <- qtukey(1-alpha,ntr, DFerror)
    nr<-unique(nn[, 2])
    nr1<-1/mean(1/nn[,2])
    if(console){
      cat("\nStudy:", main)
      cat("\n\nHSD Test for",name.y,"\n")
      cat("\nMean Square Error: ",MSerror,"\n\n")
      cat(paste(name.t,",",sep="")," means\n\n")
      print(data.frame(row.names = means[,1], means[,2:6]))
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("Critical Value of Studentized Range:", Tprob,"\n")
    }
    HSD <- Tprob * sqrt(MSerror/nr)
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV,MSD=HSD)
    if ( group & length(nr) == 1 & console) cat("\nMinimun Significant Difference:",HSD,"\n")
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    if ( length(nr) != 1) statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    sig<-NULL
    LCL<-dif
    UCL<-dif
    pvalue<-rep(0,nn)
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      #if (means[i, 2] < means[j, 2]){
      #comb[1, k]<-j
      #comb[2, k]<-i
      #}
      dif[k]<-means[i,2]-means[j,2]
      sdtdif<-sqrt(MSerror * 0.5*(1/means[i,4] + 1/means[j,4]))
      if(unbalanced)sdtdif<-sqrt(MSerror /nr1)
      pvalue[k]<- round(1-ptukey(abs(dif[k])/sdtdif,ntr,DFerror),4)
      LCL[k] <- dif[k] - Tprob*sdtdif
      UCL[k] <- dif[k] + Tprob*sdtdif
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    if(!group){
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console){cat("\nComparison between treatments means\n\n")
        print(comparison)}
      groups=NULL
      #groups<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,4],std.err=means[,3])
    }
    if (group) {
      comparison=NULL
      # Matriz de probabilidades
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nTreatments with the same letter are not significantly different.\n\n")
        print(groups)
      }
    }
    parameters<-data.frame(test="Tukey",name.t=name.t,ntr = ntr, StudentizedRange=Tprob,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters,
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }

LSD.test <-
  function (y, trt, DFerror, MSerror, alpha = 0.05, p.adj = c("none","holm","hommel",
                                                              "hochberg", "bonferroni", "BH", "BY", "fdr"), group = TRUE, main = NULL,console=FALSE)
  {
    p.adj <- match.arg(p.adj)
    clase <- c("aov", "lm")
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    if ("aov" %in% class(y) | "lm" %in% class(y)) {
      if(is.null(main))main<-y$call
      A <- y$model
      DFerror <- df.residual(y)
      MSerror <- deviance(y)/DFerror
      y <- A[, 1]
      ipch <- pmatch(trt, names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")    
    means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    std.err <- sqrt(MSerror)/sqrt(nn[, 2]) # change sds[,2]
    Tprob <- qt(1 - alpha/2, DFerror)
    LCL <- means[, 2] - Tprob * std.err
    UCL <- means[, 2] + Tprob * std.err
    means <- data.frame(means, std=sds[,2], r = nn[, 2],
                        LCL, UCL,medians)
    names(means)[1:2] <- c(name.t, name.y)
    ntr <- nrow(means)
    nk <- choose(ntr, 2)
    if (p.adj != "none") {
      a <- 1e-06
      b <- 1
      for (i in 1:100) {
        x <- (b + a)/2
        xr <- rep(x, nk)
        d <- p.adjust(xr, p.adj)[1] - alpha
        ar <- rep(a, nk)
        fa <- p.adjust(ar, p.adj)[1] - alpha
        if (d * fa < 0)
          b <- x
        if (d * fa > 0)
          a <- x
      }
      Tprob <- qt(1 - x/2, DFerror)
    }
    nr <- unique(nn[, 2])
    if(console){
      cat("\nStudy:", main)
      if(console)cat("\n\nLSD t Test for", name.y, "\n")
      if (p.adj != "none")cat("P value adjustment method:", p.adj, "\n")
      cat("\nMean Square Error: ", MSerror, "\n\n")
      cat(paste(name.t, ",", sep = ""), " means and individual (",
          (1 - alpha) * 100, "%) CI\n\n")
      print(data.frame(row.names = means[, 1], means[, 2:8]))
      cat("\nAlpha:", alpha, "; DF Error:", DFerror)
      cat("\nCritical Value of t:", Tprob, "\n")
    }
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)     
    if (length(nr) == 1)  LSD <- Tprob * sqrt(2 * MSerror/nr)
    if ( group & length(nr) == 1 & console) {
      if(p.adj=="none") cat("\nleast Significant Difference:",LSD,"\n")
      else cat("\nMinimum Significant Difference:",LSD,"\n")
    }
    if ( group & length(nr) != 1 & console) 
      cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    
    if ( length(nr) == 1 & p.adj=="none") statistics<-data.frame(statistics, t.value=Tprob,LSD=LSD) 
    if ( length(nr) == 1 & p.adj!="none") statistics<-data.frame(statistics, t.value=Tprob,MSD=LSD)    
    LSD=" "
    comb <- utils::combn(ntr, 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    pvalue <- dif
    sdtdif <- dif
    sig <- rep(" ", nn)
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]
      
      dif[k] <-means[i, 2] - means[j, 2]
      sdtdif[k] <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,4]))
      pvalue[k] <- 2 * (1 - pt(abs(dif[k])/sdtdif[k], DFerror))
    }
    if (p.adj != "none")
      pvalue <- p.adjust(pvalue, p.adj)
    pvalue <- round(pvalue,4)
    for (k in 1:nn) {
      if (pvalue[k] <= 0.001)
        sig[k] <- "***"
      else if (pvalue[k] <= 0.01)
        sig[k] <- "**"
      else if (pvalue[k] <= 0.05)
        sig[k] <- "*"
      else if (pvalue[k] <= 0.1)
        sig[k] <- "."
    }
    tr.i <- means[comb[1, ], 1]
    tr.j <- means[comb[2, ], 1]
    LCL <- dif - Tprob * sdtdif
    UCL <- dif + Tprob * sdtdif
    comparison <- data.frame(difference = dif, pvalue = pvalue, "signif."=sig, LCL, UCL)
    if (p.adj !="bonferroni" & p.adj !="none"){
      comparison<-comparison[,1:3]
      #    statistics<-statistics[,1:4]
    }
    rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
    if (!group) {
      if(console){
        cat("\nComparison between treatments means\n\n")
        print(comparison)
      }
      groups <- NULL
      #   statistics<-statistics[,1:4]
    }
    if (group) {
      comparison=NULL
      # Matriz de probabilidades
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nTreatments with the same letter are not significantly different.\n\n")
        print(groups)  
      }   
    }
    parameters<-data.frame(test="Fisher-LSD",p.ajusted=p.adj,name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters, 
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }

REGW.test <-
  function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL,console=FALSE)
  {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
      if(is.null(main))main<-y$call
      A<-y$model
      DFerror<-df.residual(y)
      MSerror<-deviance(y)/DFerror
      y<-A[,1]
      ipch<-pmatch(trt,names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean	
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)
    
    #   row.names(means)<-means[,1]
    ntr<-nrow(means)
    Tprob<-NULL
    for(i in 2:(ntr-2)) Tprob[i-1]<- qtukey(p=(1-alpha)^(i/ntr),i,df=DFerror)
    Tprob[ntr-2] <- qtukey(p=(1-alpha),ntr-1, df=DFerror)
    Tprob[ntr-1]   <- qtukey(p=(1-alpha),ntr  , df=DFerror)
    if(Tprob[ntr-3]> Tprob[ntr-2]) Tprob[ntr-2]<-Tprob[ntr-3]
    if(Tprob[ntr-2]> Tprob[ntr-1]) Tprob[ntr-1]<-Tprob[ntr-2]
    
    nr <- unique(nn[,2])
    
    #"Critical Value of Studentized Range")
    if(console){
      cat("\nStudy:", main)
      cat("\n\nRyan, Einot and Gabriel and Welsch multiple range test\nfor",name.y,"\n")
      cat("\nMean Square Error: ",MSerror,"\n\n")
      cat(paste(name.t,",",sep="")," means\n\n")
      print(data.frame(row.names = means[,1], means[,2:6]))
    }
    if(length(nr) == 1 ) sdtdif <- sqrt(MSerror/nr)
    else {
      nr1 <-  1/mean(1/nn[,2])
      sdtdif <- sqrt(MSerror/nr1)
    }
    REGW <- Tprob * sdtdif
    names(REGW)<-2:ntr
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
    regw<-data.frame(Table=Tprob,CriticalRange=REGW)
    if ( group & length(nr) == 1 & console){
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("\nCritical Range\n")
      print(REGW)  
    }
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    if ( length(nr) != 1) regw<-NULL
    Omeans<-order(means[,1],decreasing = TRUE)
    Ordindex<-order(Omeans)
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    DIF<-dif
    LCL<-dif
    UCL<-dif
    pvalue<-dif
    odif<-dif
    sig<-NULL
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      dif[k]<-means[i,2]-means[j,2]
      DIF[k]<-abs(dif[k])
      nx<-abs(i-j)+1
      odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
      if(odif[k] <= ntr-2) pvalue[k]<- 1-(ptukey(DIF[k]/sdtdif,odif[k],DFerror))^(ntr/odif[k])
      if(odif[k] > ntr-2)  pvalue[k]<- 1-(ptukey(DIF[k]/sdtdif,odif[k],DFerror))
      pvalue[k]<-round(pvalue[k],4)
      LCL[k] <- dif[k] - REGW[odif[k]-1]
      UCL[k] <- dif[k] + REGW[odif[k]-1]
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    
    if(!group){  
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console){cat("\nComparison between treatments means\n\n")
        print(comparison)}
      groups=NULL
    }
    if (group) {
      comparison=NULL
      # Matriz de probabilidades
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nMeans with the same letter are not significantly different.\n\n")
        print(groups)
      }
    }
    parameters<-data.frame(test="REGW",name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters,regw=regw,
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }

scheffe.test <-
  function (y, trt, DFerror, MSerror, Fc, alpha=0.05, group=TRUE,main = NULL,console=FALSE)
  {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    clase<-c("aov","lm")
    if(is.null(main))main<-paste(name.y,"~", name.t)
    if("aov"%in%class(y) | "lm"%in%class(y)){
      if(is.null(main))main<-y$call
      A<-y$model
      DFerror<-df.residual(y)
      MSerror<-deviance(y)/DFerror
      Fc<-anova(y)[trt,4]
      y<-A[,1]
      ipch<-pmatch(trt,names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean	
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)
    #   row.names(means)<-means[,1]
    ntr<-nrow(means)
    Fprob <- qf(1-alpha,ntr-1, DFerror)
    Tprob <- sqrt(Fprob * (ntr - 1))
    nr<-unique(nn[, 2])
    if(console){
      cat("\nStudy:", main)
      cat("\n\nScheffe Test for",name.y,"\n")
      cat("\nMean Square Error  :",MSerror,"\n\n")
      cat(paste(name.t,",",sep="")," means\n\n")
      print(data.frame(row.names = means[,1], means[,2:6]))
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("Critical Value of F:", Fprob,"\n")
    }
    scheffe <- Tprob*sqrt(2*MSerror/nr)
    statistics<-data.frame(MSerror=MSerror,Df=DFerror, F=Fprob,Mean=Mean,CV=CV)
    if ( group & length(nr) == 1 & console) cat("\nMinimum Significant Difference:",scheffe,"\n")
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    if ( length(nr) == 1) statistics<-data.frame(statistics,Scheffe=Tprob,CriticalDifference=scheffe)
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    sig<-NULL
    LCL<-dif
    UCL<-dif
    pvalue<-rep(0,nn)
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      
      dif[k]<-means[i,2]-means[j,2]
      sdtdif<-sqrt(MSerror * (1/means[i,4] + 1/means[j,4]))
      pvalue[k]<- round(1-pf(abs(dif[k])^2/((ntr-1)*sdtdif^2),ntr-1,DFerror),4)
      
      LCL[k] <- dif[k] - Tprob*sdtdif
      UCL[k] <- dif[k] + Tprob*sdtdif
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    if(!group){
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("Difference" = dif, pvalue=pvalue,sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console){cat("\nComparison between treatments means\n\n")
        print(comparison)}
      groups=NULL
    }
    if (group) {
      if(console) {
        cat("\nMeans with the same letter are not significantly different.\n\n")
      }
      comparison=NULL
      # Matriz de probabilidades
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) print(groups)
    }
    parameters<-data.frame(test="Scheffe",name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters, 
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }

SNK.test <-
  function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL,console=FALSE)
  {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    clase<-c("aov","lm")
    if(is.null(main))main<-paste(name.y,"~", name.t)
    if("aov"%in%class(y) | "lm"%in%class(y)){
      if(is.null(main))main<-y$call
      A<-y$model
      DFerror<-df.residual(y)
      MSerror<-deviance(y)/DFerror
      y<-A[,1]
      ipch<-pmatch(trt,names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean	
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)
    ntr<-nrow(means)
    Tprob <- qtukey(1-alpha,2:ntr, DFerror)
    nr <- unique(nn[,2])
    #"Critical Value of Studentized Range")
    if(console) {
      cat("\nStudy:", main)
      cat("\n\nStudent Newman Keuls Test\nfor",name.y,"\n")
      cat("\nMean Square Error: ",MSerror,"\n\n")
      cat(paste(name.t,",",sep="")," means\n\n")
      print(data.frame(row.names = means[,1], means[,2:6]))
    }
    if(length(nr) == 1 ) sdtdif <- sqrt(MSerror/nr)
    else {
      nr1 <-  1/mean(1/nn[,2])
      sdtdif <- sqrt(MSerror/nr1)
    }
    SNK <- Tprob * sdtdif
    names(SNK)<-2:ntr
    snk<-data.frame(Table=Tprob,CriticalRange=SNK)
    if ( group & length(nr) == 1 & console){
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("\nCritical Range\n")
      print(SNK)
    }
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    if ( length(nr) != 1) snk<-NULL
    Omeans<-order(means[,2],decreasing = TRUE)
    Ordindex<-order(Omeans)
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    LCL<-dif
    UCL<-dif
    sig<-NULL
    pvalue<-dif
    odif<-dif
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      dif[k]<-means[i,2]-means[j,2]
      nx<-abs(i-j)+1
      odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
      pvalue[k]<- round(1-ptukey(abs(dif[k])/sdtdif,odif[k],DFerror),4)
      LCL[k] <- dif[k] - SNK[odif[k]-1]
      UCL[k] <- dif[k] + SNK[odif[k]-1]
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    if(!group){  
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console){cat("\nComparison between treatments means\n\n")
        print(comparison)}
      groups=NULL
    }
    if (group) {
      comparison=NULL
      # Matriz de probabilidades
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nMeans with the same letter are not significantly different.\n\n")
        print(groups)
      }
    }
    parameters<-data.frame(test="SNK",name.t=name.t,ntr = ntr,alpha=alpha)
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters, snk=snk,
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"	
    invisible(output)
  }
