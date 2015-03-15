require(stringr)
read.file <- function(file,skip,verbose=T){
    if(verbose) print(paste("read", file))
    tmp <- read.table(file,skip = skip,sep = "\t",
                      header=T,na.strings = c(" +",""),
                      fill=T)
    
    tmp <- tmp[!is.na(tmp$Subject),] 
    
    if(sum(!str_detect(tmp$Subject,"^0[012][0-9]_[1-8]$|^0[012][0-9]_test[12]$")))
        print(paste("id",tmp$Subject[1]))

    if(sum(tmp$Stim.Type %in% c("hit","incorrect"))==0) return(NULL)

    tmp <- lapply(tmp,function(x) {
        if( class(x) %in% c("character","factor") ){
            x <- factor(gsub(" ","",as.character(x)))
            return(x)}else{ return(x) }})
    
    tmp <- as.data.frame(tmp)
    
    pause <- which(tmp$Event.Type=="Picture" & tmp$Code=="Pause")
    if(length(pause)>0){
        drei <- which(tmp$Code==3 & !is.na(tmp$Code))
        drei <- drei[drei > pause][1:2]
        if(pause + 1 < drei[1]){
            tmp <- tmp[-(pause:drei[2]),]
        }}

    
    tmp <- tmp[!(tmp$Event.Type %in% c("Pause","Resume")), ]

    first.pic <- min(which(tmp$Event.Type=="Picture" & !is.na(tmp$Event.Type) )) - 1
    tmp <- tmp[-(1:first.pic),]

    last.pic <- min(which(tmp$Event.Type=="Picture" & !is.na(tmp$Event.Type) &
                              tmp$Code=="Fertig!" & !is.na(tmp$Code)))
    tmp <- tmp[-(last.pic:nrow(tmp)),]

    zeilen <- which(tmp$Event.Type %in% c("Response"))
    zeilen <- sort(unique(c(zeilen,zeilen-1)))
    zeilen <- zeilen[zeilen>0]
    tmp <- tmp[zeilen,]
    
    responses <- which(tmp$Code %in% c(1,2))
    events <- responses-1
    tmp$Type <- NA
    tmp$Type[responses] <- as.character(tmp$Event.Type[events])

    if(length(tmp$Type[responses])!=length(tmp$Event.Type[events])) { print(file)}
    tmp$Event.Code <- NA
    tmp$Event.Code[responses] <- as.character(tmp$Code[events])
    tmp$Time1 <- NA
    tmp$Time1[responses] <- tmp$Time[events]
    tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
    tmp$Duration[responses] <- as.character(tmp$Duration[events])
    tmp$Uncertainty.1[responses] <- as.character(tmp$Uncertainty.1[events])
    tmp$ReqTime[responses] <- as.character(tmp$ReqTime[events])
    tmp$ReqDur[responses] <- as.character(tmp$ReqDur[events])
    tmp$Pair.Index[responses] <- as.character(tmp$Pair.Index[events])
    

    tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
    tmp <- tmp[tmp$Event.Type=="Response" & !is.na(tmp$Type),]
    tmp <- tmp[tmp$Type=="Picture" & !is.na(tmp$Type),]
    return(tmp)
}

read.files <- function(filesdir,skip=3,recursive=F,pattern="."){
    files <- dir(filesdir,
                 full.names = T,
                 recursive = recursive,
                 pattern = pattern)
    Reduce(rbind,lapply(files,read.file,skip=skip))}


    
df.t.test <- function(df,group,col,indcol){
    t.test.helper <- function(x,col,indcol,group){
        tob <- t.test(x[,col] ~ x[,indcol])
        tmp <- data.frame(data = paste(col,"by",indcol),
                          group = x[1,group],
                          mean.group.1 = tob$estimate[1],
                          mean.group.2 = tob$estimate[2],
                          name.test.stat = tob$statistic,
                          conf.lower = tob$conf.int[1],
                          conf.upper = tob$conf.int[2],
                          pval = tob$p.value,
                          alternative = tob$alternative,
                          tob$method)
        names(tmp)[3:4] <- make.names(names(tob$estimate))
        row.names(tmp) <- x[1,group]
        tmp
    }
    df.l <- split(df[,c(col,indcol,group)],df[,group])
    Reduce(rbind,lapply(df.l,t.test.helper,col=col,indcol=indcol,group=group))}


