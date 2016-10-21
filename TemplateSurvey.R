## This function will survey all .wav files in one folder with two templates. 
## Compared by pearson correlation in spectropgram, the function returns similarity peaks with file names, temporal distributions, which template the peaks match.
## path, template1 and template2 are character elements indicationg the directory of survey folder and two templates
## filenameas, filenameae, filenamebs and filenamebe are numeric elements to cut two segaments from original file names to be presented on the final output table

Surveytemplates <- function(path, template1, template2, 
                            filenamebs = 2, filenamebe = 2, filenameas = 4, filenameae = 13){

    library(seewave)
    library(tuneR)
    library(warbleR)
    library(monitoR)
    library(ggplot2)

    fn <- list.files(path = path, pattern = "*.wav", full.name = T, recursive = FALSE)
    ## create templates
    templatec1 <- makeCorTemplate(template1, wl = 300, ovlp = 90, dens = 1, name = "call")
    templates1 <- makeCorTemplate(template2, wl = 300, ovlp = 90, dens = 1, name = "song")

    ## create output table
    surveycom <<- data.frame(date.time = 0, time = 0, score = 0, detection = "a", V5 = "a", V6 = "a")
    surveycom$V5 <<- as.character(surveycom$V5)
    surveycom$V6 <<- as.character(surveycom$V6)

    cm <- "pearson"
    
    detectionCS <- function(x){
        ##substract time and file name information from the files
        fname <- substr(x, (nchar(path)+filenameas), (nchar(path)+filenameae))
        bname <- substr(x, (nchar(path)+filenamebs), (nchar(path)+filenamebe))
        
        cscorestemc <- corMatch(survey = x, templates = templatec1, show.prog = T, rec.tz = "GMT", cor.method = cm, warn = F, write.wav = F)
        callsur <- findPeaks(cscorestemc)
        cscorestems <- corMatch(survey = x, templates = templates1, show.prog = T, rec.tz = "GMT", cor.method = cm, warn = F, write.wav = F)
        songsur <- findPeaks(cscorestems)
        
        surc <- callsur@peaks$call
        surc <- surc[surc$detection == "TRUE",]
        if(nrow(surc) == 0){
        }else{
             surc[,1] <- as.numeric(surc[,1])
             surc[,4] <- as.character(surc[,4])
             surc[,4] <- "Template 1"
             surc[,5] <- fname
             surc[,6] <- bname}
        
        surs <- songsur@peaks$song
        surs <- surs[surs$detection == "TRUE",]
        if(nrow(surs) == 0){
        }else{
            surs[,1] <- as.numeric(surs[,1])
            surs[,4] <- as.character(surs[,4])
            surs[,4] <- "Template 2"
            surs[,5] <- fname
            surs[,6] <- bname}
        
        cs <- rbind(surc,surs)
        surveycom <<- rbind(surveycom,cs)
    }
    
    lapply(fn,detectionCS)
    surveycom <<- surveycom[2:nrow(surveycom),]
    surveycom[,5] <<- as.numeric(surveycom[,5])
    timea <<- as.numeric(surveycom[,5])
    timea <<- as.POSIXlt(timea, origin = "1970-01-01")
    suptable <<- data.frame(timea)
    outputtable <<- cbind(suptable, surveycom[, c(6,2,3,4)])
    colnames(outputtable) <<- c("file name a", "file name b", "detection time (s)", "similarity score ", "template")
}


