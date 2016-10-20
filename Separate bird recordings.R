
## seprate bird call and song recordings from noise recordings and save separated files in working directory with a new name indicating the bird and recording time.
## the function serves only for mono-channel recordings

##path is one character element for the directory of your recordings
##lbird is one character element for the name of the bird in left channel
##rbird is one character element for the name of the bird in right channel
##f1 is one numeric element for recording frequency in hz, the default is 44100 hz
##threshold is one numeric element in khz for frequency above which will be determined to be a bird sound once detected
separaZe<-function(path,lbird,rbird, f1 = 44100, threshold = 3) {

  library(seewave)
  library(tuneR)
  fn<-list.files(path = path, pattern="*.wav", full.name = T, recursive = FALSE)
  n=1
  m=1
  names1<-vector()
  bird1<-vector()
  date1<-vector()
  time1<-vector()
  
  lapply(fn, function(x) {
    stime<-file.mtime(x)
    s<-readWave(x)
    sl<-c(s@left,0)
    sr<-c(s@right,0)
    maxl<-max(sl)
    maxr<-max(sr)
    if (maxl>maxr) {
      b<-dfreq(sl, f= f1, plot="FALSE")
      c<-max(b[,2])
      if(c>threshold){
        name<-paste(lbird, "_", as.character(as.numeric(stime)), "_",as.character(n),".wav",sep="")
        savewav(sl, f = f1, filename = name)
        datel<-substr(stime,1,10)
        timel<-substr(stime,12,19)
        names1<<-c(names1,name)
        bird1<<-c(bird1,lbird)
        date1<<-c(date1,datel)
        time1<<-c(time1,timel)
        n<<-n+1}
    }else{
      b<-dfreq(sr, f= f1, plot="FALSE")
      c<-max(b[,2])
      if(c>threshold){
        name<-paste(rbird, "_", as.character(as.numeric(stime)), "_",as.character(m),".wav",sep="")
        savewav(sr, f = f1, filename = name)
        dater<-substr(stime,1,10)
        timer<-substr(stime,12,19)
        names1<<-c(names1,name)
        bird1<<-c(bird1,rbird)
        date1<<-c(date1,dater)
        time1<<-c(time1,timer)
        m<<-m+1}
    }
  })
  print(names1)
  print(bird1)
  print(date1)
  print(time1)
  frame1<<-data.frame(names1,bird1,date1,time1)
  colnames(frame1)<<-c("File","Bird","Date","Time")
}

## the output will be new saved files and a table. In the table there will be original file name, bird name, recording date and time.