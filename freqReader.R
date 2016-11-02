#library(Cairo)   # May need to run: install.packages("Cairo")
library(RCurl)    # May need to run: install.packages("RCurl")

# Files to load
file1 = "http://maximromanov.github.io/projects/ngrams/1gram_NRM_NgramChronoTable_10up_BW"
file2 = "http://maximromanov.github.io/projects/ngrams/1gram_NRM_NgramChronoTable_50up_BW_test"
file3 = "http://maximromanov.github.io/projects/ngrams/1gram_NRM_NgramChronoTable_Cummulative"

# Some variables
periods = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)


# Load word frequency data
ngrams <- read.csv(text = getURL(file1), header=F, sep="\t")
# give it a few minutes --- the file is large so it takes a while to download it and then load it into memory
colnames(ngrams) <- c("ngram", "freq", "0", "50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "1050", "1100", "1150", "1200", "1250", "1300", "1350", "1400", "1450")

# Load freqs by periods
allFreqs = read.csv(text = getURL(file3), header=F, sep=",")
allFreqs = allFreqs[,2]
allFreqs = allFreqs[2:30]

wordFreqRelRE <- function(freqTable, item, spanVar){
  # item = "w?Hdvn[Ay], vn[A]"
  item = gsub(", ?", "|", item)
  item = paste0("^(", item, ")$")
  tempData = subset(freqTable, grepl(item, ngram))
  tempData = subset(tempData, select = -c(ngram, freq))
  tempData = colSums(tempData)
  tempData = as.integer(as.vector(tempData))
  tempData = tempData[1:29]/as.integer(as.vector(allFreqs))*100
  #smoothingmainVar = tempData
  smoothingmainVar = predict(loess(tempData~periods, span=spanVar))
  smoothingmainVar[smoothingmainVar < 0] = 0
  return(smoothingmainVar)
}


#CairoPDF("test.pdf", width=12, height=6, family="Geeza Pro", encoding="utf8")

xlabVarAH = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)
xlabVarCE = as.integer((xlabVarAH-xlabVarAH/33)+622)
# ce = int(ah)-(int(ah)/33)+622

colVec = c("red", "darkgreen", "blue", "orange", "maroon")
searchVector = c("w?Hdvn[Ay], vn[A]", "w?Axbrn[Ay], An[A]", "w?AnbAn[Ay], n[A]")

spanVar = .25

plot(bty="n",
  x=periods, xlim = c(0,1400), xaxt="n",
  y=wordFreqRelRE(ngrams, searchVector[1], spanVar), ylab="Word Frequencies (%)",
  type="n", col="red",
  #xlab = "",
  xlab="Word frequencies across 14 centuries and 7,800 Arabic texts")
# AXES
axis(1, at=xlabVarAH,labels=xlabVarAH, col.axis="black", las=2)
mtext("AH",1,line=1,at=1450,col="black")
axis(3, at=xlabVarAH,labels=xlabVarCE, col.axis="black", las=2)
mtext("CE",3,line=1,at=1450,col="black")

# Plot ngrams
counter = 0
for (n in searchVector){
  counter = counter+1
  lineN = wordFreqRelRE(ngrams, n, spanVar)
  #lineN = smoothingmainVar
  lines(x=periods, y=lineN, col=colVec[counter], lwd=5)
}

legend('topleft', searchVector, 
       lty=1, col=colVec[1:length(searchVector)], bty='n', cex=.75, , lwd=5)

dev.off()
