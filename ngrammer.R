library(Cairo)

#"/Users/romanov/Documents/a.CombinedCollections/Ngrams/__useableMeta"
#useableMeta <- read.delim("~/Documents/a.CombinedCollections/Ngrams/__useableMeta_DatedOnly", header=FALSE)
#tempData = useableMeta[useableMeta$V7 <= 1300,]
#hist(tempData$V7)
#nrow(tempData)

fileName = "/Users/romanov/Documents/a.CombinedCollections/Ngrams/2_ngrammed_collections/1gram_NRM_NgramChronoTable_10up"
fileName = "/Users/romanov/Documents/a.CombinedCollections/Ngrams/2_ngrammed_collections/1gram_NRM_NgramChronoTable_10up_BW"
ngrams = read.delim(fileName, header=F)
colnames(ngrams) <- c("ngram", "freq", "0", "50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "1050", "1100", "1150", "1200", "1250", "1300", "1350", "1400", "1450")

#saveRDS(ngrams, file=paste0(fileName,".rds"))
#ngrams = readRDS("/Users/romanov/Documents/a.CombinedCollections/Ngrams/2_ngrammed_collections/1gram_NRM_NgramChronoTable.rds")

#freqs <- subset(ngrams, select = -c(ngram, freq))
#allFreqs = colSums(freqs)
#allFreqs = as.character(as.vector(allFreqs))

byPeriods = "/Users/romanov/Documents/a.CombinedCollections/Ngrams/2_ngrammed_collections/1gram_NRM_NgramChronoTable_Cummulative"
#write(allFreqs, byPeriods)
#write.csv(allFreqs, file = byPeriods,row.names=FALSE, na="", col.names=FALSE, sep=",")
#write.csv(allFreqs, file = byPeriods)
allFreqLoad = read.delim(byPeriods, header=F, sep=",")
allFreqs = allFreqLoad[,2]
allFreqs = allFreqs[2:30]

#test_matrix = as.matrix(ngrams)
#saveRDS(test_matrix, file=paste0(fileName,"_matrix.rds"))

wordFreqAbs <- function(freqTable, item, spanVar){
  x = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
  tempData = freqTable[freqTable$ngram == item,]
  tempData = as.character(as.vector(tempData))
  smoothingmainVar = predict(loess(tempData[3:31]~x, span=spanVar))
  smoothingmainVar[smoothingmainVar < 0] = 0
  return(smoothingmainVar)
}

wordFreqRel <- function(freqTable, item, spanVar){
  x = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
  #freqTable = ngrams
  #item = "AnbAnA"
  tempData = freqTable[freqTable$ngram == item,]
  tempData = as.integer(as.vector(tempData))
  tempData = tempData[3:31]/as.integer(as.vector(allFreqs))*100
  smoothingmainVar = predict(loess(tempData~x, span=spanVar))
  smoothingmainVar[smoothingmainVar < 0] = 0
  return(smoothingmainVar)
}

# testAbs = wordFreqAbs(ngrams, "انبانا")
# testRel = wordFreqRel(ngrams, "انبانا")

test = subset(ngrams, grepl("^(w?AxbrnA|w?Axbrny|AnA)$", ngram))
test = ngrams[ with(ngrams,  grepl("^(w?AxbrnA|Axbrny|AnA)$", ngram)), ]

test <- subset(ngrams, select = -c(ngram, freq))
testCum <- colSums(test)
#allFreqs = as.character(as.vector(allFreqs))

wordFreqRelRE <- function(freqTable, item, spanVar){
  x = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
  #freqTable = ngrams
  #item = "^(w?AxbrnA|Axbrny|AnA)$"
  tempData = subset(freqTable, grepl(item, ngram))
  tempData = subset(tempData, select = -c(ngram, freq))
  tempData = colSums(tempData)
  tempData = as.integer(as.vector(tempData))
  tempData = tempData[1:29]/as.integer(as.vector(allFreqs))*100
  #smoothingmainVar = tempData
  smoothingmainVar = predict(loess(tempData~x, span=spanVar))
  smoothingmainVar[smoothingmainVar < 0] = 0
  return(smoothingmainVar)
}

# 
# tempData1 = ngrams[ngrams$ngram == "انبانا",]
# tempData2 = ngrams[ngrams$ngram == "حدثنا",]
# tempData3 = ngrams[ngrams$ngram == "اخبرنا",]
# 
# try1 = as.character(as.vector(tempData1))
# try2 = as.character(as.vector(tempData2))
# try3 = as.character(as.vector(tempData3))
# 
# plot(x=x, y=try2[3:length(try2)], type="n", col="red")
# lines(x=x, y=try1[3:length(try1)], type="l", col="red")
# lines(x=x, y=try2[3:length(try2)], type="l", col="blue")
# lines(x=x, y=try3[3:length(try3)], type="l", col="black")

# par(mfrow=c(1,1))
# dev.off() # run to reset margins

# CairoPDF(file = ifelse(onefile, "Rplots.pdf","Rplot%03d.pdf"),
#width = 6, height = 6, onefile = TRUE, family = "Helvetica",
#title = "R Graphics Output", fonts = NULL, version = "1.1",
#paper = "special", encoding, bg, fg, pointsize, pagecentre)

CairoPDF("test.pdf", width=12, height=6, family="Geeza Pro", encoding="utf8")

#pdf(type="cairo", width=12, height=6)

x = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
xlabVarAH = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)
xlabVarCE = as.integer((xlabVarAH-xlabVarAH/33)+622)
# ce = int(ah)-(int(ah)/33)+622

colVec = c("red", "darkgreen", "blue", "orange", "maroon")
searchVector = c("^(w?Hdvn[Ay]|vn[A])$", "^(w?Axbrn[Ay]|An[A])$", "^(w?AnbAn[Ay]|n[A])$")
annotVector = c("haddatha-na and variants", "akhbara-na and variants", "anba'ana and variants")

searchVector = c("^(fy)$", "^(Cn)$", "^(mn)$")
annotVector = c("fi", "'an", "min")


spanVar = .25


plot(bty="n",
  x=x, xlim = c(0,1400), xaxt="n",
  y=wordFreqRelRE(ngrams, searchVector[1], spanVar), ylab="Word Frequencies (%)",
  type="n", col="red",
  xlab = "",
  main="")
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
  lines(x=x, y=lineN, col=colVec[counter], lwd=5)
}

legend('topleft', annotVector, 
       lty=1, col=colVec[1:length(annotVector)], bty='n', cex=.75, , lwd=5)

dev.off()
