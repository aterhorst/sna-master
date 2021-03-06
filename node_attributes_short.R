#####################################################
#                                                   #
#            R script to read in and clean          #
#               node attribute data                 #
#                                                   #
#####################################################

# read node attribute data

nodes <- read_excel("surveydata.xlsx", sheet = 1)
fn <- "temp.csv"
write.csv(nodes, file = fn, row.names = FALSE)
nodes <- read.csv(fn)
if (file.exists(fn)) file.remove(fn) # clean up garbage

# remove strings from numeric data

nodes$Age <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Age)) # extract years only
nodes$Experience <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Experience)) # extract years only
nodes$Tenure <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Tenure)) # extract years only
nodes$Location <- as.numeric(gsub("([0-9]*).*","\\1",nodes$Location)) # extract numeric postcode only

# fix column names

colnames(nodes)[10] <- "Occupation"

# totalize scale items

## first we need to reverse scores where necessary

nodes$Openness2 <- 10 - nodes$Openness2 # openness scale item 2
nodes$Conscientiousness1 <- 10 - nodes$Conscientiousness1 # conscientious scale item 1
nodes$Agreeableness2 <- 10 - nodes$Agreeableness2 # agreeableness scale item 2

## next we need to aggregate 12 scales from 32 items

## TIPI personality

nodes$Openness <- round(rowMeans(subset(nodes, select = c(Openness1,Openness2)), na.rm = TRUE)/10, digits = 2) # openness
nodes$Conscietiousness <- round(rowMeans(subset(nodes, select = c(Conscientiousness1,Conscietiousness2)), na.rm = TRUE)/10, digits = 2) # consceintiousness
nodes$Agreeableness <- round(rowMeans(subset(nodes, select = c(Agreeableness1,Agreeableness2)), na.rm = TRUE)/10, digits = 2) # agreeableness

##

nodes$Competence <- round(rowMeans(subset(nodes, select = c(Competence1,Competence2,Competence3)), na.rm = TRUE)/10, digits = 2) # job competence

##

nodes$SelfDetermination <- round(rowMeans(subset(nodes, select = c(SelfDetermination1,SelfDetermination2,SelfDetermination3)), na.rm = TRUE)/10, digits = 2) # self determination

## creative self-efficacy
 
nodes$Creativity <- round(rowMeans(subset(nodes, select = c(Creativity1,Creativity2,Creativity3,Creativity4)), na.rm = TRUE)/10, digits = 2) # creativie self-efficacy

## work motivation

nodes$Amotivation <- round(rowMeans(subset(nodes, select = c(Amotivation1,Amotivation2,Amotivation3)), na.rm = TRUE)/10, digits = 2) # amotivation
nodes$ExtrinsicRegulationSocial <- round(rowMeans(subset(nodes, select = c(ExtrinsicRegulationSocial1,ExtrinsicRegulationSocial2,ExtrinsicRegulationSocial3)), na.rm = TRUE)/10, digits = 2) # extrinsic regulation - social
nodes$ExtrinsicRegulationMaterial <- round(rowMeans(subset(nodes, select = c(ExtrinsicRegulationMaterial1,ExtrinsicRegulationMaterial2,ExtrinsicRegulationMaterial3)), na.rm = TRUE)/10, digits = 2) # extrinsic regulation material
nodes$IntrojectedRegulation <- round(rowMeans(subset(nodes, select = c(IntrojectedRegulation1,IntrojectedRegulation2,IntrojectedRegulation3,IntrojectedRegulation4)), na.rm = TRUE)/10, digits = 2) # introjected regulation
nodes$IdentifiedRegulation <- round(rowMeans(subset(nodes, select = c(IdentifiedRegulation1,IdentifiedRegulation2,IdentifiedRegulation3)), na.rm = TRUE)/10, digits = 2) # identified regulation
nodes$IntrinsicMotivation <- round(rowMeans(subset(nodes, select = c(IntrinsicMotivation1,IntrinsicMotivation2,IntrinsicMotivation3)), na.rm = TRUE)/10, digits = 2) # intrinsic motivation

# remove unwanted columns now we have totalized scores

# column_names <- as.data.frame(names(nodes)) # get numbered list of columns
# column_names # lists column names with corresponding column numbers
node_summary <- subset(nodes, select=-c(3:4,13:51)) # drop unwanted columns using column numbers
            

