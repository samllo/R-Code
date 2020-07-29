library(ROCR)
library(randomForest)
library(rpart)
library(Hmisc)
library(partykit)
library(party)
library(ggplot2)
setwd("/Users/sam/Documents/Masters")
getwd()

Data <-BPD21_8_16
Data

str(Data)
dim(Data)
names(Data)

#remive variables shrub forbes variety, PlotName and gpx 1, canopy cand row 1.10.8rosewood
Data <- Data [-c( 42,14, 13, 46, 62),-c(1,12,13,19)]
dim(Data)
names(Data)
Ba<- Data$Basal.Area 
Sd<- Data$Stem.Density
Ts<-Data$Tree.Species
sfd<-Data$Shrubs.Forbs.D
cor.test?
cor(Ba,Sd)
cor.test(Sd,Ba)#p-value = 2.885e-12
cor.test(Ba,Ts)#p-value = 0.08933
cor.test(Ts,Sd) #p-value = 0.00103
cor.test(Ba,sfd) # p-value = 0.08837

?c
## elephant data correlations
pairs(Data[,c(3:9)])
pairs(Data[,c(6:11)])

Data$Stem.Density <- NULL

Data

summary(Data$Class) 
# B BD BR CC  E  G  H  M  S 
# 1  1  3 47  1  1  4  1 27 
Data[Data=="BR" |Data=="BD" ]<-"B" 
Data


Data[Data=="E" |Data=="H"  ]<-"M" 
summary(Data$nearest.class ) 
# B BR CC  E  G  H  M  S 
# 4  0 19  0  8  0  7 48 
summary(Data$Class) 
# B BD BR CC  E  G  H  M  S 
# 5  0  0 47  0  1  0  6 27 
Data$Class<-sub("G","S",Data$Class )
summary(Data$Class) 
Data<-Data
# data object for random forest
Data.rF <- Data[ , c(1:15)]
names(Data.rF)
str(Data.rF)

x<-Data.rF[ , c(1,3:15)]
y<-as.factor(Data.rF[,2])
summary(y)
nrow(x)
names(x)
str(x)
x[,2] <-  as.factor(x[,2])
x[,4] <-  as.factor(x[,4])
x[,10] <-  as.factor(x[,10])
x[,12] <-  as.factor(x[,12])
x[,14] <-  as.factor(x[,14])
x[,12]<-droplevels.factor(x[,12])
str(x)
# running rf
set.seed(123)
bongrF1 <- randomForest(y ~ ., data = x,
                        importance=TRUE, ntree=10000, na.action=na.omit)
set.seed(321)
bongrF2 <- randomForest(y ~ ., data = x,
                        importance=TRUE, ntree=10000, na.action=na.omit)
set.seed(666)
bongrF3 <- randomForest(y ~ ., data = x,
                        importance=TRUE, ntree=10000, na.action=na.omit)


print(bongrF1);print(bongrF2); print(bongrF3) 
varImpPlot(bongrF1); varImpPlot(bongrF2); varImpPlot(bongrF3)  

#make combined imp plots
myrf<-bongrF1

names(myrf)
myrf$importance

myimp1 <- (
  ((myrf$importance[,3]-min(myrf$importance[,3]))/(max(myrf$importance[,3])-min(myrf$importance[,3]))) + 
    ((myrf$importance[,4]-min(myrf$importance[,4]))/(max(myrf$importance[,4]-min(myrf$importance[,4])))) )/2
myimp1 <- sort(myimp1)

myimp1


myrf<-bongrF2
myimp2 <- (
  ((myrf$importance[,3]-min(myrf$importance[,3]))/(max(myrf$importance[,3])-min(myrf$importance[,3]))) + 
    ((myrf$importance[,4]-min(myrf$importance[,4]))/(max(myrf$importance[,4]-min(myrf$importance[,4])))) )/2
myimp2 <- sort(myimp2)

myrf<-bongrF3
myimp3 <- (
  ((myrf$importance[,3]-min(myrf$importance[,3]))/(max(myrf$importance[,3])-min(myrf$importance[,3]))) + 
    ((myrf$importance[,4]-min(myrf$importance[,4]))/(max(myrf$importance[,4]-min(myrf$importance[,4])))) )/2
myimp3 <- sort(myimp3)

###

 n = c(1:13) 
 s = c("Elephant.Presence" , "Damage.Presence"  , "Elephant.Dung.Piles" , "Class" , "TRI"  , "Elephant.Damage"  , "nearest.class " , "Tree.Species" , "Distance.to.nearest.class" , "Distance.to.CC.edge"  , "Basal.Area", "Shrubs.Forbs.D"  , "Elevation")       
 b = c( 0.03880922, 0.09775254, 0.15966991 ,0.16784759, 0.19165870 ,0.20844614, 0.23777625 ,
        0.31916795, 0.34638311, 0.43064592
         ,0.47117328, 0.61811202 ,1.00000000) 

row3 <- data.frame(n,s,b)
col_headings <- c( "n","HabitatFactor","Variableimportance")
names(row3) <- col_headings
row3$HabitatFactor <- factor(row3$HabitatFactor, levels = row3$HabitatFactor [order(row3$Variableimportance)])
str(row3)
row3<-row3

#row3[,1] <-  as.factor(row3[,1])

ggplot(row3, aes(HabitatFactor, Variableimportance, colour= n)) +
  geom_point( aes(group=HabitatFactor), size=5) +
  coord_cartesian() +
  scale_color_gradientn(colours = rainbow(8))+
  geom_hline(yintercept = 0.4)+
   guides(col = guide_legend(nrow = 13))+
  geom_path () +
 
  theme_bw()

#ggplot(row3, aes(HabitatFactor, Variableimportance, colour= n)) +
 # geom_point( aes(group=HabitatFactor), size=5) +
  #scale_color_gradientn(colours = rainbow(8),guide = "colourbar")+
  #coord_cartesian() +
  #guides(col = guide_legend(nrow = 13))+
  #geom_hline(yintercept = 0.4)+
  #geom_path () +  
  #theme_bw()

####
##exploratory folowing rf

myimp1
ppi<-300
png(paste("FILENAME", "impy1.png", sep=""), width=3*ppi, height=13*ppi, res=ppi)
par(mfrow=c(3,1))
plot(x$Elevation~y)
plot(x$Shrubs.Forbs.D~y)
plot(x$Basal.Area~y)
plot(x$Canopy.H~y)
plot(x$Distance.to.CC.edge ~y)

cor.test(x$Elevation,y) square



#glm.out=glm(Bongo.Presence ~  Elevation  , family=binomial(logit), data=Data.rF)
#plot(Bongo.Presence ~  Elevation  ,data=Data.rF)
#curve(predict(glm.out,Elevation=x ,type="resp"),add=TRUE , data=Data.rF)


#lines(Data.rF$Bongo.Presence, glm.out$fitted, type="l", col="red")
plot(x$Elevation~y)

plot(x$Elevation,y)
plot(x$Shrubs.Forbs.D,y)
plot(x$Basal.Area,y)
plot(x$Canopy.H,y)
plot(x$Distance.to.CC.edge ,y)

elevationaverage <- subset(Data.rF, Bongo.Presence==1, 
                                   select=Elevation)
summary(elevationaverage )#mean 2341 # median 2396
EVTN<-2341
Emed<-2396

ppi<-300
png(paste("bongelevation", ".png", sep=""), width=6*ppi, height=4*ppi, res=ppi)
plot(Data.rF$Elevation, jitter(Data.rF$Bongo.Presence, factor=0.3),main=paste(""), yaxt='n',xlab="", ylab="")
abline(v=EVTN, lty=1, lwd=0.8, col="darkorange1")
abline(v=Emed, lty=1, lwd=0.8, col="dodgerblue2")
mtext("Presence =1 abscence =0", side=2, line=2)
mtext(paste("Elevation (m)"), side=1, line=2)
axis(2, at = seq(0, 1, by = 1), las=2)
legend(2850, 0.85, legend=c("Mean", "Median"),
       col=c("darkorange1", "dodgerblue2"), lty=1, cex=1)

dev.off()

asd<-(cm^{-2})

basalavg<- subset(Data.rF, Bongo.Presence==1, select=Basal.Area)
summary(basalavg)
MnBa<- 682.96 
ModBa <-722.98 

ppi<-300
png(paste("bongba", ".png", sep=""), width=6*ppi, height=4*ppi, res=ppi)

plot(Data.rF$Basal.Area, jitter(Data.rF$Bongo.Presence, factor=0.3),main=paste(""), yaxt='n',  xlab = expression("BA/ha" ~ (m^{2})), ylab="")
abline(v=MnBa, lty=1, lwd=0.8, col="darkorange1")
abline(v=ModBa, lty=1, lwd=0.8, col="dodgerblue2")
mtext("Presence =1 abscence =0", side=2, line=2)
axis(2, at = seq(0, 1, by = 1), las=2)
legend(1700, 0.85, legend=c("Mean", "Median"),
       col=c("darkorange1", "dodgerblue2"), lty=1, cex=1)

dev.off()


####
summary(Data.rF$Elevation)
summary(Data.rF$Canopy.H)

normCHEV<-data.frame(2390 ,18.26)

names(Data.rF)
ElevCH<- Data.rF[ , c(1,2,9)]

ECHPRES<- subset(ElevCH, Bongo.Presence==1, 
                 select=c(Elevation, Canopy.H))
ECHABS<- subset(ElevCH, Bongo.Presence==0, 
                select=c(Elevation, Canopy.H))

chavrg<- subset(Data.rF, Bongo.Presence==1, select=Canopy.H)
summary(chavrg)


MNCH<-21.30 
plot(ECHABS, pch=20)
points(ECHPRES,col="red" )
points(normCHEV,pch=20,col="blue" )
abline(h=MNCH, lty=1, lwd=0.8, col="darkorange1")
abline(v=EVTN, lty=1, lwd=0.8, col="dodgerblue2")


?mtext

plot(x$Distance.to.nearest.class,y)
plot(x$TRI,y)
plot(x$Class,y)
plot(x$Canopy.H,y)
### works but simplqplot(seq_along(ggimp1$myimp1), ggimp1$myimp1)
myimp1
#########
####elevation canopy hieght graphs
#########
var.test(x$Elevation~y)#
var.test(x$Shrubs.Forbs.D  ~y)
var.test(x$Basal.Area~y)
var.test(x$Canopy.H~y)
var.test(x$Distance.to.CC.edge ~y)#
wilcox.test(x$Elevation~y)
wilcox.test(x$Shrubs.Forbs.D  ~y)
wilcox.test(x$Basal.Area~y)
wilcox.test(x$Canopy.H~y)
wilcox.test(x$Distance.to.CC.edge ~y)#
plot(x$Elevation~y)
plot(x$Distance.to.CC.edge~y)
plot(x$Shrubs.Forbs.D~y)
plot(x$Basal.Area~y)
plot(x$Canopy.H~y)
#outline=FALSE
y<-as.factor(y)
##############//////////////////////////////////////////////////////////////////
evtnB1<- subset(Data.rF, Bongo.Presence==1, 
                 select=c(Elevation))

evtnB0<- subset(Data.rF, Bongo.Presence==0, 
                select=c(Elevation))
summary(evtnB1)#2341
summary(evtnB0)#2402
sapply(evtnB1,sd)# 104.2943
sapply(evtnB0,sd)#341.5629 

b1min<-(2341-104.2943)
b1plus<-(2341+104.2943)
b0min<-(2402-341.5629 )
b0plus<-(2402+341.5629 )

ppi<-300
png(paste("bongelvtn BXP", ".png", sep=""), width=6*ppi, height=6.5*ppi, res=ppi)

plot(x$Elevation~y,ylim=c(1800, 2800),xlim=c(0, 3),col=c('dodgerblue', 'darkorange1'),alpha=20
     ,main=paste("Bongo Occurrence Elevations"),
     xlab="", ylab="", pch=18 )
grid(NA ,NULL, lwd = 1.5,lty=3, col="grey37") 
plot(x$Elevation~y,col=c('dodgerblue', 'darkorange1'),alpha=20
,whisklwd=3,staplelwd=3,whisklty=1,
xlab="", ylab="", add=T)
mtext("Elevation (m)", side=2, line=2)
mtext(paste("Bongo Abscence=0/Presence=1"), side=1, line=2)
arrows(1, b0min ,1, b0plus, length=0.12, angle=90, code=3, lwd=3,col="red")
arrows(1, 2402 ,1, 2403, length=0.15, angle=90, code=3, lwd=3,col="red")
arrows(2, b1min ,2, b1plus, length=0.12, angle=90, code=3, lwd=3,col="red")
arrows(2, 2341 ,2, 2342, length=0.15, angle=90, code=3, lwd=3,col="red")
legend(2, 2700, legend=c("Mean/SD"),
       col=c("red"), lty=1,lwd=3, cex=1)


dev.off()

##/////////////////////////////////////////////////////////////////////////////////
#p= seq(1,length(ggimp1$myimp1))
#testgg= ggimp1$myimp1

#ggp <- data.frame(p,testgg)

#g <- ggplot(ggp) + geom_point(aes(x=p,y=testgg),label = rownames(ggp))
#g + geom_text()


###this is the code i changed slightly for a merged implot but still couldnt get it to work 
###ive put your orginal below my attempt
#############################################
#### code for importance 
#you made need to tweak this for your random forest objectg
#put your random forest object into var called "myrf"
names(myimp1)
names(M1N)
M1N<-rbind(1:14)
M1N<-as.data.frame(M1N)
colnames(M1N) <- c("Elephant Presence"     ,    "Damage Presence"    ,       "Class"   ,                 
                       "Elephant Dung Piles"   ,    "Elephant Damage"   ,        "TRI"      ,                
                       "Nearest Class"     ,        "Tree Species"    ,          "Distance To Nearest Edge",
                       "Distance to CC Edge" ,      "Canopy Height "         ,         "Basal Area"           ,    
                       "Shrub Density"      ,      "Elevation" )
names(myimp1)
names(myimp2)

M2N<-rbind(1:14)
M2N<-as.data.frame(M2N)
colnames(M2N) <- c("Elephant Presence"     ,    "Damage Presence"    ,       "Class"   ,                 
                   "Elephant Dung Piles"   ,    "Elephant Damage"   ,        "TRI"      ,                
                   "Nearest Class"     ,        "Tree Species"    ,          "Distance To Nearest Edge",
                     "Canopy Height "     ,  "Distance to CC edge"    ,         "Basal Area"           ,    
                   "Shrub Density"      ,      "Elevation" )

names(myimp3)
M3N<-rbind(1:14) 
M3N<-as.data.frame(M3N)
colnames(M3N) <- c("Elephant Presence"     ,    "Damage Presence"    ,       "Class"   ,                 
                   "Elephant Dung Piles"   ,    "Elephant Damage"   ,        "TRI"      ,                
                   "Nearest Class"     ,        "Tree Species"    ,          "Distance To Nearest Edge",
                   "Canopy Height "     ,  "Distance to CC edge"    ,         "Basal Area"           ,    
                   "Shrub Density"      ,      "Elevation" )

names(M2N)
names(M3N)
#par(mfrow=c(3,1))

#print to file
?plot
#myimp1

ppi<-300
png(paste("FILENAME", "impy1.png", sep=""), width=3*ppi, height=13*ppi, res=ppi)
par(mfrow=c(3,1))
threshold <- 0.4
fresh<-0.0
offset <- c(.2,.2,.1,.21,.19,.09,.17,.16,.26,-.15,-.11,-.08,-.1,.13)
plot(myimp1, main=paste("Variable Importance Run 1"),
     xlab="", ylab="", pch=18, col=554,
     ylim=c(-.3,1.1), xlim=c(0.5,14.5), cex=1.4, xaxt="n")
abline(h=threshold, lty=3, lwd=1.5, col="darkorange1")
abline(h=fresh, lty=1, lwd=0.6, col="black")
mtext("Importance", side=2, line=2)
mtext(paste(""), side=1, line=1)
text(myimp1-offset, names(M1N), cex=0.65, pos=3 , col="gray48", srt=90)



#myimp2
offset <- c(.2,.2,.1,.21,.19,.09,.17,.16,.26,-.11,-.15,-.08,-.1,.13)
plot(myimp2, main=paste("Variable Importance Run 2"),
     xlab="", ylab="", pch=18, col=554,
     ylim=c(-.3,1.1), xlim=c(0.5,14.5), cex=1.4, xaxt="n")
abline(h=threshold, lty=3, lwd=1.5, col="darkorange1")
abline(h=fresh, lty=1, lwd=0.6, col="black")
mtext("Importance", side=2, line=2)
mtext(paste(""), side=1, line=1)
text(myimp2-offset, names(M2N), cex=0.65, pos=3 , col="gray48", srt=90)

#myimp3

offset <- c(.2,.2,.1,.21,.19,.09,.17,.16,.26,-.11,-.15,-.08,-.1,.13)
plot(myimp3, main=paste("Variable Importance Run 3"),
     xlab="", ylab="", pch=18, col=554,
     ylim=c(-.3,1.1), xlim=c(0.5,14.5), cex=1.4, xaxt="n")
abline(h=threshold, lty=3, lwd=1.5, col="darkorange1")
abline(h=fresh, lty=1, lwd=0.6, col="black")
mtext("Importance", side=2, line=2)
mtext(paste(""), side=1, line=1)
text(myimp3-offset, names(M3N), cex=0.65, pos=3 , col="gray48", srt=90)

dev.off()

#####
#Fig
ppi<-300

?plot(myimp1, main=paste("TITLE",
                       xlab="", ylab="", pch=17, col="blue",
                       ylim=c(-.2,1.1), xlim=c(0.5,8.5), cex=1.3, xaxt="n"),
     abline(h=threshold, lty=3, lwd=1, col="green4"),
     mtext("Y AXIS TITLE", side=2, line=2),
     mtext(paste("X AXIS TITLE"), side=1, line=1),
     text(myimp, names(myimp), cex=0.7, pos=1, col="red", srt=30)
)
dev.off()

########## redone averaqed var imp plot
print(myimp3)

AvgIp<-avgimp
AvgIp <- sort(AvgIp$V2)
SDip<-sdimp
SDip <-SDip$V2
minus<-c(AvgIp-SDip)
plus<-c(AvgIp+SDip)


ppi<-300
png(paste("SDimps", "impy1.png", sep=""), width=6*ppi, height=6.5*ppi, res=ppi)

threshold <- 0.4
fresh<-0.0
offset <- c(.22,.22,.1,.23,.21,.09,.18,.17,.28,-.18,-.13,-.09,-.12,.13)
plot(AvgIp, main=paste("RF Mean Variable Importance"),
     xlab="", ylab="", pch=18, col=554,
     ylim=c(-.3,1.1), xlim=c(0.5,14.5), cex=1.4, xaxt="n")
abline(h=threshold, lty=3, lwd=1.5, col="darkorange1")
abline(h=fresh, lty=1, lwd=0.6, col="black")
mtext("Mean Importance (+/- SD)", side=2, line=2)
mtext(paste(""), side=1, line=1)
text(AvgIp-offset, names(M1N), cex=0.7, pos=3 , col="gray28", srt=90)
arrows(c(1:14), minus,c(1:14), plus, length=0.08, angle=90, code=3, lwd=1.7,col="gray37")

dev.off()










##################
#### CI TREE  #### names(x)
##################
depvar<-y
subset.ci<-x
names(x)
subset.ci <-subset.ci[,c(1,8,7,9,11)]
#subset.ci <-subset.ci[,c(1,7,9,11)]
#subset.ci <-subset.ci[,c(1,8,7)]
subset.ci <-x
attach(subset.ci)
#citree<-ctree(depvar~.,
#           data=subset.ci,
#         controls=ctree_control(mincriterion=0.1))
#plot(citree)
myctree <-ctree(depvar~.,
                data=subset.ci,
                controls=ctree_control(mincriterion=0.5, maxdepth=4))
plot(myctree)
#Plot the inference tree
ppi<-300
png(paste("ME",".CI2.png", sep=""), width=5*ppi, height=4*ppi, res=ppi)
plot(myctree, type="simple", 
     main=paste("n=subsample size, y=(i,j); bongo presence: i=0, j=1"),
     inner_panel=node_inner(myctree,
                            abbreviate = FALSE,           # short variable names
                             pval = TRUE,                 # no p-values
                            id = FALSE),                  # no id of node
     terminal_panel=node_terminal(myctree, 
                                  abbreviate = TRUE,
                                  digits = 2,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE))
dev.off()
hist(Data$Stem.Density)

#############################################
########################################response curves
elerc<-NDVIeleRC
rainrc<-ndviRainrc
plot(elerc)

plot(a1erc$y~a1erc$x)

ppi<-300
png(paste("bongRCs", ".png", sep=""), width=6*ppi, height=7.5*ppi, res=ppi)
par(mfrow=c(2,1))

plot(elerc$y~elerc$x, main=paste("Response of Bongo to Elevation"),
     ylab="",xlab="", pch=1, 
      xlim=c(2000,3500),ylim=c(0,0.03), cex=0.01)
grid(NULL,NULL, lwd = 1.5,lty=3, col="grey37") 

abline(v=2250,lwd = 1.5,lty=3, col="grey37") 
abline(v=2750,lwd = 1.5,lty=3, col="grey37") 
abline(v=3250,lwd = 1.5,lty=3, col="grey37")
mtext("Logistic Output", side=2, line=3)
mtext("(Probability of Presence)", side=2, line=2)
mtext(paste("Elevation (m)"), side=1, line=2)

lines(a1erc$y~a1erc$x, lwd=1, col="dodgerblue")
lines(a2erc$y~a2erc$x, lwd=1, col="dodgerblue")
lines(a3erc$y~a3erc$x, lwd=1, col="dodgerblue")
lines(a4erc$y~a4erc$x, lwd=1, col="dodgerblue")
lines(a5erc$y~a5erc$x, lwd=1, col="dodgerblue")
lines(a6erc$y~a6erc$x, lwd=1, col="dodgerblue")
lines(a7erc$y~a7erc$x, lwd=1, col="dodgerblue")
lines(a8erc$y~a8erc$x, lwd=1, col="dodgerblue")
lines(a9erc$y~a9erc$x, lwd=1, col="dodgerblue")
lines(a10erc$y~a10erc$x, lwd=1, col="dodgerblue")
lines(a11erc$y~a11erc$x, lwd=1, col="dodgerblue")
lines(a12erc$y~a12erc$x, lwd=1, col="dodgerblue")
lines(a13erc$y~a13erc$x, lwd=1, col="dodgerblue")
lines(a14erc$y~a14erc$x, lwd=1, col="dodgerblue")
lines(a15erc$y~a15erc$x, lwd=1, col="dodgerblue")

lines(elerc$y~elerc$x, lwd=4, col="red")




plot(rainrc$y~rainrc$x, main=paste("Response of Bongo to Rainfall"),
     ylab="",xlab="", pch=1, 
     xlim=c(1000,2000),ylim=c(0,0.035), cex=0.01)
grid(NULL,NULL, lwd = 1.5,lty=3, col="grey37") 


mtext("Logistic Output", side=2, line=3)
mtext("(Probability of Presence)", side=2, line=2)
mtext(paste("Annual Rainfall (mm)"), side=1, line=2)

lines(r1$y~r1$x, lwd=1, col="dodgerblue")
lines(r2$y~r2$x, lwd=1, col="dodgerblue")
lines(r3$y~r3$x, lwd=1, col="dodgerblue")
lines(r4$y~r4$x, lwd=1, col="dodgerblue")
lines(r5$y~r5$x, lwd=1, col="dodgerblue")
lines(r6$y~r6$x, lwd=1, col="dodgerblue")
lines(r7$y~r7$x, lwd=1, col="dodgerblue")
lines(r8$y~r8$x, lwd=1, col="dodgerblue")
lines(r9$y~r9$x, lwd=1, col="dodgerblue")
#lines(r10$y~r10$x, lwd=1, col="dodgerblue")
#lines(r11$y~r11$x, lwd=1, col="dodgerblue")
lines(r12$y~r12$x, lwd=1, col="dodgerblue")
#lines(r13$y~r13$x, lwd=1, col="dodgerblue")
lines(r14$y~r14$x, lwd=1, col="dodgerblue")
lines(r15$y~r15$x, lwd=1, col="dodgerblue")

lines(rainrc$y~rainrc$x, lwd=4, col="red")

dev.off()
