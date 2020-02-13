# SET2-Question 1
awardsdataset=read.csv(file.choose(),sep=",",header = T)
awardsdataset
dim(awardsdataset)
awardsdataset
awardsdataset[awardsdataset$BHARAT.RATNA>mean(awardsdataset$BHARAT.RATNA),'NOBEL.PRIZE']
awardsdataset$decade=0

#Case1
awardsdataset
awardsdataset$decade=NULL
for(i in 2:61){
  if(awardsdataset$YEAR>=1910 & awardsdataset$YEAR<=1920){
    awardsdataset$decade='Decade 1'
  }else if(awardsdataset$YEAR>=1920 & awardsdataset$YEAR<=1930){
    awardsdataset$decade='Decade 2'
  }else if(awardsdataset$YEAR>=1930 & awardsdataset$YEAR<=1940){
    awardsdataset$decade='Decade 3'
  }
}
#Case 2
p=NULL
olympicyear=awardsdataset[awardsdataset$NOBEL.PRIZE>0,'YEAR']
olympicyear # Shows in which year noble pize is given
for (i in 1:7){p[i]=olympicyear[i+1]-olympicyear[i]}
p #P is the diffrence of years in getting noble prize
#To check in how many years India will get noble Prize again
mean(p)#Approx after 14 years from 2017 ie 2031 or 2032, India will be winning noble prize

#Case3
#For PadmaviBhusan for each party
britishvibhusan=sum(awardsdataset[which(awardsdataset$year>=1947 & awardsdataset$GOVT=='BRITISH'),'PADMA.VIBHUSHAN'])
britishvibhusan
incvibhusan=sum(awardsdataset[which(awardsdataset$GOVT=='INC'),'PADMA.VIBHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
jpvibhusan=sum(awardsdataset[which(awardsdataset$GOVT=='JP'),'PADMA.VIBHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
jdvibhusan=sum(awardsdataset[which(awardsdataset$GOVT=='jd'),'PADMA.VIBHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
sjpvibhusan=sum(awardsdataset[which(awardsdataset$GOVT=='SJP'),'PADMA.VIBHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
(bjpvibhusan=sum(awardsdataset[which(awardsdataset$GOVT=='BJP'),'PADMA.VIBHUSHAN']))/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
(padmavibhusan=c(britishvibhusan,incvibhusan,jpvibhusan,jdvibhusan,sjpvibhusan,bjpvibhusan))/nrow(awardsdataset[awardsdataset$GOVT=='INC',])

#For PadmaBhusan for each party
britishbhushan=sum(awardsdataset[which(awardsdataset$GOVT=='BRITISH'),'PADMA.BHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
incbhusan=sum(awardsdataset[which(awardsdataset$GOVT=='INC'),'PADMA.BHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
jpbhusan=sum(awardsdataset[which(awardsdataset$GOVT=='JP'),'PADMA.BHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
jdbhusan=sum(awardsdataset[which(awardsdataset$GOVT=='jd'),'PADMA.BHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
sjpbhusan=sum(awardsdataset[which(awardsdataset$GOVT=='SJP'),'PADMA.BHUSHAN'])/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
(bjpbhusan=sum(awardsdataset[which(awardsdataset$GOVT=='BJP'),'PADMA.BHUSHAN']))/nrow(awardsdataset[awardsdataset$GOVT=='INC',])
padmabhusan=c(britishbhushan,incbhusan,jpbhusan,jdbhusan,sjpbhusan,bjpbhusan)
padmabhusan

Party=c('British','INC','JP','JD','SJP','BJP')
partymax=data.frame(party=Party,padmavibhusan=padmavibhusan,padmabhusan=padmabhusan,stringsAsFactors = F)
partymax

partymax[which.max(partymax$padmavibhusan),'party'] #For padmavibhusan o/p: BJP
partymax[which.max(partymax$padmabhusan),'party'] #For padmavibhusan    o/p: BJP

#Case4
cor(awardsdataset$PADMA.VIBHUSHAN,awardsdataset$PADMA.BHUSHAN)
#the corelational coefficient value is 0.55, which indicates that these column are
#moderately relative
cor(awardsdataset$PADMA.VIBHUSHAN,awardsdataset$BHARAT.RATNA)#the corelational coefficient value is 0.55, which indicates that these column are
#least related to each other

#Case5
#Total Awards won in 19th century
awards19=sum(awardsdataset[which(awardsdataset$YEAR<2000),'TOTAL'])
awards19

#Total Awards won in 20th century
awards20=sum(awardsdataset[which(awardsdataset$YEAR>=2000),'TOTAL'])
awards20

#%change from 19th to 20th century
Totalchange=((awards19-awards20)/awards19)*100
Totalchange

#Set2- Question 2
Days=c('MON','TUE','WED','THU','FRI','SAT','SUN')
weeklydata=data.frame(Earnings=c(500,500,700,800,1000,0,0),Spendings=c(100,200,200,300,500,500,300),stringsAsFactors = F)
saving=weeklydata$Earnings-weeklydata$Spendings
weeklydata=cbind(weeklydata,saving)
rownames(weeklydata)=Days
weeklydata
weeklydata['MON',] #It will show the all 3 values spending,earning and saving of Monday
weeklydata['TUE','saving'] # It shows the saving on monday

