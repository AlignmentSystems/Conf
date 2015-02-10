#gc()

ConferenceWorthwhile <-function(Tolerance=0){

#Get raw dummy data for conference A
A.raw<-read.delim("A.txt",header=TRUE,sep=",")
#Get raw dummy data for conference B
B.raw<-read.csv("B.csv", header=TRUE)
#
a<-A.raw
b<-B.raw

#Manipulate a and b to get the names to a common format

colnames(a)[(names(a) == "Name")] <- "Person.Name"
colnames(a)[(names(a) == "Job.Title")] <- "Person.Title"
colnames(a)[(names(a) == "Company")] <- "Person.Organisation"
colnames(a)[(names(a) == "Topic")] <- "Person.Topic"

colnames(b)[(names(b) == "Speaker")] <- "Person.Name"
colnames(b)[(names(b) == "Title")] <- "Person.Title"
colnames(b)[(names(b) == "Firm")] <- "Person.Organisation"
colnames(b)[(names(b) == "Subject")] <- "Person.Topic"

#Now, we want to work out how many speakers are unique
a.uniqueNames <-as.vector(unique(unlist(a$Person.Name)))
b.uniqueNames <-as.vector(unique(unlist(b$Person.Name)))

#Now, we look for duplication - where a speaker speaks more than once
#at one conference.
SpeakingMoreThanOnceinA = length(a$Person.Name)-length(a.uniqueNames)
SpeakingMoreThanOnceinB = length(b$Person.Name)-length(b.uniqueNames)

#spum is Speaker Unique matrix
spum<-as.vector(unique(unlist(list(a.uniqueNames,b.uniqueNames))))
spum<-as.matrix(spum)

colnames(spum)<-c("Person.Name")

a.freq<-tapply(a$Person.Name, a$Person.Name, length)
a.freq<as.vector(a.freq)
a.freq<-as.matrix(a.freq)
#a.freq<-as.matrix(as.vector(tapply(a$Person.Name, a$Person.Name, length)))


b.freq<-tapply(b$Person.Name, b$Person.Name, length)
b.freq<as.vector(b.freq)
b.freq<-as.matrix(b.freq)
#b.freq<-as.matrix(as.vector(tapply(b$Person.Name, b$Person.Name, length)))


dummy<-merge(spum,a.freq, by.x="Person.Name",by.y=0,all=TRUE )
dummy<-merge(dummy,b.freq, by.x="Person.Name",by.y=0,all=TRUE )

#A little inelegant, adding Person.Name again to get the other two names added...
colnames(dummy)<-c("Person.Name","in.a","in.b")

dummy<-within(dummy,{
		both<-1*(!is.na(in.a)&!is.na(in.b)) 
		}
)
SameSpeaker<-sum(dummy$both)

#SameSpeakerAndSpeech<-0
#SameFirms<-0
#SameTopics<-0

return(list(
SpeakersInA = length(a.uniqueNames)
,
SpeakersInB = length(b.uniqueNames)
,
UniqueToA=sum(is.na(dummy$in.b))
,
UniqueToB=sum(is.na(dummy$in.a))
,
SameSpeaker=SameSpeaker
,
PeopleSpeakingMoreThanOnceinA=SpeakingMoreThanOnceinA
,
PeopleSpeakingMoreThanOnceinB=SpeakingMoreThanOnceinB
#,
#SameSpeakerAndSpeech=SameSpeakerAndSpeech,
#SameFirms=SameFirms,
#SameTopics=SameTopics
))
}

ConferenceWorthwhile() 

