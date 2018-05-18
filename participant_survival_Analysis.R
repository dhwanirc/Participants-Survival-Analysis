df <- read.csv("participants_cleaned.csv",na.strings = c("NA","NaN","","?","N/A"))

cols=c("Contact.ID" ,"Fiscal.Year" ,"Team.Division","Is.Prior.Participant","Emails.Sent","Total.of.All.Confirmed.Gifts...","Participant.Occupation","Participant.Connection.to.MS","Participant.Gender")
participants<-df[,cols]



participants$Participant.Gender[is.na(participants$Participant.Gender)]="Male"

participants$Is.Prior.Participant<-as.character(participants$Is.Prior.Participant)
participants$Is.Prior.Participant[is.na(participants$Is.Prior.Participant)]="No"
participants$Is.Prior.Participant<-as.factor(participants$Is.Prior.Participant)


participants$Participant.Connection.to.MS<-as.character(participants$Participant.Connection.to.MS)
participants$Participant.Connection.to.MS[is.na(participants$Participant.Connection.to.MS)]="Friend has MS"
participants$Participant.Connection.to.MS<-as.factor(participants$Participant.Connection.to.MS)


participants$Participant.Occupation<-as.character(participants$Participant.Occupation)
participants$Participant.Occupation[is.na(participants$Participant.Occupation) & participants$Team.Division=="Corporate"]="Engineering"
participants$Participant.Occupation[is.na(participants$Participant.Occupation) & participants$Team.Division=="Family and Friends"]="Healthcare"

participants$Participant.Occupation<-as.factor(participants$Participant.Occupation)

participants <- participants[complete.cases(participants),]
summary(participants)

library(data.table)
setDT(participants)


participants$Team.Division <- as.factor(participants$Team.Division)
participants$Is.Prior.Participant <- as.factor(participants$Is.Prior.Participant)
participants$Participant.Occupation <- as.factor(participants$Participant.Occupation)
participants$Participant.Connection.to.MS <- as.factor(participants$Participant.Connection.to.MS)
participants$Participant.Gender <- as.factor(participants$Participant.Gender)

getmode <- function(v) {
  levels(v)[which.max(table(v))]
}

participants_summary<-participants[,.(FirstYear=min(Fiscal.Year),LastYear=max(Fiscal.Year),Team.Division=getmode(Team.Division),Is.Prior.Participant=getmode(Is.Prior.Participant),Total.Emails.Sent=sum(Emails.Sent),SUM_of_Total.of.All.Confirmed.Gi=sum(Total.of.All.Confirmed.Gifts...),Participant.Occupation=getmode(Participant.Occupation),Participant.Connection.to.MS=getmode(Participant.Connection.to.MS),Participant.Gender.y=getmode(Participant.Gender)), by=Contact.ID]

participants_summary$lenfol=participants_summary$LastYear- participants_summary$FirstYear+1
participants_summary$fstat=0
participants_summary$fstat[participants_summary$LastYear==2017]=1

participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Friend has MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Relative has MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Sibling has MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Spouse has MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="I have a Friend or Co-worker with MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="I have a Friend of Co-worker with MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Relative: Parent of person with MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Relative: Sibling of person with MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Parent has MS" ] <- "Friend/Relative has MS"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Child has MS" ] <- "Friend/Relative has MS"


participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="None" ] <- "Other"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Caregiver of Person with MS" ] <- "Other"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Care Manager of Person with MS" ] <- "Other"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Possible MS" ] <- "Other"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Blank" ] <- "Other"
participants_summary$Participant.Connection.to.MS[participants_summary$Participant.Connection.to.MS=="Blank" ] <- "Other"


participants_summary$Team.Division <- as.character(participants_summary$Team.Division)
participants_summary$Team.Division[participants_summary$Team.Division == 'Organization/Club'] <- 'Other'
#participants_summary$Team.Division[participants_summary$Team.Division == 'Club/Organization'] <- 'Organization/Club'
participants_summary$Team.Division[participants_summary$Team.Division == 'School'] <- 'Other'
participants_summary$Team.Division <- as.factor(participants_summary$Team.Division)


write.csv(participants_summary,file = "Survival_participantsnew.csv")

library(survival)
library(ggplot2)
library(ggkm)
detach(data)
attach(participants_summary)


#~1 means single curve for all the data
km1 <- survfit(Surv(lenfol,fstat)~1)
summary(km1)
plot(km1, xlab = "Time", ylab = "Survival Probability")


km2 <- survfit(Surv(lenfol,fstat)~participants_summary$Participant.Gender.y)
summary(km2)
plot(km2, xlab = "Time", ylab = "Survival Probability")
ggplot(participants_summary, aes(time = lenfol, status = fstat, color = factor(participants_summary$Participant.Gender))) + geom_km()


km3 <- survfit(Surv(lenfol,fstat)~participants_summary$Team.Division)
summary(km3)
plot(km3, xlab = "Time", ylab = "Survival Probability")
ggplot(participants_summary, aes(time = lenfol, status = fstat, color = factor(participants_summary$Team.Division))) + geom_km()+ labs( colour = "Team Division")

km4 <- survfit(Surv(lenfol,fstat)~participants_summary$Is.Prior.Participant)
summary(km4)
plot(km4, xlab = "Time", ylab = "Survival Probability")
ggplot(participants_summary, aes(time = lenfol, status = fstat, color = factor(participants_summary$Is.Prior.Participant))) + geom_km() + labs( colour = "Is prior participant")

km5 <- survfit(Surv(lenfol,fstat)~participants_summary$Participant.Connection.to.MS)
summary(km5)
plot(km5, xlab = "Time", ylab = "Survival Probability")
ggplot(participants_summary, aes(time = lenfol, status = fstat, color = factor(participants_summary$Participant.Connection.to.MS))) + geom_km()+ labs( colour = "Participant connection ot MS")



x = data.frame(surv =as.double(),
               time = as.integer())[5,2]


x$surv = km3$surv[1:15]
x$time = km3$time[1:15]
x<-as.data.frame(x)
write.csv(x,file="Team_division.csv")
