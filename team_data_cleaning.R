teams<-read.csv("2013-2017 Bike Teams.csv",stringsAsFactors = FALSE)
library(anchors)
teams2  <- replace.value(teams,teams$Team.Division,from=c("Bike Shop","Bike Shops","Bike Club"), to="Bike Shop")
teams$Team.Division[teams$Team.Division=="Bike Shop" ] <- "Bike Club"
teams$Team.Division[teams$Team.Division=="Bike Shops"] <- "Bike Club"

teams$Team.Division[teams$Team.Division=="Family/Friends"] <- "Family and Friends"
teams$Team.Division[teams$Team.Division=="Frien`s and Family"] <- "Family and Friends"
teams$Team.Division[teams$Team.Division=="Friend and Family"] <- "Family and Friends"
teams$Team.Division[teams$Team.Division=="Friends and Family"] <- "Family and Friends"
teams$Team.Division[teams$Team.Division=="Ohana and Friends"] <- "Family and Friends"
teams$Team.Division[teams$Team.Division=="Ohana "] <- "Family and Friends"

teams$Team.Division[teams$Team.Division=="Open"] <- "Open Team"

teams$Team.Division[teams$Team.Division=="Civic Team"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Organization"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Organization (Clubs, Civic Groups, etc.)"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Organization (Clubs; Civic Groups; Place of Worship; etc.)"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Organization (Clubs; Civic Groups; etc.)"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Religious"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Place of Worship"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Place of worship"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Place of worship"] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Association" ] <- "Club/Organization"
teams$Team.Division[teams$Team.Division=="Bike Club" ] <- "Club/Organization"

teams$Team.Division[teams$Team.Division=="Corporation"] <- "Corporate"
teams$Team.Division[teams$Team.Division=="Small Business"] <- "Corporate"

teams$Team.Division[teams$Team.Division=="School "] <- "School"
teams$Team.Division[teams$Team.Division==""] <- "Other"
teams$Team.Division[teams$Team.Division=="Beer/Brewery"] <- "Other"
teams$Team.Division[teams$Team.Division=="Open Team" ] <- "Other"
teams$Team.Division[teams$Team.Division=="Volunteer Group" ] <- "Other"
unique(teams$Team.Division)

participants<-read.csv("2013-2017 Bike MS Participants.csv",stringsAsFactors = FALSE)

participants$Team.Division[participants$Team.Division=="Bike Shop" ] <- "Bike Club"
participants$Team.Division[participants$Team.Division=="Bike Shops"] <- "Bike Club"

participants$Team.Division[participants$Team.Division=="Family/Friends"] <- "Family and Friends"
participants$Team.Division[participants$Team.Division=="Frien`s and Family"] <- "Family and Friends"
participants$Team.Division[participants$Team.Division=="Friend and Family"] <- "Family and Friends"
participants$Team.Division[participants$Team.Division=="Friends and Family"] <- "Family and Friends"
participants$Team.Division[participants$Team.Division=="Ohana and Friends"] <- "Family and Friends"
participants$Team.Division[participants$Team.Division=="Ohana "] <- "Family and Friends"

participants$Team.Division[participants$Team.Division=="Open"] <- "Open Team"

participants$Team.Division[participants$Team.Division=="Civic Team"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Organization"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Organization (Clubs, Civic Groups, etc.)"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Organization (Clubs; Civic Groups; Place of Worship; etc.)"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Organization (Clubs; Civic Groups; etc.)"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Religious"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Place of Worship"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Place of worship"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Place of worship"] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Association" ] <- "Club/Organization"
participants$Team.Division[participants$Team.Division=="Bike Club" ] <- "Club/Organization"

participants$Team.Division[participants$Team.Division=="Corporation"] <- "Corporate"
participants$Team.Division[participants$Team.Division=="Small Business"] <- "Corporate"

participants$Team.Division[participants$Team.Division=="School "] <- "School"
participants$Team.Division[participants$Team.Division==""] <- "Other"
participants$Team.Division[participants$Team.Division=="Beer/Brewery"] <- "Other"
participants$Team.Division[participants$Team.Division=="Open Team" ] <- "Other"
participants$Team.Division[participants$Team.Division=="Volunteer Group" ] <- "Other"

unique(participants$Team.Division)

write.csv(participants,file="participants_cleaned.csv")

team_corp<-teams[is.element(teams$Team.Division, "Corporate"),]
team_fnf<-teams[is.element(teams$Team.Division, "Family and Friends"),]

good_team_row<-which(team_corp$Team.Total.Confirmed....>=team_corp$Team.Goal...)
bad_team_row<-which(team_corp$Team.Total.Confirmed....<team_corp$Team.Goal...)
good_team_corp<-team_corp[good_team_row,]
bad_team<-team_corp[bad_team_row,]


good_team$good.team="yes"
bad_team$good.team="no"
teamsss<-rbind(good_team,bad_team)
write.csv(good_team,file="good_team_corp.csv")
write.csv(bad_team,file="bad_team_corp.csv")


good_team_row<-which(team_fnf$Team.Total.Confirmed....>=team_fnf$Team.Goal...)
bad_team_row<-which(team_fnf$Team.Total.Confirmed....<team_fnf$Team.Goal...)
good_team_fnf<-team_fnf[good_team_row,]
bad_team<-team_fnf[bad_team_row,]
write.csv(good_team,file="good_team_fnf.csv")
write.csv(bad_team,file="bad_team_fnf.csv")

all_good_teams = rbind(good_team_corp,good_team_fnf)
write.csv(all_good_teams,file="allgood_team_corp.csv")




