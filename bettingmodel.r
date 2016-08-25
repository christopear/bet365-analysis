df.2014 = read.football.file(r2014, 2:10)
df.2015 = read.football.file(r2015, 2:10)
create_data_info = function(df) {
  df$Date = as.Date(df$Date, format="%d/%m/%Y")
  df$dayssince = as.numeric(Sys.Date() - df$Date)
  df$finalscorediff = as.numeric(as.character(df$FTHG)) - as.numeric(as.character(df$FTAG))
  df[order(df$Date),]
}
df.2012 = create_data_info(df.2012)
df.2013 = create_data_info(df.2013)
df.2014 = create_data_info(df.2014)
df.2015 = create_data_info(df.2015)
create_standings = function(df) {
  the_scores = as.character(df$FTR)
  homewinner = the_scores
  homewinner[which(homewinner == "H")] = "3"
  homewinner[which(homewinner == "D")] = "1"
  homewinner[which(homewinner == "A")] = "0"
  awaywinner = the_scores
  awaywinner[which(awaywinner == "H")] = "0"
  awaywinner[which(awaywinner == "D")] = "1"
  awaywinner[which(awaywinner == "A")] = "3"
  homegoalsfor = as.character(df$FTHG)
  awaygoalsfor = as.character(df$FTAG)
  test = data.frame(date = df$Date, team=df$HomeTeam, score=homewinner, points=0, goals=homegoalsfor, conceded=awaygoalsfor, GD=0, position=0, week=0,
                    stringsAsFactors=FALSE)
  test2 = data.frame(date = df$Date, team=df$AwayTeam, score=awaywinner, points=0, goals=awaygoalsfor, conceded=homegoalsfor, GD=0, position=0, week=0,
                     stringsAsFactors=FALSE)
  score_table = rbind(test, test2)
  score_table = score_table[order(score_table$date),]
  for (team in levels(score_table$team)) {
    only_this_team = which(score_table$team == team)
    score_table[only_this_team, "points"] = cumsum(as.numeric(score_table[only_this_team, "score"]))
    score_table[only_this_team, "goals"] = cumsum(as.numeric(score_table[only_this_team, "goals"]))
    score_table[only_this_team, "conceded"] = cumsum(as.numeric(score_table[only_this_team, "conceded"]))
    score_table[only_this_team, "GD"] = as.numeric(score_table[only_this_team, "goals"]) - as.numeric(score_table[only_this_team, "conceded"])
  }
  i = 1
  week = 1
  while(i <= nrow(score_table)) {
    temp_table = score_table[i:(i+19),]
    temp_table = temp_table[order(temp_table$points, temp_table$GD, temp_table$goals, decreasing=TRUE),]
    temp_table$position = 1:20
    temp_table$week = week
    score_table[i:(i+19),] = temp_table
    i= i+ 20
    week = week + 1
  }
  #score_table = score_table[order(score_table$date),]
  score_table[,c("date", "team", "score", "points","goals", "GD", "position", "week")]
}
massive_df = function(df) {
  stand_home = create_standings(df)
  stand_away = create_standings(df)
  names(stand_home) = paste("Home", names(stand_home), sep="")
  names(stand_home)[1:2] = c("Date", "HomeTeam")
  names(stand_away) = paste("Away", names(stand_away), sep="")
  names(stand_away)[1:2] = c("Date", "AwayTeam")
  test = merge(df, stand_home)
  test = merge(test, stand_away)
  test
}
create_entry = function(df, massive, date, week, HomeTeam, AwayTeam) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  editme = massive[1,]
  editme$Date = date
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  e.h = e.h[-c(1,2)]
  e.a = e.a[-c(1,2)]
  names(e.h) = paste("Home", names(e.h), sep="")
  names(e.a) = paste("Away", names(e.a), sep="")
  editme[1,names(e.h)] = e.h
  editme[1,names(e.a)] = e.a
  editme$HomeTeam = HomeTeam
  editme$AwayTeam = AwayTeam
  editme$Homescore = -1
  editme$Awayscore = -1
  editme$Homeweek = week
  editme$Awayweek = week
  editme
}
h = massive_df(df.2015)
h
paste(names(massive_df), sep=" ")
paste(names(h), sep=" ")
paste(names(h), collapse=" ")
paste(names(h), collapse="+")
head(h)
h.slim = h[-c(2,3,6,9)]
h.numeric = as.matrix(sapply(h.slim, as.numeric))
h.numeric = as.dataframe(as.matrix(sapply(h.slim, as.numeric)) )
h.numeric = as.data.frame(as.matrix(sapply(h.slim, as.numeric)) )
head(h.numeric)
mylm = lm(formula=finalscorediff~Date+AwayTeam+HomeTeam
          +FTHG+FTAG+FTR+HTHG+HTAG+HTR+dayssince
          +finalscorediff+Homescore+Homepoints+Homegoals
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD+Awayposition+Awayweek
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Date
          +FTHG+FTAG+HTHG+HTAG+dayssince
          +finalscorediff+Homescore+Homepoints+Homegoals
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD+Awayposition+Awayweek
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Date
          +dayssince
          +finalscorediff+Homescore+Homepoints+Homegoals
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD+Awayposition+Awayweek
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Date
          +finalscorediff+Homescore+Homepoints+Homegoals
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD+Awayposition+Awayweek
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Date
          +finalscorediff+Homescore+Homepoints
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD+Awayposition
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Date
          +finalscorediff+Homescore+Homepoints
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~finalscorediff+Homescore+Homepoints
          +HomeGD+Homeposition+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~finalscorediff+Homescore+Homepoints
          +HomeGD+Homeweek+Awayscore+Awaypoints
          +Awaygoals+AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~finalscorediff+Homescore+Homepoints
          +HomeGD+Awayscore+Awaypoints
          +Awaygoals+AwayGD
          ,data=h.numeric)
summary(mylm)
predict(mymodel, newdata)
newdata=create_entry(df.2015,h, as.Date("0015-11-01"), 11, "Crystal Palace", "Man United")
predict(mylm, newdata)
predict(mylm, as.numeric(newdata))
predict(mylm, as.numeric(as.character(newdata)))
newdata
newdata=create_entry(df.2015,h, as.Date("0015-11-01"), 11, "Crystal Palace", "Man United")
newdata
as.numeric(as.character(newdata))
predict(mylm, as.numeric(as.character(newdata)))
predict(mylm, data=newdata)
mylm = lm(formula=finalscorediff~Homescore+Homepoints
          +HomeGD+Awayscore+Awaypoints
          +Awaygoals+AwayGD
          ,data=h.numeric)
summary(mylm)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
}
create_entry("Crystal Palace", "Man United", df.2015)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  print(e.h)
  print(e.a)
}
create_entry("Crystal Palace", "Man United", df.2015)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  print(e.h)
  print(e.a)
  data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
             Awayscore=e.a$score, Awaypoints=e.a$points,
             Awaygoals=e.a$goals, AwayGD=e.a$GD)
}
create_entry("Crystal Palace", "Man United", df.2015)
print(e.h)
print(e.a)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
             Awayscore=e.a$score, Awaypoints=e.a$points,
             Awaygoals=e.a$goals, AwayGD=e.a$GD)
}
create_entry("Crystal Palace", "Man United", df.2015)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  data.frame(as.numeric(data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
                                   Awayscore=e.a$score, Awaypoints=e.a$points,
                                   Awaygoals=e.a$goals, AwayGD=e.a$GD)))
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
ata=
  predict(mylm, data=newdata)
predict(mylm, data=newdata)
predict(mylm,newdata)
newdata
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
             Awayscore=e.a$score, Awaypoints=e.a$points,
             Awaygoals=e.a$goals, AwayGD=e.a$GD)
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
newdata
mode(newdata)
mode(newdata[,1])
predict(mylm,newdata)
h.numeric$Homescore
mode(h.numeric$Homescore)
newdata = as.numeric(create_entry("Crystal Palace", "Man United", df.2015))
newdata
predict(mymodel, newdata)
summary(mylm)
predict(mylm,newdata)
mode(h.numeric$AwayGD)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  as.data.frame(as.matrix(sapply(data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
                                            Awayscore=e.a$score, Awaypoints=e.a$points,
                                            Awaygoals=e.a$goals, AwayGD=e.a$GD), as.numeric)))
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
newdata
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  t(as.data.frame(as.matrix(sapply(data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
                                              Awayscore=e.a$score, Awaypoints=e.a$points,
                                              Awaygoals=e.a$goals, AwayGD=e.a$GD), as.numeric))))
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
newdata
predict(mylm,newdata)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  as.data.frame(t(as.matrix(sapply(data.frame(Homescore=e.h$score, Homepoints=e.h$points,HomeGD=e.h$GD,
                                              Awayscore=e.a$score, Awaypoints=e.a$points,
                                              Awaygoals=e.a$goals, AwayGD=e.a$GD), as.numeric))))
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
newdata
predict(mylm,newdata)
h = massive_df(df.2015)
h.slim = h[-c(2,3,6,9)]
h.numeric = as.data.frame(as.matrix(sapply(h.slim, as.numeric)))
paste(names(h.numeric), collapse="+")
mylm = lm(formula=finalscorediff~Date+Homepoints+Homegoals
          +HomeGD+Homeposition+Homeweek+Awaypoints+Awaygoals
          +AwayGD+Awayposition
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints+Homegoals
          +HomeGD+Homeposition+Homeweek+Awaypoints+Awaygoals
          +AwayGD+Awayposition
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaypoints+Awaygoals
          +AwayGD+Awayposition
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaypoints+Awaygoals
          +AwayGD+Awayposition+Homeposition*Awayposition+HomeGD*AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaypoints+Awaygoals
          +AwayGD+Homeposition*Awayposition+HomeGD*AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaypoints+Awaygoals
          +AwayGD+Homeposition*Awayposition+HomeGD*AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaygoals
          +AwayGD+Homeposition*Awayposition+HomeGD*AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaygoals
          +AwayGD+Awayposition+HomeGD*AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaygoals
          +AwayGD+HomeGD*AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Homeweek+Awaygoals
          +AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homepoints
          +HomeGD+Homeposition+Awaygoals
          +AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~HomeGD+Homeposition+Awaygoals
          +AwayGD
          ,data=h.numeric)
summary(mylm)
mylm = lm(formula=finalscorediff~Homeposition+Awaygoals
          +AwayGD
          ,data=h.numeric)
summary(mylm)
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  as.data.frame(t(as.matrix(sapply(data.frame(
    Homeposition=e.h$position,
    Awaygoals=e.a$goals,
    AwayGD=e.a$GD)
    as.numeric))))
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
newdata
create_entry = function(HomeTeam, AwayTeam, df) {
  s = create_standings(df)
  backwards = s[order(s$date, decreasing=TRUE),]
  e.h = backwards[backwards["team"]==HomeTeam,][1,]
  e.a = backwards[backwards["team"]==AwayTeam,][1,]
  data.frame(Homeposition=as.numeric(e.h$position),
             Awaygoals=as.numeric(e.a$goals),
             AwayGD=as.numeric(e.a$GD))
}
newdata = create_entry("Crystal Palace", "Man United", df.2015)
newdata
predict(mylm,newdata)
newdata = create_entry("Man City", "Bournemouth", df.2015)
predict(mylm,newdata)
newdata = create_entry("Tottenham", "Bournemouth", df.2015)
predict(mylm,newdata)
newdata = create_entry("Bournemouth", "Tottenham", df.2015)
predict(mylm,newdata)
newdata = rbind(create_entry("West Ham", "Chelsea", df.2015)
                newdata = rbind(create_entry("West Ham", "Chelsea", df.2015))
                newdata = create_entry("Bournemouth", "Tottenham", df.2015)
                newdata = rbind(create_entry("West Ham", "Chelsea", df.2015))
                predict(mylm,newdata)
                newdata
                newdata = create_entry("Bournemouth", "Tottenham", df.2015)
                newdata = rbind(newdata, create_entry("West Ham", "Chelsea", df.2015))
                predict(mylm,newdata)
                levels(data.2015$HomeTeam)
                levels(df.2015$HomeTeam)
                newdata = create_entry("West Ham", "Chelsea", df.2015)
                newdata = rbind(newdata, create_entry("Leicester", "Crystal Palace", df.2015))
                newdata = rbind(newdata, create_entry("Norwich", "West Brom", df.2015))
                newdata = rbind(newdata, create_entry("Stoke", "Watford", df.2015))
                newdata = rbind(newdata, create_entry("Aston Villa", "Swansea", df.2015))
                newdata = rbind(newdata, create_entry("Arsenal", "Everton", df.2015))
                newdata = rbind(newdata, create_entry("Sunderland", "Newcastle", df.2015))
                newdata = rbind(newdata, create_entry("Bournemouth", "Tottenham", df.2015))
                newdata = rbind(newdata, create_entry("Man United", "Man City", df.2015))
                newdata = rbind(newdata, create_entry("Liverpool", "Southampton", df.2015))
                rownames(newdata) = c("West Ham", "Leicester", "Norwich", "Stoke", "Aston Villa",
                                      "Arsenal", "Sunderland", "Bournemouth", "Man United", "Liverpool")
                predict(mylm,newdata)
                df.2015
                df.2015.temp = df.2015[1:90,]
                h = massive_df(df.2015)
                h.slim = h[-c(2,3,6,9)]
                h.numeric = as.data.frame(as.matrix(sapply(h.slim, as.numeric)))
                newdata = create_entry("West Ham", "Chelsea", df.2015.temp)
                newdata = rbind(newdata, create_entry("Leicester", "Crystal Palace", df.2015.temp))
                newdata = rbind(newdata, create_entry("Norwich", "West Brom", df.2015.temp))
                newdata = rbind(newdata, create_entry("Stoke", "Watford", df.2015.temp))
                newdata = rbind(newdata, create_entry("Aston Villa", "Swansea", df.2015.temp))
                newdata = rbind(newdata, create_entry("Arsenal", "Everton", df.2015.temp))
                newdata = rbind(newdata, create_entry("Sunderland", "Newcastle", df.2015.temp))
                newdata = rbind(newdata, create_entry("Bournemouth", "Tottenham", df.2015.temp))
                newdata = rbind(newdata, create_entry("Man United", "Man City", df.2015.temp))
                newdata = rbind(newdata, create_entry("Liverpool", "Southampton", df.2015.temp))
                rownames(newdata) = c("West Ham", "Leicester", "Norwich", "Stoke", "Aston Villa",
                                      "Arsenal", "Sunderland", "Bournemouth", "Man United", "Liverpool")
                predict(mylm,newdata)
                h = massive_df(df.2015)
                h.slim = h[-c(2,3,6,9)]
                h.numeric = as.data.frame(as.matrix(sapply(h.slim, as.numeric)))
                newdata = create_entry("West Ham", "Chelsea", df.2015)
                newdata = rbind(newdata, create_entry("Leicester", "Crystal Palace", df.2015))
                newdata = rbind(newdata, create_entry("Norwich", "West Brom", df.2015))
                newdata = rbind(newdata, create_entry("Stoke", "Watford", df.2015))
                newdata = rbind(newdata, create_entry("Aston Villa", "Swansea", df.2015))
                newdata = rbind(newdata, create_entry("Arsenal", "Everton", df.2015))
                newdata = rbind(newdata, create_entry("Sunderland", "Newcastle", df.2015))
                newdata = rbind(newdata, create_entry("Bournemouth", "Tottenham", df.2015))
                newdata = rbind(newdata, create_entry("Man United", "Man City", df.2015))
                newdata = rbind(newdata, create_entry("Liverpool", "Southampton", df.2015))
                rownames(newdata) = c("West Ham", "Leicester", "Norwich", "Stoke", "Aston Villa",
                                      "Arsenal", "Sunderland", "Bournemouth", "Man United", "Liverpool")
                predict(mylm,newdata)
                newdata = create_entry("Chelsea", "Liverpool", df.2015)
                newdata = rbind(newdata, create_entry("Crystal Palace", "Man United", df.2015))
                newdata = rbind(newdata, create_entry("Swansea", "Arsenal", df.2015))
                newdata = rbind(newdata, create_entry("Newcastle", "Stoke", df.2015))
                newdata = rbind(newdata, create_entry("West Brom", "Leicester", df.2015))
                newdata = rbind(newdata, create_entry("Man City", "Norwich", df.2015))
                newdata = rbind(newdata, create_entry("Watford", "West Ham", df.2015))
                newdata = rbind(newdata, create_entry("Everton", "Sunderland", df.2015))
                newdata = rbind(newdata, create_entry("Southampton", "Bournemouth", df.2015))
                newdata = rbind(newdata, create_entry("Tottenham", "Aston Villa", df.2015))
                rownames(newdata) = c("CP", "Swan", "Newc", "West B", "Man C", "Wat", "Ever", "Soton", "Tott")
                predict(mylm,newdata)
                rownames(newdata) = c("Chelsea", "CP", "Swan", "Newc", "West B", "Man C", "Wat", "Ever", "Soton", "Tott")
                predict(mylm,newdata)
                o = predict(mylm,newdata)
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds - 1
                  odds[which(odds > 0)] = odds + 1
                }
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds - 1
                  odds[which(odds > 0)] = odds + 1
                  odds
                }
                convert_prediction(o)
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds[which(odds < 0)] - 1
                  odds[which(odds > 0)] = odds[which(odds > 0)] + 1
                  odds
                }
                convert_prediction(o)
                o
                convert_prediction(o)
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds[which(odds < 0)] - 1
                  odds[which(odds > 0)] = odds[which(odds > 0)] + 1
                  odds[which(odds < 10 || odds > 10)] = 0
                  odds
                }
                o
                convert_prediction(o)
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds[which(odds < 0)] - 1
                  odds[which(odds > 0)] = odds[which(odds > 0)] + 1
                  odds[which(odds < -10 || odds > 10)] = 0
                  odds
                }
                o
                convert_prediction(o)
                1:10[which(1:10 < 5)]
                1:10[1:10 < 5]
                1:10 < 5
                1:10[TRUE TRUE TRUE TRUE FALSE FALSE FALSE FALSE FALSE FALSE]
                1:10[c(TRUE TRUE TRUE TRUE FALSE FALSE FALSE FALSE FALSE FALSE)]
                1:10[c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)]
                which(1:50 < 5)
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds[which(odds < 0)] - 1
                  odds[which(odds > 0)] = odds[which(odds > 0)] + 1
                  odds[which(odds < -10 || odds > 10)] = 0
                  odds
                }
                o
                convert_prediction = function(odds) {
                  odds = 1/odds
                  odds[which(odds < 0)] = odds[which(odds < 0)] - 1
                  odds[which(odds > 0)] = odds[which(odds > 0)] + 1
                  odds
                }
                o
                convert_prediction(o)
                plot(mymodel)
                plot(mylm)
                