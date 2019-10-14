#Aging in pitchers and batters

library(Lahman)

library(dplyr)
library(tidyr)
library(ggplot2)

data("PitchingPost")

glimpse(Pitching)

#Q1
# Sort out 2018 and IPouts over 250.
pitcher = Pitching %>% filter(yearID == 2018 & IPouts > 250) %>% left_join(People) %>%
  mutate(age = yearID - birthYear)
#plot
ggplot(pitcher, aes(x=age, y= ERA))+ geom_point()+
  geom_smooth(method="loess", se=F) +
  labs(y = "ERA",
       x = "Age") +
  ggtitle("ERA over Pitcher's Age") +
  theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"))
#Average ERA over age
avg_era_tab = q1 %>% group_by(age) %>% summarise(avg_era = mean(ERA)) 


write.csv(avg_era_tab, file="avg_era_tab.csv",
          row.names=FALSE)

  
ggplot(avg_era_tab, aes(x=age)) + geom_area(aes(y=avg_era,fill="ERA")) +
  labs(title="Average ERA over Pitcher's Age", 
       y="ERA", x= "AGE") +
  theme(plot.title = element_text(hjust = 0.5))


#Q2
# best
best = q1 %>% arrange(ERA) %>% select(nameFirst, nameLast, ERA) %>% top_n(-5)
# worst
worst = q1 %>% arrange(desc(ERA)) %>% select(nameFirst, nameLast, ERA) %>% top_n(5)

worst
write.csv(best, file="best_era.csv",
          row.names=FALSE)

write.csv(worst, file="worst_era.csv",
          row.names=FALSE)
#Q03
q1 %>% arrange(ERA) %>% select(playerID, nameFirst, nameLast, ERA) %>% top_n(-1)

degrom = Pitching %>% filter(playerID == "degroja01")%>% left_join(People) %>%
  mutate(age = yearID - birthYear)
degrom_df_short= degrom %>% select(nameFirst, nameLast, yearID, age, W, L, ERA, ERA, SO, BAOpp)

write.csv(degrom_df_short, file="degrom.csv",
          row.names=FALSE)
degrom %>% select(yearID, ERA, BAOpp, BFP)
qplot(degrom$age, degrom$BAOpp)
qplot(degrom$age, degrom$BAOpp)

ggplot(degrom, aes(x=age)) + geom_line(aes(y=ERA, colour = "ERA")) +
  geom_line(aes(y=BAOpp *15, colour = "BAOpp")) +
  scale_y_continuous(sec.axis = sec_axis(~./15, name = "BAOpp"))+ 
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "ERA",
         x = "Age",
         colour = "Parameter") +
  ggtitle("deGrom's career ERA and BAOpp") +
  theme(plot.title = element_text(hjust = 0.5))


# Q04 
glimpse(Batting)
# Sort out 2018 and AB over 200.
batter = Batting %>% filter(yearID == 2018 & AB > 200)%>% left_join(People) %>%
  mutate(age = yearID - birthYear) %>% mutate(BA=round(H/AB,digits=3))
# Plot
ggplot(batter, aes(x=age, y= BA))+ geom_point()+
  geom_smooth(method="loess", se=F) +
  labs(y = "BA",
       x = "Age") +
  ggtitle("BA over Batter's Age") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 15))
# Average batting average over age
avg_ba_tab = batter %>% group_by(age) %>% summarise(avg_ba = mean(BA))
write.csv(avg_ba_tab, file="avg_ba_tab.csv",
          row.names=FALSE)

ggplot(avg_ba_df, aes(age,avg_ba)) + geom_line()

#Q05
# Best
best_batter = batter %>% arrange(desc(BA)) %>% select(nameFirst, nameLast, BA) %>% top_n(5)
# Worst
worst_batter = batter %>% arrange(BA) %>% select(nameFirst, nameLast, BA) %>% top_n(-5)


write.csv(best_batter, file="best_batter.csv",
          row.names=FALSE)

write.csv(worst_batter, file="worst_batter.csv",
          row.names=FALSE)

#Q06
batter %>% arrange(BA) %>% select(playerID, nameFirst, nameLast, BA) %>% top_n(1)
Betts = Batting %>% filter(playerID =="bettsmo01")%>% left_join(People) %>%
  mutate(age = yearID - birthYear)%>% mutate(BA=round(H/AB,digits=3))

betts_career = Betts%>% select(nameFirst, nameLast, yearID,age, AB, BA, H, RBI, HR)


write.csv(betts_career, file="betts.csv",
          row.names=FALSE)

Batting %>% filter(yearID >=1988 & AB > 500) %>% group_by(yearID) %>% summarize(max(H/AB,na.rm=T))

ggplot(Betts, aes(x=age, y=BA)) + geom_line(aes(y=BA, colour = "BA")) +
  labs(y = "BA",x = "Age") +
  ggtitle("Betts Career BA") +
  theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"))



+ geom_line(aes(y=ERA, colour = "ERA")) +
  geom_line(aes(y=BAOpp *15, colour = "BAOpp")) +
  scale_y_continuous(sec.axis = sec_axis(~./15, name = "BAOpp"))+ 
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "ERA",
       x = "Age",
       colour = "Parameter") +
  ggtitle("deGrom's career ERA and BAOpp") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(q1$age, q1$ERA, main="ERA over Age")+ xlab("Age") + 
  ggtitle("ERA over Pitcher's Age") +
  ylab("ERA") +theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm", se=F)

