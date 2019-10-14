
library(dplyr)
library(tidyr)
library(ggplot2)
team = read.csv("http://vicpena.github.io/sta9750/Teams1719.csv")


per = team %>% mutate(home_per = HomeW/(HomeW+AwayW))
home_advan = team %>% mutate(diff_H_A = HomeW - AwayW)


home_advan %>% group_by(League) %>% summarise(Avg= mean(diff_H_A), 
                                              Lowest= min(diff_H_A), 
                                              Highest = max(diff_H_A))

home_advan %>% group_by(Year) %>% summarise(avg= mean(diff_H_A))


best_home_advan = home_advan %>% group_by(Year) %>% arrange(Year, diff_H_A) %>% top_n(1)


write.csv(best_home_advan, file="best_home_advan.csv",
          row.names=FALSE)

lowest_home_advan = home_advan %>% group_by(Year) %>% arrange(Year, diff_H_A) %>% top_n(-1)
write.csv(lowest_home_advan, file="lowest_home_advan.csv",
          row.names=FALSE)


home_advan %>% group_by(Team) %>% summarise(avg=mean(diff_H_A)) %>% 
  arrange(avg) %>% top_n(1)
home_advan %>% group_by(Team) %>% summarise(avg=mean(diff_H_A)) %>% 
  arrange(avg) %>% top_n(-1)



test =home_advan %>% group_by(Year) %>% summarise(avg=mean(diff_H_A))

test %>% filter(Team == "Cubs")

ggplot(home_advan, aes(Year, diff_H_A, fill=factor(Year))) + 
  geom_boxplot()

home_advant =home_advan %>% mutate_at("Year", factor)


ggplot(home_advant, aes(Year, diff_H_A, fill=Year)) + 
  geom_boxplot() + facet_wrap(~League) + xlab("Year") + 
  ggtitle("Home Advantage between 2017 and 2019 in MLB") +
  ylab("Win Game") +theme(plot.title = element_text(hjust = 0.5))

