library(ggplot2)
library(tidyverse)
source("functions.R")

df1 <- read.csv("ffa_customrankings2017-0.csv",   
                        stringsAsFactors = FALSE)
df2 <- df1 %>% select(player, playerposition, points, lower, upper, dropoff, 
                      tier, risk)
keepers <- c("LeVeon Bell"=43, 
                  "Alshon Jeffery"=14, 
                  "Devonta Freeman"=15,
                  "Carlos Hyde"=21, 
                  "Melvin Gordon"=18, 
                  "LeGarrette Blount"=13, 
                  "Isaiah Crowell"=10,
                  "Jordan Howard"=23, 
                  "Drew Brees"=13,
                  "Michael Thomas"=6,
                  "Michael Crabtree"=11)
names(keepers)[!(names(keepers) %in% df2$player)]
df3 <- filter(df2, !(player %in% names(keepers)))
df3 <- df3 %>% group_by(playerposition) %>% 
    mutate(posrank = rank(-points))

p1 <- ggplot(df3, aes(x=posrank, y=tier, group=playerposition)) +
    geom_path(aes(color=playerposition)) + xlim(0, 50)
replace_rank <- c("QB"=15, "RB"=40, "WR"=40, "TE"=12)
replace_value <- c("QB"=filter(df3, playerposition=="QB" & 
                               posrank==replace_rank[["QB"]])$points, 
                   "RB"=filter(df3, playerposition=="RB" & 
                               posrank==replace_rank[["RB"]])$points,
                   "WR"=filter(df3, playerposition=="WR" & 
                               posrank==replace_rank[["WR"]])$points,
                   "TE"=filter(df3, playerposition=="TE" & 
                               posrank==replace_rank[["TE"]])$points)
df3$vor <- rep(NA, nrow(df3))
for (i in 1:nrow(df3)){
    df3$vor[i] <- df3$points[i] - replace_value[[df3$playerposition[i]]]
}

starter_dollars <- (200 * 12) - sum(keepers) - (12 * 16)
starters <- rbind(filter(df3, playerposition=="QB")[1:12, ],
                  filter(df3, playerposition=="RB")[1:30, ],
                  filter(df3, playerposition=="WR")[1:30, ],
                  filter(df3, playerposition=="TE")[1:12, ])

total_vor <- sum(starters$vor)
vor_value <- starter_dollars / total_vor
starters$value <- starters$vor * vor_value
starters$value <- starters$value * (1 + ((.066) * (5 - starters$risk)))
board <- group_by(starters, playerposition) %>% 
  arrange(desc(value)) %>% 
  select(player, playerposition, posrank, tier, risk, vor, value)
board$risk <- round(board$risk, 2) 
board$vor <- round(board$vor, 2) 
board$value <- round(board$value, 0) 
# add in value premium for elite players
for (i in 1:nrow(board)){
 if (board$posrank[i]<=4){
   board$value[i] <- board$value[i] * 1.1
 }
}
# recalibrate values
calibration <- starter_dollars / sum(board$value)
board$value <- round(board$value * calibration, 0)
board$id <- seq(1, nrow(board), 1)

drafted <- data.frame(id=c(), player=c(), cost=c())
id <- c()

