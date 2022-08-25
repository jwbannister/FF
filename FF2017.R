library(ggplot2)
library(tidyverse)
library(ffanalytics)
source("~/code/FF/functions.R")

positions <- c("QB", "RB", "WR", "TE")
my_scrape <- scrape_data(src = c("FantasyPros"), pos = positions, season = 2020, week = 0)
for (pos in positions){
    if (exists("df2")){
        tmp <- select(my_scrape[[pos]], player, points=site_pts)
        tmp$position <- rep(pos, nrow(tmp))
        df2 <- rbind(df2, tmp)
    } else{
        tmp <- select(my_scrape[[pos]], player, points=site_pts)
        tmp$position <- rep(pos, nrow(tmp))
        df2 <- tmp
    }
}

roster_no = c("QB"=24, "RB"=56, "WR"=56, "TE"=20)
replace_value = c("QB"=NA, "RB"=NA, "WR"=NA, "TE"=NA)
for (pos in positions){
    replace_value[pos] = mean(filter(df2, position==pos)$points[(roster_no[pos]+1):(roster_no[pos]+6)])
}
df2$vor = rep(NA, nrow(df2))
for (i in c(1:nrow(df2))){
    df2$vor[i] <- df2$points[i] - replace_value[df2$position[i]]
}

df3 <- df2 %>% filter(vor>0) 
df3 <- remove_rownames(df3)
df3 <- column_to_rownames(df3, var="player")

roster_dollars <- (200 * 12) 
total_vor <- sum(df3$vor)
vor_value <- roster_dollars / total_vor
df3$value <- round(df3$vor * vor_value, 0)

df3$group = rep(0, nrow(df3))
for (pos in positions){
    tmp <- df3 %>% filter(position == pos) %>% select(value)
    centers <- kmeans(tmp, 4, nstart = 25)$center
    centers <- sort(centers, decreasing = TRUE)
    clust <- kmeans(tmp, centers = centers)$cluster
    df3[df3$position==pos, ]$group <- clust
}

# add in value premium for elite players
for (i in 1:nrow(df3)){
 if (df3$group[i]==1){
   df3$value[i] <- df3$value[i] * 1.1
 }
}

# recalibrate values
calibration <- starter_dollars / sum(df3$value)
df3$value <- round(df3$value * calibration, 0)

write.csv(df3, "~/Desktop/FF.csv", row.names=T)
