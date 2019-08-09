val <- function(name_frag){
    a <- board$id[grep(as.character(name_frag), board$player, ignore.case=T)]
    b <- board$player[grep(name_frag, board$player, ignore.case=T)]
    d <- board$value[grep(name_frag, board$player, ignore.case=T)]
    data.frame(id=a, player=b, value=d)
}

draft <- function(omitid, cost){
    drafted <<- rbind(drafted, 
                     data.frame(id=omitid, 
                                player=board[board$id==omitid, ]$player, 
                                cost=cost))
    board <<- filter(board, id!=omitid)
    total_vor <<- sum(board$vor)
    starter_dollars <<- (200 * 12) - (12 * 16) - sum(keepers) - sum(drafted$cost) 
    vor_value <<- starter_dollars / total_vor
    board$value <<- board$value * (1 + ((.066) * (5 - board$risk)))
    board$value <<- board$vor * vor_value
    # add in value premium for elite players
    for (i in 1:nrow(board)){
     if (board$posrank[i]<=4){
       board$value[i] <<- board$value[i] * 1.1
     }
    }
    # recalibrate values
    calibration <- starter_dollars / sum(board$value)
    board$value <<- round(board$value * calibration, 0)
}

top <- function(pos){
    pos <- toupper(pos)
    filter(board, playerposition==pos) %>% arrange(posrank)
}





    


