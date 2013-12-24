# Author: John Horton
# Purpose: This code loads data from a congestion game experiment conducted on MTurk. 

library(ggplot2)
library(plyr)
library(mlogit)
library(Hmisc)
library(JJHmisc)

df <- read.csv("../../data/congestion_game_mturk.csv")

# WorkerId ---> worker.id 
# HITId ---> scenario.id
# AssignmentID ---> id 
# position ---> 1:5
# amount - Prize amount
# NumBids ---> 
# NumPlayers
# NumBids 

processLine <- function(row){
    "This ugly function turns the 'wide' MTurk format into a more resonable format"
    l <- as.list(row)
    common.items <-list(
             "worker.id" = as.character(l[["WorkerId"]]),
             "scenario.id" = as.character(l[["HITId"]]),
             "id" = as.character(l[["AssignmentId"]]), 
             "num.bids" = as.character(l[["Input.NumBids"]]),
             "num.players" = as.character(l[["Input.NumPlayers"]]), 
             "decision.time" = as.character(l[["WorkTimeInSeconds"]])
              )        
    lol <- list(
        c(common.items,
          list(
              "position" = 1,
              "amount" = as.character(l[["Input.amount1"]]),
              "selected" = I(l[["Answer.amount1"]] != "")
              )), 
        c(common.items,
          list(
              "position" = 2,
              "amount" = as.character(l[["Input.amount2"]]),
              "selected" = I(l[["Answer.amount2"]] != "")
              )), 
        c(common.items,
          list(
              "position" = 3,
              "amount" = as.character(l[["Input.amount3"]]),
              "selected" = I(l[["Answer.amount3"]] != "")
              )), 
        c(common.items,
          list(
              "position" = 4,
              "amount" = as.character(l[["Input.amount4"]]),
              "selected" = I(l[["Answer.amount4"]] != "")
              )), 
        c(common.items,
          list(
              "position" = 5,
              "amount" = as.character(l[["Input.amount5"]]),
              "selected" = I(l[["Answer.amount5"]] != "")
              )))
    df.tmp <- do.call(rbind, lapply(lol, data.frame, stringsAsFactors = FALSE))
    df.tmp
}

l <- apply(df, 1, processLine)

DF <- do.call(rbind, l)

# This creates a choice set ID by concatening all the amounts together into one string. 
DF <- ddply(DF, .(id), transform, scenarioID = gsub(" ", "-", do.call(paste,
                                      as.list(sort(as.numeric(as.character(amount)), decreasing = TRUE)))))




#########################################################
# LOAD THEORETICAL PREDICTIONS ABOUT CHOICE PROBABILITIES
#########################################################

df.theory <- read.csv("../../data/ne_strategy_predictions.csv")


##############################
# BASIC DESCRIPTIVE STATISTICS
##############################

#------------------
# Who participated? 
#------------------

# 1. How many unique workers participated?
# 2. What's the distribution of HITs/worker? Max? Min? Median? Mean



############################
# DID PAGE PLACEMENT MATTER?
############################


##############################
# For each scenario, 
##############################

#-----------------------------------------------
# Let's consider the 100-100-100-100-50 scenario
#-----------------------------------------------


df.empirical <- subset(DF, num.bids == 1 & scenarioID == "100-100-100-100-50")
df.empirical.summary <- ddply(df.empirical, .(num.players, amount), summarise, 
                              num.obs = sum(selected))

df.empirical.summary <- ddply(df.empirical.summary, .(num.players),
                              transform,
                              n = sum(num.obs),
                              s = num.obs, 
                              p = num.obs/sum(num.obs))


df.empirical.summary$ymin = mapply(function(s,n) binconf(s,n)[2], df.empirical.summary$s, df.empirical.summary$n)
df.empirical.summary$ymax = mapply(function(s,n) binconf(s,n)[3], df.empirical.summary$s, df.empirical.summary$n)
   
df.empirical.summary$num.players <- with(df.empirical.summary, reorder(num.players, as.numeric(as.character(num.players)),
mean))
df.empirical.summary[,"num.obs"] <- NULL
df.theory <- subset(read.csv("../../data/ne_strategy_predictions.csv"), scenarioID == "100-100-100-100-50")
df.theory.summary <- ddply(df.theory, .(id, n, W), summarise, p = sum(p))
df.theory.summary[,"id"] <- NULL
colnames(df.theory.summary) <- c("num.players", "amount", "p") 
df.theory.summary$pred.type <- "Theoretical"
df.empirical.summary$pred.type <- "Empirical"

df.theory.summary$ymin <- NA
df.theory.summary$ymax <- NA

df.empirical.summary$n <- NULL
df.empirical.summary$s <- NULL

df.combined <- rbind(df.theory.summary, df.empirical.summary)

df.combined$num.players <- with(df.combined, as.numeric(as.character(num.players)))
df.combined$amount <- with(df.combined, as.numeric(as.character(amount)))

#df.combined$num.players <- sapply(df.combined$num.players, function(x) paste0("Num other players = ", x))

g <- ggplot(data = df.combined,
            aes(x = factor(amount),
                y = p,
                fill = pred.type)) +
    geom_bar(aes(y = p, fill  = pred.type), position = position_dodge()) + 
    facet_wrap(~num.players, ncol = 7) +
        xlab("Choices") + ylab("Probability of Selection") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5, position = position_dodge()) +
    geom_point(position = position_dodge()) + 
    theme_bw() +
    scale_fill_manual(values = c("red", "black"))

print(g)

g <- ggplot(data = subset(df.combined, amount == 100),
            aes(x = (1 + num.players),
                y = p,
                linetype = pred.type)) +
    geom_line() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
    theme_bw() +
    scale_y_continuous(limits = c(0,1), label = percent) + 
    scale_x_sqrt(breaks = c(1,2,11, 26, 51, 101), labels = c(0, 1, 10, 25, 50, 100), limits = c(0, 150)) +
    xlab("Number of other players") +
    ylab("% players choosing \$100 prize") +
    theme(legend.position = "top") +
    annotate("text", x = 130, y = .90, label = "Predicted \n(Nash Eq.)", size = 4) +
    annotate("text", x = 130, y = .60, label = "Actual \nProportion", size = 4) +
    theme(legend.position = "None") 

print(g)

writeImage(g, "comparison100-100-100-100-50", width = 5, height = 3)





# Empirical 
g <- qplot(amount, p, data = df.empirical.summary) + geom_bar() +
    facet_wrap(~num.players, ncol = 7) +
    xlab("Choices") + ylab("Probability of Selection") + theme_bw()

writeImage(g, "comparison100-100-100-100-50")


qplot(as.numeric(amount), as.numeric(selected), data = DF) + geom_smooth() + geom_line() + geom_jitter()


id.single.50 <- "100100100100 50"

df.single.50 <- subset(DF, choice.set == id.single.50)

ddply(df.single.50, .(amount), summarise, sum(selected)/length(selected))


# Does position matter absolutely? 
m <- lm(selected ~ factor(position) - 1, data = DF)
summary(m)

m <- lm(selected ~ amount -1, data = DF)



# Let's consider cases with a single bid

df.1bid <- subset(DF, num.bids == 1)
df.1bid$amount <- as.numeric(as.character(df.1bid$amount))

df.1bid$num.players <- as.numeric(as.character(df.1bid$num.players))

df.1bid <- ddply(df.1bid, .(id), transform, max.value = max(amount))

df.1bid.collapse <- ddply(df.1bid, .(id, num.players), summarise,
                          num.bids.made = sum(selected),
                          max.value = max(max.value),
                          max.selected = max(selected * amount))


# When there were no other players, how many chose the max value?

# Workers realize that when there are no other players, you should always just choose the maximum. 
m <- lm(I(max.selected == max.value) ~ factor(num.players) - 1, data = df.1bid.collapse)

m <- lm(selected ~ factor(position) - 1, data = df.1bid)


# Is there an arbitrage opportunity?

scenario.1.50 <- subset(ddply(df.1bid, .(id), summarise,
                              one.50 = sum(amount) == 450,
                              amount1 = amount[1]), one.50 == TRUE)


DF$num.players <- as.numeric(as.character(DF$num.players))

df.1.50 <- subset(DF, id %in% scenario.1.50$id & num.players > 25)

table(df.1.50$num.players)

ddply(df.1.50, .(amount), summarise, frac = mean(selected) * length(selected))
