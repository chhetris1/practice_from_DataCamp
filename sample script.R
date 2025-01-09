beads <- rep(c("red", "blue", "pink"), times = c(2,3,5))
class(beads)
length(beads)
sample(beads, 3)
B <- 10000
events <- replicate(B, sample(beads,1))
length(events)
table(events)
prop.table(table(events))
