## Legislature and Immigration Policy

## legislator analysis
# lel <- melt(legislator.states, id.vars=c('id', 'tick'))
# lew <- dcast(lel, id ~ tick + variable, subset = .(variable=="votes"))

## legislator analysis

legislators$ideo2 <- legislators$ideology^2
m2 <-plm(votes ~ ideo2 + ideology, data=legislators, index=c("id", "tick"), model="between")
with(legislators, {plot(ideology, votes)})