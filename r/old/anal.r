# who is migrating?
# migrations <- subset(person.events, event=='migrate')



persons.melt <- melt(persons, id.vars=c('id', 'tick'))
#persons.wide.tsm <- dcast(persons.melt, id ~ tick + variable, subset = .(variable=="ticks_since_migrate"))

## ticks since migrate across all subjects
ggplot(data=persons, aes(x=tick, y=ticks_since_migrate, group = id)) + geom_line()

m1 <-plm(worth ~ employed + migrate, data=persons, index=c("id", "tick"), model="within")
m3 <-plm(worth ~ ticks_since_migrate + education + age + city_name, data=persons, index=c("id", "tick"), model="between")
m4 <-plm(worth ~ migrate + education + age + city_name + employed + tick, data=persons, index=c("id", "tick"), model="between")

library(pglm)

anb <- pglm(migrate ~ employed + education + age + city_name, data = persons, family = binomial('logit'), model = "between", method = "bfgs", print.level = 3, R = 5, index=c("id", "tick"))




## classify different types of inds


