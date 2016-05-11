## batch timeline of political openness
with(b.100[[1]], {
	ggplot(country.states, aes(x=tick, y=chamber_median, color=id)) + geom_line()
})

## batch timeline -- population
with(b.100[[3]], {
	ggplot(city.states, aes(x=tick, y=num_persons, color=id)) + geom_line()
})

# consolidate runs for country A
n_ticks = 74
c_a <- NULL
for(i in 1:n_ticks){
	tmp <- sapply(b.100$runs, function(x){
		x$countries[x$countries$tick == i & x$countries$name == "COUNTRY A",]
	})
	c_a = cbind(c_a, tmp)
}
c_a <- data.frame(t(c_a))
for(i in 1:ncol(c_a)) c_a[,i] = unlist(c_a[,i])

# create summary data frame
c1 = c_a[1,][-1,]
for(i in 1:n_ticks){

	aa <- summary(1)
	p <- c_a[1:length(aa),]
	cns <- colnames(p)

	for(j in 1:ncol(p)){
		if(cns[j] == 'id')
			p[,j] = names(aa)
		else if(cns[j] == 'tick')
			p[,j] = i
		else if(is.numeric(p[,j]))
			p[,j] = as.vector(summary(c_a[c_a$tick == i,j]))
		else next
	}
	c1 = rbind(c1, p)
}
#ggplot(c1, aes(x=tick, y=num_persons, color=id)) + geom_line()

## populations by city
country.plots <- list()
vars <- c('num_persons','num_citizens','chamber_median')
for(v in vars)
	country.plots[[v]] <- ggplot(c1, aes_string(x="tick", y=v, color="id")) + geom_line() + ggtitle(v)
grid.arrange(grobs=country.plots, ncol=2, nrow=2, top='Populations by City')
