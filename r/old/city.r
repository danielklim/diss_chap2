## Jobs filled across all cities
econ.jobs_filled <- ggplot(city.states, aes(x=tick, y=jobs_filled, color=id)) + geom_line()
econ.jobs_filled

## gross populations across all cities
city.pop <- ggplot(city.states, aes(x=tick, y=num_persons, color=id)) + geom_line()
city.pop

## populations by city
city.plots <- list()
for(Id in unique(city.melt$id)){
	s <- subset(city.melt, id == Id & variable %in% c('num_persons','num_citizens','num_natives_employed'))
	city.plots[[Id]] <- ggplot(s, aes(x=tick, y=value, color=variable)) + geom_line()  + ggtitle(Id)
}
grid.arrange(grobs=city.plots, ncol=2, nrow=2, top='Populations by City')

## immigration policy across all countries
country.chamber_median <- ggplot(country.states, aes(x=tick, y=chamber_median, color=id)) + geom_line()
country.chamber_median

