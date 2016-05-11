path <- "C:/Users/daniel/Dropbox/repast/migration/output/"
files <- c(
	"person.agents",
	"person.states",
	"person.events",
	"legislator.agents",
	"legislator.states",
	"legislator.events",
	"city.agents",
	"city.states",
	"country.agents",
	"country.states"
)
ext <- ".csv"

# load basic files
for(f in files) assign(f,  read.csv(paste0(path, f, ext)))

cities <- merge(city.states, city.agents, by.x="id", by.y="id", all.x=TRUE)
cities$city_tick = apply(cities, 1, function(r){paste0(r['id'], r['tick'])})
#apply(city.states, 1, function(r){str(r)})

city.melt <- melt(city.states, id.vars=c('id', 'tick'))
#persons.wide.tsm <- dcast(persons.melt, id ~ tick + variable, subset = .(variable=="ticks_since_migrate"))


# merge persons data
persons <- merge(person.states, person.agents, by.x="id", by.y="id", all.x=TRUE)
persons <- merge(persons, city.agents, by.x="city_in", by.y="id", all.x=TRUE, suffixes = c(".person",".city_in"))

persons$migrate = 0
persons$ticks_since_migrate = NA
for(i in 1:nrow(person.events)){
	r <- person.events[i,]
	if(r$event == 'migrate') persons$migrate[persons$id==r$id & persons$tick==r$tick] <- 1
}

for(id in unique(persons$id)){
	rs <- persons[persons$id==id,]
	rs <- rs[order(rs$tick),]
	j <- NA
	for(i in 1:nrow(rs)){
		r <- rs[i,]

		if(r$migrate == 1) j = 0
		else if(!is.na(j)) j = j + 1

		persons$ticks_since_migrate[persons$id==r$id & persons$tick==r$tick] <- j
	}
}


legislators <- merge(legislator.states, legislator.agents, by.x="id", by.y="id", all.x=TRUE)
legislators$city_tick = apply(legislators, 1, function(r){paste0(r['city'], r['tick'])})
legislators <- merge(legislators, cities, by.x="city_tick", by.y="city_tick", all.x=TRUE, suffixes = c(".legislator",".city"))
legislators$id = legislators$id.legislator
legislators$tick = legislators$tick.legislator
legislators$id.legislator = NULL
legislators$tick.legislator = NULL

#legislators <- merge(legislators, city.states, by.x="city_tick", by.y="city_tick", all.x=TRUE, suffixes = c(".legislator",".city"))
