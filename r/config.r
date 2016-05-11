# setwd('C:/Users/daniel/Dropbox/dissertation/chap3/r')
# scenario <- 's1_basic'
# batch <- '100'
# "C:/Users/daniel/Dropbox/repast/migration/output/%SCENARIO%/%BATCH%/%RUN%/"

scenario <- 's2_same'
batch <- '160112-090131'
batch_path <- paste("C:/Users/daniel/Dropbox/repast/migration/output", scenario, batch, '', sep = '/')
# files <- c(
# 	"person.agents",
# 	"person.states",
# 	"person.events",
# 	"legislator.agents",
# 	"legislator.states",
# 	"legislator.events",
# 	"city.agents",
# 	"city.states",
# 	"country.agents",
# 	"country.states"
# )
ext <- ".csv"
reqs = c(
	'grid',
	'plyr',
	'reshape2',
	'gplots',
	'ggplot2',
	'ggmap',
	'plm',
	# 'sqldf',
	# 'zoo',
	'riverplot'
)