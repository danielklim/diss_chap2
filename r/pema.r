'pema' <- setRefClass("pema",
	fields = list(
		path = "character",	# run path

		# inheritable from pemas
		ext = "character",	# ext of log files -- must be uniform
		agents.pattern = "character",	# regex pattern to identify agent files
		files.acceptable = "vector",
		keep.raw = 'logical',	# keep raw data after load?

		# files found on load
		files.found = "vector",		# files found in run dir
		raw = "list",			# raw data

		# processed from load
		agents = "data.frame",
		countries = "data.frame",
		cities = "data.frame",
		persons = "data.frame",
		legislators = "data.frame"

	),
	#prototype = prototype(),
	methods = list(
		# http://stackoverflow.com/questions/13517007/defining-default-field-values-for-instances-of-s4-reference-classes
		initialize = function(...){
			path <<- './1/'
			files.acceptable <<- c(
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
			ext <<- "csv"
			agents.pattern <<- "(^|\\s|,)(.*)\\.agents"
			keep.raw <<- FALSE
			agents <<- data.frame(id = character(0), name = character(0), type = character(0), stringsAsFactors = FALSE)

			# http://www.r-bloggers.com/a-new-r-trick-for-me-at-least/
			# http://stackoverflow.com/questions/3414078/unpacking-argument-lists-for-ellipsis-in-r
			#y <- do.call(callSuper, modifyList(defaults, list(...)), env=environment())
			#return(y)
			callSuper(...)
		},
		#http://stackoverflow.com/questions/29733168/using-default-values-in-defined-class-in-r
		#prototype=prototype(d=list(d1=c('as','sd'), d2=c(2,3,4), d3=5)))
		load = function(dir){

			# check for all possible files; read and set flags
			for(f in files.acceptable){
				full_path <- paste0(dir, f, '.', ext)
				if(file.exists(full_path)){
					raw[[f]] <<- read.csv(full_path, stringsAsFactors = FALSE)
					files.found <<- append(files.found, f)
				}
			}

			# get all agent names
			for(agent_sheets in ls(name=raw, pattern=agents.pattern))
				for(agent_sheet in agent_sheets){
					rex <- regexec(agents.pattern, agent_sheet)
					agent_rows <- raw[[agent_sheet]]
					for(A_i in 1:nrow(agent_rows)){
						A <- agent_rows[A_i,]
						type <- substr(agent_sheet, rex[[1]][3], attr(rex[[1]], "match.length")[3])
						agents[nrow(agents) + 1,] <<- c(A$id, ifelse(is.null(A$name), NA, A$name), type)
					}
				}

			# countries
			countries <<- merge(raw$country.states, raw$country.agents, by.x="id", by.y="id", all.x=TRUE)

			# cities
			cities <<- merge(raw$city.states, raw$city.agents, by.x="id", by.y="id", all.x=TRUE)
			cities$city_tick <<- apply(cities, 1, function(r){paste0(r['id'], r['tick'])})
			#city.melt <<- melt(raw$city.states, id.vars=c('id', 'tick'))

			# persons
			persons <<- merge(raw$person.states, raw$person.agents, by.x="id", by.y="id", all.x=TRUE)
			persons <<- merge(persons, raw$city.agents, by.x="city_in", by.y="id", all.x=TRUE, suffixes = c(".person",".city_in"))

			# merge person event histories
			persons$migrate <<- 0
			local(for(i in 1:nrow(raw$person.events)){
				# migration event
				r <- raw$person.events[i,]
				if(r$event == 'migrate')
					persons$migrate[persons$id==r$id & persons$tick==r$tick] <<- 1
			})

			# legislators
			legislators <<- merge(raw$legislator.states, raw$legislator.agents, by.x="id", by.y="id", all.x=TRUE)
			legislators$city_tick <<- apply(legislators, 1, function(r){paste0(r['city'], r['tick'])})
			legislators <<- merge(legislators, cities, 
				by.x="city_tick", by.y="city_tick", all.x=TRUE, suffixes = c(".legislator",".city"))
			legislators$id <<- legislators$id.legislator
			legislators$tick <<- legislators$tick.legislator
			legislators$id.legislator <<- NULL
			legislators$tick.legislator <<- NULL
			
			if(FALSE){

				# pf("\t\tCalculating ticks since migrate...\n")
				# persons$ticks_since_migrate = NA
				# for(id in unique(persons$id)){
				# 	rs <- persons[persons$id==id,]
				# 	rs <- rs[order(rs$tick),]
				# 	j <- NA
				# 	for(i in 1:nrow(rs)){
				# 		r <- rs[i,]

				# 		if(r$migrate == 1) j = 0
				# 		else if(!is.na(j)) j = j + 1

				# 		persons$ticks_since_migrate[persons$id==r$id & persons$tick==r$tick] <- j
				# 	}
				# }
			}
		}

	)
)

#plot.pemaPerson <- function(){}
#asd = pema(ext='txt')
#asd$load(paste0(batch_path, '1/'))