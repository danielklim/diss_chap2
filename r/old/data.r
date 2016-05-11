#list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
# source("data.batch.r")

batchLoad <- function(batch, obj_name = "b.%BATCH%", agents_pattern = "(^|\\s|,)(.*)\\.agents"){

	dirs <- unlist(sapply(list.dirs(path=batch_path, full.names=FALSE), function(x){if(nchar(x)>0) paste0(batch_path, x, sep='/')}))
	pf("Loading batch ", batch, " -- ", length(dirs), " runs\n")

	ln <- list()

	# batch stats
	ln$agents = data.frame(id = character(0), name = character(0), batch = integer(0), type = character(0))
	ln$runs <- vector("list", length(dirs))

	pb <- txtProgressBar(style = 3, min = 1, max = length(dirs))

	for(i in 1:length(dirs)){
		dir <- dirs[i]
		#pf("\n-------------- Run ", i, "--------------\n")
		
		# load basic files
		ln$runs[[i]] = list();

		ln$runs[[i]] = within(ln$runs[[i]], {
			local(for(f in files) assign(f, read.csv(paste0(dir, f, ext)), envir = parent.frame(2)))

			# compile list of agents -- ghetto for now
			#for(A_i in 1:nrow(country.agents)){A <- country.agents[A_i,]; ln$agents[nrow(ln$agents) + 1,] <<- c(A$id, A$name, i, "country")}
			local(
			
				#moo <- data.frame(id = character(0), name = character(0), batch = integer(0), type = character(0))
				with(b.100$runs[[1]],{
				for(agent_sheets in ls(pattern=agents_pattern))
					for(agent_sheet in agent_sheets){
						rex <- regexec(agents_pattern, agent_sheet)
						for(A_i in 1:nrow(get(agent_sheet))){
							A <- get(agent_sheet)[A_i,]
							#print(A)
							ln$agents[nrow(ln$agents) + 1,] <<- c(A$id, ifelse(is.null(A$name), NA, A$name), i,  substr(agent_sheet, rex[[1]][3], attr(rex[[1]], "match.length")[3]))
						}
						#print(agent_sheet)
						#print(rex)
					}
				})

			)

			# prep country data
			#pf("\tPrepping country data...\n")
			countries <- merge(country.states, country.agents, by.x="id", by.y="id", all.x=TRUE)

			# prep city data
			#pf("\tPrepping city data...\n")
			cities <- merge(city.states, city.agents, by.x="id", by.y="id", all.x=TRUE)
			cities$city_tick = apply(cities, 1, function(r){paste0(r['id'], r['tick'])})

			city.melt <- melt(city.states, id.vars=c('id', 'tick'))

			# merge persons data
			#pf("\tPrepping person data...\n")
			persons <- merge(person.states, person.agents, by.x="id", by.y="id", all.x=TRUE)
			persons <- merge(persons, city.agents, by.x="city_in", by.y="id", all.x=TRUE, suffixes = c(".person",".city_in"))

			#pf("\t\tMerging events...\n")
			persons$migrate = 0
			local(for(i in 1:nrow(person.events)){
				r <- person.events[i,]
				if(r$event == 'migrate') persons$migrate[persons$id==r$id & persons$tick==r$tick] <<- 1
			})

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

			# merge persons data
			#pf("\tPrepping legislator data...\n")
			legislators <- merge(legislator.states, legislator.agents, by.x="id", by.y="id", all.x=TRUE)
			legislators$city_tick = apply(legislators, 1, function(r){paste0(r['city'], r['tick'])})
			legislators <- merge(legislators, cities, by.x="city_tick", by.y="city_tick", all.x=TRUE, suffixes = c(".legislator",".city"))
			legislators$id = legislators$id.legislator
			legislators$tick = legislators$tick.legislator
			legislators$id.legislator = NULL
			legislators$tick.legislator = NULL
		})

		setTxtProgressBar(pb, i)
	}

	assign(sub("%BATCH%", batch, obj_name), ln, envir=globalenv())
	close(pb)
}

load.mig_abm <- function(){
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
}

