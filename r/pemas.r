'pemas' <- setRefClass("pemas",
	fields = list(
		batch_path = "character",

		# params for passing to each pema object
		ext = "character",	# ext of log files -- must be uniform
		agents.pattern = "character",	# regex pattern to identify agent files
		files.acceptable = "vector",
		keep.raw = 'logical',	# keep raw data after load?

		# processed from load
		runs = "list"

	),
	methods = list(
		# http://stackoverflow.com/questions/13517007/defining-default-field-values-for-instances-of-s4-reference-classes
		initialize = function(...){
			batch_path <<- './'
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

			callSuper(...)
		},
		inheritables = function(){
			return(list(
				files.acceptable = files.acceptable,
				ext = ext,
				agents.pattern = agents.pattern,
				keep.raw = keep.raw
			))
		},
		load = function(batch_path){

			# run directories
			dirs <- unlist(sapply(list.dirs(path=batch_path, full.names=FALSE), 
				function(x){if(nchar(x)>0) paste0(batch_path, x, sep='/')}))
			ppf("Loading batch ", batch_path, " -- ", length(dirs), " runs\n")


			pb <- txtProgressBar(style = 3, min = 0, max = length(dirs))

			for(i in 1:length(dirs)){
				dir <- dirs[i]
				print(dir)
								
				run_i <- do.call(pema, inheritables())
				print(run_i)
				run_i$load(dir)
				runs[[i]] <<- run_i

				setTxtProgressBar(pb, i)
			}

			close(pb)

			# batch stats
			ppf("Calculating batch stats...")
			#ln$agents = data.frame(id = character(0), name = character(0), batch = integer(0), type = character(0))
			#ln$runs <- vector("list", length(dirs))

			#assign(sub("%BATCH%", batch, obj_name), ln, envir=globalenv())
			ppf("done!")
		}
	)
)

asd = pemas()
# asd$load(paste0(batch_path, '1/'))