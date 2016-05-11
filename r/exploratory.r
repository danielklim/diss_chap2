if(FALSE){
	setwd("C:/Users/daniel/Dropbox/dissertation/chap3/r")
	source("exploratory.r")
	source("data.r")
	source("config.r")
	#reloadData()
}

#######################

options(stringsAsFactors = FALSE)
lapply(reqs, require, character.only = TRUE)

reload <- function(){source("C:/Users/daniel/Dropbox/dissertation/chap3/src/exploratory.r")}
clear <- function(){
	reload <- get("reload")
	rm(list=ls(all=TRUE, envir=globalenv()), envir=globalenv())
	assign("reload", reload, envir=globalenv())
}
s <- summary
reloadData <- function(){source("data.r")}
pf <- function(...){cat(paste0(...));flush.console();}

surveyPersons <- function(){
	dev.new()
	par(mfrow=c(2,2), ask=T)

	for(Id in person.states$id){
		States <- subset(person.states, id==Id)
		Events <- subset(person.events, id==Id)
		Actor <- subset(person.agents, id==Id)

		# stats
		stats <- list(
			id = Actor$id,
			age = Actor$age,
			education = Actor$education,
			sex = Actor$sex,
			from = city.agents[city.agents$id==Actor$city_from,]$city_name
		)
		attr(stats, "class") <- "simple.list"
		textplot(stats)

		# events
		textplot(Events)

		# worth
		plot_timeline(States$tick, States$worth, Events, main="worth")
		plot_timeline(States$tick, States$u_avg, Events, main="happiness")

		#plot(States$tick, States$worth, main=Id, type="l")
		#by(Events, 1:nrow(Events), function(row){if(row$action == "migrate") abline(v=row$tick)})

	}
}

plot_timeline <- function(x, y, events, ...){
	plot(x, y, type="l", ...)
	by(events, 1:nrow(events), function(row){
		with(row, {
			y_at_tick <- y[x==tick]
			if(!length(y_at_tick)) TRUE
			else if(event == "migrate") points(tick, y_at_tick, pch="m")
			else if(event == "job_hired") points(tick, y_at_tick, pch="h")
			else if(event == "job_fired") points(tick, y_at_tick, pch="f")
		})

	})

}
