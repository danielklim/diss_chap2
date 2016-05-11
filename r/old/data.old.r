path <- "C:/Users/daniel/Dropbox/repast/migration/output/"
files <- c(
	"person.agents",
	"person.states",
	"person.events",
	"legislator.agents",
	"legislator.states",
	"city.agents",
)

loadCSV <- function(f, path, ext=".csv", envir=.GlobalEnv){
	if(!exists(f, envir=envir)) 
		assign(f,  read.csv(paste0(path, f, ext)), envir=envir)
}

loadABMData <- function(){
	envir <- globalenv()

	for(f in files) 
		loadCSV(f, path, envir=envir)
	
	if(!exists("persons", envir=envir)){
		persons <- merge(person.states, person.agents, by.x="id", by.y="id", all.x=TRUE)
		persons <- merge(persons, city.agents, by.x="city_in", by.y="id", all.x=TRUE, suffixes = c(".person",".city_in"))
		assign("persons",  persons, envir=envir)
	}
}

#if(!exists("person.agents")) person.agents <- read.csv(paste0(path, "person.agents.csv"))
#if(!exists("person.states")) person.states <- read.csv(paste0(path, "person.states.csv"))
#if(!exists("person.events")) person.events <- read.csv(paste0(path, "person.events.csv"))
#if(!exists("legislator.states")) legislator.events <- read.csv(paste0(path, "legislator.states.csv"))
#if(!exists("city.agents")) city.agents <- read.csv(paste0(path, "city.agents.csv"))
