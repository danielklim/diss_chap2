Account <- setRefClass("Account", fields = list(balance = "numeric"))

a = Account()

Account$methods(list(
	bank = function(){cat('BOFA')},
	setbal = function(i){
		balance <<- i
	}
))