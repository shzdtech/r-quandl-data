rm(list=ls(all=TRUE)) 
options(width = 438L)

#check if the working directory is correct
if (tail(unlist(strsplit(getwd(),"/")),n=1) != "r-quandl-data") {
	print(paste("Wrong working directory:", getwd()))
	break
}

##---------------------------------- futures symbol ----------------------------------
#ExUL = data.frame(Exchange=character(), Underlying=character())
source("//VMWARE-HOST/Shared Folders/Documents/workspace/r-basis-analysis/ExUL.R", echo=FALSE, encoding="GBK")

ExUL = ExUL[ExUL$dsName=="quandl",]

#---------------------------------- retrieve future data from Quandl ----------------------------------
library("Quandl")
Quandl.api_key("fmbXd-RBdq8byYyRnTaB")

#Month and MonthCode
Month = data.frame(MonthCode=c("F","G","H","J","K","M","N","Q","U","V","X","Z"), Month=as.character(formatC(seq(1,12),width=2,flag='0')))
#get all data for 1st time
YmRange = c(201512, 202012)
#update latest data
#YmRange = c(as.numeric(paste(unlist(strsplit(as.character(Sys.Date()),"-"))[1:2],collapse="")), 202012)

QuandlTicker = vector()
for (k in 1:dim(ExUL)[1]) {
#	download all datasets related to the Underlying
	fpath = paste(getwd(), "/Quandl_datasets_codes/", sep="")	
	fname = paste(ExUL[k,"Exchange"], "-datasets-codes.csv", sep="")	
	zt = read.table(file=paste(fpath,fname,sep=""), header=FALSE, sep=",")
	names(zt) = c("Ticker", "Description")
	zt[,"Ticker"] = as.character(zt[,"Ticker"])
#	only select required "Exchange/Underlying" contracts, e.g., nn = "CME/CL", plus month and year code, nchar(nn)+5
	nn = paste(ExUL[k,"Exchange"], ExUL[k,"Underlying"], sep="/")	
	ind = grepl(nn,zt[,"Ticker"]) & (nchar(zt[,"Ticker"])==(nchar(nn)+5))
	mm = zt[ind,]
#	add more columns, Ticker = [Exchange/Underlying][MonthCode][Year]
	mm[,"Exchange"] = as.character(ExUL[k,"Exchange"])
	mm[,"Underlying"] = as.character(ExUL[k,"Underlying"])
	mm[,"MonthCode"] = substr(mm[,"Ticker"], nchar(nn)+1, nchar(nn)+1)	
	mm[,"Month"] = Month[match(mm[,"MonthCode"],Month[,"MonthCode"]), "Month"]	
	mm[,"Year"] = substr(mm[,"Ticker"], nchar(nn)+2, nchar(nn)+5)
#	only download the contracts within the YearRange
	ym = as.numeric(paste(mm[,"Year"],mm[,"Month"],sep=""))
	ymind = ((YmRange[1]<=ym) & (ym<=YmRange[2]))
	QuandlTicker = rbind(QuandlTicker, mm[ymind,])
}

#order contracts
QuandlTicker = QuandlTicker[order(QuandlTicker[,"Underlying"], QuandlTicker[,"Year"], QuandlTicker[,"Month"]),]
	
#create directory if not exist
ExUL = unique(QuandlTicker[,c("Exchange","Underlying")])
for (k in 1:dim(ExUL)[1]) {
	ff = paste(ExUL[k,"Exchange"], ExUL[k,"Underlying"], sep="_")
	if (!dir.exists(ff)) {
		dir.create(ff)
	}
}

#retreive data from Quandl
for (i in 1:dim(QuandlTicker)[1]) {		
	result = tryCatch({
				xt = Quandl(as.character(QuandlTicker[i,"Ticker"]), order="asc")
#				convert the Quandl data file header to Wind format, e.g., 
#				Quandl: Date, Open, High, Low, Close, Pre Settle, Settle, Volume, Open Interest, Turnover
#				Wind: DATETIME, PRE_SETTLE, OPEN, HIGH, LOW, CLOSE, SETTLE, VOLUME, OI
				gg = toupper(names(xt))
				gg[match("DATE",gg)] = "DATETIME"
				gg[match("TRADE DATE",gg)] = "DATETIME"
				gg[match("PRE SETTLE",gg)] = "PRE_SETTLE"
				gg[match("TOTAL VOLUME",gg)] = "VOLUME"
				gg[match("OPEN INTEREST",gg)] = "OI"
				gg[match("PREV. DAY OPEN INTEREST",gg)] = "OI"
				names(xt) = gg
#				export to a file	
				fpath = paste(getwd(), "/", paste(QuandlTicker[i,"Exchange"],QuandlTicker[i,"Underlying"],sep="_"), "/", sep="")				
				fname = paste(QuandlTicker[i,"Exchange"], QuandlTicker[i,"Underlying"], QuandlTicker[i,"Year"], QuandlTicker[i,"Month"], "csv", sep=".")	
				write.table(xt, file=paste(fpath,fname,sep=""), sep=",", row.names=FALSE, col.names=TRUE)	
#				process status
				print(paste(QuandlTicker[i,"Ticker"], head(xt,n=1)[,1], tail(xt,n=1)[,1], dim(xt)[1], "(", i, "/", dim(QuandlTicker)[1], ")"))				
			}, warning = function(war) {
				print(war)
				print(paste(QuandlTicker[i,"Ticker"]))
			}, error = function(err) {
				print(err)
				print(paste(QuandlTicker[i,"Ticker"]))		
			}, finally = {				
			})
}	

#--------------------------------------------------------------------------------------------------------------------------
#Month = cbind(c("F","G","H","J","K","M","N","Q","U","V","X","Z"), formatC(seq(1,12),width=2,flag='0'))
#Year = seq(2016, 2017)
#MonthYear = merge(Month, Year)
#QuandlTicker = merge(MonthYear, ExUL)
#names(QuandlTicker) = c("MonthCode", "Month", "Year", "Exchange", "Underlying")
#QuandlTicker = cbind(QuandlTicker, Ticker=as.character(paste(QuandlTicker$Exchange,"/",QuandlTicker$Underlying,QuandlTicker$MonthCode,QuandlTicker$Year,sep="")))

