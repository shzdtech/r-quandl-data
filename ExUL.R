options(width = 438L)

#---------------------------------- futures symbol ----------------------------------
ExUL = data.frame()
##CME
ExUL = rbind(ExUL, data.frame(Exchange="CME", Underlying="CL", cList=c("SETTLE.VOLUME"), dsName="quandl", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # 
#ExUL = rbind(ExUL, data.frame(Exchange="CME", Underlying="NG", cList=c("SETTLE.VOLUME"), dsName="quandl")) # 
##CBOE
#ExUL = rbind(ExUL, data.frame(Exchange="CBOE", Underlying="VX", cList=c("SETTLE.VOLUME"), dsName="quandl", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # 
#CFFEX
ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="TF", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # 5-year Treasury Bond Futures
ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="T", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # 10-year Treasury Bond Futures
ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="IF", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # CSI 300 Index Futures
ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="IH", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # SSE 50 Index Futures
ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="IC", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # CSI 500 Index Futures
#DCE
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="A", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2004,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # No.1 Soybeans
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="M", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=10, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2004,2017),width=4,flag="0"),collapse="."), c.mg=paste(c("01","05","09"),collapse="."), fmonth_vec=paste(c("01","05","09"),collapse="."))) # Soybean Meal
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="Y", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2004,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Soybean Oil
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="I", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=10, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2001,2017),width=4,flag="0"),collapse="."), c.mg=paste(c("01","05","09"),collapse="."), fmonth_vec=paste(c("01","05","09"),collapse="."))) # Iron Ore
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="J", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=10, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2001,2017),width=4,flag="0"),collapse="."), c.mg=paste(c("01","05","09"),collapse="."), fmonth_vec=paste(c("01","05","09"),collapse="."))) # Coke
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="JM", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Coking Coal
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="P", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # RBD Palm Olein
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="C", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Corn
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="CS", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Corn Starch
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="L", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # LLDPE
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="V", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # PVC
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="PP", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # PP
#SHFE
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="CU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=6, c.yg=paste(formatC(seq(2010,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Copper
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AL", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=6, c.yg=paste(formatC(seq(2010,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Aluminum
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="ZN", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=6, c.yg=paste(formatC(seq(2010,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Zinc
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="PB", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Lead
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="NI", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Nickel
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="SN", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Tin
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Gold
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AG", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Silver
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="RB", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=10, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2001,2017),width=4,flag="0"),collapse="."), c.mg=paste(c("01","05","10"),collapse="."), fmonth_vec=paste(c("01","05","10"),collapse="."))) # Rebar
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="HC", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=12, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2001,2017),width=4,flag="0"),collapse="."), c.mg=paste(c("01","05","10"),collapse="."), fmonth_vec=paste(c("01","05","10"),collapse="."))) # Hot-rolled Coil
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="BU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=6, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2014,2017),width=4,flag="0"),collapse="."), c.mg=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."), fmonth_vec=paste(formatC(seq(1,12),width=2,flag="0"),collapse="."))) # Bitumen
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="RU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind", nmbd=1, nmfa=10, exOutlier=FALSE, term.len=3, c.yg=paste(formatC(seq(2010,2017),width=4,flag="0"),collapse="."), c.mg=paste(c("01","05","09"),collapse="."), fmonth_vec=paste(c("01","05","09"),collapse="."))) # Natural Rubber
##CZCE
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#
ExUL[,"Symbol"] = paste(ExUL[,"Exchange"], ExUL[,"Underlying"], sep=".")
#used in regression analysis, for grouping different month contracts
ExUL[,"c.mg.reg"] = ExUL[,"c.mg"]
ExUL[ExUL[,"Symbol"]=="SHFE.RB","c.mg.reg"] = "01.05.09"
ExUL[ExUL[,"Symbol"]=="SHFE.HC","c.mg.reg"] = "01.05.09"
ExUL = unique(ExUL)

#---------------- benchmark index ----------------
#obtained from "get_wind_contract.R"
#prepared in "prepar_dataset.R", i.e., combine different index source and calculate the mean
#cbind to term structure dataset in "term_structure.R"
INDEX = data.frame()
#SHFE.CU
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="CU", Code="S0182161", dsName="wind")) # www.ccmn.cn
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="CU", Code="S0105511", dsName="wind")) # www.shmet.com
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="CU", Code="S5806983", dsName="wind")) # www.shwuzi.com
#SHFE.AL
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="AL", Code="S0182162", dsName="wind")) # www.ccmn.cn
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="AL", Code="S0105512", dsName="wind")) # www.shmet.com
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="AL", Code="S5806985", dsName="wind")) # www.shwuzi.com
#SHFE.ZN
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="ZN", Code="S0048087", dsName="wind")) # www.ccmn.cn
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="ZN", Code="S0105514", dsName="wind")) # www.shmet.com
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="ZN", Code="S5806987", dsName="wind")) # www.shwuzi.com
#DCE.I
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0110172", dsName="wind")) # Lianyungang
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0110182", dsName="wind")) # Rizhao
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0167713", dsName="wind")) # Tianjin
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0174655", dsName="wind")) # Qingdao
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0183601", dsName="wind")) # Caofeidian
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0183606", dsName="wind")) # Jingtang
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0183615", dsName="wind")) # Zhenjiang
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0183618", dsName="wind")) # Jiangyin
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0183620", dsName="wind")) # Nantong
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0202769", dsName="wind")) # Luojing
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="I", Code="S0202790", dsName="wind")) # Taicang
#DCE.J
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5118197", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120134", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120133", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120124", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120125", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120129", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120130", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120132", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120126", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120127", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120128", dsName="wind")) # 
INDEX = rbind(INDEX, data.frame(Exchange="DCE", Underlying="J", Code="S5120131", dsName="wind")) # 
#SHFE.RB
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="RB", Code="S0033227", dsName="wind")) # Shanghai	HRB400 20mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="RB", Code="S0033213", dsName="wind")) # Beijing	HRB400 20mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="RB", Code="S0073207", dsName="wind")) # Tianjin	HRB400 20mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="RB", Code="S5704770", dsName="wind")) # Hangzhou	HRB400 20mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="RB", Code="S5704771", dsName="wind")) # Nanjing	HRB400 20mm
#SHFE.HC
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S0073208", dsName="wind")) # Tianjin 	3.0mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S0033249", dsName="wind")) # Shanghai	3.0mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5704839", dsName="wind")) # Hangzhou	3.0mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5704840", dsName="wind")) # Nanjing	3.0mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S0073209", dsName="wind")) # Tianjin 	4.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S0033272", dsName="wind")) # Shanghai	4.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5704863", dsName="wind")) # Hangzhou	4.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5704864", dsName="wind")) # Nanjing	4.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5708313", dsName="wind")) # Tianjin 	5.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5708299", dsName="wind")) # Shanghai	5.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5708300", dsName="wind")) # Hangzhou	5.75mm
INDEX = rbind(INDEX, data.frame(Exchange="SHFE", Underlying="HC", Code="S5708301", dsName="wind")) # Nanjing	5.75mm
#
INDEX[,"Symbol.IND"] = paste(INDEX[,"Exchange"], INDEX[,"Underlying"], INDEX[,"Code"], sep=".")
INDEX[,"Symbol.FUT"] = paste(INDEX[,"Exchange"], INDEX[,"Underlying"], sep=".")
INDEX[,"sDate"] = as.Date("2000-01-01")
INDEX[,"eDate"] = Sys.Date()
INDEX = unique(INDEX)

#---------------- fixed income rate ----------------
FIXED_INCOME = data.frame()
#SHIBOR
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="001", Code="M0017138", dsName="wind")) # Overnight
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="007", Code="M0017139", dsName="wind")) # 1 week
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="014", Code="M0017140", dsName="wind")) # 2 week
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="030", Code="M0017141", dsName="wind")) # 1 month
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="090", Code="M0017142", dsName="wind")) # 3 month
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="180", Code="M0017143", dsName="wind")) # 6 month
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="270", Code="M0017144", dsName="wind")) # 9 month
FIXED_INCOME = rbind(FIXED_INCOME, data.frame(Exchange="SHIBOR", Underlying="360", Code="M0017145", dsName="wind")) # 1 year
#
FIXED_INCOME[,"Symbol"] = paste(FIXED_INCOME[,"Exchange"], FIXED_INCOME[,"Underlying"], sep=".")
FIXED_INCOME[,"Days"] = as.numeric(as.character(FIXED_INCOME[,"Underlying"]))
FIXED_INCOME[,"sDate"] = as.Date("2000-01-01")
FIXED_INCOME[,"eDate"] = Sys.Date()
FIXED_INCOME = unique(FIXED_INCOME)

##---------------- contracts ----------------
#Exchange="CME"; Underlying="CL"; dsName="quandl"; exOutlier=FALSE; c.yg=formatC(seq(2001,2017),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
#Exchange="DCE"; Underlying="I"; dsName="wind"; nmbd=1; nmfa=12; exOutlier=FALSE; c.yg=formatC(seq(2001,2017),width=4,flag='0'); c.mg=c("01","05","09"); fmonth_vec=c("01","05","09");
##Exchange="DCE"; Underlying="M"; dsName="wind"; c.yg=formatC(seq(2001,2017),width=4,flag='0'); c.mg=c("01","05","09"); fmonth_vec=c("01","05","09");
##Exchange="DCE"; Underlying="C"; dsName="wind"; c.yg=formatC(seq(2001,2017),width=4,flag='0'); c.mg=c("01","05","09"); fmonth_vec=c("01","05","09");
#Exchange="SHFE"; Underlying="CU"; dsName="wind"; nmbd=1; nmfa=6; exOutlier=FALSE; c.yg=formatC(seq(2014,2017),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
#Exchange="SHFE"; Underlying="AL"; dsName="wind"; nmbd=1; nmfa=6; exOutlier=FALSE; c.yg=formatC(seq(2004,2017),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
##Exchange="SHFE"; Underlying="ZN"; dsName="wind"; nmbd=1; nmfa=6; exOutlier=FALSE; c.yg=formatC(seq(2001,2017),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
##Exchange="SHFE"; Underlying="RB"; dsName="wind"; nmbd=1; nmfa=12; exOutlier=FALSE; c.yg=formatC(seq(2001,2017),width=4,flag='0'); c.mg=c("01","05","10"); fmonth_vec=c("01","05","10");
