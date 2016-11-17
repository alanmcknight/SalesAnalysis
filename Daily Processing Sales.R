library(lubridate)
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")
Data <- read.csv("DailyStats.csv", stringsAsFactors =FALSE)

Data$UserID <- substr(Data$BTXPolref, 1, 6)
Data$BFDPbalance[is.na(Data$BFDPbalance)] <- 0
Data$BTXDatecreated[Data$BTXTrantype == "Renewal"] <- as.character(Data$BTXDtraised[Data$BTXTrantype == "Renewal"])
Data$BTXTrantype <- gsub('Transfrd NB', 'Renewal', Data$BTXTrantype)
PolicySales <- subset(Data, (BTXTrantype == "New Business" | BTXTrantype == "Renewal") & (BTXPoltype %in% c("AG", "AR", "BD", "CA", "CC", "CL", "CP", "CR", "CV", "CW", "DO", "EB", "EL", "EN", "FL", "FP", "FS", "GT", "HH", "HL", "HO", "HQ", "HT",  "LI", "LP", "LV", "MA", "MC", "MT", "MV", "MY", "NH", "OC", "OS", "PB", "PC", "PD", "PI", "PK", "PL", "PO", "RP", "SB", "SC", "SH", "SL", "TH", "TL", "TM", "TR", "TV", "TW", "XX")))
# PolicySales$BTXDatecreated[PolicySales$BTXTrantype == "Renewal"] <- as.character(PolicySales$BTXDtraised[PolicySales$BTXTrantype == "Renewal"])
# PolicySales$BTXTrantype <- gsub('Transfrd NB', 'Renewal', PolicySales$BTXTrantype)

x <- rbind(Data, PolicySales)
x <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(Data), ]
PolicySales$FinanceValue <- round(PolicySales$BFDPbalance * .096, 2)

###### Traffic Costs and Sources
PolicySales$TrafficCost <- 0
PolicySales$Source <- "None"
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE)] <- 42.50
PolicySales$TrafficCost[grepl("APRCC-", PolicySales$ECWebref, ignore.case=FALSE)] <- 50
PolicySales$TrafficCost[grepl("APRUS-", PolicySales$ECWebref, ignore.case=FALSE)] <- 50
PolicySales$Source[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Quotezone"
PolicySales$Source[grepl("APRCC-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Call Connection"
PolicySales$Source[grepl("APRUS-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Uswitch"
PolicySales[PolicySales$BTXTrantype == "Renewal", "TrafficCost"] <- 0

##### Assign Executive
code = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
PolicySales$BTXExec[PolicySales$BTXExec == ""] <- "0"
Executive = c("Not Assigned", "Mark", "Audrey", "Aine", "Louise", "Megan", "Elaine", "Susan", "Stephen")
execRef <- data.frame(code, Executive)
PolicySales <- merge(PolicySales, execRef, by.x="BTXExec", by.y="code", all.x=TRUE)

PolicySales$Cancellation <- "N"
PolicySales$CancellationCommission <- 0
PolicySales$AddOnValue <- 0
PolicySales$Discount <- 0

PolicySales$TotalValue <- PolicySales$BTXCommamt + PolicySales$FinanceValue - PolicySales$TrafficCost

#####Age Calculation
Today <- as.Date("2016/11/01")
PolicySales$Age = round(as.numeric((Today - as.Date(PolicySales$BCMDob, "%d/%m/%Y")) / 365.25), 0)

PolicySales$Age.Range = cut(PolicySales$Age, 
                            breaks = c(-Inf, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, Inf), 
                            labels = c("17-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "70+"), 
                            right = FALSE)

#Day of Week Created Calculation
PolicySales$Day.of.Week = weekdays(as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y"))

#Month Created Calculation
#PolicySales$Month1 = month((as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y")))
PolicySales$Month1 = strftime(as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y"), "%m")
PolicySales$Month2 = months(as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y"))
PolicySales$Month <- with(PolicySales, paste0(Month1, ". ", Month2))
PolicySales$Month1 <- NULL
PolicySales$Month2 <- NULL

#Email Domain
PolicySales$Email.Domain = sub("^[^@]*", "", PolicySales$BCMEmail)

#UK Postcode Area and Region
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool")
PostcodeRegions <- read.csv("PostcodeRegions.csv")
PolicySales$Post.Code.Prefix = gsub('[[:digit:]]+', '', substring(as.character(PolicySales$BCMPcode), 1, 2))
PolicySales$Postcode.Area = PostcodeRegions$AREA[ match(PolicySales$Post.Code.Prefix, PostcodeRegions$POSTCODE.PREFIX)]
PolicySales$Postcode.Region = PostcodeRegions$REGION[ match(PolicySales$Post.Code.Prefix, PostcodeRegions$POSTCODE.PREFIX)]

#Quote Map Location
Postcodes <- read.csv("ukpostcodes.csv")
#setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/Partner Analysis")
PolicySales$BCMPcode <- toupper(PolicySales$BCMPcode)
PolicySales$Longitude = Postcodes$longitude[ match(PolicySales$BCMPcode, Postcodes$postcode)]
PolicySales$Latitude = Postcodes$latitude[ match(PolicySales$BCMPcode, Postcodes$postcode)]

#write.csv(PolicySales, "DailyStatsProcessed.csv")

##### COMBINE SALES REPORT

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Monthly Sales")
a <- read.csv("AllSalesReport-2016-08.csv", stringsAsFactors =FALSE)
b <- read.csv("AllSalesReport-2016-09.csv", stringsAsFactors =FALSE)
Sales <- rbind(a, b)
Sales$POSTCODE <- toupper(Sales$POSTCODE)

DailySales <- merge(PolicySales, Sales, by.x=c("BCMDob", "BCMPcode"), by.y=c("BIRTH.DATE", "POSTCODE"), all.x=TRUE)

##### COMBINE QUOTES REPORT

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/Quote Reports")
Quotes <- read.csv("Quotes09.csv", stringsAsFactors =FALSE)
Quotes1 <- read.csv("Quotes10.csv", stringsAsFactors =FALSE)
Quotes <- rbind(Quotes, Quotes1)
Quotes$Date.Of.Birth <- gsub("[.]", "/", Quotes$Date.Of.Birth)

DailySales[DailySales$BTXPoltype == "PC",] <- merge(DailySales[DailySales$BTXPoltype == "PC",], Quotes, by.x=c("BCMDob", "BCMPcode"), by.y=c("Date.Of.Birth", "Post.Code"), all.x=TRUE)

DailySales <- merge(DailySales, Quotes, by.x=c("BCMDob", "BCMPcode"), by.y=c("Date.Of.Birth", "Post.Code"), all.x=TRUE)
DailySales[DailySales$BTXPoltype != "PC",][,which( colnames(DailySales)=="Quote.Reference" ):which( colnames(DailySales)=="Site.Name.110" )] <- NA
DailySales <- DailySales[!is.na(DailySales$BTXPolref),]
DailySales <- within(DailySales, Source[!is.na(Quote.Reference) & Source != 'Uswitch'] <- 'Quotezone')
DailySales <- within(DailySales, TrafficCost[!is.na(Quote.Reference) & Source != 'Uswitch' & BTXTrantype == "New Business"] <- 42.5)
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")
#write.csv(DailySales, "DailySales.csv", row.names=F)

bspot <- which(names(DailySales)=="Price.Position.1")

#UK Residency Calculation
DailySales$Uk.Resident.Date <- gsub("[.]", "/", DailySales$Uk.Resident.Date)
DailySales <- data.frame(DailySales[1:(bspot-1)], UK.Residency.Years = round(as.numeric((Today - as.Date(DailySales$Uk.Resident.Date, "%d/%m/%Y")) / 365.25), 0), DailySales[(bspot):ncol(DailySales)])

#Licence Calculation
DailySales$How.long.have.you.held.this.licence. <- gsub("[.]", "/", DailySales$How.long.have.you.held.this.licence.)
DailySales <- data.frame(DailySales[1:(bspot-1)], Licence.Years = round(as.numeric((Today - as.Date(DailySales$How.long.have.you.held.this.licence., "%d/%m/%Y")) / 365.25), 0), DailySales[(bspot):ncol(DailySales)])

#Vehicle Value Range Calculation
DailySales <- data.frame(DailySales[1:(bspot-1)], Vehicle.Value.Range = cut(DailySales$What.is.the.estimated.value.of.the.vehicle., 
                                                                            breaks = c(-Inf, 100, 500, 1000, 2000, 3000, 5000, 7000, 10000, 15000, 20000, 30000, Inf), 
                                                                            labels = c("Less than £100", "£100-£500", "£500-£1000", "£1000-£2000", "£2000-£3000", "£3000-£5000", "£5000-£7000", "£7000-£10,000", "£10,000-£15,000", "£15,000-£20,000", "£20,000-£30,000", "£30,000 +"), 
                                                                            right = FALSE), DailySales[(bspot):ncol(DailySales)])

#DATE SOLD AND RAISED DIFFERENCE

#Who.is.the.registered.keeper.of.the.vehicle.
DailySales$Who.is.the.registered.keeper.of.the.vehicle.[grepl("^M", DailySales$Who.is.the.registered.keeper.of.the.vehicle., ignore.case=FALSE)] <- "BLANK"

#Who.is.the.registered.keeper.of.the.vehicle.
DailySales$Who.is.the.owner.of.the.vehicle.[grepl("^M", DailySales$Who.is.the.owner.of.the.vehicle., ignore.case=FALSE)] <- "BLANK"

##### Apricot Position
UploadColumnNumbers <- ncol(DailySales)
NumberofProviders <- (ncol(DailySales)-(which( colnames(DailySales)=="Price.Position.1" )-1))/3
for(j in 1:nrow(DailySales)){
  ColumnMatch <- match("Apricot Agg (OGI standalone)",DailySales[j,(which( colnames(DailySales)=="Price.Position.1"):UploadColumnNumbers)])
  if(!is.na(ColumnMatch)){
    DailySales[j,UploadColumnNumbers+1] <- DailySales[j, ColumnMatch+93]
    DailySales[j,UploadColumnNumbers+2] <- DailySales[j, ColumnMatch+94]
    DailySales[j,UploadColumnNumbers+3] <- (ColumnMatch)/3
  }
}
colnames(DailySales)[UploadColumnNumbers+1] <- "Selected.Provider.Price"
colnames(DailySales)[UploadColumnNumbers+2] <- "Insurer"
colnames(DailySales)[UploadColumnNumbers+3] <- "Apricot.Position"
DailySales <- DailySales[, c((-1:which( colnames(DailySales)=="Price.Position.4" )+2), (ncol(DailySales)-2):(ncol(DailySales)))]

#Premium Range Calculation
DailySales <- data.frame(DailySales[1:(bspot-1)], Price.Returned.Range = cut(as.numeric(DailySales$Selected.Provider.Price), 
                                                                             breaks = c(-Inf, 250, 500, 750, 1000, 2000, 3000, 5000, 7000, 10000, 15000, 20000, 30000, Inf), 
                                                                             labels = c("Less than £250", "£250-£500", "£500-£750", "£750-£1000", "£1000-£2000", "£2000-£3000", "£3000-£5000", "£5000-£7000", "£7000-£10,000", "£10,000-£15,000", "£15,000-£20,000", "£20,000-£30,000", "£30,000 +"), 
                                                                             right = FALSE), DailySales[(bspot):ncol(DailySales)])


##### UPDATE MASTER REPORT
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/SalesAnalysis")
Master <- read.csv("ApricotSalesMasterHeaderOnly.csv", stringsAsFactors =FALSE)
##### ADD DAILY SALES
Master <- rbind(Master, DailySales)

##### Renewal Date Setup
Master$BTXDatecreated <- gsub("29/02/2016", "28/02/2016", Master$BTXDatecreated)
Master$BTXDatecreated <- as.Date(Master$BTXDatecreated, "%d/%m/%Y")
Master$Renewal <- (Master$BTXDatecreated) + years(1)

# ##### ADD ADDITIONAL PRODUCTS
# ADDITIONALPRODUCTS <- subset(x, BTXTrantype != "Cancellation" & BTXTrantype !="Endorsement" & BTXTrantype !="Journal")
# ADDON <- aggregate(as.numeric(ADDITIONALPRODUCTS$BTXCommamt), by=list(Category=ADDITIONALPRODUCTS$UserID), FUN=sum)
# Master$AddOnValue <- Master$AddOnValue + ADDON$x[match(Master$UserID, ADDON$Category)]
# Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

##### ADD ADDITIONAL PRODUCTS
ADDITIONALPRODUCTS <- subset(x, BTXTrantype != "Cancellation" & BTXTrantype !="Endorsement" & BTXTrantype !="Journal")
#ADDITIONALPRODUCTS$BTXDatecreated <- as.Date(ADDITIONALPRODUCTS$BTXDatecreated, "%d/%m/%Y")
for( i in 1: nrow(Master)){
  ADDITIONALPRODUCTS1 <- subset(ADDITIONALPRODUCTS , ADDITIONALPRODUCTS$UserID == Master$UserID[i] & ADDITIONALPRODUCTS$BTXDatecreated >= Master$BTXDatecreated[i] & ADDITIONALPRODUCTS$BTXDatecreated < Master$Renewal[i])
  Master$AddOnValue[i] <- sum(ADDITIONALPRODUCTS1$BTXOrigdebt)
}
Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

##### ADD ENDORSEMENTS AND JOURNALS
ADDITIONALPRODUCTS2 <- subset(x, BTXTrantype == "Endorsement" | BTXTrantype == "Journal")
#ADDITIONALPRODUCTS2$BTXDatecreated <- as.Date(ADDITIONALPRODUCTS$BTXDatecreated, "%d/%m/%Y")
for( i in 1: nrow(Master)){
  ADDITIONALPRODUCTS1 <- subset(ADDITIONALPRODUCTS2 , ADDITIONALPRODUCTS2$BTXPolref == Master$BTXPolref[i] & ADDITIONALPRODUCTS2$BTXDatecreated >= Master$BTXDatecreated[i] & ADDITIONALPRODUCTS2$BTXDatecreated < Master$Renewal[i])
  Master$AddOnValue[i] <- Master$AddOnValue[i] + sum(ADDITIONALPRODUCTS1$BTXOrigdebt)
}
Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

# ADDON2 <- aggregate(as.numeric(ADDITIONALPRODUCTS2$BTXCommamt), by=list(Category=ADDITIONALPRODUCTS2$BTXPolref), FUN=sum)
# Master$AddOnValue <- Master$AddOnValue + ADDON2$x[match(Master$BTXPolref, ADDON2$Category)]
# Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

##### ADD CANCELLATIONS
CANCELLATIONS <- subset(Data, BTXTrantype == "Cancellation")
CANCELLATIONS$BTXDatecreated <- as.Date(CANCELLATIONS$BTXDatecreated, "%d/%m/%Y")

Master <- merge(Master,CANCELLATIONS, by="BTXPolref", all.x = TRUE)
Master <- within(Master, Cancellation[BTXTrantype.y == 'Cancellation'] <- 'Cancellation')
Master$BTXCommamt.y[is.na(Master$BTXCommamt.y)] <- 0
Master$BTXTrantype.y[is.na(Master$BTXTrantype.y)] <- 0
for(i in 1:nrow(Master)){
  if(Master$BTXTrantype.y[i] == 'Cancellation' & Master$BTXDatecreated.y[i] >= Master$BTXDatecreated.x[i] & Master$BTXDatecreated.y[i] < Master$Renewal[i]){
    Master$CancellationCommission[i] <- Master$BTXCommamt.y[i]
    Master$TrafficCost[i] <- 0
    Master$FinanceValue[i] <- 0}}
Master <- Master[,1:which( colnames(Master)=="Renewal")]


##### ADD DISCOUNT
Charges <- subset(x, BTXTrantype == "Charge")
Charges$BTXDatecreated <- as.Date(Charges$BTXDatecreated, "%d/%m/%Y")
for( i in 1: nrow(Master)){
  Charges1 <- subset(Charges , Charges$BTXPolref == Master$BTXPolref[i] & Charges$BTXDatecreated >= Master$BTXDatecreated[i] & Charges$BTXDatecreated < Master$Renewal[i])
  Master$Discount[i] <- sum(Charges1$BTXOrigdebt)
}
Master$Discount[is.na(Master$Discount)] <- 0


Master$TotalValue <- Master$BTXCommamt + Master$CancellationCommission + Master$FinanceValue + Master$AddOnValue - Master$TrafficCost + Master$Discount

colnames(Master) <- gsub("[.]x", "", colnames(Master))
colnames(Master) <- gsub("[.]y", "", colnames(Master))
Master$Renewal <- NULL

write.table(Master, "ApricotSalesMaster4.csv", row.names= FALSE, sep=",")
