library(lubridate)
library(plyr)
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")
Data <- read.csv("DailyStats3.csv", stringsAsFactors =FALSE)
PaymentDates <- read.csv("PaymentDates.csv", stringsAsFactors =FALSE)

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")
PCLFinance <- read.csv("Commercial PCL Commission Tracker.csv", stringsAsFactors =FALSE)

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")

#Data$BTXDatecreated <- as.Date(Data$BTXDatecreated, "%d/%m/%Y")
Data <- merge(Data,PaymentDates, by=c("BTXPolref","BTXTrantype", "BTXDatecreated", "BTXDtraised"),all.x=TRUE)
Data <- Data[!duplicated(Data), ]
#Data$BTXDatecreated <- as.Date(Data$BTXDatecreated, "%d/%m/%Y")
#Data <- transform(Data, min = pmin(as.Date(Data$BTXPaydt.x, "%d/%m/%Y"), as.Date(Data$BTXPaydt.y, "%d/%m/%Y"), na.rm = TRUE))
Data <- transform(Data, min = pmin(as.Date(Data$BTXPaydt.x, "%d/%m/%Y"), as.Date(Data$BTXPaydt.y, "%Y-%m-%d"), na.rm = TRUE))
Data$BTXPaydt.x <- Data$min
colnames(Data)[colnames(Data) == 'BTXPaydt.x'] <- 'BTXPaydt'
#colnames(Data)[colnames(Data) == 'BTXDatecreated.x'] <- 'BTXDatecreated'

Data$BTXPaydt.y <- NULL
#Data$BTXDatecreated.y <- NULL
Data$min <- NULL

PaymentDates <- Data[, c("BTXPolref", "BTXTrantype", "BTXDatecreated", "BTXDtraised", "BTXPaydt")]
PaymentDates <- PaymentDates[!is.na(PaymentDates$BTXPaydt),]
PaymentDates <- PaymentDates[!duplicated(PaymentDates), ]
write.csv(PaymentDates, "PaymentDates.csv", row.names = F)

Data$BTXDatecreated <- as.Date(Data$BTXDatecreated, "%d/%m/%Y")

Data$UserID <- substr(Data$BTXPolref, 1, 6)
Data$BFDPbalance[is.na(Data$BFDPbalance)] <- 0
Data$BTXTrantype <- gsub('Transfrd NB', 'Renewal', Data$BTXTrantype)
Data[is.na(Data$BTXPaydt) & Data$BTXTrantype == "Renewal", "BTXTrantype"] <- "Pending Renewal"
Data$BTXDatecreated[Data$BTXTrantype == "Renewal"] <- as.character(Data$BTXPaydt[Data$BTXTrantype == "Renewal"])
Data$BTXDatecreated[Data$BTXTrantype == "Pending Renewal"] <- as.Date(Data$BTXDtraised[Data$BTXTrantype == "Pending Renewal"], "%d/%m/%Y")
PolicySales <- subset(Data, (BTXTrantype == "New Business" | BTXTrantype == "Renewal" | BTXTrantype == "Pending Renewal") & (BTXPoltype %in% c("AG", "AR", "BD", "CA", "CC", "CL", "CP", "CR", "CV", "CW", "DO", "EB", "EL", "EN", "FL", "FP", "FS", "GT", "HH", "HL", "HO", "HQ", "HT",  "LI", "LP", "LV", "MA", "MC", "MT", "MV", "MY", "NH", "OC", "OS", "PB", "PC", "PD", "PI", "PK", "PL", "PO", "RP", "SB", "SC", "SH", "SL", "TH", "TL", "TM", "TR", "TV", "TW", "XX")))
PolicySales[is.na(PolicySales$BTXPaydt) & PolicySales$BTXTrantype == "Renewal", "BTXTrantype"] <- "Pending Renewal"

x <- rbind(Data, PolicySales)
x <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(Data), ]
x$BTXDtraised <- as.Date(x$BTXDtraised, "%d/%m/%Y")
PolicySales$FinanceValue <- round(PolicySales$BFDPbalance * .096, 2)

PCLFinance$BTXPolref <- PCLFinance$Broker.Ref
z <- lapply(intersect(PolicySales$BTXPolref, PCLFinance$BTXPolref),function(id) {
  d1 <- subset(PolicySales,BTXPolref==id)
  d2 <- subset(PCLFinance,BTXPolref==id)
  
  d1$indices <- sapply(d1$BTXDatecreated,function(d) which.min(abs((as.Date(d2$Funded.Date, "%d/%m/%Y") - d))))
                       d2$indices <- 1:nrow(d2)
                       
                       merge(d1,d2,by=c('BTXPolref','indices'))
})
  
  z2 <- do.call(rbind,z)
  z2$indices <- NULL
  z2 <- z2[c("BTXPolref", "BTXDatecreated", "Overrider", "Funded.Date")]
  PolicySales <- merge(PolicySales, z2, by.x=c('BTXPolref', "BTXDatecreated"), by.y=c('BTXPolref', "BTXDatecreated"), all.x = T)
  PolicySales$Overrider[is.na(PolicySales$Overrider)] <- 0
  PolicySales[(PolicySales$Overrider != 0) & ((as.Date(PolicySales$Funded.Date, "%d/%m/%Y") <  (as.Date(PolicySales$BTXDtraised, "%d/%m/%Y"))) | (as.Date(PolicySales$Funded.Date, "%d/%m/%Y") - (as.Date(PolicySales$BTXDtraised, "%d/%m/%Y")) > 350)), "Overrider"] <- 0
  PolicySales$FinanceValue <- PolicySales$FinanceValue +  PolicySales$Overrider
  PolicySales$Overrider <- NULL
  PolicySales$Funded.Date <- NULL
  #PolicySales$FinanceValue2 <- z2$Overrider[ z2$BTXPolref %in% PolicySales$BTXPolref]


###### Traffic Costs and Sources PART 1
PolicySales$TrafficCost <- 0
PolicySales$Source <- "None"
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "PC"] <- 42.50
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "TW"] <- 46.50
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "HQ"] <- 42.50
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "HQ" & (PolicySales$HWBuildingssi == 0 | PolicySales$HWContentssi == 0) & PolicySales$BTXDatecreated > "2017-02-08"] <- 36.50

### SINGLE COMBINED HOME ###
#PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "HQ" &  ] <- 36.50
### SINGLE COMBINED HOME ###

PolicySales$TrafficCost[grepl("APRCC-", PolicySales$ECWebref, ignore.case=FALSE)] <- 50
PolicySales$TrafficCost[grepl("APRUS-", PolicySales$ECWebref, ignore.case=FALSE)] <- 50
PolicySales$Source[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Quotezone"
PolicySales$Source[grepl("APRCC-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Call Connection"
PolicySales$Source[grepl("APRUS-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Uswitch"
PolicySales[PolicySales$BTXTrantype == "Renewal" | PolicySales$BTXTrantype == "Pending Renewal" , "TrafficCost"] <- 0

##### Assign Executive
code = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
Executive = c("Mark", "Audrey", "Aine", "Louise", "Megan", "Elaine", "Susan", "Stephen", "Admin User")
execRef <- data.frame(code, Executive)
code2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, "B", "C", "D", "J", "M", "O")
Payment = c("Mastercard", "Visa", "Visa Delta", "Visa Electron", "Maestro", "Credit Card 6", "Credit Card 7", "Credit Card 8", "Credit Card 9", "Bank Credit", "Cheque", "Paid Direct", "Journal", "Money/Cash", "Bank-Other")
paymentRef <- data.frame(code2, Payment)
PolicySales <- merge(PolicySales, execRef, by.x="BTXExec", by.y="code", all.x=TRUE)
PolicySales <- merge(PolicySales, paymentRef, by.x="BTXPaymethod", by.y="code2", all.x=TRUE)
colnames(PolicySales)[ncol(PolicySales)] <- "PaymentMethod"
PolicySales$Executive <- as.character(PolicySales$Executive)
PolicySales$Executive[is.na(PolicySales$Executive)] <- "Not Assigned"
PolicySales$PaymentMethod <- as.character(PolicySales$Payment)
PolicySales$PaymentMethod[is.na(PolicySales$Payment)] <- "Not Assigned"

PolicySales$Cancellation <- "N"
PolicySales$CancellationCommission <- 0
PolicySales$CancellationDate <- "2100-01-01"
PolicySales$AddOnValue <- 0
PolicySales$AddOnCount <- 0
PolicySales$Discount <- 0

##### Set Product
ProductCode = c("AG", "AR", "BD", "CA", "CC", "CL", "CP", "CR", "CV", "CW", "DO", "EB", "EL", "EN", "FL", "FP", "FS", "GT", "HH", "HL", "HO", "HQ", "HT",  "LI", "LP", "LV", "MA", "MC", "MT", "MV", "MY", "NH", "OC", "OS", "PB", "PC", "PD", "PI", "PK", "PL", "PO", "RP", "SB", "SC", "SH", "SL", "TH", "TL", "TM", "TR", "TV", "TW", "XX")
Product = c("Agricultural Vehicle", "All Risks", "Book Debts", "Caravan", "Commercial Combined", "Combined Liability", "Computer", "Classic Car", "Commercial Vehicle", "Contractors Works", "DirectorsAndOfficers", "Eng.Bus.Interrupt", "Employers Liability", "Engineering", "Fleet", "Fire & Perils", "Fire Schedule", "Goods In Transit", "Holiday Home", "Hauliers Liability", "Horses", "Household", "Hotels/Guesthouses", "Property Owners", "Loss Of Profits", "Livestock", "Marine", "Motorcycle", "Motor Trade", "Multi Vehicle", "Money", "Nursing Homes", "Office Combined", "Office And Surgery", "Property Balance", "Private Car", "Products Liability", "Prof Indemnity", "Commercial Package", "Public Liability", "Property Owners", "Pubs and Restaurants", "Self-Build", "Shop", "Shop2", "Sprinkler Leakage", "Theft", "Tenants Liability", "Tradesman", "Travel", "Travel2", "Truckwriter", "Miscellaneous")
ProductRef <- data.frame(ProductCode, Product)
PolicySales <- merge(PolicySales, ProductRef, by.x="BTXPoltype", by.y="ProductCode", all.x=TRUE)

PolicySales$TotalValue <- PolicySales$BTXCommamt + PolicySales$FinanceValue - PolicySales$TrafficCost

#####Age Calculation
Today <- as.Date("2017/04/01")
PolicySales$Age = round(as.numeric((Today - as.Date(PolicySales$BCMDob, "%d/%m/%Y")) / 365.25), 0)
PolicySales$Age[is.na(PolicySales$Age)] <- 0
PolicySales$Age.Range = cut(PolicySales$Age, 
                            breaks = c(-Inf, 17, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, Inf), 
                            labels = c("Not Provided", "17-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "70+"), 
                            right = FALSE)

#Hour of Day Created Calculation
PolicySales$Hour.Of.Day <- substr(PolicySales$QAActivequotetime, 1, 2)

#Day of Week Created Calculation
PolicySales$Day.of.Week = weekdays(PolicySales$BTXDatecreated)

#Week of Year Created Calculation
PolicySales$Week.of.Year = week(PolicySales$BTXDatecreated)

#Month Created Calculation
#PolicySales$Month1 = month((as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y")))
PolicySales$Month1 = strftime(PolicySales$BTXDatecreated, "%y.%m")
PolicySales$Month2 = substr(months(PolicySales$BTXDatecreated), 1, 3)
PolicySales$Month <- with(PolicySales, paste0(Month1, ". ", Month2))
PolicySales$Month1 <- NULL
PolicySales$Month2 <- NULL

#Month Created Calculation
#PolicySales$Month1 = month((as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y")))
PolicySales$Month1 = strftime(as.Date(PolicySales$BTXDtraised, "%d/%m/%Y"), "%y.%m")
PolicySales$Month2 = substr(months(as.Date(PolicySales$BTXDtraised, "%d/%m/%Y")), 1, 3)
PolicySales$MonthStartDate <- with(PolicySales, paste0(Month1, ". ", Month2))
PolicySales$Month1 <- NULL
PolicySales$Month2 <- NULL

#Year Calculation
#PolicySales$Year = (as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y"))
PolicySales$Year = as.numeric(format(PolicySales$BTXDatecreated,'%Y'))

#Email Domain
PolicySales$Email.Domain = sub("^[^@]*", "", PolicySales$BCMEmail)

#UK Postcode Area and Region
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool")
PostcodeRegions <- read.csv("PostcodeRegions.csv")
PolicySales$Post.Code.District = gsub( " .*$", "", PolicySales$BCMPcode)
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
c <- read.csv("AllSalesReport-2016-10.csv", stringsAsFactors =FALSE)
d <- read.csv("AllSalesReport-2016-11.csv", stringsAsFactors =FALSE)
e <- read.csv("AllSalesReport-2016-12.csv", stringsAsFactors =FALSE)
f <- read.csv("AllSalesReport-2017-01.csv", stringsAsFactors =FALSE)
g <- read.csv("AllSalesReport-2017-02.csv", stringsAsFactors =FALSE)
h <- read.csv("AllSalesReport-2017-03.csv", stringsAsFactors =FALSE)
Sales <- rbind(a, b, c, d, e, f, g, h)
Sales <- subset(Sales, COMPANY == "Apricot Agg (OGI standalone)")
Sales$POSTCODE <- toupper(Sales$POSTCODE)

DailySales <- merge(PolicySales, Sales, by.x=c("BCMDob", "BCMPcode"), by.y=c("BIRTH.DATE", "POSTCODE"), all.x=TRUE)

##### COMBINE QUOTES REPORT

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/Quote Reports")
Quotes <- read.csv("ExistingSalesQuotes.csv", stringsAsFactors =FALSE)
#Quotes1 <- read.csv("Quotes12.csv", stringsAsFactors =FALSE)
#Quotes2 <- read.csv("Quotes01.csv", stringsAsFactors =FALSE)
#Quotes3 <- read.csv("Quotes02.csv", stringsAsFactors =FALSE)
Quotes4 <- read.csv("Quotes03.csv", stringsAsFactors =FALSE)
Quotes5 <- read.csv("Quotes04.csv", stringsAsFactors =FALSE)
Quotes <- rbind(Quotes, Quotes4, Quotes5)
#ExistingSalesQuotes <- Quotes[which(Quotes$Date.Of.Birth %in% DailySales$BCMDob & Quotes$Post.Code %in% DailySales$BCMPcode), ]
#write.csv(ExistingSalesQuotes, "ExistingSalesQuotes.csv", row.names=FALSE)

#DailySales[DailySales$BTXPoltype == "PC",] <- merge(DailySales[DailySales$BTXPoltype == "PC",], Quotes, by.x=c("BCMDob", "BCMPcode"), by.y=c("Date.Of.Birth", "Post.Code"), all.x=TRUE)
DailySales <- merge(DailySales, Quotes, by.x=c("BCMDob", "BCMPcode"), by.y=c("Date.Of.Birth", "Post.Code"), all.x=TRUE)
DailySales[DailySales$BTXPoltype != "PC",][,which( colnames(DailySales)=="Quote.Reference" ):which( colnames(DailySales)=="Site.Name.110" )] <- NA
DailySales <- DailySales[!is.na(DailySales$BTXPolref),]
DailySales <- within(DailySales, Source[!is.na(Quote.Reference) & Source == 'None'] <- 'Quotezone')
DailySales <- within(DailySales, TrafficCost[!is.na(Quote.Reference) & Source == 'None' & BTXTrantype == "New Business" & BTXPoltype == "PC"] <- 42.5)
DailySales <- within(DailySales, TrafficCost[!is.na(Quote.Reference) & Source == 'None' & BTXTrantype == "New Business" & BTXPoltype == "HH"] <- 42.5)
DailySales <- within(DailySales, TrafficCost[!is.na(Quote.Reference) & Source == 'None' & BTXTrantype == "New Business" & BTXPoltype == "TW"] <- 46.5)
#setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")
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
                                                                            breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1800, 2000, 2500, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000, 15000, 20000, 25000, 30000, 35000, 50000, Inf), 
                                                                            labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-£500", "£500-£600", "£600-£700", "£700-£800", "£800-£900", "£900-£1,000", "£1,000-£1,100", "£1,100-£1,200", "£1,200-£1,300", "£1,300-£1,400", "£1,400-15,00", "£1,500-£1,600", "£1,600-£1,800", "£1,800-£2,000", "£2,000-£2,500", "£2,500-£3,000", "£3,000-£4000", "£4,000-£5,000", "£5,000-£6,000", "£6,000-£7,000", "£7,000-£8,000", "£8,000-£9,000", "£9,000-£10,000", "£10,000-£12,000", "£12,000-£15,000", "£15,000-£20,000", "£20,000-£25,000", "£25,000-£30,000", "£30,000-£35,000", "£35,000-£50,000", "£50,000+"),
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
    DailySales[j,UploadColumnNumbers+1] <- DailySales[j, ColumnMatch+(which( colnames(DailySales)=="Price.Position.1" )-3)]
    DailySales[j,UploadColumnNumbers+2] <- DailySales[j, ColumnMatch+(which( colnames(DailySales)=="Price.Position.1" )-2)]
    DailySales[j,UploadColumnNumbers+3] <- (ColumnMatch)/3
  }
}
colnames(DailySales)[UploadColumnNumbers+1] <- "Selected.Provider.Price"
colnames(DailySales)[UploadColumnNumbers+2] <- "Insurer"
colnames(DailySales)[UploadColumnNumbers+3] <- "Apricot.Position"
DailySales <- DailySales[, c((-1:which( colnames(DailySales)=="Price.Position.4" )+2), (ncol(DailySales)-2):(ncol(DailySales)))]
DailySales <- DailySales[order(DailySales$Apricot.Position),] 
DailySales <- ddply(DailySales, c("BTXPolref","BTXDatecreated"), head, 1)
#DailySales <- unique(DailySales, by = c('BTXPolref','BTXDatecreated'))



#Premium Range Calculation
DailySales <- data.frame(DailySales[1:(bspot-1)], Price.Returned.Range = cut(as.numeric(DailySales$Selected.Provider.Price), 
                                                                             breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1800, 2000, 2500, 3000,Inf), 
                                                                             labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-£500", "£500-£600", "£600-£700", "£700-£800", "£800-£900", "£900-£1,000", "£1,000-£1,100", "£1,100-£1,200", "£1,200-£1,300", "£1,300-£1,400", "£1,400-15,00", "£1,500-£1,600", "£1,600-£1,800", "£1,800-£2,000", "£2,000-£2,500", "£2,500-£3,000", "£3,000+"), 
                                                                             right = FALSE), DailySales[(bspot):ncol(DailySales)])

###### Traffic Costs and Sources PART 2
DailySales$TrafficCost[grepl("APR-", DailySales$ECWebref, ignore.case=FALSE) & DailySales$BTXPoltype == "PC" & DailySales$Apricot.Position > 0] <- 42.50
DailySales$TrafficCost[grepl("APR-", DailySales$ECWebref, ignore.case=FALSE) & DailySales$BTXPoltype == "TW" & DailySales$QUOTE.REFERENCE != "NA"] <- 46.50
DailySales$TrafficCost[grepl("APR-", DailySales$ECWebref, ignore.case=FALSE) & DailySales$BTXPoltype == "HQ" & DailySales$QUOTE.REFERENCE != "NA"] <- 42.50
DailySales$TrafficCost[grepl("APR-", DailySales$ECWebref, ignore.case=FALSE) & DailySales$BTXPoltype == "HQ" & (DailySales$HWBuildingssi == 0 | DailySales$HWContentssi == 0) & DailySales$BTXDatecreated > "2017-02-08" & DailySales$QUOTE.REFERENCE != "NA"] <- 36.50


##### UPDATE MASTER REPORT
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/SalesAnalysis")
Master <- read.csv("ApricotSalesMasterHeaderOnly.csv", stringsAsFactors =FALSE)
##### ADD DAILY SALES
Master <- rbind(Master, DailySales)

##### Renewal Date Setup
Master$BTXDtraised1 <- Master$BTXDtraised
Master$BTXDtraised1 <- gsub("29/02/2016", "28/02/2016", Master$BTXDtraised1)
Master$BTXDtraised <- as.Date(Master$BTXDtraised, "%d/%m/%Y")
Master$BTXDtraised1 <- as.Date(Master$BTXDtraised1, "%d/%m/%Y")
Master$Renewal <- (Master$BTXDtraised1) + years(1)
Master$BTXDtraised1 <- NULL

##### ADD ADDITIONAL PRODUCTS
ADDITIONALPRODUCTS <- subset(x, BTXTrantype != "Cancellation" & BTXTrantype !="Endorsement" & BTXTrantype !="Journal" & BTXTrantype !="Charge")
ADDITIONALPRODUCTSCANCELLED <- subset(x, BTXTrantype == "Cancellation" & BTXTrantype !="Endorsement" & BTXTrantype !="Journal" & BTXTrantype !="Charge")
#ADDITIONALPRODUCTS <- ADDITIONALPRODUCTS[(!((ADDITIONALPRODUCTS$BTXPolref %in% ADDITIONALPRODUCTSCANCELLED$BTXPolref) & (ADDITIONALPRODUCTS$BTXDtraised <= ADDITIONALPRODUCTSCANCELLED$BTXDtraised))),]
ADDITIONALPRODUCTS <- merge(ADDITIONALPRODUCTS, ADDITIONALPRODUCTSCANCELLED, by.x=c('BTXPolref', 'BTXDtraised'), by.y=c('BTXPolref', 'BTXDtraised'), all.x = TRUE)
ADDITIONALPRODUCTS <- ADDITIONALPRODUCTS[!grepl("Cancellation", ADDITIONALPRODUCTS$BTXTrantype.y),]
ADDITIONALPRODUCTS <- ADDITIONALPRODUCTS[, 1:(ncol(ADDITIONALPRODUCTSCANCELLED))]

for( i in 1: nrow(Master)){
  ADDITIONALPRODUCTS1 <- subset(ADDITIONALPRODUCTS , ADDITIONALPRODUCTS$UserID == Master$UserID[i] & ADDITIONALPRODUCTS$BTXDtraised >= Master$BTXDtraised[i] & ADDITIONALPRODUCTS$BTXDtraised < Master$Renewal[i])
  Master$AddOnValue[i] <- sum(ADDITIONALPRODUCTS1$BTXCommamt)
  Master$AddOnCount[i] <- nrow(subset(ADDITIONALPRODUCTS1, ADDITIONALPRODUCTS1$BTXCommamt != 0))
}
Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

##### ADD ENDORSEMENTS AND JOURNALS
ADDITIONALPRODUCTS2 <- subset(x, BTXTrantype == "Endorsement" | BTXTrantype == "Journal")
#ADDITIONALPRODUCTS2$BTXDatecreated <- as.Date(ADDITIONALPRODUCTS$BTXDatecreated, "%d/%m/%Y")
for( i in 1: nrow(Master)){
  ADDITIONALPRODUCTS1 <- subset(ADDITIONALPRODUCTS2 , ADDITIONALPRODUCTS2$BTXPolref == Master$BTXPolref[i] & ADDITIONALPRODUCTS2$BTXDtraised >= Master$BTXDtraised[i] & ADDITIONALPRODUCTS2$BTXDtraised < Master$Renewal[i])
  Master$AddOnValue[i] <- Master$AddOnValue[i] + sum(ADDITIONALPRODUCTS1$BTXCommamt)
  Master$AddOnCount[i] <- Master$AddOnCount[i] + nrow(subset(ADDITIONALPRODUCTS1, ADDITIONALPRODUCTS1$BTXCommamt != 0))
}
Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

##### ADD CANCELLATIONS
CANCELLATIONS <- subset(Data, BTXTrantype == "Cancellation")
CANCELLATIONS$BTXDtraised <- as.Date(CANCELLATIONS$BTXDtraised, "%d/%m/%Y")

Master <- merge(Master,CANCELLATIONS, by="BTXPolref", all.x = TRUE)
Master$BTXDatecreated.y <- Master$BTXDatecreated.y
#Master <- within(Master, Cancellation[BTXTrantype.y == 'Cancellation'] <- 'Cancellation')
Master$BTXCommamt.y[is.na(Master$BTXCommamt.y)] <- 0
Master$BTXTrantype.y[is.na(Master$BTXTrantype.y)] <- 0
for(i in 1:nrow(Master)){
  if(Master$BTXTrantype.y[i] == 'Cancellation' & Master$BTXDtraised.y[i] >= Master$BTXDtraised.x[i] & Master$BTXDtraised.y[i] < Master$Renewal[i]){
    Master$CancellationCommission[i] <- Master$BTXCommamt.y[i]
    Master$Cancellation[i] <- "Y"
    Master$CancellationDate[i] <- toString(Master$BTXDatecreated.y[i])
    if(as.numeric(Master$BTXDatecreated.y[i] - Master$BTXDtraised.x[i]) < 15 & Master$Source[i] != "Quotezone"){
      Master$TrafficCost[i] <- 0}
    if(as.numeric(Master$BTXDatecreated.y[i] - Master$BTXDtraised.x[i]) < 30 & Master$Source[i] == "Quotezone"){
      Master$TrafficCost[i] <- 0}
    Master$FinanceValue[i] <- round((floor(as.double(difftime(Master$BTXDtraised.y[i], Master$BTXDtraised.x[i])/30.42))-1)*(Master$FinanceValue[i]/11), 2)
    if(Master$FinanceValue[i] <0){Master$FinanceValue[i] = 0}
  }}
Master <- Master[,1:which( colnames(Master)=="Renewal")]

##### ADD DISCOUNT
Charges <- subset(x, BTXTrantype == "Charge")
#Charges$BTXDatecreated <- as.Date(Charges$BTXDatecreated, "%d/%m/%Y")
for( i in 1: nrow(Master)){
  Charges1 <- subset(Charges , Charges$BTXPolref == Master$BTXPolref[i] & Charges$BTXDtraised >= Master$BTXDtraised[i] & Charges$BTXDtraised < Master$Renewal[i])
  Master$Discount[i] <- round(sum(Charges1$BTXOrigdebt), 2)
}
Master$Discount[is.na(Master$Discount)] <- 0

Master$TotalValue <- round(Master$BTXCommamt + Master$CancellationCommission + Master$FinanceValue + Master$AddOnValue - Master$TrafficCost + Master$Discount, 2)

#Premium Range Calculation
bspot <- which(names(Master)== "ECWebref.x")
Master <- data.frame(Master[1:(bspot-1)], BTXOrigdebt.Range = cut(as.numeric(Master$BTXOrigdebt), 
                                                                  breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1800, 2000, 2500, 3000,Inf), 
                                                                  labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-£500", "£500-£600", "£600-£700", "£700-£800", "£800-£900", "£900-£1,000", "£1,000-£1,100", "£1,100-£1,200", "£1,200-£1,300", "£1,300-£1,400", "£1,400-15,00", "£1,500-£1,600", "£1,600-£1,800", "£1,800-£2,000", "£2,000-£2,500", "£2,500-£3,000", "£3,000+"), 
                                                                  right = FALSE), Master[(bspot):ncol(Master)])

colnames(Master) <- sub('[.]x$', '', colnames(Master))
colnames(Master) <- sub('[.]y$', '', colnames(Master))
Master$Renewal <- NULL

write.table(Master, "ApricotSalesMaster2.csv", row.names= FALSE, sep=",")


#### EndorsementValues
Endorsements <- subset(Data, BTXTrantype == "Endorsement")
Endorsements <- merge(Endorsements, Master, by="BTXPolref", all.x=TRUE)
Endorsements <- Endorsements[Endorsements$BTXDatecreated.x > Endorsements$BTXDatecreated.y, ]
Endorsements <- Endorsements[rev(order(Endorsements$BTXDatecreated.y)),]
Endorsements <- Endorsements[, 1:19]
Endorsements <- unique(Endorsements)


Charges <- subset(Data, BTXTrantype == "Charge")
Charges = aggregate(BTXOrigdebt~BTXPolref+BTXDatecreated, data=Charges, sum, na.rm=TRUE)

Endorsements <- merge(Endorsements, Charges, by.x=c("BTXPolref", "BTXDatecreated.x"), by.y=c("BTXPolref", "BTXDatecreated"), all.x=TRUE)
#v <- Endorsements$BTXDatecreated.x==Endorsements$BTXDatecreated
Endorsements$BTXOrigdebt <- replace(Endorsements$BTXOrigdebt, which(Endorsements$BTXDatecreated.x != Endorsements$BTXDatecreated), 0)

Endorsements <- Endorsements[,c("BTXPolref", "BTXDatecreated.x", "BTXCommamt.x", "BTXOrigdebt")]
Endorsements$Total <- Endorsements$BTXCommamt.x + Endorsements$BTXOrigdebt
Endorsements = aggregate(Total~BTXPolref+BTXDatecreated.x, data=Endorsements, sum, na.rm=TRUE)

write.table(Endorsements, "Endorsements.csv", row.names= FALSE, sep=",")

#### CancellationValues
