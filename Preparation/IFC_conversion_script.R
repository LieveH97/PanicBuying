IFC <- data.frame(Date=seq(as.Date("2018-01-01"),as.Date("2020-12-31"),by="days"))
IFC$Week <- strftime(IFC$Date,format = "%V")
IFC$Date_IFC <- c(paste(rep("2018-",365), c(rep(sprintf('%0.2d', 1:13), each=28),13), "-", c(rep(sprintf('%0.2d', 1:28), 13),29),sep=""),
                  paste(rep("2019-",365), c(rep(sprintf('%0.2d', 1:13), each=28),13), "-", c(rep(sprintf('%0.2d', 1:28), 13),29),sep=""),
                  paste(rep("2020-",169), c(rep(sprintf('%0.2d', 1:6), each=28),"06"), "-", c(rep(sprintf('%0.2d', 1:28), 6),29),sep=""),
                  paste(rep("2020-",197), c(rep(sprintf('%0.2d', 7:13), each=28),13), "-", c(rep(sprintf('%0.2d', 1:28), 7),29),sep=""))
IFC$Week_IFC <- c(rep(sprintf('%0.2d', 1:52),each=7),NA,rep(sprintf('%0.2d', 1:52),each=7),NA,rep(sprintf('%0.2d', 1:24),each=7),NA,
                  rep(sprintf('%0.2d', 25:52),each=7),NA)

save(IFC, file="C:/PhD KU Leuven/OneDrive - KU Leuven/Projects_Github/RetailCOVID19/R_Files/Preparation/conversion_IFC.RData")
