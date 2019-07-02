########################Data prep

#Read in data
nsfData2 <- read.csv("/Users/user/Desktop/Gender Boldness/NSF_2006to2017_3.csv")

#Remove NA columns
#index <- map_lgl(nsfData, ~ all(is.na(.)))
#nsfData <- nsfData[!index,]

na.omit(nsfData2)

boxplot(Award ~ Gender, data = nsfData, lwd = 2, ylab = 'NUMS')
stripchart(Awarde ~ Gender, vertical = TRUE, data = nsfData, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue')

