andreasen <- read.csv("reports/andreasens_sizeindex.csv")
standard <- read.csv("data/container_assessment.csv")

si <- andreasen[andreasen$volume >= 20,]

unique(andreasen$volume)

andreasen_45 <- andreasen[andreasen$volume == 45,]
  andreasen_45$pass <- ifelse(andreasen_45$sizeindex >=51 & andreasen_45$sizeindex <=75, "pass", "fail")
  
andreasen_75 <- andreasen[andreasen$volume == 75,]
  andreasen_75$pass <- ifelse(andreasen_75$sizeindex >=79 & andreasen_75$sizeindex <=117, "pass", "fail")
  
andreasen_100 <- andreasen[andreasen$volume == 100,]
  andreasen_100$pass <- ifelse(andreasen_100$sizeindex >=102 & andreasen_100$sizeindex <=150, "pass", "fail")
  
andreasen_200 <- andreasen[andreasen$volume == 200,]
  andreasen_200$pass <- ifelse(andreasen_200$sizeindex >=185 & andreasen_200$sizeindex <=272, "pass", "fail")
  
andreasen_400 <- andreasen[andreasen$volume == 400,]
  andreasen_400$pass <- ifelse(andreasen_400$sizeindex >=330 & andreasen_400$sizeindex <=494, "pass", "fail")
  
andreasen_750 <- andreasen[andreasen$volume == 750,]
  andreasen_750$pass <- ifelse(andreasen_750$sizeindex >=577 & andreasen_750$sizeindex <=849, "pass", "fail")
  
andreasen_1500 <- andreasen[andreasen$volume == 1500,]
  andreasen_1500$pass <- ifelse(andreasen_1500$sizeindex >=1048 & andreasen_1500$sizeindex <=1542, "pass", "fail")
  
andreasen_2000 <- andreasen[andreasen$volume == 2000,]
  andreasen_2000$pass <- ifelse(andreasen_2000$sizeindex >=1343 & andreasen_2000$sizeindex <=1975, "pass", "fail")

    
table(andreasen_45$pass == "pass")

