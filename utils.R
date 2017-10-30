

preprocess <- function(dt){
  dt[, Pclass := factor(Pclass, ordered = F, levels = c(1,2,3))] # no ordered, as MLR does not support it
  
  meanFares <- dt[, .(meanFare = mean(Fare, na.rm = T)), by = Pclass]
  
  dt[is.na(Fare), Fare:= meanFares[Pclass==dt[is.na(Fare),Pclass], meanFare] ]
  
  spltNames <- strsplit(dt$Name, c(","), fixed=T)
  dt[, LastName := sapply(spltNames, function(x) x[1])]
  restName <- sapply(spltNames, function(x) x[2])
  
  spltNames <- strsplit(restName, c("."), fixed=T)
  dt[, Title := sapply(spltNames, function(x) trimws( x[1]))]
  dt <- dt[Title == "Don", Title := "Mr"]
  dt <- dt[Title %in% c("Mme","Dona"), Title := "Mrs"]
  dt <- dt[Title == "Mlle", Title := "Miss"]
  dt <- dt[Title %in% c("Capt","Major"), Title := "Col"]
  dt[, Title :=factor(Title)]
  
  dt[ is.na(Cabin), Cabin:="Missing"]
  dt[ Cabin != "", CabinType := substr(Cabin, 1, 1)]
  dt[ Cabin == "", CabinType := "X"]
  dt[, CabinType := factor(CabinType)]
  
  # meanAges <- dt[, .(meanAge=mean(Age, na.rm=T)), by = Title]
  # #meanAgesPclass <- dt[, .(meanAge=mean(Age, na.rm=T)), by = c("Title","Pclass")]
  # 
  # replAges <- sapply(dt[is.na(Age)]$Title, function(x){meanAges[Title==x]$meanAge})
  # dt[is.na(Age), Age:= replAges]
  
  dt
} 

featureEngineering <- function(dt){
  setDT(dt)
  dt[, Child := ( Age<18 )]
  dt$FamilySize <- dt$SibSp + dt$Parch + 1
  dt[,Nobility := Title %in% c("Jonkheer","the Countess","Sir","Lady") ]
  dt<-as.data.frame(dt)
  dt
}

reType <- function(dt){
  for (field in c("Name","LastName","Cabin","Ticket","Child","Nobility")){
    dt[[field]] <- as.factor(dt[[field]])
  }
  dt
}

checkLevels <- function(tr,te, verbose=F){
  
  nm <- names(tr)
  fct <- sapply(nm, function(x) is.factor(tr[[x]]))
  fn <- nm[fct]
  
  for (field in fn) {
    
    if (verbose){
      print(field)
      print(levels(tr[[field]]))
      print(levels(te[[field]]))
      print("")
    }
    
    levels(te[[field]]) <- levels(tr[[field]])
    
  }
  
  return(te)
    
}
