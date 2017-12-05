## Advent of code 

## Dec 1st

captcha <- "3893445835429722678558456317563893861752455542588369533636585887178232467588827193173595918648538852463974393264428538856739259399322741844613957229674619566966921656443476317729968764183945899765294481327998956154956571467872487576314549468261122281384513266834769436913544431258253346374641589492728885222652146158261225296144835682556133922436438188211288458692217737145834468534829945993366314375465767468939773939978272968388546791547526366348163672162245585168892858977723516752284597322176349412485116173844733679871253985762643852151748396593275274582481295864991886985988427966155944392352248314629138972358467959614279553511247863869663526823326467571462371663396188951696286916979923587358992127741723727623235238531991996999181976664226274715591531566495345212849683589582225465555847312199122268773923175183128124556249916458878785361322713513153175157855597289482439449732469754748544437553251412476225415932478849961897299721228198262823515159848941742786272262236888514421279147329383465929358896761449135917829473321834267122759371247338155787774952626616791265889922959653887288735233291968146648533754958199821789499914763279869931218136266492627818972334549751282191883558361871277375851259751294611921756927694394977764633932938573132221389861617195291742156362494769521829599476753198422283287735888197584327719697758442462886311961723849326959213928195182293316227334998926839139915138472514686689887874559367524254175582135318545912361877139367538434683933333264146289842238921989275112323681356256979576948644489986951538689949884787173194457523474156229389465725473817651516136514446513436419126533875125645855223921197481833434658264655912731133356464193251635637423222227273192628825165993827511625956856754776849919858414375874943572889154281862749595896438581889424559988914658387293414662361364793844213298677236787998677166743945812899526292132465751582925131262933636228593134861363493849168168765261647652342891576445292462341171477487223253795935253493869317616741963486473"
captcha2 <- paste0(captcha,substr(captcha,1,1))
captcha2Items <- strsplit(captcha2,"")[[1]]

sum = 0
for (j in seq(2,length(captcha2Items))){
  i = j-1
  if (captcha2Items[i] == captcha2Items[j]){
    sum = sum + as.numeric(captcha2Items[i])
  }
}

captchaItems <- strsplit(captcha,"")[[1]]
captchaItemsExt <- c(captchaItems,captchaItems[1:(length(captcha2Items)/2)])


sum = 0
for (j in seq(((length(captchaItems)/2)+1),length(captchaItemsExt))){
  i = j-(length(captchaItems)/2)
  if (captchaItemsExt[i] == captchaItemsExt[j]){
    sum = sum + as.numeric(captchaItemsExt[i])
  }
}


# Dec 2nd
library(matrixStats)
checkSums <- fread('/Users/rogersmeets/Documents/adventofcode/dec2_input.txt')

checkSums[ , difMaxMin := rowMaxs(as.matrix(.SD))-rowMins(as.matrix(.SD)), .SDcols = names(checkSums)]
sum(checkSums$difMaxMin)

checkSums[ , difMaxMin := NULL]

rowResults <- NULL
for (i in seq(1,nrow(checkSums))){
  theRow <- as.numeric(unlist(checkSums[c(i),]))
  print(theRow)
  for (j in seq(1,length(theRow))){
    for (k in seq(1,length(theRow))){
      if (j != k){
        if (theRow[j] %% theRow[k] == 0){
          rowResults <- c(rowResults,theRow[j]/theRow[k])
        }
      }
    }
  }
}

# Dec 3rd

createSpiral <- function(maxValue){
  
  x <- 0
  y <- 0
  val <- 1
  counter <- 1
  countvec <- rep(1,counter)
  
  while (val<maxValue){
    if (counter %% 2 != 0){
      for (i in countvec){
        if(val == maxValue){
          break
        }
        else{
          x <- x+i
          val <- val+1
        }
      }
      for (i in countvec){
        if (val == maxValue){
          break
        }
        else{
          y <- y+i
          val <- val+1
        }
      }
    }
    if (counter %% 2 == 0){
      for (i in countvec){
        if (val == maxValue){
          break
        }
        else{
          x <- x-i
          val <- val+1
        }
      }
      for (i in countvec){
        if (val == maxValue){
          break
        }
        else{
          y <- y-i
          val <- val+1
        }
      }
    }
  counter <- counter+1
  countvec <- rep(1, counter)
  }
return(c(x,y,val))
}

theResults <- createSpiral(289326)
destination <- theResults[1:2]
origin <- c(0,0)

manhattan_dist <- function(rating1, rating2){
  distance <- abs(rating1-rating2)
  distance <- sum(distance)
  return(distance)
}

manhattan_dist(destination, origin)


createNewSpiral <- function(maxValue){
  
  x <- 0
  y <- 0
  val <- 1
  counter <- 1
  countvec <- rep(1,counter)
  valueList <- list('0,0'=1)
  
  while (valueList[length(valueList)][[1]]<=maxValue){
    if (counter %% 2 != 0){
      for (i in countvec){
        if(valueList[length(valueList)][[1]] > maxValue){
          break
        }
        else{
          x <- x+i
          val <- 0
          for (i in c(x-1,x,x+1)){
            for (j in c(y-1,y,y+1)){
              if (!is.null(valueList[paste(i,j,sep=',')][[1]])){
                val <- val + valueList[paste(i,j,sep=',')][[1]]
              }
            }
          }
          valueList[paste(x,y,sep=',')] <- val
        }
      }
      for (i in countvec){
        if (valueList[length(valueList)][[1]] > maxValue){
          break
        }
        else{
          y <- y+i
          val <- 0
          for (i in c(x-1,x,x+1)){
            for (j in c(y-1,y,y+1)){
              if (!is.null(valueList[paste(i,j,sep=',')][[1]])){
                val <- val + valueList[paste(i,j,sep=',')][[1]]
              }
            }
          }
          valueList[paste(x,y,sep=',')] <- val
        }
      }
    }
    if (counter %% 2 == 0){
      for (i in countvec){
        if (valueList[length(valueList)][[1]] > maxValue){
          break
        }
        else{
          x <- x-i
          val <- 0
          for (i in c(x-1,x,x+1)){
            for (j in c(y-1,y,y+1)){
              if (!is.null(valueList[paste(i,j,sep=',')][[1]])){
                val <- val + valueList[paste(i,j,sep=',')][[1]]
              }
            }
          }
          valueList[paste(x,y,sep=',')] <- val
        }
      }
      for (i in countvec){
        if (valueList[length(valueList)][[1]] > maxValue){
          break
        }
        else{
          y <- y-i
          val <- 0
          for (i in c(x-1,x,x+1)){
            for (j in c(y-1,y,y+1)){
              if (!is.null(valueList[paste(i,j,sep=',')][[1]])){
                val <- val + valueList[paste(i,j,sep=',')][[1]]
              }
            }
          }
          valueList[paste(x,y,sep=',')] <- val
        }
      }
    }
    counter <- counter+1
    countvec <- rep(1, counter)
  }
  return(val)
}

# Dec 4th

checkValidity <- function(passphrase){
  
  tokens <- strsplit(passphrase, split = " ")
  for (i in seq_along(tokens[[1]])){
    for (j in seq_along(tokens[[1]])){
      if (i != j){
        if (tokens[[1]][i] == tokens[[1]][j]){
          theBreak <- T
          break
        }
      }
    theBreak <- F  
    }
  if (theBreak){
    break
    }
  }

  if (theBreak){  
    return(F)
  }
  else{
    return(T)
  }
}

checkValidity("aa bb cc dd aaa")

passPhrases <- readLines('/Users/rogersmeets/Documents/adventofcode/day4_passphrases.txt')

validVector <- sapply(passPhrases, checkValidity)
sum(validVector)


checkNewValidity <- function(passphrase){
  
  tokens <- strsplit(passphrase, split = " ")
  for (i in seq_along(tokens[[1]])){
    for (j in seq_along(tokens[[1]])){
      if (i != j){
        if (nchar(tokens[[1]][i]) == nchar(tokens[[1]][j])){
          tokenizei <- strsplit(tokens[[1]][i],'')[[1]]
          tokenizej <- strsplit(tokens[[1]][j],'')[[1]]
          
          tokenizei <- sort(tokenizei)
          tokenizej <- sort(tokenizej)
          
          if (all(tokenizei == tokenizej)){
            theBreak <- T
            break
          }
        }
      }
      theBreak <- F  
    }
    if (theBreak){
      break
    }
  }
  
  if (theBreak){  
    return(F)
  }
  else{
    return(T)
  }
}

  validVector <- sapply(passPhrases, checkNewValidity)
  sum(validVector)

# Dec 5th
  
testVec <- c(0,3,0,1,-3)

exitSteps <- function(vector){
  
  stepNum <- 0
  stillIn <- T
  index <- 1
  
  while (stillIn == T){
    
    memory <- index
    index <- index + vector[index]
    vector[memory] <- vector[memory]+1
    
    if (is.na(vector[index])){
      stepNum <- stepNum + 1
      stillIn <- F
      break
    }
  
  stepNum <- stepNum + 1
  }
  return(stepNum)  
}

actualVec <- as.numeric(readLines('/Users/rogersmeets/Documents/adventofcode/day5_input.txt'))
  
exitSteps(actualVec)


exitNewSteps <- function(vector){
  
  stepNum <- 0
  stillIn <- T
  index <- 1
  
  while (stillIn == T){
    
    memory <- index
    index <- index + vector[index]
    if (vector[memory] >= 3){
      vector[memory] <- vector[memory]-1
    }
    else{
      vector[memory] <- vector[memory]+1  
    }
    
    if (is.na(vector[index])){
      stepNum <- stepNum + 1
      stillIn <- F
      break
    }
    
    stepNum <- stepNum + 1
  }
  return(stepNum)  
}

exitNewSteps(actualVec)
