#Teil 3-e
# Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert kategorisiert

#Ueberpruefen, ob die eingegebene Variable quantilbasiert kategorisiert werden kann.
transformieren <- function(x)
{
  if (is.numeric(x)) return(x)
  else
  {
    if(is.factor(x)) 
    {
      x <- as.numeric(x)
      return(x)
    }
    else stop("Diese Variable kann nicht quantilbasiert kategorisiert werden")
  }
}

#Kategorisierung der Variable
kategorisieren <- function(x)
{
  qtil <- quantile(x,  probs = c(1/3, 2/3))
  vec <- x
  for(i in c(1:length(vec))) 
  {
    if(vec[i] >= qtil[2]) vec[i] <- "Hoch"
    else
    {
      if(vec[i] >= qtil[1]) vec[i] <- "Mittel"
      else vec[i] <- "Niedrig"
    }
    levels(vec) <- c("Niedrig", "Mittel", "Hoch")
  }
  return(vec)
}

#Die main Funktion zu benutzen
func_e <- function(x)
{
  
  return(kategorisieren(transformieren(x)))
}

x <- sample(c(1:10), 100, replace=TRUE)
y <- factor(sample(c(1:15), 100, replace=TRUE))
z <- factor(sample(c("sehr gering","gering","eher gering","normal","eher hoch","hoch","sehr hoch"), 100, replace=TRUE),
            levels = c("sehr gering","gering","eher gering","normal","eher hoch","hoch","sehr hoch"))

func_e(x)
func_e(y)
func_e(z)