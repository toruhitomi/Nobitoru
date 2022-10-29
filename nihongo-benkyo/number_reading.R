number_reading <- function(input) {
  
  if (!is.numeric(input)) {
    input <- as.numeric(input)
  }
  if (is.na(input)) stop("input was something wrong!")
  
  # Base set
  base <- data.frame(
    number = 0:9,
    reading = c("zero", "ichi", "ni", "san", "yon", "go", "roku", "nana", "hachi", "kyu")
  )
  units <- data.frame(
    number = 10^seq(0, 12),
    reading = c("", "juu", "hyaku", "sen", "man", "juu man", "hyaku man", "sen man", "oku", "juu oku", "hyaku oku", "sen oku", "cho")
  )
  
  input0 <- input
  power_of_10 <- floor(log(input0, 10))
  num_seq <- numeric(length = power_of_10 + 1)
  target <- character()
  for (ii in seq(from = power_of_10, to = 0)) {
    if (input0 > 0 & floor(log(input0, 10)) == ii) {
      tmp <- floor(input0 / (10 ^ ii))
      num_seq[length(num_seq) - ii] <- tmp
      input0 <- input0 - tmp * (10^ii)
      
      target <- c(target, sprintf("%s%s", 
                                  base$reading[tmp+1], 
                                  units$reading[units$number == 10^ii]))
    }
  }
  
  target <- stringr::str_flatten(target, " ")
  
  # Dealing with irregularities
  target <- gsub("ichijuu", "juu", target)
  target <- gsub("ichihyaku", "hyaku", target)
  target <- gsub("ichisen", "sen|issen", target)
  
  target <- gsub("sanhyaku", "sanbyaku", target)
  target <- gsub("rokuhyaku", "roppyaku", target)
  target <- gsub("hachihyaku", "happyaku", target)
  
  target <- gsub("sansen", "sanzen", target)
  target <- gsub("hachisen", "hassen", target)
  
  if (power_of_10 > 0) {
    target <- gsub(" zero|zero ", "", target)
  }
  
  return(target)
}
