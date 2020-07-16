# Functions for COPE project - cleaning and analysis 

## directory set 
data_dir <- "../clean_data/"

## GGPLOT AESTHETICS 

color_list <- c("navy", "light blue", "pink", "dark green", "purple")
my_colors_fill = scale_fill_manual(values = color_list)
my_colors_color = scale_color_manual(values = color_list)

custom_theme <- theme_bw()  +
  # center the caption
  theme(plot.caption = element_text(hjust = 0.5),
  # make axis text larter
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 14),
 # plot.caption = element_text(size = 10),
  # make bold title
  plot.title = element_text(face="bold"),
  # add nicer header for facets
  strip.text = element_text(size = 16))


## my function to generate simple descriptives from numeric variables
numeric_descriptives <- function(variable_list, dataframe){
  
  dataselect <- dataframe %>%
    select(variable_list) 
  
  means <- dataselect %>%
    map_df(mean, na.rm = T)
  sds <- dataselect %>%
    map_df(sd, na.rm = T)
  
  table <- data.frame(rbind(means,sds)) %>%
    mutate(value = c("Mean", "SD")) %>%
    select(value, everything ())
  return(table)
}


## function for concerting numeric 4pt likert scale to rating
convert_4pt_likert <- function(variable){
  new_factor <- paste0(variable, "_factor")
  new_factor =  as.factor(ifelse(variable== 0, "No distress",
                                 ifelse(variable == 1, "Mild distress", 
                                        ifelse(variable == 2, "Moderate distress", 
                                               ifelse(variable == 3, "High distress", NA)))))
  return(new_factor)
}

# function for converting numeric 7 pt likert scale to similar rating 
convert_7pt_likert <- function(variable){
  new_factor <- paste0(variable, "_factor")
  new_factor =  as.factor(ifelse(variable== 0, "No distress",
                                 ifelse(variable == 1 | variable == 2, "Mild distress", 
                                 ifelse(variable == 3 | variable == 4, "Moderate distress",  
                                 ifelse(variable == 5 | variable == 6, "High distress", NA)))))
  return(new_factor)
}




  # for word clouds, from Anna Niedbala
wordMatrix = function(text,stopwords,n) {
  
  text=lemmatize_strings(textProcess(text,stopwords))
  text=gsub("\\bnumb\\b"," number ",text)
  text=gsub("\\bnumberer"," number ",text)
  text=gsub("\\be mail\\b"," email ",text)
  text=gsub("\\bre sell\\b"," resell ",text)
  text=gsub("\\bo k\\b"," okay ",text)
  text=gsub("\\badvert[[:alpha:]]+\\b"," ads ",text)
  text=gsub("\\blist\\b"," ad ",text)
  text=gsub("\\bdatum\\b"," date ",text)
  text=lemmatize_strings(textProcess(text,""))
  corpus=VCorpus(VectorSource(text))    
  BigramTokenizer <-
    function(x) unlist(lapply(lapply(ngrams(words(x$content), n),sort), paste, collapse = " "), use.names = FALSE)
  
  dtm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer,removePunctuation = TRUE))
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  print(head(d, 10))
  
  set.seed(1234)
  
  return(d)
}

# source from an R markdown script 
# https://gist.github.com/noamross/a549ee50e8a4fd68b8b1
source_rmd = function(file, skip_plots = TRUE) {
  temp = tempfile(fileext=".R")
  knitr::purl(file, output=temp)
  
  if(skip_plots) {
    old_dev = getOption('device')
    options(device = function(...) {
      .Call("R_GD_nullDevice", PACKAGE = "grDevices")
    })
  }
  source(temp)
  if(skip_plots) {
    options(device = old_dev)
  }
}
