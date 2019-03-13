library(rvest)
library(httr)
library(tidyverse)

# Settings
years = 2010:2018
searches = list(
  R = '"the R software" OR "the R project" OR "r-project.org" OR "R development core" OR "bioconductor" OR "lme4" OR "nlme" OR "lmeR function" OR "ggplot2" OR "Hmisc" OR "r function" OR  "r package" OR "mass package" OR "plyr package" OR "mvtnorm"',
  SPSS = 'SPSS -"SPSS Modeler" -"Amos"',
  SAS = '"SAS Institute" -JMP -"Enterprise Miner"',
  STATA = '("stata" "college station") OR "StataCorp" OR "Stata Corp" OR "Stata Journal" OR "Stata Press" OR "stata command" OR "stata module"',
  Prism = 'GraphPad Prism',
  JASP = '("jasp" (bayesian OR bayes OR wagenmakers) OR ("jasp package" OR "jasp software" OR "jasp team" OR "jasp-stats") -"jasper" -"joint attention symbolic" -EURASIP -"Journal of Applied School Psychology" -"Journal of Applied social psychology"',
)
sleep_interval = c(1, 10)  # Uniformly break between searches in this interval to prevent scholar from rejecting searches
scholar_prefix = 'https://scholar.google.dk/scholar?hl=en&as_sdt=0%2C5&as_ylo=9999&as_yhi=9999&q='


###################
# HANDY FUNCTIONS #
###################

# Build the URL string
get_url = function(software, year) {
  url_prefix = gsub('9999', as.character(year), scholar_prefix)  # Enter year
  url_search = gsub(' ', '+', searches[[software]])  # Escape spaces
  url_search = gsub('\"', '%22', url_search)  # Escape quotes
  url = paste(url_prefix, url_search, sep='')
  url
}

# Do the web search
get_html = function(url) {
  html = read_html(url)
  #html = content(GET(url))
  html
}

extract_citations = function(html) {
  # Extract the citation number
  hits_strings = html %>%
    html_nodes(css='.gs_ab_mdw') %>%  # Name of the class where we can find citation number
    html_text()
  hits_string = strsplit(hits_strings[2], ' ')[[1]][2]  # Second hit, second "word"
  hits_numeric = as.numeric(gsub(',', '', hits_string))  # As numeric, not string
  hits_numeric
}

get_citations = function(software, year) {
  # Sleep to prevent HTTP error 503
  sleep_duration = runif(1, sleep_interval[1], sleep_interval[2])
  Sys.sleep(sleep_duration)
  
  # Do the search
  url = get_url(software, year)
  html = get_html(url)
  citations =  extract_citations(html)
  
  # Status and return
  print(sprintf('Got %i scholar citations in %i for %s', citations, year, software))
  citations
}


#################
# DO THE SEARCH #
#################
citation_history = expand.grid(years, names(searches))
names(citation_history) = c('year', 'software')

citation_history = citation_history %>%
  filter(software == 'STATA') %>%
  rowwise() %>%
  mutate(
    citations = get_citations(software, year)
  )

# Save it so you don't have to repeat in case Scholar locks you out
write.csv(citation_history, 'citations.csv', row.names = F)
