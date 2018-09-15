
charactersource <- "https://www.opensourceshakespeare.org/views/plays/characters/chardisplay.php"
rawcharacterlist <- charactersource %>% read_html %>%
  html_nodes("table") %>% magrittr::extract2(2) %>% 
  html_nodes(xpath="//td//span//b") %>% html_text()

characterlist <- str_split(rawcharacterlist, " ") %>% unlist

rhymedata <- unique(characterlist)

#Remove " 's " from all
rhymedata <- gsub("'s", "",  rhymedata, fixed=TRUE)
rhymedata <- str_replace_all(rhymedata, "[:punct:]", "")
rhymedata <- tolower(rhymedata)
rhymedata <- unique(rhymedata)
rhymedata <- as.data.frame(rhymedata, stringsAsFactors = FALSE)
names(rhymedata) <- "name"

rhymedata$first_two_letters <- str_sub(rhymedata$name, 1L, 2L)
rhymedata$last_two_letters <- str_sub(rhymedata$name, -2L, -1L)
rhymedata$first_three_letters <- str_sub(rhymedata$name, 1L, 3L)
rhymedata$last_three_letters <- str_sub(rhymedata$name, -3L, -1L)



#Can scrape from www.shakespeare-online.com/plays/characters/charactersA.html and so on,
#loop through pages, extract pronunciations, get number of syllables in each name

library(rvest)

baseurl <- "http://www.shakespeare-online.com/plays/characters/characters"
paths <- paste0(baseurl, LETTERS[-c(24,26)], ".html")

namelist <- read_html(paths[1]) %>% html_nodes('p') %>% html_text
for(i in 2:length(paths)){
names <- read_html(paths[i]) %>% html_nodes('p') %>% html_text
namelist <- c(namelist, names)
}
rm(names)
namelist <- namelist[-c(1373:1376)]

#Note that this misses <b>Aaron the Moor</b> - [AIR-un] <i>Titus Andronicus</i></p>, Autolycus
#bc they entered names irregularly
#However, does get most of them.


#Alright, looks like 1372 names.
#Should be about 800 characters.
#Start by just doing the ones that give pronunciation tips:
pronouncenames <- namelist[str_which(namelist, "\\[")]
pronouncenames <- as.data.frame(pronouncenames, stringsAsFactors = FALSE)
names(pronouncenames) <- "name"
pronouncenames$pronunciation <- str_extract(pronouncenames$name, "(?<=\\[)[A-Za-z\\-]+")
pronouncenames$syllablecount <- 1+str_count(pronouncenames$pronunciation, "\\-")
pronouncenames$syllable1 <- NA
pronouncenames$syllable2 <- NA
pronouncenames$syllable3 <- NA
pronouncenames$syllable4 <- NA
pronouncenames$syllable5 <- NA
library(magrittr)

syllablelist <- str_split(pronouncenames$pronunciation, pattern="\\-")

#This gives a 5-column data frame, but with repeats
pronouncenames[,c(4:8)] <- as.data.frame(do.call(rbind, syllablelist))
#I can clean up by using syllablecount to know which columns to set to NAs

pronouncenames[pronouncenames$syllablecount < 5,8] = NA
pronouncenames[pronouncenames$syllablecount < 4,7] = NA
pronouncenames[pronouncenames$syllablecount < 3,6] = NA
pronouncenames[pronouncenames$syllablecount < 2,5] = NA


#Also, add last_syllable and second_to_last_syllable columns
pronouncenames$secondtolastsyllable <- str_extract(pronouncenames$pronunciation, "[a-zA-Z]+(?=\\-[a-zA-Z]+$)")
pronouncenames$lastsyllable <- str_extract(pronouncenames$pronunciation, "[a-zA-Z]+$")
 
openxlsx::write.xlsx(pronouncenames, file = "shakespearerhymingnames.xlsx")


##Add list of elements, do stringdist fuzzy join by names and/or pronunciation to characters
library(pdftools)
library(fuzzyjoin)
library(stringr)
elementtext <- pdf_text(pdf="https://fac.ksu.edu.sa/sites/default/files/chem_element_pronunciation.pdf")

#clean up

#Okay, format:
#\n     Ac  ACTINIUM...................... ack-TIN-ee-um (\"um\" rhymes with \"gum\")
#Columns: symbol, elementname, pronunciation, notes
elementtext <- paste(elementtext[[1]], elementtext[[2]])
#Element symbols have a capital letter, then 0 or 1 lowercase, followed by at least 3 uppercase letters
symbols <- str_extract_all(elementtext, "\n([:space:])*[A-Z]([a-z]{0,1})(?=([:space:]*)[A-Z]{3})")
symbols <- unlist(symbols)
symbols <- str_replace_all(symbols, "\n", "")
symbols <- str_replace_all(symbols, "[:space:]", "")
#Added an initial S that looks incorrect; remove it
symbols <- symbols[-1]


#elementname is at least 3 capital letters, then 0 or 1 whitespace, then a lot of dots
elementname <- str_extract_all(elementtext, "([A-Z]{3,})([:space:]*)(\\.){3,}") %>% unlist
elementname <- str_replace_all(elementname, "(\\.)", "")
elementname <- str_trim(elementname)

#Pronunciations: might add an extra line for each one with multiple valid pronunciations
#preceded by dots and maybe whitespace, stop at either whitespace or a comma
pronunciation <- str_extract_all(elementtext, "(?<=(\\.){3}([:space:]{0,1}))[A-Za-z\\-]+([:space:]|\\,)") %>% unlist
#Clean out the \r. 
pronunciation <- str_replace_all(pronunciation, "\r", "")
#Let's ignore alt pronunciations for now. When want to use them, leave in trailing commas so know which have alt pronunciations.
pronunciation <- str_replace_all(pronunciation, "\\,", "")

pronunciation <- str_trim(pronunciation)

elements <- cbind(symbols, elementname, pronunciation) %>% data.frame

###Add syllable columns
elements$syllablecount <- 1+str_count(elements$pronunciation, "\\-")
elements$syllable1 <- NA
elements$syllable2 <- NA
elements$syllable3 <- NA
elements$syllable4 <- NA
elements$syllable5 <- NA
elements$syllable6 <- NA

e_syllablelist <- str_split(elements$pronunciation, pattern="\\-")

#This gives a 6-column data frame, but with repeats
elements[,c(5:10)] <- as.data.frame(do.call(rbind, e_syllablelist))
#I can clean up by using syllablecount to know which columns to set to NAs

elements[elements$syllablecount < 6,10] = NA
elements[elements$syllablecount < 5,9] = NA
elements[elements$syllablecount < 4,8] = NA
elements[elements$syllablecount < 3,7] = NA
elements[elements$syllablecount < 2,6] = NA

elements$secondtolastsyllable <- str_extract(elements$pronunciation, "[a-zA-Z]+(?=\\-[a-zA-Z]+$)")
elements$lastsyllable <- str_extract(elements$pronunciation, "[a-zA-Z]+$")

#Now, we want to match elements to character names. Probably the easiest initial test is to left join
#elements, characternames on last two columns, then try string dist joining them.

#I think best is to remove leading consonants, tolower everything, then try join
test_elements <- select(elements, elementname, pronunciation, secondtolastsyllable, lastsyllable)
test_characters <- select(pronouncenames, name, pronunciation, secondtolastsyllable, lastsyllable)

#Replace "me", "he", etc. with "ee"
test_elements$secondtolastsyllable <- str_to_lower(test_elements$secondtolastsyllable)
test_elements[!is.na(test_elements$secondtolastsyllable),3] <- str_replace_all(test_elements[!is.na(test_elements$secondtolastsyllable),3], "^[^aeiou](?=e)", "e")
test_elements[!is.na(test_elements$secondtolastsyllable),3] <- str_replace_all(test_elements[!is.na(test_elements$secondtolastsyllable),3], "^eee", "ee")

#remove 1 or 2 leading consonants

test_elements[!is.na(test_elements$secondtolastsyllable),3] <- str_replace_all(test_elements[!is.na(test_elements$secondtolastsyllable),3], "^[^(aeiou)]{1,2}", "")

#Replace "me", "he", etc. with "ee"
test_elements$lastsyllable <- str_to_lower(test_elements$lastsyllable)
test_elements$lastsyllable <- str_replace_all(test_elements$lastsyllable, "^[^aeiou](?=e)", "e")
test_elements$lastsyllable <- str_replace_all(test_elements$lastsyllable, "^eee", "ee")

#remove leading consonants
test_elements$lastsyllable <- str_replace_all(test_elements$lastsyllable, "^[^(aeiou)]{1,2}", "")

#Replace "me", "he", etc. with "ee"
test_characters$secondtolastsyllable <- str_to_lower(test_characters$secondtolastsyllable)
test_characters[!is.na(test_characters$secondtolastsyllable),3] <- str_replace_all(test_characters[!is.na(test_characters$secondtolastsyllable),3], "^[^aeiou](?=e)", "e")
test_characters[!is.na(test_characters$secondtolastsyllable),3] <- str_replace_all(test_characters[!is.na(test_characters$secondtolastsyllable),3], "^eee", "ee")

#remove 1 or 2 leading consonants
test_characters[!is.na(test_characters$secondtolastsyllable),3] <- str_replace_all(test_characters[!is.na(test_characters$secondtolastsyllable),3], "^[^(aeiou)]{1,2}", "")

#Replace "me", "he", etc. with "ee"
test_characters$lastsyllable <- str_to_lower(test_characters$lastsyllable)
test_characters$lastsyllable <- str_replace_all(test_characters$lastsyllable, "^[^aeiou](?=e)", "e")
test_characters$lastsyllable <- str_replace_all(test_characters$lastsyllable, "^eee", "ee")

#remove leading consonants
test_characters$lastsyllable <- str_replace_all(test_characters$lastsyllable, "^[^(aeiou)]{1,2}", "")

#Let's replace NAs
test_elements[is.na(test_elements$secondtolastsyllable),3] <- "!!!!!"
test_characters[is.na(test_characters$secondtolastsyllable),3] <- "!!!!!"

test <- inner_join(test_characters, test_elements, by=c("lastsyllable"))

#Inner_join by last syllable and by 2nd to last syllable both give a lot, but joining by both at once
#fails. Solution? try doing the two and then joining by character and element.
testlast <- inner_join(test_characters, test_elements, by=c("lastsyllable"))
testsecondtolast <- inner_join(test_characters, test_elements, by=c("secondtolastsyllable"))
testcompound <- inner_join(testlast, testsecondtolast, by=c("name", "elementname"))


#fuzzy join doesn't like NAs. Replace NAs with a symbol? Yeah, that'll do nicely.
test_2_elements <- select(elements, elementname, pronunciation, secondtolastsyllable, lastsyllable)
test_2_elements[is.na(test_2_elements$secondtolastsyllable),3] <- "!!!!!"
test_2_characters <- select(pronouncenames, name, pronunciation, secondtolastsyllable, lastsyllable)
test_2_characters[is.na(test_2_characters$secondtolastsyllable),3] <- "!!!!!"
test2 <- stringdist_left_join(test_2_elements, test_2_characters, max=1, by=c("lastsyllable", "secondtolastsyllable"))


#Try out:
test_raw_pronunciation <- stringdist_inner_join(test_2_characters, test_2_elements, by="pronunciation")

#Looks like best way is inner join by last syllable.
openxlsx::write.xlsx(test, file = "elementshakespearematches.xlsx")




#I thinkI can improve this with more care to cleaning up character syllables. "oh" to "o",
#"ahr" to "ar", "eehr" to "eer", "urr" to "ur", "ett" to "et", "awn" to "on","iff" to "if"
#And maybe rather than joins, I want to count matches. Count number of matching syllables.
#Alignment problem, where want to try different lineups of syllables to maximize matches.
#Doing last syllable is tough cause of number of -ums. Misses, for example, Lear-Cerium.
#So, what if I do a sort of moving window, where compare syllables of a character to those of an element,
#count up matches, assign to one with the most matches.

#Alright, first case: make a function to clean up syllables, then slide first of
#characters over first element to see how it goes.

#I think best is to remove leading consonants, tolower everything, then try join
syllables_element <- select(elements, paste0("syllable", 1:6))
syllables_element[,c(1:6)] <- apply(syllables_element, 2, as.character)
syllables_element[,c(1:6)] <- apply(syllables_element, 2, tolower)

syllables_character <- select(pronouncenames, paste0("syllable", 1:5))
syllables_character[,c(1:5)] <- apply(syllables_character, 2, as.character)
syllables_character[,c(1:5)] <- apply(syllables_character, 2, tolower)

#I may want to do stringsasfactors false much earlier on.



#Okay, steps:
#compare syllable5 to syllable1
#compare 4 to 1 and 5 to 2
#compare 3 to 1, 4 to 2, and 5 to 3
#compare 2 to 1, 3 to 2, 4 to 3, 5 to 4
#compare 1 to 1, 2 to 2, 3 to 3, 4 to 4, 5 to 5
#compare 1 to 2, 2 to 3, 3 to 4, 4 to 5, 5 to 6
#compare 1 to 3, 2 to 4, 3 to 5, 4 to 6
#compare 1 to 4, 2 to 5, 3 to 6
#compare 1 to 5, 2 to 6
#compare 1 to 6
#10 steps. At each step, record the number of matches

#Maybe purrr::has_element is useful
library(purrr)
#detect_index() to align?
#reduce() or accumulate() might help
#magrittr is_in() or equals() might help

#Good chars to compare: syllables_character[c(61,308,450,451),]
#Bur trum, nor thum, voh lum nee ah, voh lum nee us
#"ur" is a good syllable to compare on
#167 ur ping hum is a good character to try
#also 212 hur mee uh

#This should take a column. Use map to apply it to all columns.
clean_syllables <- function(x){
  require(dplyr)
  require(stringr)
  #Replace "me", "he", etc. with "ee"
  x <- str_to_lower(x) %>% str_replace_all("^[^aeiou](?=e$)", "e") %>% str_replace_all("eee", "ee") %>%
    #Some specific ones I think should help:
    str_replace_all("ahr", "ar") %>%
    str_replace_all("eehr", "eer") %>%
    str_replace_all("urr", "ur") %>%
    str_replace_all("ett", "et") %>%
    str_replace_all("awn", "on") %>%
    str_replace_all("iff", "if") %>%
    str_replace_all("ox", "awks") %>%
    str_replace_all("our", "or") %>%
    str_replace_all("ore", "or") %>%
    str_replace_all("ongue", "ong") %>%
    str_replace_all("^[^(aeiou NA)]{1,2}", "")
  
  
  x
  
  #Ugh, this loses information with "bear" to "ear" and such though... might just tolower and leave it for now.
  
}


#alright, first try the clean_syllables fct below
clean_syllables_element <- as.data.frame(apply(syllables_element, 2, clean_syllables))
clean_syllables_element[,c(1:6)] <- apply(clean_syllables_element, 2, as.character)

clean_syllables_character <- as.data.frame(apply(syllables_character, 2, clean_syllables))
clean_syllables_character[,c(1:5)] <- apply(clean_syllables_character, 2, as.character)


#clean_syllables_character 212 has "ur ee" in syllable1 and syllable2
#should match clean_syllables_element 30, 32, and 98

#Maybe the intersect() fct, either dplyr or base version?
base::intersect(clean_syllables_character[212,], clean_syllables_element[30,])
#that gives a result, at least
#element 56 has ur ee in syllable2 and syllable3
base::intersect(clean_syllables_character[212,], clean_syllables_element[56])
#Yes! So, base intersect and count the non-nas?
base::intersect(clean_syllables_character[212,], clean_syllables_element[30,]) %>% unlist %>% n_distinct -1
base::intersect(clean_syllables_character[104,], clean_syllables_element[56,]) %>% unlist %>% n_distinct -1
#Okay, that counts matches! Great. (n_distinct may fail for ones with repeating syllables? ignore for now)
#removing NA and columns named 3.1 instead of 3 from the base::intersect output will give the uniqe values

base::intersect(clean_syllables_character[104,], clean_syllables_element[56,]) %>% select(-contains(".")) %>% unique %>% length() -1

count_matches <- function(firstmatch, secondmatch){
  matches <- base::intersect(firstmatch, secondmatch) %>% select(-contains(".")) %>% unique %>% length() -1

  return(matches)
}

apply(X=clean_syllables_element, 1, FUN=count_matches, secondmatch=clean_syllables_character[212,]) %>% which.max
#That returns the most matches. BUT, it looks like intersect can return matches that aren't adjacent... problem.
#match() maybe? can use it to align, at least.


find_best_matches <- function(columninsecondtable, firsttable){
  x <- apply(X=firsttable, 1, FUN=count_matches, secondmatch=columninsecondtable)
  
  #return the positions of all max values, including ties
  return(which(x==max(x), arr.ind=TRUE))
}

find_best_matches(clean_syllables_character[212,], clean_syllables_element)



#Can't get apply to work, so:
matches <- data.frame(pronouncenames[,1])
matches[,2:20] <- NA
matches <- data.frame(cbind(dput(paste0("match", 1:20))))
for(i in 1:nrow(clean_syllables_character)){
  matches[i,] <- find_best_matches(clean_syllables_character[i,], clean_syllables_element)
}
#Can't get this to work.


####Alright, come back and finish the matching later

save(list=ls(), file="rhymeanalysis.Rdata")
