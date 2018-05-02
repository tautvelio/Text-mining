library(tm)
library(wordcloud)
library(memoise)
library(markdown)

###
# Knygų sąrašas
###
books <<- list(
  "Vincas Mykolaitis-Putinas" = "altoriusesely",
  "Balys Sruoga. Dievų miškas" = "dievumiskas",
  "Ignas Šeinius. Kuprelis" = "kuprelis"
  
)

###
# Patvirtina jeigu knyga yra sąraše
###
validateBook <- function (book){
  if (!(book %in% books)){
      stop("Tokios knygos sąraše nėra")
  }
}

###
# Gauname knygos eilutes
###
getBookLines <- memoise(function(book){
  # Patvirtina knygą
  validateBook(book)

  # Pildo failo pavadinimą
  fileName <- paste(c("data/", book, ".txt"), collapse = "")

  # Skaito knygą iš failo
  text.v <- scan(fileName, what="character", sep="\n")

  # Atpažįsta pirmą ir paskutinę eilutes
  chapter.1.position <- grep("^CHAPTER 1", text.v)
  the.end.position <- grep("^THE END", text.v)

  text.v[chapter.1.position:the.end.position]
})

###
# Gauname visą knygos tekstą
###
getBookText <- memoise(function(book){
  book.lines <- getBookLines(book)
  paste(book.lines, collapse = " ")
})

###
# Gauname skyrių sąrašą
###
getBookChapters <- function(book){

  # Sukuria tuščią vektorių chapters
  chapters <- c()

  book.lines <- getBookLines(book)
  book.lines.quantity <- length(book.lines)

  chapter.positions <- grep("^CHAPTER \\d", book.lines)
  chapter.quantity <- length(chapter.positions)

  for (i in 1:(chapter.quantity - 1)){

      chapter.lines <- book.lines[chapter.positions[i]:chapter.positions[i + 1]]
      chapter <- paste(chapter.lines, collapse = " ")
      chapters <- c(chapters, chapter)
  }

  # Prideda paskutinį skyrių
  chapter.lines <- book.lines[chapter.positions[chapter.quantity]:book.lines.quantity]
  chapter <- paste(chapter.lines, collapse = " ")
  chapters <- c(chapters, chapter)

  
  # Grąžina chapters
  chapters
}

getTermMatrixPerBook <- memoise(function(book, quantity){
  
  text <- getBookText(book)
  matrix <- getTermMatrix(text)
  matrix[1:quantity]
})

getPerChapterDataFrame <- memoise(function(book, quantity){

  # Ieško žodžių, kurios naudosime
  text <- getBookText(book)
  matrix <- getTermMatrix(text)
  matrix <- matrix[1:quantity]
  words <- names(matrix)

  # Gauna chapters
  chapters <- getBookChapters(book)

  # Renka duomenis
  chapterV <- c()
  wordV <- c()
  countV <- c()

  for (j in 1:length(words)) {

    word <- words[j]

    for (i in 1:length(chapters)){

      chapter <- chapters[i]
      chapterMatrix <- getTermMatrix(chapter)

      chapterV <- c(chapterV, i)
      wordV <- c(wordV, word)

      if (word %in% names(chapterMatrix)){

        countV <- c(countV, chapterMatrix[[word]])

      } else {

        countV <- c(countV, 0)
      }
    }
  }

  # Data frame su duomenimis
  df <- data.frame(chapter = chapterV, word = wordV, count = countV)
  df$chapter <- as.factor(df$chapter)
  df$word <- factor(df$word, levels = words)
  df$count <- as.numeric(df$count)

  df
})

###
# Term Document Matrix
###
getTermMatrix <- memoise(function(text) {

  
  corpus = Corpus(VectorSource(text))
  # Visos raidės mažosios
  corpus = tm_map(corpus, content_transformer(tolower))
  # Panaikina kablelius
  corpus = tm_map(corpus, removePunctuation)
  # Panaikina skaicius
  corpus = tm_map(corpus, removeNumbers)

  myDTM =
    TermDocumentMatrix(
      corpus,
      # Nustato mažiausią žodžio ilgį
      control = list(wordLengths = c(6,Inf))
    )
  matrix = as.matrix(myDTM)

  # Matricos rušiavimas
  sort(rowSums(matrix), decreasing = TRUE)
})
