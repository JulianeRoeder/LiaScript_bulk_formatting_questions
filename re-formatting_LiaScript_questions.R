#### Re-formatting LiaScript questions in R
#### author: Juliane Röder"
#### date: 2024-05-22

## Load table with questions

dat <- read.table("Pool_of_questions.csv", sep = ";", header = TRUE, 
                  encoding = "latin1",  # encoding to cover German Umlaute
                  comment.char = "")

## Select all parts of a question for re-assembly

liaQuestions <- 
  Reduce(function(...) rbind(...), 
         lapply(1:dim(dat)[1], function(i) {
           
           qes <- dat$Question[i]  # value of question text
           
           ansx <- grep("Option", names(dat)) # positions of all answers
           ansx <- ansx[dat[i , ansx] != "" & !is.na(dat[i , ansx])]  # reduce to the given options
           
           posc <- ansx[(substr(dat[i , ansx], 1, 1) == "*") & !is.na(dat[i , ansx])] # position(s) of correct answer(s)
           
           cora <- dat[i , posc]  # value(s) of correct answer(s)
           cora <- sub("^.", "", cora)  # "^." targets any first character of a string
           
           posw <- ansx[!(ansx %in% posc)] # position(s) of wrong answer(s)
           
           wroa <- dat[i , posw]  # value(s) or wrong answers
           
           if(dat$question_type[i] == "scq") {
             corfield <- "- [(X)] "
             wrofield <- "- [( )] "
           } else {
             if (dat$question_type[i] == "mcq") {
               corfield <- "- [[X]] "
               wrofield <- "- [[ ]] " 
               
             } else {
               print("Not a single choice or multiple choice question.")
             }
           }
           
           qeslia <- paste(paste(qes, "\n", sep = ""),
                           "\n",
                           paste(corfield, cora, "\n", sep = "", collapse = ""),
                           paste(wrofield, wroa, "\n", sep = "", collapse = ""),
                           "\n", sep = ""
           )
           
           return(qeslia)
         })
  )

write(liaQuestions, file = "LiaScript_questions.md")
