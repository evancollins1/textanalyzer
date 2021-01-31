####################################
# Evan Collins
# 18 January 2021
####################################


# See in RStudio viewer or browser
# particles()
# Load TASA corpus file and udmodel file when publishing

 
# Shiny usage: call particles in your UI

# if (interactive()) {
  
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(particlesjs)
  library(rgl)
  library(readtext)
  library(stringr)
  library(udpipe)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(word2vec)
  library(text2vec)
  library(Rtsne)
  library(ggplot2)
  library(plotly)
  library(LSAfun)
  library(Rmisc)
  library(gridExtra)
  library(syuzhet)
  library(wordcloud)
  library(ngram)
  library(base)
  library(rlang)
  library(mgsub)
  library(radarchart)
  library(quanteda)
  library(scales)
  library(network)
  
  
  
  ui <- fluidPage(
    
    particles(),
    
    # Set background color
    setBackgroundColor(
      color=c("#3A416A"),
    ),
    
    # Set text color
    tags$head(tags$style('h1 {color:white;}')),
    tags$head(tags$style('h3 {color:white;}')),
    tags$head(tags$style('h4 {color:white;}')),
    tags$head(tags$style('h5 {color:white;}')),
    tags$head(tags$style('h6 {color:white;}')),
    
    # App title ----
    titlePanel(h1("📚 Text Analyzer")),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Upload .docx (Microsoft Word) file",
                  multiple = FALSE,
                  accept = c(".docx")),
        
        # Include clarifying text ----
        helpText("Note: Ensure to upload a .docx file that contains text only. Text must be at least two sentences. Wait up to 100 seconds to process."),
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        
        h3("Coherence"),
        h5("📖This is a measure of the similarity in contextual meaning between two adjacent sentences."),
        h5('✏️Consider the following example:'),
        h5('"The car is fantastic. I am happy I decided to purchase the car. The dog is black."'),
        h5('Coherence is calculated by comparing the contextual meaning of two adjacent sentences. In this example, sentence pair 1+2 is given a coherence value of ~0.80, whereas sentence pair 2+3 is given a coherence value of ~0.05.'),
        h5("📈The plot below displays the sequence of sentence pairs on the x-axis and the coherence values on the y-axis."),
        h6("📝(Boring🥱) Technical details: This measure utilizes Latent Semantic Analysis (LSA). LSA analyzes solves relations between word and passage meanings using Singular Value Decomposition (SVD). LSA creates a word-document matrix, with each row representing a unique word in the text and each column representing a text document or other unit of context (e.g. a sentence). 
The entries in this matrix are the frequency of the word in the context. An SVD of the matrix is then applied which results in a 100–500 dimensional semantic space. In the derived semantic space, words, sentences, paragraphs, or any other unit of text are represented as vectors by the sum of the vectors of the individual words contained in the text. The word and large unit of text vectors can be compared against each other in order to measure the amount of semantic similarity."),
        plotOutput('plot_coh'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Emotion"),
        h5("📖This is a measure that can classify sentences according to associations with eight discrete emotions: anger, fear, anticipation, trust, surprise, sadness, joy, and disgust. Note that some sentences may not be classified."),
        h5('✏️Consider the following example:'),
        h5('"I am afraid of heights. I love pie though."'),
        h5('The first sentence is classified as fear. The second sentence is classified as joy.'),
        h5("📈The plot below displays the eight different emotions on the x-axis and the sentence count on the y-axis."),
        h6("📝(Boring🥱) Technical details: This measure implements Saif Mohammad's NRC Emotion lexicon."),
        plotOutput('plot_emotion'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Percent Positive and Negative Sentiment Sentences"),
        h5("📖This is a measure that can classify sentences according to associations with two discrete emotions: positive and negative. Note that some sentences may not be classified."),
        h5('✏️Consider the following example:'),
        h5('"I am afraid of heights. I love pie though."'),
        h5('The first sentence is classified as negative. The second sentence is classified as positive.'),
        h5("📈The plot below displays a lovely 🥧 chart that reflects the proportion of sentences that are classified as either positive or negative."),
        h6("📝(Boring🥱) Technical details: This measure implements Saif Mohammad's NRC Emotion lexicon."),
        plotOutput('plot_posneg'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Positive (+) / Negative (-) Sentiment Score by Sentence"),
        h5("📖This is a measure that quantifies the sentiment of each sentence. The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment."),
        h5('✏️Consider the following example:'),
        h5('"I am afraid of heights. I love pie though."'),
        h5('The first sentence has a sentiment score of -0.75. The second sentence has a sentiment score of 0.75.'),
        h5("📈The plot below displays the sentence number on the x-axis and the sentiment score on the y-axis."),
        h6("📝(Boring🥱) Technical details: This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
        plotOutput('plot_posneg_sentence'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Overall Text Sentiment"),
        h5("📖This is a measure that quantifies the sentiment of the entire text. The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment."),
        h5('✏️Consider the following example:'),
        h5('"I love biking. It is so fun."'),
        h5('The overall text sentiment score is 1.5.'),
        h5("📈The box below displays the score."),
        h6("📝(Boring🥱) Technical details: This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy entire texts as well."),
        verbatimTextOutput('text_sent'),
        
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Most Positive Sentence of Text"),
        h5("📖This measure simply displays the sentence with the greatest positive sentiment score as calculated in the above measure."),
        h5('✏️Consider the following example:'),
        h5('"I am afraid of heights. I love pie though."'),
        h5('The first sentence has a sentiment score of -0.75. The second sentence has a sentiment score of 0.75. Thus, if this was the entire text provided, then the second sentence would be displayed for this measure.'),
        h5("📈The box below displays the sentence."),
        h6("📝(Boring🥱) Technical details: This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
        verbatimTextOutput('most_pos_sentence'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Most Negative Sentence of Text"),
        h5("📖This measure simply displays the sentence with the most negative sentiment score as calculated in the above measure."),
        h5('✏️Consider the following example:'),
        h5('"I am afraid of heights. I love pie though."'),
        h5('The first sentence has a sentiment score of -0.75. The second sentence has a sentiment score of 0.75. Thus, if this was the entire text provided, then the first sentence would be displayed for this measure.'),
        h5("📈The box below displays the sentence."),
        h6("📝(Boring🥱) Technical details: This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
        verbatimTextOutput('most_neg_sentence'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("25 Most Frequently Used Words of Text"),
        h5("📖This measure displays the 25 most frequently used words across the entire text."),
        h5("📈The plot below displays the 25 most frequently used word on the x-axis and their respective counts on the y-axis."),
        plotOutput('plot_most_freq_words'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words"),
        h5("📖This is a measure that quantifies the sentiment of the 25 most frequently used words (as featured immediately above). The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment."),
        h5("📈The plot below displays the word on the x-axis and the sentiment score on the y-axis."),
        h6("📝(Boring🥱) Technical details: This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
        plotOutput("plot_sent_most_freq_words"),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words with Non-Zero Sentiments"),
        h5("📖This is a measure that quantifies the sentiment of the 25 most frequently used words with non-zero sentiments. The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment. This measure first determines which words of the text have non-zero sentiment scores. The 25 most frequently used of these selected words are plotted below."),
        h5("📈The plot below displays the word on the x-axis and the sentiment score on the y-axis."),
        h6("📝(Boring🥱) Technical details: This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
        plotOutput("plot_sent_most_freq_words_nonzero"),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Word Cloud"),
        h5("📈The cool visualization below displays the 100 most frequently used words across the entire text. Size is proportional to frequency."),
        plotOutput("plot_word_cloud"),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Flesch Reading Ease"),
        h5('📖This is a measure of readability scored between 0 and 100. The higher the score, the more readable (i.e. more easily understood by younger demographics) it is. Scores 90-100 denote very easy to read and understandable to the average 5th grader. Scores 80-90 denote easy to read and understandable to the average 6th grader. Scores 70-80 denote fairly easy to read and understandable to the average 7th grader. Scores 60-70 denote "plain English" and understandable to the average 8th/9th grader. Scores 50-60 denote fairly difficult to read and understandable to the average 10th-12th grader. Scores 30-50 denote difficult to read and understandable to the average college student. Scores 10-30 denote very difficult to read and best understood by university graduates. Scores 0-10 denote extremely difficult to read and best understood by university graduates.'),
        h5("📈The box below displays the Flesch Reading Ease score."),
        h6("📝(Boring🥱) Technical details: Flesch readability tests work by taking into account sentence and word counts. The mathematical formula is as follows:"),
        h6("206.835 - 1.015*(total words/total sentences) - 84.6*(total syllables/total words)"),
        verbatimTextOutput('Flesch'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Flesch-Kincaid Grade Level"),
        h5('📖This is a measure of the approximate reading grade level of the text. For example, a Flesh-Kincaid level of 8 signifies that the average reader needs a grade 8 level of reading or above to understand it.'),
        h5("📈The box below displays the Flesch-Kincaid Grade Level score."),
        h6("📝(Boring🥱) Technical details: Flesch readability tests work by taking into account sentence and word counts. The mathematical formula is as follows:"),
        h6("0.39*(total words/total sentences) + 11.8*(total syllables/total words) - 15.59"),
        verbatimTextOutput('Flesch.Kincaid'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Mean Number of Syllables per Word"),
        h5('📖This is a measure that quantifies the average number of syllables per word."'),
        h5("📈The box below displays the mean number of syllables per word."),
        verbatimTextOutput('meanWordSyllables'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Word Count"),
        h5('📖This is a measure that quantifies the total word count of the text."'),
        h5("📈The box below displays the total word count."),
        verbatimTextOutput('WordCount_Raw'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Content Word Count (i.e. Nouns, Verbs, Adjectives, Adverbs)"),
        h5('📖This is a measure that quantifies the total word count for nouns, verbs, adjectives, and adverbs."'),
        h5("📈The box below displays the total word count for nouns, verbs, adjectives, and adverbs."),
        verbatimTextOutput('WordCount_Content'),
        
        
        tags$hr(style="border-color: white;"),
        h3("Percent Trust"),
        h5("📖This measure is the percentage of sentences in the text that are classified as trust. This measure utilizes the classification system that determines associations with eight discrete emotions: anger, fear, anticipation, trust, surprise, sadness, joy, and disgust."),
        h5('✏️Consider the following example:'),
        h5('"I believe the doctor. I do not like the app though."'),
        h5('The first sentence is classified as trust; the second is not. The total number of sentences contained in this text is 2. Thus, the percent trust is 1/2 or 0.5.'),
        h5("📈The box below displays percent trust."),
        h6("📝(Boring🥱) Technical details: This measure implements Saif Mohammad's NRC Emotion lexicon."),
        verbatimTextOutput('percent_trust'),
        
        
        tags$hr(style="border-color: white;"),
        h3("Lexical Diversity"),
        h5("📖This measure is the number of unique words used across the entire text."),
        h5('✏️Consider the following example:'),
        h5('"He is tall. He is smart."'),
        h5('The lexical diversity for this text is 4, as this is the number of unique words.'),
        h5("📈The box below displays the lexical diversity."),
        verbatimTextOutput('lexical_diversity'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Lexical Density"),
        h5("📖This is a measure the number of unique words (i.e. lexical diversity) divided by the total number of words (i.e. raw word count). A lexical density of 1 indicates that no words were repeated; thus, this measure is an indicator of word repetition. As lexical density increases, repetition decreases. (Note: this does not imply strictly consecutive repetition)."),
        h5('✏️Consider the following example:'),
        h5('"He is tall. He is smart."'),
        h5('The lexical diversity (i.e. number of unique words) for this text is 4. The raw word count is 6. Thus, the lexical density is 4/6 or 0.666.'),
        h5("📈The box below displays the lexical density."),
        verbatimTextOutput('lexical_density'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Percent Consecutive Repetition 🗣️"),
        h5("📖This is a measure that quantifies the percentage of the text that consists of consecutive repetition, i.e. the repetition of adjacent words. This percentage reflects the number of instances where a word is consecutively repeated divided by the number of words not consecutively repeated across the text. This measure is more appropriate when analyzing the transcripts of verbal responses, as consecutive repetition is more likely to be a feature of speech than writing."),
        h5('✏️Consider the following example:'),
        h5('"I I love like love cars. It is just like like that they are so cool."'),
        h5('The number of consecutive repetitions in this text is 2 (i.e. consecutive repetitions of "I I" and "like like". The number of instances where a word is not consecutively repeated is 12. Thus, the percent consecutive repetition for this text is 2/12 or 0.166.'),
        h5("📈The box below displays the percent consecutive repetition."),
        verbatimTextOutput('Consec_Rep_Per'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Network Graph for Most Disorganized Sentence of Text"),
        h5("📖This visualization displays the sentence with the greatest value for word count minus lexical diversity. This difference is greater when the word repetition (not necessarily consecutive) is greater. Thus, disorganization in this context denotes more repeated words. The sentence with the greatest number of repeated words is displayed below."),
        h5('✏️Consider the following example:'),
        h5('"I I love like love cars. It is just like like that they are so cool."'),
        h5("Consider sentence 1. The raw word count is 6. The lexical diversity (i.e. number of unique words) is 4. Thus, its disorganization value in this context is 2. Consider sentence 2. The raw word count is 10. The lexical diversity is 9. Thus, its disorgnization value in this context is 1. Hence, sentence 1 is the most disorganized."),
        h5("📈The box below displays the sentence with the greatest value for word count - lexical diversity."),
        h5("📈The network visualization below displays this most disorganized sentence as a sequence of dots and connecting lines. Each dot represents a unique word in the sentence. The arrowed lines between the dots connect adjacent words as found in the text. Greater repetition of words causes greater intersection of these lines as the path criss-crosses around shared words."),
        verbatimTextOutput('disorg_sentence'),
        plotOutput("plot_disorg_sentence"),
        
        
        tags$hr(style="border-color: white;"),
        h3("Mean Semantic Efficiency"),
        h5("📖This is a measure that reflects the average semantic efficiency of a text. In other other words, this reflects the effective conciseness of a text. A higher score reflects more concise word choice. A lower (and perhaps even negative score) reflects a redundancy in vocabulary either through direct repetition of a word or through repeating denotatively similar words. (Note: this is the most technical measure, so this description is somewhat superficial.)"),
        h5('✏️Consider the following example.'),
        h5('"The loud dog is is very noisy." vs. "The dog is noisy."'),
        h5('The first sentence would score lower on this semantic efficiency metric for two reasons: (1) the repetition of "is", and (2) the redundancy of using both "loud" and "noisy" to describe the dog. The second sentence would score a 1, i.e. perfect efficiency, as the sentence does not present any redundancies. The mean semantic efficiency would simply be the mean of the semantic efficiency measures for the two sentences.'),
        h5("📈The box below displays the mean semantic efficiency."),
        h6("📝(Boring🥱) Technical details: This measure vectorizes the nouns, verbs, adjectives, and adverbs (i.e. content words) of each sentence of the text. The measure subsequently implements Word2Vec via the skip-gram neural network model. The 15-dimension vectors then undergo dimensionality reduction to 2-dimensions. The plotted word embeddings are displayed at the bottom of the page. For each sentence, every content word that is within a small radius of another content word is counted as an overlap/redundancy. The total number of redundancies per sentence is summed and divided by the total number of content words in the sentence. This value is subtracted from 1 to give the semantic efficiency metric per sentence. This value reflected below is simply the average of the sentences' semantic efficiencies. This average value is also graphed as a horizontal line in the subsequent plot."),
        verbatimTextOutput('Semantic_Efficiency_Avg'),
        
        
        
        tags$hr(style="border-color: white;"),
        h3("Semantic Efficiency by Sentence"),
        h5("📖This is a measure that reflects the semantic efficiency per sentence of a text. In other other words, this reflects the effective conciseness of each sentence. A higher score reflects more concise word choice for that sentence. A lower (and perhaps even negative score) reflects a redundancy in vocabulary either through direct repetition of a word or through repeating denotatively similar words. (Note: this is the most technical measure, so this description is somewhat superficial.)"),
        h5('✏️Consider the following example.'),
        h5('"The loud dog is is very noisy." vs. "The dog is noisy."'),
        h5('The first sentence would score lower on this semantic efficiency metric for two reasons: (1) the repetition of "is", and (2) the redundancy of using both "loud" and "noisy" to describe the dog. The second sentence would score a 1, i.e. perfect efficiency, as the sentence does not present any redundancies.'),
        h5("📈The plot below displays the sentence number on the x-axis and the semantic efficiency on the y-axis."),
        h6("📝(Boring🥱) Technical details: This measure vectorizes the nouns, verbs, adjectives, and adverbs (i.e. content words) of each sentence of the text. The measure subsequently implements Word2Vec via the skip-gram neural network model. The 15-dimension vectors then undergo dimensionality reduction to 2-dimensions. The plotted word embeddings are displayed at the bottom of the page. For each sentence, every content word that is within a small radius of another content word is counted as an overlap/redundancy. The total number of redundancies per sentence is summed and divided by the total number of content words in the sentence. This value is subtracted from 1 to give the semantic efficiency metric per sentence."),
        plotOutput('sem_eff_sentence'),
        
        
        tags$hr(style="border-color: white;"),
        h3("Word Embeddings"),
        h5("📈The plot below displays the word embeddings, i.e. vector coordinates that were produced for each content word of the text. Note that this graph is not really interpretable; it is being included as an addendum."),
        h6("📝(Boring🥱) Technical details: This measure vectorizes the nouns, verbs, adjectives, and adverbs (i.e. content words) of each sentence of the text. The measure implements Word2Vec via the skip-gram neural network model. The 15-dimension vectors then undergo dimensionality reduction to 2-dimensions. The plotted word embeddings are displayed."),
        plotOutput('tsne_plot'),
        
      )
    )
  )
  
  
  server <- function(input, output, session) {
    
    observe({
      file1 = input$file1
      if (is.null(file1)) {
        return(NULL)
      }
      #if (file_ext(file1$name) != 'docx'){
       # return("Incorrect file format. Upload a .docx file.")
      #}
    
      input_text <- readtext(file1$datapath)
      input_text <- input_text[[2]]
      # without special characters
      input_text <- mgsub(input_text, c("~", "@", "#", "$", "%", "^", "&", "*", "\\(", "\\)", "-", "_", "+", "=", "<", ">", "\\:", ";", '\\"', '\n', '"\n', '“', '”'), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""))
      
      # Coherence
      load("TASA.rda")
      coherence_web <- coherence(as.character(input_text), split=c(".","!","?"), tvectors=TASA, breakdown=F)
      data_coherence <- data.frame("Coherence" = coherence_web[[1]], "Sentence_Pair" = c(1:length(coherence_web[[1]])))
      
      output$plot_coh <- renderPlot({
        ggplot(data_coherence, aes(x=Sentence_Pair, y=Coherence)) + geom_col(fill = "seagreen3") + xlab("Sentence Pair") + ggtitle("Coherence by Sentence Pair for Text") + geom_hline(yintercept=coherence_web[[2]], linetype="dashed", color = "darkgreen") + annotate(geom="text", x=length(coherence_web[[1]]), y=coherence_web[[2]] + 0.02, label="Average", color="darkgreen")
      })
      
      
      # Emotion
      sentiment_web <- get_nrc_sentiment(input_text)
      sentiment_web <- data.frame(t(sentiment_web))
      names(sentiment_web)[1] <- "count"
      sentiment_web <- cbind("sentiment" = rownames(sentiment_web), sentiment_web)
      sentiment_web_emotion <- sentiment_web[1:8, ]
      sentiment_web_posneg <- sentiment_web[9:10, ]
      
      output$plot_emotion <- renderPlot({
        ggplot(sentiment_web_emotion, aes(sentiment, count, fill=sentiment)) + geom_bar(stat="identity") + scale_y_continuous(breaks=seq(from = 0, to = max(sentiment_web_emotion$count)+1, by = 1)) + ylim(0, max(sentiment_web_emotion$count)+1) + theme_minimal() +theme( axis.text.x=element_blank()) + ggtitle("Emotion Analysis of Text") + ylab("Sentence Count") + xlab("Emotion")
      })
      
      
      
      # Percent Positive and Negative Sentiment Sentences
      sentiment_web_posneg <- sentiment_web_posneg %>% 
        arrange(desc(sentiment)) %>%
        mutate(prop = count / sum(sentiment_web_posneg$count) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop )
      
      output$plot_posneg <- renderPlot({
        ggplot(sentiment_web_posneg, aes(x="", y=prop, fill=sentiment)) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme_void() +  theme(legend.position="none") + geom_text(aes(y = ypos, label = sentiment), color = "white", size=6) + scale_fill_brewer(palette="Set1")
      })
      
      
      # Positive (+) / Negative (-) Score by Sentence
      # Split by sentence
      input_text_split <- unlist(strsplit(input_text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
      # Sentiment score by sentence
      sentscore_by_sentence <- get_sentiment(input_text_split)
      by_sentence_df <- data.frame("sentence" = input_text_split, "sentiment_score" = sentscore_by_sentence, "sentence_number" = 1:length(input_text_split))
      
      output$plot_posneg_sentence <- renderPlot({ 
      ggplot(by_sentence_df, aes(x=sentence_number, y=sentiment_score)) + geom_col(fill = "lightsteelblue2") + xlab("Sentence Number") + ggtitle("Positive (+) / Negative (-) Sentiment Score by Sentence for Text") + geom_hline(yintercept=mean(by_sentence_df$sentiment_score), linetype="dashed", color = "lightsteelblue4") + annotate(geom="text", x=length(by_sentence_df$sentence_number), y=mean(by_sentence_df$sentiment_score) + 0.2, label="Average", color="lightsteelblue4") + scale_x_continuous(breaks=pretty_breaks()) + ylab("Positive/Negative Sentiment Score") + geom_hline(yintercept=0, color = "red")
      })
      
      
      # Overall Text Sentiment
      output$text_sent <- renderPrint({get_sentiment(input_text)})
      
      
      # Most Positive Sentence
      most_pos_sentence <- by_sentence_df$sentence[by_sentence_df$sentiment_score == max(by_sentence_df$sentiment_score)]
      output$most_pos_sentence <- renderPrint({as.character(most_pos_sentence)})
      
      
      # Most Negative Sentence
      most_neg_sentence <- by_sentence_df$sentence[by_sentence_df$sentiment_score == min(by_sentence_df$sentiment_score)]
      output$most_neg_sentence <- renderPrint({as.character(most_neg_sentence)})
      
      
      # 25 Most Frequently Used Words
      udmodel <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")
      tagged_input_text <- as.data.frame(udpipe_annotate(udmodel, as.character(input_text)))
      # Get only content words
      tagged_input_text_content <- tagged_input_text[tagged_input_text$upos == "NOUN" | tagged_input_text$upos == "VERB" | tagged_input_text$upos == "ADJ" | tagged_input_text$upos == "ADV" | tagged_input_text$upos == "PUNCT", ]
      # Remove ,
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token == ",", ]
      # Remove 's
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$lemma == "'s", ]
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token == "’s", ]
      # Remove &
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token == "&", ]
      # Remove '
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token %in% grep("'", tagged_input_text_content$token, value = TRUE), ]
      # Remove 's
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$lemma %in% grep("'s", tagged_input_text_content$token, value = TRUE), ]
      # Remove -
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token %in% grep("-", tagged_input_text_content$token, value = TRUE), ]
      # Remove (
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token %in% grep("\\(", tagged_input_text_content$token, value = TRUE), ]
      # Remove )
      tagged_input_text_content <- tagged_input_text_content[!tagged_input_text_content$token %in% grep("\\)", tagged_input_text_content$token, value = TRUE), ]
      
      # Remove punctuation for some calculations
      tagged_input_text_content1 <- tagged_input_text_content[!tagged_input_text_content$token == ".", ]
      tagged_input_text_content1 <- tagged_input_text_content1[!tagged_input_text_content1$token == "?", ]
      tagged_input_text_content1 <- tagged_input_text_content1[!tagged_input_text_content1$token == "!", ]
      tagged_input_text_content1 <- tagged_input_text_content1[!tagged_input_text_content1$token %in% grep("\\.", tagged_input_text_content1$token, value = TRUE), ]
      
      
      word_freq <- sort(table(tagged_input_text_content1$token), decreasing = TRUE)
      word_freq_df <- as.data.frame(word_freq[1:25])
      tagged_input_text_lemma1 <- tagged_input_text_content1$lemma
      tagged_input_text_lemma1_text <- as.vector(knitr::combine_words(tagged_input_text_lemma1, sep = " "))
      
      output$plot_most_freq_words <- renderPlot({
      ggplot(word_freq_df, aes(Var1, Freq)) + geom_bar(stat="identity", fill="darkslategray3") + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Text Words") + ylab("Frequency") + ggtitle("25 Most Frequently Used Words of Text") 
      })
      
      # Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words
      sent_value <- get_sentiment(names(word_freq))
      sent_value_df <- as.data.frame(sent_value)
      sent_value_df$Word <- names(word_freq)
      sent_value_df$Word <- factor(sent_value_df$Word, levels = sent_value_df$Word)
      
      output$plot_sent_most_freq_words <- renderPlot({
      ggplot(sent_value_df[1:25, ], aes(Word, sent_value)) + geom_bar(stat="identity", fill="plum3") + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Text Words") + ylab("Positive/Negative Sentiment Score") + ggtitle("Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words")
      })
      
      
      # Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words with Non-Zero Sentiments
      sent_value_df_nonzero <- sent_value_df[!sent_value == 0, ]
      sent_value_df_nonzero$Word <- factor(sent_value_df_nonzero$Word, levels = sent_value_df_nonzero$Word)
      
      output$plot_sent_most_freq_words_nonzero <- renderPlot({
      ggplot(sent_value_df_nonzero[1:25, ], aes(Word, sent_value)) + geom_bar(stat="identity", fill="plum3") + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Text Words") + ylab("Positive/Negative Sentiment Score") + ggtitle("Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words with Non-Zero Sentiments")
      })
      
      
      # Word Cloud
      output$plot_word_cloud <- renderPlot({
      wordcloud(names(word_freq[1:100]), word_freq, min.freq = 1, random.order = FALSE,
                scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))
      })
      
      
      # Flesch Reading Ease
      readability <- textstat_readability(input_text, c("meanSentenceLength","meanWordSyllables", "Flesch.Kincaid", "Flesch"), remove_hyphens = TRUE, min_sentence_length = 1, max_sentence_length = 10000, intermediate = FALSE)
      head(readability)
      output$Flesch <- renderPrint({readability$Flesch})
      
    
      # Flesch Kincaid Grade Level
      output$Flesch.Kincaid <- renderPrint({readability$Flesch.Kincaid})
      
      
      # Mean Word Syllables
      output$meanWordSyllables <- renderPrint({readability$meanWordSyllables})
      
      
      # Word Count (raw)
      WordCount_Raw <- wordcount(input_text) 
      output$WordCount_Raw <- renderPrint({WordCount_Raw})
      
      
      # Content Word Count - i.e. nouns, verbs, adjectives, adverbs
      WordCount_Content <- wordcount(tagged_input_text_lemma1_text)
      output$WordCount_Content <- renderPrint({WordCount_Content})
      
      
      # Percent Trust
      output$percent_trust <- renderPrint({get_nrc_sentiment(input_text)[,8] / length(input_text_split)})    
        
      # Lexical Diversity - number of unique words used
      lexical_diversity <- lengths(lapply(strsplit(tolower(mgsub::mgsub(as.character(input_text), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " "), unique))
      output$lexical_diversity <- renderPrint({lexical_diversity})
      
      
      # Lexical Density - number of unique words divided by total number of words (word repetition)
      lexical_density <- lexical_diversity / WordCount_Raw  
      output$lexical_density <- renderPrint({lexical_density})
      
      
      # Percent Consecutive Repetition
      repetition_data <- rle(as.numeric(as.factor(strsplit(tolower(mgsub::mgsub(as.character(input_text), c("\\.", ","), c("", ""))), split = " ")[[1]])))
      # See how many instances are repeated and divide by total number of non-repeated words
      Consec_Rep_Per <- length(repetition_data[[1]][repetition_data[[1]] >= 2]) / sum(repetition_data[[1]][repetition_data[[1]] == 1])
      output$Consec_Rep_Per <- renderPrint({Consec_Rep_Per})
      
      
      # Network Graph for Most Disorganized Sentence (i.e. Greatest Word Count - Lexical Diversity)
      disorg_value <- 0
      for (i in 1:length(input_text_split)){
        disorg_value[i] <- wordcount(input_text_split[i]) - lengths(lapply(strsplit(tolower(mgsub::mgsub(as.character(input_text_split[i]), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " "), unique)) 
      }
      index_ref <- which(disorg_value == max(disorg_value))
      disorg_sentence <- input_text_split[index_ref]
      output$disorg_sentence <- renderPrint({disorg_sentence})
      
      source <- strsplit(tolower(mgsub::mgsub(as.character(disorg_sentence), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " ")[[1]]
      destination <- 0
      for (i in 1:length(source)-1){
        destination[i] <- source[i+1]
      }
      destination[length(source)] <- "." 
      edge_list <- tibble(from = source, to = destination)
      node_list <- tibble(id = 1:length(source))
      routes_network <- network(edge_list, vertex.attr = node_list, matrix.type = "edgelist", ignore.eval = FALSE)
      output$plot_disorg_sentence <- renderPlot({
      plot(routes_network, vertex.cex = 3, main = "Network Graph for Most Disorganized Sentence of Text", vertex.col = "red", edge.col = "black")
      })
      
      
      # Lemmatize - get ContentWords column for dataframe
      # String together all lemma words by sentence (usage 2)
      tagged_input_text_content_lemma <- tagged_input_text_content$lemma
      content_text <- as.vector(knitr::combine_words(tagged_input_text_content_lemma, sep = " "))
      content_text <- gsub(" \\.", "\\.", content_text)
      content_text <- gsub(" \\!", "\\!", content_text)
      content_text <- gsub(" \\?", "\\?", content_text)
      
      content_text_split <- unlist(strsplit(content_text, "([[:punct:]])", perl=T))
      
      # Word2Vec
      model <- word2vec(x = as.character(content_text_split), type = "skip-gram", dim = 15, iter = 100, min_count = 1)
      emb <- as.matrix(model)
      # Dimensionality reduction
      tsne <- Rtsne(emb, perplexity = 50, pca = TRUE)
      # Plot Word embeddings
      tsne_plot <- tsne$Y %>%
        as.data.frame() %>%
        mutate(word = row.names(emb)) %>%
        ggplot(aes(x = V1, y = V2, label = word)) +
        geom_text(size = 1)
      output$tsne_plot <- renderPlot({tsne_plot})
      
      # Create dictionary of embeddings
      tsne_df <- as.data.frame(tsne$Y)
      tsne_df$word <- row.names(emb)
      
      for (j in 1:nrow(tagged_input_text_content1)){
        wordofint <- tagged_input_text_content1$lemma[j]
        # Source TSNE dictionary and load to df_list dataframes
        tagged_input_text_content1$V1[j] <- tsne_df$V1[tsne_df$word == wordofint]
        tagged_input_text_content1$V2[j] <- tsne_df$V2[tsne_df$word == wordofint]
      }
      
      
      # Overlap number
      # Initialize
      tagged_input_text_content1$overlap_number <- 0
      for (j in unique(tagged_input_text_content1$sentence_id)){
        # per word
        for (k in 1:(length(tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$sentence_id))-1){
          V1_TF <- abs(as.numeric(na.omit(tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$V1[k+1:length(tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$sentence_id)])) - tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$V1[k]) < 0.25
          V2_TF <- abs(as.numeric(na.omit(tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$V2[k+1:length(tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$sentence_id)])) - tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$V2[k]) < 0.25
          tagged_input_text_content1[tagged_input_text_content1$sentence_id == j, ]$overlap_number[k] <- sum(V1_TF[V1_TF == TRUE] == V2_TF[V2_TF == TRUE])
        }
      }
      # Overlap total number
      # Initialize
      tagged_input_text_content1$overlap_total_number <- 0
      
      # per sentence
      for (j in 1:nrow(tagged_input_text_content1)){
        tagged_input_text_content1$overlap_total_number[j] <- aggregate(tagged_input_text_content1$overlap_number, by=list(tagged_input_text_content1$sentence_id), FUN = sum)[which(aggregate(tagged_input_text_content1$overlap_number, by=list(tagged_input_text_content1$sentence_id), FUN = sum)$Group.1 == tagged_input_text_content1$sentence_id[j]), 2]
      }
      # Total sentence content words
      # per row
      for (j in 1:nrow(tagged_input_text_content1)){
        tagged_input_text_content1$total_sentence_content_words[j] <- as.data.frame(table(tagged_input_text_content1$sentence_id))[which(as.data.frame(table(tagged_input_text_content1$sentence_id))$Var1 == tagged_input_text_content1$sentence_id[j]), 2]
      }
      # Semantic efficiency per sentence
      # per row
      for (j in 1:nrow(tagged_input_text_content1)){
        tagged_input_text_content1$semantic_efficiency[j] <- 1 - (tagged_input_text_content1$overlap_total_number[j]/tagged_input_text_content1$total_sentence_content_words[j])
      }
      
      
      # Average Semantic efficiency 
      Semantic_Efficiency_Avg <- mean(tagged_input_text_content1$semantic_efficiency)
      output$Semantic_Efficiency_Avg <- renderPrint({Semantic_Efficiency_Avg})
      
      
      # Semantic efficiency by sentence
      sem_eff_df <- unique(tagged_input_text_content1[,c('sentence_id','semantic_efficiency')])
      
      output$sem_eff_sentence <- renderPlot({
        ggplot(sem_eff_df, aes(x=sentence_id, y=semantic_efficiency)) + geom_col(fill = "lightpink2") + xlab("Sentence Number") + ggtitle("Semantic Efficiency by Sentence for Text") + geom_hline(yintercept=mean(sem_eff_df$semantic_efficiency), linetype="dashed", color = "lightpink4") + annotate(geom="text", x=length(sem_eff_df$semantic_efficiency), y=mean(sem_eff_df$semantic_efficiency) + 0.2, label="Average", color="lightpink4") + scale_x_continuous(breaks=pretty_breaks()) + ylab("Semantic Efficiency")
      })
      
      
    })
    
  }
    
  
  shinyApp(ui, server)
  
