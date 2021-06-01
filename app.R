####################################
# Evan Collins
# 24 May 2021
# Credit: LIWC dictionary courtesy of Pennebaker (UofT)
####################################

# Load udmodel, TASA, and .dic files when publishing

# ============ Install and load the packages =======================
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(reshape2)
library(rgl)
library(readtext)
library(stringr)
library(udpipe)
library(dplyr)
library(purrr)
library(tidyr)
library(word2vec)
library(keras)
library(reticulate)
library(text2vec)
library(Rtsne)
library(plotly)
library(LSAfun)
library(Rmisc)
library(lme4)
library(lmerTest)
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
library(textreadr)
library(network)
library(tools)
library(readtext)
library(ggplot2)
library(devtools)
library(quanteda)
library(quanteda.textstats)
library(topicmodels)
library(stringr)
library(tidytext)
library(politeness)
library(spacyr)
library(highcharter)
library(RSentiment)
library(tokenizers)

# To check upload bugs
#rsconnect::showLogs(appName="doge",streaming=TRUE)
#rsconnect::configureApp("doge", size="xxlarge")



#=== UI Code Starts ===
header <- dashboardHeader(title= img(src = 'doge.png',
                                     title = "doge", height = "65px"))

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'menu',
              menuItem(strong("Basics"),tabName = 'basics', icon = icon("th-list", lib = "glyphicon")),
              menuItem(strong("Sentiment"),tabName = 'sentiment', icon = icon("heart-empty", lib = "glyphicon")),
              menuItem(strong("Coherence"),tabName = 'coherence', icon = icon("knight", lib = "glyphicon")),
              menuItem(strong("Mindfulness"),tabName = 'mindfulness', icon = icon("tree-deciduous", lib = "glyphicon")),
              menuItem(strong("Descriptions"),tabName = 'description', icon = icon("pencil", lib = "glyphicon"))
  ),
  hr(),
    # Input: Select a file ----
    textAreaInput("text1", "Paste text below and click Submit. Text must be at least 2 sentences and have at least 80 words.", rows=25),
    #submitButton("Submit"),
    actionButton("submit", "Submit", icon("refresh"))
    #textOutput("text_1"),
    
    # Include clarifying text ----
    #helpText("Instructions: Paste text into box above and click submit button. Analysis may take > 30 seconds if text is long.")
)

body <- dashboardBody(
  
  # Title 
  tags$head(tags$style(HTML(
    '.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    font-weight: bold;font-size: 16px;
    }
    '))),
  tags$head(tags$style(HTML(
    '.myClass { 
    font-size: 20px;
    line-height: 50px;
    text-align: left;
    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    padding: 0 15px;
    overflow: hidden;
    color: white;
    }
    '))),
  #tags$head(tags$style(HTML("div.main-header {text-align: center;}"))),
  tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass">                                                        📄 DOcument GEnome 🧬</span>\');
                   })
                   ')),
  tags$head(tags$style(HTML('.modal-sm {width: 80px;}'))),
  
  # Set the boxes of all charts
  tags$head(tags$style(HTML(".col-sm-2,.col-sm-12,.col-sm-4,.col-sm-12,.col-sm-6,.col-sm-7,.col-sm-5 {
                            position: relative;
                            min-height: 1px;
                            padding-right: 5px;
                            padding-left: 5px;"))),
  
  tags$head(tags$style(HTML(".container-fluid {padding-left: 5px; padding-right: 5px;}"))),
  tags$head(tags$style(HTML(".form-group {margin-bottom: -15px;}"))),
  
  tags$head(tags$style(HTML(".box {margin-bottom: 10px;}"))),
  
  
  tags$head(tags$style(HTML("#col_word_cloud,#col_freq {padding-left:0px;padding-right:0px;} "))),
  #tags$head(tags$style(HTML("#col_emotion,#col_sentiment {padding-left:0px;padding-right:0px;} "))),
  tags$head(tags$style(HTML(".box-header {text-align: left;} "))),
  tags$head(tags$style(HTML("#network_panel {width:100%;} "))),
  
  
  tabItems(
    tabItem("basics",
            fluidPage(
              fluidRow(
                column(width = 6, 
                       box(width=NULL, height=200, solidHeader = F, title = strong("Word Count"),
                           htmlOutput("WordCount_Raw"),
                           htmlOutput("WordCount_Content")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Word Cloud"),
                           plotOutput("plot_word_cloud")
                           #wordcloud2Output("plot_word_cloud",height = "400px")
                        ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Most Common Words within each LDA Topic"),
                           plotOutput("plot_lda_common_words",height = "400px")
                       ),
                       box(width=NULL, height=300, solidHeader = F, title = strong("Lexical Metrics"),
                           htmlOutput("lexical_diversity"),
                           htmlOutput("lexical_density")
                       )
                  ), 
                       
                column(width = 6,
                       box(width=NULL, height=200, solidHeader = F, title = strong("Sentence Count"),
                           htmlOutput("SentenceCount")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("25 Most Frequently Used Words"),
                           highchartOutput("plot_most_freq_words",height = "400px")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Words with Greatest Difference in Beta between LDA Topic 2 and Topic 1"),
                           plotOutput("plot_lda_beta",height = "400px")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Word Embeddings"),
                           plotOutput("tsne_plot", height = "400px")
                       )
                      )
                    )
                  )
               
        ),
    
    
    
    
    tabItem("sentiment",
            fluidPage(
              fluidRow(
                column(width = 6,
                       box(width=NULL, height=500, solidHeader = F, title = strong("Sentiment Analysis"),
                           plotOutput("plot_sentiment",height = "400px")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Positive|Negative Sentiment Score by Sentence"),
                           plotOutput("plot_posneg_sentence",height = "400px")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Positive|Negative Sentiment Score for 25 Most Frequently Used Words"),
                           plotOutput("plot_sent_most_freq_words",height = "400px")
                       )
                ), 
                
                column(width = 6, 
                       box(width=NULL, height=500, solidHeader = F, title = strong("Sentiment Polarity of Sentences"),
                           highchartOutput("sentiment_plot",height = 400)
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Most Polar Sentences"),
                           htmlOutput("most_pos_sentence"),
                           htmlOutput("most_neg_sentence")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Positive|Negative Sentiment Score for 25 Most Frequently Used Words with Non-Zero Sentiments"),
                           plotOutput("plot_sent_most_freq_words_nonzero",height = "400px")
                       )
                )
              )
            )
            
    ),
    
    
    tabItem("coherence",
            fluidPage(
              fluidRow(
                column(width = 6,
                       box(width=NULL, height=500, solidHeader = F, title = strong("Coherence by Sentence Pair"),
                           plotOutput("plot_coh",height = "400px")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Network Graph for Most Disorganized Sentence"),
                           plotOutput("plot_disorg_sentence",height = "400px")
                       ),
                       box(width=NULL, height=400, solidHeader = F, title = strong("Readability Metrics"),
                           htmlOutput("meanWordSyllables"),
                           htmlOutput("Flesch_score"),
                           htmlOutput("Flesch_Kincaid_score"),
                       )
                ),
                
                column(width = 6,
                       box(width=NULL, height=500, solidHeader = F, title = strong("Semantic Efficiency by Sentence"),
                           plotOutput("sem_eff_sentence",height = "400px")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Network Graph for Most Organized Sentence"),
                           plotOutput("plot_org_sentence",height = "400px")
                       )
                ) 
                
              )
            )
            
    ),
    
    
    tabItem("mindfulness",
            fluidPage(
              fluidRow(
                column(width = 6,
                       box(width=NULL, height=200, solidHeader = F, title = strong("Percent Mindfulness"),
                           htmlOutput("percent_mindfulness")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Sensory-based Words"),
                           highchartOutput("plot_sensory",height = 400)
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Percent Past, Present, and Future (Finite) Verbs"),
                           highchartOutput("plot_tense",height = 400)
                       )
                ),
                
                column(width = 6,
                       box(width=NULL, height=200, solidHeader = F, title = strong("Percent Judgmental"),
                           htmlOutput("percent_judgmental")
                       ),
                       box(width=NULL, height=500, solidHeader = F, title = strong("Concreteness Score by Sentence (-1 [Very Abstract] to +1 [Very Concrete])"),
                           plotOutput("plot_concreteness",height = "400px")
                       ),
                       box(width=NULL, height=300, solidHeader = F, title = strong("Pronoun Usage"),
                           htmlOutput("percent_personal_pronouns"),
                           htmlOutput("percent_impersonal_pronouns")
                       )
                ) 
              )
            )
            
    ),
    
    
    
    tabItem("description",
            fluidPage(
              fluidRow(
                
                box(width=12, height=600, title = strong("Preliminary Notes"),
                    strong("Text requirements:"),
                    tags$ul(
                      tags$li("The text is required to have at least 2 sentences due to the coherence by sentence pairs metric. The text is required to have at least 80 words due to the LDA and word embeddings metrics. Note that, in another program, the remaining metrics could be run on texts of shorter length.")
                    ),
                    
                    strong("Sample text:"),
                    tags$ul(
                      tags$li("Matt Velloso, a technical advisor to Microsoft’s CEO, got 24,000 likes on this tweet posted in November 2018: “Difference between machine learning and AI: If it is written in Python, it's probably machine learning. If it is written in PowerPoint, it's probably AI.” This sums up the AI frenzy that has seized marketing departments and media pundits for the last three years. With the coming of age of machine learning and deep learning, many have hastily jumped to the conclusion that, at long last, humans are on the verge of creating a machine in their own image, capable of autonomous thinking general artificial intelligence somehow emerging from more and more complex algorithms. Yes, neural networks have revolutionized the computer vision space and transformed natural language processing. Baidu has, for instance, just achieved the highest score ever in the General Language Understanding Evaluation with its unique model. Yes, intelligent machines are now beating humans at games like go. But, is it really what we expect when we hear the word “intelligent”? We were promised bots we could chat with and autonomous cars zipping through our road grids. And yet, the main change we see in our daily lives is that we’re now able to dictate music search queries to our digital assistant while we still have our hands on the driving wheel and eyes on the road. If a couple of machines might be considered as having passed the Turing test on a narrow scope, an undisputed success still seems a distant prospect. Let’s face it: So far, the artificial intelligence plastered all over PowerPoint slides hasn’t lived up to its hype. It is not a mere question of delayed time to market. In those domains where sci-fi AI features were the most advertised, the current breed of AI has entered a zone of diminishing returns: It needs to siphon ever more data for results that are only improving marginally. The sheer cost of collecting and cleansing the statistically representative data is quickly becoming prohibitive. The processing power required to train or apply AI algorithms is stretching Moore’s law way beyond its limits, and quantum computing, no less, is now expected to save the day.")
                    ),
                ),
                
                
                box(width=12, height=4200, title = strong("Output Descriptions"),
                    h5(strong("Basics:"),style = 'color:rgb(0, 112, 192)'),
                    strong("Word Count"),
                    tags$ul(
                      tags$li("Total Word Count: This is a measure that quantifies the total word count of the text."),
                      tags$li("Nouns, Verbs, Adjectives, Adverbs Word Count: This is a measure that quantifies the total word count for nouns, verbs, adjectives, and adverbs (i.e., content words).")
                    ),
                    
                    strong("Sentence Count"),
                    tags$ul(
                      tags$li("This is a measure that quantifies the total sentence count of the text.")
                    ),
                    
                    
                    strong("Word Cloud"),
                    tags$ul(
                      tags$li("This visualization displays the 100 most frequently used words in the text. Size is proportional to frequency.")
                    ),
                    
                    
                    strong("25 Most Frequently Used Words"),
                    tags$ul(
                      tags$li("This measure displays the 25 most frequently used words in the text."),
                      tags$li("The scrollable barplot displays the 25 most frequently used words on the y-axis and their respective counts on the x-axis.")
                    ),
                    
                    
                    strong("Most Common Words within each LDA Topic"),
                    tags$ul(
                      tags$li("This measure displays the most common words within each Latent Dirichlet Allocation (LDA) topic."),
                      tags$li("LDA is an unsupervised learning method which discovers different topics underlying a collection of documents, where each document is a collection of words."),
                      tags$li("This analysis has sentences as documents. A sentence-term matrix is derived, and the LDA topic number is set to two."),
                      tags$li("Relatively common words (e.g., the, and, or) are filtered out."),
                      tags$li("This visualization lets us understand the two LDA topics that were extracted from the text.")
                    ),
                    
                    
                    strong("Words with Greatest Difference in Beta between LDA Topic 2 and Topic 1"),
                    tags$ul(
                      tags$li("This measure displays the 20 words that have the greatest difference in beta values between LDA topic 1 and LDA topic 2."),
                      tags$li("The beta value is the per-topic-per-word probability."),
                      tags$li("This difference in beta values is estimated as the log2 ratio of the two: log2(beta_2/beta_1). A log ratio is useful as it makes the difference symmetrical."),
                      tags$li("This visualization lets us better understand the two LDA topics that were extracted from the text.")
                    ),
                    
                    strong("Lexical Metrics"),
                    tags$ul(
                      tags$li("Lexical Diversity: This measure is the number of unique words used across the entire text. Consider the following example:
'He is tall. He is smart.' The lexical diversity for this text is 4, as this is the number of unique words."),
                      tags$li("Lexical Density: This is a measure the number of unique words (i.e. lexical diversity) divided by the total number of words (i.e. raw word count). A lexical density of 1 indicates that no words were repeated; thus, this measure is an indicator of word repetition. As lexical density increases, repetition decreases. (Note: this does not imply strictly consecutive repetition). Consider the following example: 'He is tall. He is smart.' The lexical diversity (i.e. number of unique words) for this text is 4. The raw word count is 6. Thus, the lexical density is 4/6 or 0.666.")
                    ),
                    
                      strong("Words Embeddings"),
                      tags$ul(
                        tags$li("The plot displays the word embeddings, i.e. vector coordinates that were produced for each content word of the text. Note that this graph is not intuitively interpretable; it is being included as an addendum."),
                        tags$li("This measure vectorizes the nouns, verbs, adjectives, and adverbs (i.e. content words) of each sentence of the text. The measure implements Word2Vec via the skip-gram neural network model. The 15-dimension vectors then undergo dimensionality reduction to 2-dimensions. The plotted word embeddings are displayed.")
                      ),
                    
                    
                    
                    hr(),
                    h5(strong("Sentiment:"),style = 'color:rgb(0, 112, 192)'),
                    strong("Sentiment Analysis"),
                    tags$ul(
                      tags$li("This is a measure that can classify words according to associations with eight discrete emotions: anger, fear, anticipation, trust, surprise, sadness, joy, and disgust. Note that not all words are classified."),
                      tags$li("Consider the following example: 'I am afraid of heights. I love pie though.'"),
                      tags$li("The first sentence is classified as fear. The second sentence is classified as joy."),
                      tags$li("This measure implements Saif Mohammad's NRC Emotion lexicon."),
                      tags$li("The barplot displays the eight different emotions on the x-axis and the word count on the y-axis.")
                    ),
                    
                    strong("Sentiment Polarity of Sentences"),
                    tags$ul(
                      tags$li("This measure discretely classifies each sentence as negative, neutral, or positive sentiment."),
                      tags$li("Consider the following example: 'I am afraid of heights. I love pie though. The cat is red.'"),
                      tags$li("The first sentence is classified as negative. The second sentence is classified as positive. The third sentence is classified as neutral."),
                      tags$li("This measure implements Saif Mohammad's NRC Emotion lexicon.")
                    ),
                    
                    
                    strong("Positive|Negative Sentiment Score by Sentence"),
                    tags$ul(
                      tags$li("This measure quantifies the sentiment of each sentence. The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment."),
                      tags$li("Consider the following example: 'I am afraid of heights. I love pie though. The cat is red.'"),
                      tags$li("The first sentence has a sentiment score of -0.75. The second sentence has a sentiment score of 0.75."),
                      tags$li("This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
                      tags$li("The barplot displays the sentence number on the x-axis and the sentiment score on the y-axis.")
                    ),
                    
                    
                    strong("Most Polar Sentences"),
                    tags$ul(
                      tags$li("Most Positive Sentence: This measure simply displays the sentence with the greatest positive sentiment score as calculated in the above measure. Consider the following example: 'I am afraid of heights. I love pie though.'. The first sentence has a sentiment score of -0.75. The second sentence has a sentiment score of 0.75. Thus, if this was the entire text provided, then the second sentence would be displayed for this measure. This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
                      tags$li("Most Negative Sentence: This measure simply displays the sentence with the most negative sentiment score as calculated in the above measure. Consider the following example: 'I am afraid of heights. I love pie though.'. The first sentence has a sentiment score of -0.75. The second sentence has a sentiment score of 0.75. Thus, if this was the entire text provided, then the first sentence would be displayed for this measure. This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences.")
                    ),
                    
                    
                    strong("Positive|Negative Sentiment Score for 25 Most Frequently Used Words"),
                    tags$ul(
                      tags$li("This measure quantifies the sentiment of the 25 most frequently used words (as featured in the Basics tab). The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment."),
                      tags$li("This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
                      tags$li("The barplot displays the word on the x-axis and the sentiment score on the y-axis.")
                    ),
                    
                    
                    strong("Positive|Negative Sentiment Score for 25 Most Frequently Used Words with Non-Zero Sentiments"),
                    tags$ul(
                      tags$li("This measure quantifies the sentiment of the 25 most frequently used words with non-zero sentiments. The more positive the score, the more positive the sentiment; the more negative the score, the more negative the sentiment. This measure first determines which words of the text have non-zero sentiment scores. The 25 most frequently used of these selected words are plotted."),
                      tags$li("This measure implements the syuzhet sentiment dictionary developed in the Nebraska Literary Lab. This measure can quanitfy individual words or individual sentences."),
                      tags$li("The barplot displays the word on the x-axis and the sentiment score on the y-axis.")
                    ),
                    
                    
                    hr(),
                    h5(strong("Coherence:"),style = 'color:rgb(0, 112, 192)'),
                    strong("Coherence by Sentence Pair"),
                    tags$ul(
                      tags$li("This is a measure of the similarity in contextual meaning between two adjacent sentences."),
                      tags$li("Consider the following example: 'The car is fantastic. I am happy I decided to purchase the car. The dog is black.'"),
                      tags$li("Coherence is calculated by comparing the contextual meaning of two adjacent sentences. In this example, sentence pair 1+2 is given a coherence value of ~0.80, whereas sentence pair 2+3 is given a coherence value of ~0.05."),
                      tags$li("This measure utilizes Latent Semantic Analysis (LSA). LSA analyzes solves relations between word and passage meanings using Singular Value Decomposition (SVD). LSA creates a word-document matrix, with each row representing a unique word in the text and each column representing a text document or other unit of context (e.g. a sentence). The entries in this matrix are the frequency of the word in the context. An SVD of the matrix is then applied which results in a 100–500 dimensional semantic space. In the derived semantic space, words, sentences, paragraphs, or any other unit of text are represented as vectors by the sum of the vectors of the individual words contained in the text. The word and large unit of text vectors can be compared against each other in order to measure the amount of semantic similarity.")
                    ),
                    
                    
                    strong("Semantic Efficiency by Sentence"),
                    tags$ul(
                      tags$li("This is a measure that reflects the semantic efficiency per sentence of a text. In other other words, this reflects the effective conciseness of each sentence. A higher score reflects more concise word choice for that sentence. A lower (and perhaps even negative score) reflects a redundancy in vocabulary either through direct repetition of a word or through repeating denotatively similar words."),
                      tags$li("Consider the following example: 'The loud dog is is very noisy.' vs. 'The dog is noisy.'"),
                      tags$li("The first sentence would score lower on this semantic efficiency metric for two reasons: (1) the repetition of 'is', and (2) the redundancy of using both 'loud' and 'noisy' to describe the dog. The second sentence would score a 1, i.e. perfect efficiency, as the sentence does not present any redundancies."),
                      tags$li("This measure vectorizes the nouns, verbs, adjectives, and adverbs (i.e. content words) of each sentence of the text. The measure subsequently implements Word2Vec via the skip-gram neural network model. The 15-dimension vectors then undergo dimensionality reduction to 2-dimensions. The plotted word embeddings are displayed at the bottom of the page. For each sentence, every content word that is within a small radius of another content word is counted as an overlap/redundancy. The total number of redundancies per sentence is summed and divided by the total number of content words in the sentence. This value is subtracted from 1 to give the semantic efficiency metric per sentence."),
                      tags$li("The barplot displays the sentence number on the x-axis and the semantic efficiency on the y-axis.")
                    ),
                    
                    
                    
                    strong("Network Graph for Most Disorganized Sentence"),
                    tags$ul(
                      tags$li("This visualization displays the sentence with the greatest value for word count minus lexical diversity. This difference is greater when the word repetition (not necessarily consecutive) is greater. Thus, disorganization in this context denotes more repeated words. The sentence with the greatest number of repeated words is displayed below."),
                      tags$li("Consider the following brief example: 'I think I want it. It is great.'"),
                      tags$li("Consider sentence 1. The raw word count is 5. The lexical diversity (i.e. number of unique words) is 4. Thus, its disorganization value in this context is 1. Consider sentence 2. The raw word count is 3. The lexical diversity is 3. Thus, its disorgnization value in this context is 0. Hence, sentence 1 is the most disorganized."),
                      tags$li("The network visualization displays this most disorganized sentence as a sequence of dots and connecting lines. Each dot represents a unique word in the sentence. The arrowed lines between the dots connect adjacent words as found in the text. Greater repetition of words causes greater intersection of these lines as the path criss-crosses around shared words.")
                    ),
                    
                    
                    strong("Network Graph for Most Organized Sentence"),
                    tags$ul(
                      tags$li("This visualization displays the sentence with the smallest value for word count minus lexical diversity. This difference is smaller when the word repetition (not necessarily consecutive) is smaller. Thus, organization in this context denotes fewer repeated words. The sentence with the fewest number of repeated words is displayed below."),
                      tags$li("Consider the following brief example: 'I think I want it. It is great.'"),
                      tags$li("Consider sentence 1. The raw word count is 5. The lexical diversity (i.e. number of unique words) is 4. Thus, its disorganization value in this context is 1. Consider sentence 2. The raw word count is 3. The lexical diversity is 3. Thus, its disorgnization value in this context is 0. Hence, sentence 2 is the most organized."),
                      tags$li("The network visualization displays this most disorganized sentence as a sequence of dots and connecting lines. Each dot represents a unique word in the sentence. The arrowed lines between the dots connect adjacent words as found in the text. Greater repetition of words causes greater intersection of these lines as the path criss-crosses around shared words.")
                    ),
                    
                    strong("Readability Metrics"),
                    tags$ul(
                      tags$li("Mean Number of Syllables per Word: This is a measure that quantifies the average number of syllables per word."),
                      tags$li("Flesch Reading Ease: This is a measure of readability scored between 0 and 100, calculated according to: 206.835 - 1.015*(total words/total sentences) - 84.6*(total syllables/total words). The higher the score, the more readable (i.e. more easily understood by younger demographics) it is. Scores 90-100 denote very easy to read and understandable to the average 5th grader. Scores 80-90 denote easy to read and understandable to the average 6th grader. Scores 70-80 denote fairly easy to read and understandable to the average 7th grader. Scores 60-70 denote plain English and understandable to the average 8th/9th grader. Scores 50-60 denote fairly difficult to read and understandable to the average 10th-12th grader. Scores 30-50 denote difficult to read and understandable to the average college student. Scores 10-30 denote very difficult to read and best understood by university graduates. Scores 0-10 denote extremely difficult to read and best understood by university graduates."),
                      tags$li("Flesch-Kincaid Grade Level: This is a measure of the approximate reading grade level of the text, calculated according to: 0.39*(total words/total sentences) + 11.8*(total syllables/total words) - 15.59. For example, a Flesh-Kincaid level of 8 signifies that the average reader needs a grade 8 level of reading or above to understand it.")
                    ),
                    
                    
                    hr(),
                    h5(strong("Mindfulness:"),style = 'color:rgb(0, 112, 192)'),
                    strong("Percent Mindfulness"),
                    tags$ul(
                      tags$li("This bag-of-words approach measures the percentage of words in the text that are mindfulness words, i.e., words identified by Collins et al. (2009) according to LIWC that are associated with mindfulness."),
                      tags$li("The full list of words are the following: accept, allow, attention, automatic, autopilot, aware, balance, being, body, breath, calm, center, choice, commit, compassion, curiosity, discomfort, space, emotion, energy, experience, expand, feel, fight, fix, free, friend, sense, grateful, gratitude, judge, thought, let, meditate, mindful, moment, nonjudge, notice, now, ok, okay, observe, open, pause, practice, time, present, time, present, sit, react, relax, resist, response, scan, gentle, push, slow, sober, soften, ease, stop, struggle, kind, peace, willing.")
                    ),
                    
                    strong("Percent Judgmental"),
                    tags$ul(
                      tags$li("This bag-of-words approach measures the percentage of words in the text that are judmental words."),
                      tags$li("This metric is devised by combining the counts for two LIWC criteria: Discrepancy and Certainty."),
                      tags$li("Note that the dictionary list of these words cannot be shared publicly. I thank Dr. Pennebaker for graciously sharing the LIWC dictionary for this project.")
                    ),
                    
                    strong("Sensory-based Words"),
                    tags$ul(
                      tags$li("This bag-of-words approach measures counts of words that correspond to the senses of sight, touch, and sound."),
                      tags$li("This metric is devised using the LIWC dictionary.")
                    ),
                    
                    strong("Concreteness Score by Sentence (-1 [Very Abstract] to +1 [Very Concrete])"),
                    tags$ul(
                      tags$li("This measures average concretness by sentence in the text."),
                      tags$li("+1 indicates very concrete, -1 indicates very abstract."),
                      tags$li("By-sentence values were obtained by averaging the concreteness value of the component words making up each sentence. These concreteness values were sourced from Brysbaert et al. (2014) who created concretness ratings for 40,000 English word lemmas.")
                    ),
                    
                    strong("Percent Past, Present, and Future (Finite) Verbs"),
                    tags$ul(
                      tags$li("This measures percent past, present, and future tenses of the finite (i.e. non-infinitive) verbs in the text."),
                      tags$li("Tokenizer used does not utilize constituency parsing; thus, improvements to this metric are possible.")
                    ),
                
                strong("Pronoun Usage"),
                tags$ul(
                  tags$li("This measurespercent of pronouns in text that are personal pronouns and impersonal pronouns."),
                  tags$li("Usage of impersonal pronouns is negatively correlated with mindfulness according to Collins et al. (2009)."),
                  tags$li("This metric makes use of the LIWC dictionary.")
                    )
                )
                ) 
              )
            )
    
  )
  
)

ui <- dashboardPage(title = 'DOGE', header , sidebar, body)




#=== Server Code Starts ===
server <- function(input, output, session) {
  

  observeEvent(input$submit, {
    
    shiny::validate(
      need(input$text1 != "", "Text must have at least 80 words"),
      need(wordcount(input$text1) > 79, "Text must have at least 80 words")
    )
    
    progress <- shiny::Progress$new()
    progress$set(message = "Doge start think", value = 0)
    #on.exit(progress$close())
    
    # ---Initial Text Processing---
    input_text = str_squish(input$text1)
    # without special characters
    input_text <- mgsub(input_text, c("~", "@", "#", "$", "%", "^", "&", "*", "\\(", "\\)", "-", "_", "+", "=", "<", ">", "\\:", ";", '\\"', '\n', '"\n', '“', '”'), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""))
    input_text <- gsub("Mr.", "Mr", input_text)
    input_text <- gsub("Ms.", "Ms", input_text)
    input_text <- gsub("Dr.", "Dr", input_text)
    input_text <- gsub("Mrs.", "Mrs", input_text)
    
    #shiny::validate(
    #  need(wordcount(input_text) > 79, "Text must have at least 80 words")
    #)
    
    
    # ____________________ Basics ___________________
    
    
    # Total Word Count
    
    WordCount_Raw <-wordcount(input_text)
    
    output$WordCount_Raw <- renderUI({
      #shiny::validate(
      #  need(WordCount_Raw > 79, "Text must have at least 80 words")
      #)
      HTML(paste("<b>","Total Word Count: ","</b>"),"<br>","<i style = 'font-weight: bold'>",WordCount_Raw,"</i>","<hr>")
    })
    
    
    
    udmodel <- udpipe_load_model(file = "data/english-ewt-ud-2.5-191206.udpipe")
    tagged_input_text <- as.data.frame(udpipe_annotate(udmodel, as.character(input_text)))
    # Split by sentence
    #input_text_split <- unlist(strsplit(input_text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    input_text_split <- unlist(tokenize_sentences(input_text))
    tagged_input_text <- as.data.frame(udpipe_annotate(udmodel, as.character(input_text)))
    # Get only content words
    tagged_input_text_content <- tagged_input_text[tagged_input_text$upos == "NOUN" | tagged_input_text$upos == "VERB" | tagged_input_text$upos == "ADJ" | tagged_input_text$upos == "ADV", ]
    word_freq <- sort(table(tagged_input_text_content$token), decreasing = TRUE)
    word_freq_df <- as.data.frame(word_freq[1:25])
    tagged_input_text_lemma1 <- tagged_input_text_content$lemma
    tagged_input_text_lemma1_text <- as.vector(knitr::combine_words(tagged_input_text_lemma1, sep = " "))
    # ----------
    
    
    
    
    # Content Word Count
    WordCount_Content <- wordcount(tagged_input_text_lemma1_text)
    
    output$WordCount_Content <- renderUI({
      HTML(paste("<b>","Nouns, Verbs, Adjectives, Adverbs Word Count: ","</b>"),"<br>","<i style = 'font-weight: bold'>",WordCount_Content,"</i>","<hr>")
    })
    
    
    # Sentence count
    SentenceCount <- length(input_text_split)
    output$SentenceCount <- renderUI({
      HTML(paste("<b>","Sentence Count: ","</b>"),"<br>","<i style = 'font-weight: bold'>",SentenceCount,"</i>","<hr>")
    })
    
    
  
    
    # Word Cloud
    output$plot_word_cloud <- renderPlot({
      wordcloud(names(word_freq[1:100]), word_freq, min.freq = 1, random.order = FALSE,
                scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))
    })
    
    
    
    # 25 Most Frequently Used Words
    word_freq <- sort(table(tagged_input_text_content$token), decreasing = TRUE)
    word_freq_df <- as.data.frame(word_freq[1:25])
    tagged_input_text_lemma1 <- tagged_input_text_content$lemma
    tagged_input_text_lemma1_text <- as.vector(knitr::combine_words(tagged_input_text_lemma1, sep = " "))
    #ggplot(word_freq_df, aes(Var1, Freq)) + geom_bar(stat="identity", fill="darkslategray3") + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Text Words") + ylab("Frequency") + ggtitle("25 Most Frequently Used Words of Text")

    output$plot_most_freq_words <- renderHighchart(
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       //console.log(this);
                                       //console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+'count</span>:<b> '
                                       +Math.round(this.point.y.toFixed(0)) + '</b>';
                                       return result;
      }"))) %>%
      hc_xAxis(categories = word_freq_df$Var1,
               labels = list(style = list(fontSize= '11px')), max=15, scrollbar = list(enabled = T)
      )    %>%
      hc_add_series(name="Word", data = word_freq_df$Freq, type ="column",
                    color = "#4472c4", showInLegend= F)
    
    )
    
    
   
    # LDA 
    input_text_split_no_punc <- rep(NA, length(input_text_split))
    for (i in 1:length(input_text_split)){
      input_text_split_no_punc[i] <- gsub("[[:punct:]]", "", input_text_split[i])
    }
    text_df <- tibble(line = 1:length(input_text_split_no_punc), text = input_text_split_no_punc)
    # Split into words
    by_sent_word <- text_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words)
    by_sent_word <- as.data.frame(by_sent_word)
    names(by_sent_word) <- c("sentence", "word")
    # Get word count  
    by_sent_word_count <- by_sent_word %>% dplyr::count(sentence, word)
    # Create sentence-term matrix (stm) 
    text_stm <- by_sent_word_count %>%
      cast_dtm(sentence, word, n)
    # LDA 1
    # Words Most Common within each LDA Topic
    text_lda <- LDA(text_stm, k = 2, control = list(seed = 1234))
    text_topics <- tidy(text_lda, matrix = "beta")
    text_top_terms <- text_topics %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>% 
      ungroup() %>%
      arrange(topic, -beta)
    text_top_terms_1 <- text_top_terms[text_top_terms$topic == 1, ]
    text_top_terms_1 <- head(text_top_terms_1, n=10)
    text_top_terms_2 <- text_top_terms[text_top_terms$topic == 2, ]
    text_top_terms_2 <- head(text_top_terms_2, n=10)
    text_top_terms <- rbind(text_top_terms_1, text_top_terms_2)
    text_top_terms <- mutate(text_top_terms, term = reorder_within(term, beta, topic))
    
    output$plot_lda_common_words <- renderPlot({
    ggplot(text_top_terms, aes(beta, term, fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~ topic, scales = "free") + scale_y_reordered()
    })
    
    
    
    # LDA 2
    # Words with Greatest Difference in Beta between LDA Topic 2 and Topic 1
    beta_wide <- text_topics %>%
      mutate(topic = paste0("topic", topic)) %>%
      pivot_wider(names_from = topic, values_from = beta) %>% 
      filter(topic1 > .001 | topic2 > .001) %>%
      mutate(log_ratio = log2(topic2 / topic1))
    
    beta_wide_pos <- beta_wide[beta_wide$log_ratio > 0, ]
    beta_wide_neg <- beta_wide[beta_wide$log_ratio < 0, ]
    
    beta_wide_pos <- beta_wide_pos[order(beta_wide_pos$log_ratio), ]
    beta_wide_pos <- tail(beta_wide_pos, n=10)
    beta_wide_neg <- beta_wide_neg[order(beta_wide_neg$log_ratio), ]
    beta_wide_neg <- tail(beta_wide_neg, n=10)
    
    beta_wide_twenty <- rbind(beta_wide_pos, beta_wide_neg)
    
    beta_wide_twenty$term <- factor(beta_wide_twenty$term, levels = beta_wide_twenty$term)
    
    output$plot_lda_beta <- renderPlot({
    ggplot(beta_wide_twenty, aes(x=term, y=log_ratio)) + geom_bar(stat='identity', fill="darkblue") + coord_flip() + ylab("Log2 ratio of beta in topic 2 / topic 1") + xlab("")
    })
    
    
    
    
    # Lexical Diversity - number of unique words used
    lexical_diversity <- lengths(lapply(strsplit(tolower(mgsub::mgsub(as.character(input_text), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " "), unique))
    output$lexical_diversity <- renderUI({
      HTML(paste("<b>","Lexical Diversity: ","</b>"),"<br>","<i style = 'font-weight: bold'>",lexical_diversity,"</i>","<hr>")
    })
    
    
    
    # Lexical Density - number of unique words divided by total number of words (word repetition)
    lexical_density <- round(lexical_diversity / WordCount_Raw, 4)
    output$lexical_density <- renderUI({
      HTML(paste("<b>","Lexical Density: ","</b>"),"<br>","<i style = 'font-weight: bold'>",lexical_density,"</i>","<hr>")
    })
    
    
    # Word embeddings
    # Lemmatize - get ContentWords column for dataframe
    # String together all lemma words by sentence (usage 2)
    content_text_split <- rep(NA, length(unique(tagged_input_text_content$sentence_id)))
    for (i in 1:length(unique(tagged_input_text_content$sentence_id))){
      indexofint <- unique(tagged_input_text_content$sentence_id)[i]
      content_text_split[i] <- as.vector(knitr::combine_words(tagged_input_text_content$lemma[tagged_input_text_content$sentence_id == indexofint], sep = " "))
    }
    # Word2Vec
    model <- word2vec(x = as.character(content_text_split), type = "skip-gram", dim = 15, iter = 100, min_count = 1)
    emb <- as.matrix(model)
    # Dimensionality reduction
    tsne <- Rtsne(emb, perplexity = 10, pca = TRUE)
    # Plot
    tsne_plot <- tsne$Y %>%
      as.data.frame() %>%
      mutate(word = row.names(emb)) %>%
      ggplot(aes(x = V1, y = V2, label = word)) +
      geom_text(size = 2)
    output$tsne_plot <- renderPlot({tsne_plot})
    
    #progress <- shiny::Progress$new()
    progress$set(message = "Doge make progress", value = 0.25)
    #on.exit(progress$close())
    
    
    
    
    # ____________________ Sentiment ___________________
  
  
    # Sentiment
    sentiment_web <- get_nrc_sentiment(input_text)
    sentiment_web <- data.frame(t(sentiment_web))
    names(sentiment_web)[1] <- "count"
    sentiment_web <- cbind("sentiment" = rownames(sentiment_web), sentiment_web)
    sentiment_web_emotion <- sentiment_web[1:8, ]
    sentiment_web_posneg <- sentiment_web[9:10, ]
    
    output$plot_sentiment <- renderPlot({
    ggplot(sentiment_web_emotion, aes(sentiment, count, fill=sentiment)) + geom_bar(stat="identity") + scale_y_continuous(breaks=seq(from = 0, to = max(sentiment_web_emotion$count)+1, by = 1)) + ylim(0, max(sentiment_web_emotion$count)+1) + theme_minimal() + theme(axis.text.x=element_blank()) + ylab("Count")
    })
    
    
    
    # Percent Positive, Neutral, Negative
    pos_neg_neut_raw <- calculate_total_presence_sentiment(input_text_split)
    pos_neg_neut_raw[ , 2][[2]]
    total_num <- as.numeric(pos_neg_neut_raw[ , 1][[2]]) + as.numeric(pos_neg_neut_raw[ , 2][[2]]) + as.numeric(pos_neg_neut_raw[ , 3][[2]]) + as.numeric(pos_neg_neut_raw[ , 4][[2]]) + as.numeric(pos_neg_neut_raw[ , 5][[2]]) + as.numeric(pos_neg_neut_raw[ , 6][[2]])
    pos_neg_neut_df <- data.frame("Sentiment" = c("Negative", "Neutral", "Positive"), "Percent" = c( ((as.numeric(pos_neg_neut_raw[ , 2][[2]]) + as.numeric(pos_neg_neut_raw[ , 3][[2]]) + as.numeric(pos_neg_neut_raw[ , 1][[2]]))/total_num)*100,  (as.numeric(pos_neg_neut_raw[ , 4][[2]])/total_num)*100,  ((as.numeric(pos_neg_neut_raw[ , 5][[2]]) + as.numeric(pos_neg_neut_raw[ , 6][[2]]) )/total_num)*100))
    
    output$sentiment_plot <- renderHighchart(
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
                   }")))%>%
      hc_xAxis(categories = pos_neg_neut_df$Sentiment,
               labels = list(style = list(fontSize= '12px'))
      )    %>%
      #hc_colors(colors = "red", "blue", "green") %>% 
      hc_add_series(name="Sentiment", data = pos_neg_neut_df$Percent, colorByPoint = TRUE, 
                    type ="column",
                    color = "#4472c4", showInLegend= F) %>% 
      hc_yAxis(labels=list(format = '{value}%'),min=0,
               max=100,showFirstLabel = TRUE,showLastLabel=TRUE)
    )
    
    
    # Positive (+) / Negative (-) Sentiment Score by Sentence
    # Overall Text Sentiment
    overall_text_sentiment <- get_sentiment(input_text)
    # Positive (+) / Negative (-) Score by Sentence
    # Split by sentence
    input_text_split <- unlist(strsplit(input_text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    # Sentiment score by sentence
    sentscore_by_sentence <- get_sentiment(input_text_split)
    by_sentence_df <- data.frame("sentence" = input_text_split, "sentiment_score" = sentscore_by_sentence, "sentence_number" = 1:length(input_text_split))
    output$plot_posneg_sentence <- renderPlot({
    ggplot(by_sentence_df, aes(x=sentence_number, y=sentiment_score)) + geom_col(fill = "lightsteelblue2") + xlab("Sentence Number") + geom_hline(yintercept=mean(by_sentence_df$sentiment_score), linetype="dashed", color = "lightsteelblue4") + scale_x_continuous(breaks=pretty_breaks()) + ylab("Positive/Negative Sentiment Score") + geom_hline(yintercept=0, color = "red") + ggplot2::annotate(geom="text", x=0.5*length(by_sentence_df$sentence_number), y=mean(by_sentence_df$sentiment_score) + 0.2, label=paste0("Average = ", paste(round(mean(by_sentence_df$sentiment_score), 4))), color="lightsteelblue4")
    })
    
    
    
    
    # Most Positive Sentence
    most_pos_sentence <- as.character(by_sentence_df$sentence[by_sentence_df$sentiment_score == max(by_sentence_df$sentiment_score)])
    output$most_pos_sentence <- renderUI({
      HTML(paste("<b>","Most Positive Sentence: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",most_pos_sentence,"\"","</i>","<hr>")
    })
    
    
    # Most Negative Sentence
    most_neg_sentence <- as.character(by_sentence_df$sentence[by_sentence_df$sentiment_score == min(by_sentence_df$sentiment_score)])
    output$most_neg_sentence <- renderUI({
      HTML(paste("<b>","Most Negative Sentence: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",most_neg_sentence,"\"","</i>","<hr>")
    })
    
    
    
    
    # Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words
    sent_value <- get_sentiment(names(word_freq))
    sent_value_df <- as.data.frame(sent_value)
    sent_value_df$Word <- names(word_freq)
    sent_value_df$Word <- factor(sent_value_df$Word, levels = sent_value_df$Word)
    output$plot_sent_most_freq_words <- renderPlot({
    ggplot(sent_value_df[1:25, ], aes(Word, sent_value)) + geom_bar(stat="identity", fill="plum3") + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Text Words") + ylab("Sentiment")
    })
    
    
    
    
    # Positive (+) / Negative (-) Sentiment Score for 25 Most Frequently Used Words with Non-Zero Sentiments
    sent_value_df_nonzero <- sent_value_df[!sent_value == 0, ]
    sent_value_df_nonzero$Word <- factor(sent_value_df_nonzero$Word, levels = sent_value_df_nonzero$Word)
    output$plot_sent_most_freq_words_nonzero <- renderPlot({
    ggplot(sent_value_df_nonzero[1:25, ], aes(Word, sent_value)) + geom_bar(stat="identity", fill="plum3") + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Text Words") + ylab("Sentiment")
    })
    

    #progress <- shiny::Progress$new()
    progress$set(message = "Doge working hard", value = 0.5)
    #on.exit(progress$close())
    
    
    # ____________________ Coherence ___________________
    
  
    
    # Coherence
    load("data/TASA.rda")
    coherence_web <- coherence(as.character(input_text), split=c(".","!","?"), tvectors=TASA, breakdown=F)
    data_coherence <- data.frame("Coherence" = coherence_web[[1]], "Sentence_Pair" = c(1:length(coherence_web[[1]])))
    output$plot_coh <- renderPlot({
    ggplot(data_coherence, aes(x=Sentence_Pair, y=Coherence)) + geom_col(fill = "seagreen3") + xlab("Sentence Pair #") + geom_hline(yintercept=coherence_web[[2]], linetype="dashed", color = "darkgreen") + ggplot2::annotate(geom="text", x=0.5*length(coherence_web[[1]]), y=coherence_web[[2]] + 0.03, label=paste0("Average = ", paste(round(coherence_web[[2]], 4))), color="darkgreen")
    })
    
    
    # Average Semantic Efficiency 
    # Create dictionary of embeddings
    tsne_df <- as.data.frame(tsne$Y)
    tsne_df$word <- row.names(emb)
    for (j in 1:nrow(tagged_input_text_content)){
      wordofint <- tagged_input_text_content$lemma[j]
      # Source TSNE dictionary and load to df_list dataframes
      tagged_input_text_content$V1[j] <- tsne_df$V1[tsne_df$word == wordofint]
      tagged_input_text_content$V2[j] <- tsne_df$V2[tsne_df$word == wordofint]
    }
    # Overlap number
    # Initialize
    tagged_input_text_content$overlap_number <- 0
    for (j in unique(tagged_input_text_content$sentence_id)){
      # per word
      for (k in 1:(length(tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$sentence_id))-1){
        V1_TF <- abs(as.numeric(na.omit(tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$V1[k+1:length(tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$sentence_id)])) - tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$V1[k]) < 0.25
        V2_TF <- abs(as.numeric(na.omit(tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$V2[k+1:length(tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$sentence_id)])) - tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$V2[k]) < 0.25
        tagged_input_text_content[tagged_input_text_content$sentence_id == j, ]$overlap_number[k] <- sum(V1_TF[V1_TF == TRUE] == V2_TF[V2_TF == TRUE])
      }
    }
    # Overlap total number
    # Initialize
    tagged_input_text_content$overlap_total_number <- 0
    # per sentence
    for (j in 1:nrow(tagged_input_text_content)){
      tagged_input_text_content$overlap_total_number[j] <- aggregate(tagged_input_text_content$overlap_number, by=list(tagged_input_text_content$sentence_id), FUN = sum)[which(aggregate(tagged_input_text_content$overlap_number, by=list(tagged_input_text_content$sentence_id), FUN = sum)$Group.1 == tagged_input_text_content$sentence_id[j]), 2]
    }
    # Total sentence content words
    # per row
    for (j in 1:nrow(tagged_input_text_content)){
      tagged_input_text_content$total_sentence_content_words[j] <- as.data.frame(table(tagged_input_text_content$sentence_id))[which(as.data.frame(table(tagged_input_text_content$sentence_id))$Var1 == tagged_input_text_content$sentence_id[j]), 2]
    }
    # Total combinations per sentence
    for (j in 1:nrow(tagged_input_text_content)){
      tagged_input_text_content$total_sentence_combinations[j] <- (factorial(tagged_input_text_content$total_sentence_content_words[j])) / (2* factorial(tagged_input_text_content$total_sentence_content_words[j] - 2))
    }
    # Semantic efficiency per sentence
    # per row
    for (j in 1:nrow(tagged_input_text_content)){
      tagged_input_text_content$semantic_efficiency[j] <- 1 - (tagged_input_text_content$overlap_total_number[j]/tagged_input_text_content$total_sentence_content_words[j])
    }
    Semantic_Efficiency_Avg <- round(mean(tagged_input_text_content$semantic_efficiency, na.rm = T), 4)
    
    
    # Semantic efficiency by sentence
    sem_eff_df <- unique(tagged_input_text_content[,c('sentence_id','semantic_efficiency')])
    output$sem_eff_sentence <- renderPlot({
      ggplot(sem_eff_df, aes(x=sentence_id, y=semantic_efficiency)) + geom_col(fill = "lightpink2") + xlab("Sentence Number") + geom_hline(yintercept=Semantic_Efficiency_Avg, linetype="dashed", color = "lightpink4") + scale_x_continuous(breaks=pretty_breaks()) + ylab("Semantic Efficiency") + ggplot2::annotate(geom="text", x=0.5*length(sem_eff_df$semantic_efficiency), y=Semantic_Efficiency_Avg + 0.05, label=paste0("Average = ", paste(Semantic_Efficiency_Avg)), color="lightpink4")
    })
   
    
    # Network Graph for Most Disorganized Sentence (i.e. Greatest Word Count - Lexical Diversity)
    disorg_value <- 0
    for (i in 1:length(input_text_split)){
      disorg_value[i] <- wordcount(input_text_split[i]) - lengths(lapply(strsplit(tolower(mgsub::mgsub(as.character(input_text_split[i]), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " "), unique)) 
    }
    index_ref <- which(disorg_value == max(disorg_value))
    disorg_sentence <- input_text_split[index_ref]
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
    plot(routes_network, vertex.cex = 3, vertex.col = "red", edge.col = "black", displaylabels=T, label.cex=0.5)
    })
    
    
    
    
    
    # Network Graph for Most Organized Sentence (i.e. Smallest Word Count - Lexical Diversity)
    disorg_value1 <- 0
    for (i in 1:length(input_text_split)){
      disorg_value1[i] <- wordcount(input_text_split[i]) - lengths(lapply(strsplit(tolower(mgsub::mgsub(as.character(input_text_split[i]), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " "), unique)) 
    }
    index_ref1 <- which(disorg_value1 == min(disorg_value1))
    disorg_sentence1 <- input_text_split[index_ref1]
    source1 <- strsplit(tolower(mgsub::mgsub(as.character(disorg_sentence1), c("\\.", ",", "?", "!"), c("", "", "", ""))), split = " ")[[1]]
    destination1 <- 0
    for (i in 1:length(source1)-1){
      destination1[i] <- source1[i+1]
    }
    destination1[length(source1)] <- "." 
    edge_list1 <- tibble(from = source1, to = destination1)
    node_list1 <- tibble(id = 1:length(source1))
    routes_network1 <- network(edge_list1, vertex.attr = node_list1, matrix.type = "edgelist", ignore.eval = FALSE)
    output$plot_org_sentence <- renderPlot({
    plot(routes_network1, vertex.cex = 3, vertex.col = "red", edge.col = "black", displaylabels=T, label.cex=0.5)
    })
    
    
    
    # Mean number of syllables per word
    readability <- textstat_readability(input_text, c("meanSentenceLength","meanWordSyllables", "Flesch.Kincaid", "Flesch"), remove_hyphens = TRUE, min_sentence_length = 1, max_sentence_length = 10000, intermediate = FALSE)
    meanWordSyllables <- round(readability$meanWordSyllables, 4)
    output$meanWordSyllables <- renderUI({
      HTML(paste("<b>","Mean Number of Syllables per Word: ","</b>"),"<br>","<i style = 'font-weight: bold'>",meanWordSyllables,"</i>","<hr>")
    })
    
    
    
    # Flesch Reading Ease
    Flesch_score <- round(readability$Flesch, 4)
    output$Flesch_score <- renderUI({
      HTML(paste("<b>","Flesch Reading Ease: ","</b>"),"<br>","<i style = 'font-weight: bold'>",Flesch_score,"</i>","<hr>")
    })
    
    
    
    # Flesch Kincaid Grade Level
    Flesch_Kincaid_score <- round(readability$Flesch.Kincaid, 4)
    output$Flesch_Kincaid_score <- renderUI({
      HTML(paste("<b>","Flesch-Kincaid Grade Level: ","</b>"),"<br>","<i style = 'font-weight: bold'>",Flesch_Kincaid_score,"</i>","<hr>")
    })
    
    #progress <- shiny::Progress$new()
    progress$set(message = "Doge almost done!", value = 0.75)
    #on.exit(progress$close())
    
    
    # ____________________ Mindfulness ___________________

    
    # Percent of words that are mindfulness words
    # Mindfulness words based on the following reference
    # Figure 1 in https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2799300/
    mindfulness_word_list <- c("accept", "allow", "attention", "automatic", "autopilot", "aware", "balance", "being", "body", "breath", "calm", "center", "choice", "commit", "compassion", "curiosity", "discomfort", "space", "emotion", "energy", "experience", "expand", "feel", "fight", "fix", "free", "friend", "sense", "grateful", "gratitude", "judge", "thought", "let", "meditate", "mindful", "moment", "nonjudge", "notice", "now", "ok", "okay", "observe", "open", "pause", "practice", "time", "present", "time", "present", "sit", "react", "relax", "resist", "response", "scan", "gentle", "push", "slow", "sober", "soften", "ease", "stop", "struggle", "kind", "peace", "willing")
    # All words in input
    input_word_list <- tagged_input_text$lemma
    # Words that are in both lists
    input_mindfulness_words <- input_word_list[input_word_list %in% mindfulness_word_list]
    # Number of mindfulness words in input
    number_input_mindfulness_words <- length(input_mindfulness_words)
    # Percent of words that are mindfulness words
    Wordcount_all <- wordcount(input_text)
    percent_mindfulness <- round((number_input_mindfulness_words/Wordcount_all)*100, 4)
    output$percent_mindfulness <- renderUI({
      HTML(paste("<b>","Percent of Words that are Mindfulness Words: ","</b>"),"<br>","<i style = 'font-weight: bold'>",percent_mindfulness,"</i>","<hr>")
    })
    
    
    # --LIWC-- (Credit to Pennebaker)
    #load("LIWC2015_Dictionary.dic")
    # LIWC
    liwc2015dict <- dictionary(file = "data/LIWC2015_Dictionary.dic", 
                               format = "LIWC")
    liwc_by_sent <- dfm(input_text_split, dictionary = liwc2015dict)
    liwc_by_sent <- as.data.frame(liwc_by_sent)
    
    liwc_text <- dfm(input_text, dictionary = liwc2015dict)
    liwc_text <- as.data.frame(liwc_text)
    
    
    # Judgmental Score
    # Discrepancy (discrep) + Certainty (certain)
    number_judgmental <- liwc_text$discrep + liwc_text$certain
    percent_judgmental <- round((number_judgmental/Wordcount_all)*100, 4)
    output$percent_judgmental <- renderUI({
      HTML(paste("<b>","Percent of Words that are Judgmental Words: ","</b>"),"<br>","<i style = 'font-weight: bold'>",percent_judgmental,"</i>","<hr>")
    })
    
    
    
    
    # Sensory triangle chart
    sense_df <- data.frame("Sense" = c("Sight", "Sound", "Touch"), "Count" = c(liwc_text$see, liwc_text$hear, liwc_text$feel))
    output$plot_sensory <- renderHighchart(
    hc <- highchart() %>%
      hc_chart(polar = T) %>% 
      hc_xAxis(categories = sense_df$Sense, 
               labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
      hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
      hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
      hc_add_series(name = "Sensory Count", sense_df$Count, type ="area", color = "#4472c4", pointPlacement = "on")
    )
    
    
    
    # Concreteness measure
    # Sourced from supplementary materials ofL
    # http://crr.ugent.be/papers/Brysbaert_Warriner_Kuperman_BRM_Concreteness_ratings.pdf
    #load("concreteness_table.csv")
    dat_conc <- read.csv("data/concreteness_table.csv")
    #The file contains eight columns:
    #1. The word
    #2. Whether it is a single word or a two-word expression 
    #3. The mean concreteness rating
    #4. The standard deviation of the concreteness ratings
    #5. The number of persons indicating they did not know the word
    #6. The total number of persons who rated the word
    #7. Percentage participants who knew the word
    #8. The SUBTLEX-US frequency count (on a total of 51 million; Brysbaert & New, 2009)
    # We are interested in the third column 'Conc.M'.
    # It is currently scaled 1 (very abstract; language-based) to 5 (very concrete; experience-based)
    # I want to change the scale from -1 (very abstract) to 1 (concrete), so 0 is the default mid-value.
    # Call this new scaled column 'Concreteness'
    dat_conc$Concreteness <- (((dat_conc$Conc.M - 1) * 2) / 4) + -1
    tagged_input_text$Concreteness <- 0
    for (i in 1:nrow(tagged_input_text)){
      wordofint <- tagged_input_text$lemma[i]
      if (length(unique(wordofint == dat_conc$Word)) == 2){
        tagged_input_text$Concreteness[i] <- dat_conc$Concreteness[dat_conc$Word == wordofint]
      }
      else {
        tagged_input_text$Concreteness[i] <- 0
      }
    }
    # Remove punctuation
    tagged_input_text_wo_punct <- tagged_input_text[!tagged_input_text$upos == "PUNCT", ]
    concreteness_by_sent <- aggregate(Concreteness ~ sentence_id, data = tagged_input_text_wo_punct, mean)
    output$plot_concreteness <- renderPlot({
    ggplot(concreteness_by_sent, aes(x=sentence_id, y=Concreteness)) + geom_col(fill = "darkseagreen3") + xlab("Sentence Number") + geom_hline(yintercept=mean(concreteness_by_sent$Concreteness), linetype="dashed", color = "darkseagreen4") + ggplot2::annotate(geom="text", x=0.5*length(concreteness_by_sent$sentence_id), y=mean(concreteness_by_sent$Concreteness) + 0.02, label=paste0("Average = ", paste(round(mean(concreteness_by_sent$Concreteness), 4))), color="darkgreen") + scale_x_continuous(breaks=pretty_breaks()) + ylab("Concreteness Score") + geom_hline(yintercept=0, color = "red") + theme(plot.title = element_text(size = 10))
    })
    
    
    
    
    # Percent Past, Present, and Future (Finite) Verbs
    # remember that infinite verbs do not show tense
    tagged_input_text_verb <- tagged_input_text[tagged_input_text$upos == "VERB", ]
    tagged_input_text_finite_verb <- tagged_input_text_verb[!grepl("VerbForm=Inf", tagged_input_text_verb$feats), ]
    #tagged_input_text_finite_verb <- tagged_input_text_finite_verb[!grepl("VerbForm=Ger", tagged_input_text_finite_verb$feats), ]
    # Count of each tense
    past_count <- length(tagged_input_text_finite_verb[grep("Tense=Past", tagged_input_text_finite_verb$feats), 10])
    present_count <- length(tagged_input_text_finite_verb[grep("Tense=Pres", tagged_input_text_finite_verb$feats), 10])
    will_count <- tagged_input_text[grep("will", tagged_input_text$token), ]
    gerund_count <- nrow(tagged_input_text_finite_verb[grepl("VerbForm=Ger", tagged_input_text_finite_verb$feats), ])
    would_count <- nrow(tagged_input_text[tagged_input_text$token == "would", ])
    future_count <- length(will_count[will_count$upos == "AUX", 6]) + would_count + gerund_count
    verbs_pastpresfut <- data.frame("tense" = c("past", "present", "future"), "count" = c(past_count, present_count, future_count))
    #rownames(verbs_pastpresfut) <- c("past", "present", "future")
    verbs_pastpresfut <- verbs_pastpresfut %>% 
      arrange(desc(tense)) %>%
      mutate(prop = count / sum(verbs_pastpresfut$count) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    #  ggplot(verbs_pastpresfut, aes(x="", y=prop, fill=tense)) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme_void() +  theme(legend.position="none") + geom_text(aes(y = ypos, label = tense), color = "white", size=6) + scale_fill_brewer(palette="Set1") + ggtitle("Percent Past, Present, and Future (Finite) Verbs")
    output$plot_tense <- renderHighchart(
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
                   }")))%>%
      hc_xAxis(categories = verbs_pastpresfut$tense,
               labels = list(style = list(fontSize= '12px'))
      )    %>%
      #hc_colors(colors = "red", "blue", "green") %>% 
      hc_add_series(name="Tense", data = verbs_pastpresfut$prop, colorByPoint = TRUE, 
                    type ="column",
                    color = "#4472c4", showInLegend= F) %>% 
      hc_yAxis(labels=list(format = '{value}%'),min=0,
               max=100,showFirstLabel = TRUE,showLastLabel=TRUE)
    )
    
    
    
    # Percent of words that are personal pronouns
    number_personal_pronouns <- liwc_text$ppron
    percent_personal_pronouns <- round((number_personal_pronouns/Wordcount_all)*100, 4)
    output$percent_personal_pronouns <- renderUI({
      HTML(paste("<b>","Percent of Words that are Personal Pronouns: ","</b>"),"<br>","<i style = 'font-weight: bold'>",percent_personal_pronouns,"</i>","<hr>")
    })
    
    
    
    # Percent of words that are impersonal pronouns
    number_impersonal_pronouns <- liwc_text$ipron
    percent_impersonal_pronouns <- round((number_impersonal_pronouns/Wordcount_all)*100, 4)
    output$percent_impersonal_pronouns <- renderUI({
      HTML(paste("<b>","Percent of Words that are Impersonal Pronouns: ","</b>"),"<br>","<i style = 'font-weight: bold'>",percent_impersonal_pronouns,"</i>","<hr>")
    })
    
    
    #progress <- shiny::Progress$new()
    progress$set(message = "Doge done!", value = 0.1)
    on.exit(progress$close())
    
    
  })
  
}



shinyApp(ui = ui, server = server)