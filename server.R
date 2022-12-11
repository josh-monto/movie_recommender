## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# now make users/movies ratings structure
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
Rmat = Rmat[1:500, ]

top_movies_by_genre = read.csv("top_movies_by_genre.csv", check.names=FALSE)

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  # Calculate recommendations when the sbumbutton is clicked
  genre_df <- eventReactive(input$genre_btn, {
    withBusyIndicatorServer("genre_btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      pred_ids = top_movies_by_genre[input$genre_choice]
      MovieID = rep(0,10)
      for (i in 1:nrow(pred_ids[input$genre_choice])){
        MovieID[i] = which(movies$MovieID == pred_ids[i,])
      }
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = MovieID)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$genre_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- genre_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$ratings_btn, {
    withBusyIndicatorServer("ratings_btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      user_results = (1:10)/10
      user_predicted_ids = 1:10
      #print(user_ratings)
      #print(sum(!is.na(user_ratings$MovieID)))
      # if no ratings from user, just recommend default
      if (sum(!is.na(user_ratings$MovieID))==0){
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = movies$MovieID[user_predicted_ids], 
                                    Title = movies$Title[user_predicted_ids], 
                                    Predicted_rating =  user_results)
      }
      # otherwise get recommendations using UBCF
      else {
        movieIDs = colnames(Rmat)
        n.item = ncol(Rmat)  
        new.ratings = rep(NA, n.item)
        for (i in 1:dim(user_ratings)[1]){
          if (!is.na(user_ratings$MovieID[i])){
            #mid = paste("m",user_ratings$MovieID[i],sep="")
            #print(mid)
            new.ratings[which(movieIDs == paste("m",user_ratings$MovieID[i],sep=""))] = user_ratings$Rating[i]
          }
        }
        new.user = matrix(new.ratings, 
                        nrow=1, ncol=n.item,
                        dimnames = list(
                          user=paste('feng'),
                          item=movieIDs
                        ))
        new.Rmat = as(new.user, 'realRatingMatrix')
        recommender.UBCF <- Recommender(Rmat, method = "UBCF",
                                      parameter = list(normalize = 'Z-score', 
                                                       method = 'Cosine', 
                                                       nn = 20))
        p.UBCF <- predict(recommender.UBCF, new.Rmat, type="ratings")
        p.UBCF <- as.numeric(as(p.UBCF, "matrix"))
        recs = rev(tail(order(p.UBCF, decreasing = FALSE, na.last = FALSE), 10))
        rats = rev(p.UBCF[tail(order(p.UBCF, decreasing = FALSE, na.last = FALSE), 10)])
        new_rec_ids = append(recs, user_predicted_ids)
        new_rats = append(rats, user_results)
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = movies$MovieID[new_rec_ids], 
                                    Title = movies$Title[new_rec_ids], 
                                    Predicted_rating =  new_rats)
      }
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$ratings_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function