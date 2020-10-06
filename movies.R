movies <- read.csv("MoviesOnStreamingPlatforms_updated.csv")
movies <- movies[,-1]
movies <- movies %>% mutate(Rotten.Tomatoes = as.numeric(gsub("%", "", Rotten.Tomatoes)))

movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 0 & movies$Prime.Video ==0 & movies$Disney. == 0] <- "Netflix"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 0 & movies$Prime.Video ==1 & movies$Disney. == 0] <- "Prime"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 1 & movies$Prime.Video ==0 & movies$Disney. == 0] <- "Hulu"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 0 & movies$Prime.Video ==0 & movies$Disney. == 1] <- "Disney Plus"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 1 & movies$Prime.Video ==0 & movies$Disney. == 0] <- "Netflix and Hulu"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 0 & movies$Prime.Video ==1 & movies$Disney. == 0] <- "Netflix and Prime"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 0 & movies$Prime.Video ==0 & movies$Disney. == 1] <- "Netflix and Disney Plus"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 1 & movies$Prime.Video ==1 & movies$Disney. == 0] <- "Hulu and Prime"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 1 & movies$Prime.Video ==0 & movies$Disney. == 1] <- "Hulu and Disney Plus"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 0 & movies$Prime.Video ==1 & movies$Disney. == 1] <- "Prime and Disney Plus"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 1 & movies$Prime.Video ==1 & movies$Disney. == 0] <- "Netflix, Hulu and Prime"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 1 & movies$Prime.Video ==0 & movies$Disney. == 1] <- "Netflix, Hulu and Disney Plus"
movies$Streaming.Platform[movies$Netflix == 0 & movies$Hulu == 1 & movies$Prime.Video ==1 & movies$Disney. == 1] <- "Hulu, Prime and Disney Plus"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 0 & movies$Prime.Video ==1 & movies$Disney. == 1] <- "Disney Plus, Prime and Netflix"
movies$Streaming.Platform[movies$Netflix == 1 & movies$Hulu == 1 & movies$Prime.Video ==1 & movies$Disney. == 1] <- "All"


unique(movies$Streaming.Platform)
