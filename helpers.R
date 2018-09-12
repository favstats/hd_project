###### Authentification ####

readkey <- function() {
  line <- readline(prompt = "Are you there? Press [enter]")
}

authentication <- function() {
  beepr::beep(5)
  readkey()
  auth_token <- ".httr-oauth"
  if (file.exists(auth_token)) file.remove(auth_token)
  
  client_id <- "580511698053-p8mu77kktkcvb503g9q737svu9gcq101.apps.googleusercontent.com"
  
  client_key <- "Y0eW96yy-WRg90RZjVJS615o"
  
  tuber::yt_oauth(client_id, client_key)
}

###### Data Extraction ####


get_links <- function(html) {
  html <- read_html(html)
  links <- html %>% 
    # html_attr("title")
    html_nodes("#view-more .yt-formatted-string") %>% 
    html_attr("href")
  
  title <- html %>% 
    # html_attr("title")
    html_nodes("#title-wrapper #video-title , #video-title.ytd-playlist-renderer") %>% 
    html_attr("title")  
  
  playlist <- tibble(links, title)
  
  return(playlist)
}


###### Plalylist Functions ####


get_playlist_children <- function(links) {
  tuber::get_playlist_items(filter = c(playlist_id = links), max_results = 100000) %>% 
  mutate(contentDetails.videoPublishedAt = lubridate::as_date(contentDetails.videoPublishedAt)) %>% 
    dplyr::mutate(year = lubridate::year(contentDetails.videoPublishedAt)) %>% 
    dplyr::mutate(day = lubridate::day(contentDetails.videoPublishedAt)) %>% 
    dplyr::mutate(month = lubridate::month(contentDetails.videoPublishedAt, label = T))
}

# get_playlist_children(holohoax_playlist$holohoax_links[1])

get_items <- function(links) {
  cat("Starting loop. Sit back and get a tea. This might take a while...  ( ͡° ͜ʖ ͡°)\n")
  data <- links %>% 
    purrr::map_df(tidyMBO::progressively(get_playlist_children, 
                                         .n = length(links))) 
  cat("nAdding Video IDs\n")
  data <- data %>% 
    dplyr::mutate(video_id = dplyr::case_when(
      is.na(contentDetails.videoId) ~ videoId,
      TRUE ~ contentDetails.videoId
    )) 
  cat("Done!")
  return(data) 
}

###### Video Stats ####


get_video_stats <- function(ids) {
  suppressWarnings(tuber::get_stats(video_id = ids)) %>% 
    bind_rows() 
}

get_vids_details <- function(video_id) {
  video_details  <- suppressWarnings(tuber::get_video_details(video_id = video_id))
  if (!length(video_details) == 0) {
    title <- video_details$items[[1]]$snippet$localized$`title`
    id <- video_details$items[[1]]$id
    date <- video_details$items[[1]]$snippet$`publishedAt`
    channel_id <- video_details$items[[1]]$snippet$channelId
    channel_title <- video_details$items[[1]]$snippet$channelTitle
    description <- video_details$items[[1]]$snippet$description
    tags <- video_details$items[[1]]$snippet$tags
    video_details <- tibble(title,
                            id,
                            date,
                            channel_id,
                            channel_title,
                            description,
                            tags = list(tags)) 
    return(video_details)
  } else
    
    video_details <- NULL
}


get_more_details <- function(video_id){
  cat("Starting loop. Sit back and get a tea. This might take a while...  ( ͡° ͜ʖ ͡°)\n")
  cat("\nGet Video Statistics\n")
  stats <- video_id %>% 
    purrr::map_df(tidyMBO::progressively(get_video_stats, 
                                         .n = length(video_id)))
  cat("\nGet Video Details\n")
  
  details <- video_id %>% 
    purrr::map_df(tidyMBO::progressively(get_vids_details, 
                                         .n = length(video_id)))
  cat("\nMerging...\n")
  
  data <- stats %>% 
    left_join(details) %>% 
    mutate(date = lubridate::as_date(date)) %>% 
    # dplyr::mutate(year = lubridate::year(date)) %>% 
    # dplyr::mutate(day = lubridate::day(date)) %>% 
    # dplyr::mutate(month = lubridate::month(date, label = T))  %>% 
    rename(video_id = id)
  
  cat("\nDone!\n")
  
  return(data)
}





# ## tryouts
# 
# 
# ```{r, data}
# 
# holohoax_dat <- yt_search("holohoax", max_results = 50)
# 
# rev_dat <- yt_search("revisionism", max_results = 50)
# 
# rev_dat %>% 
#   mutate(year = lubridate::year(publishedAt)) %>% 
#   mutate(day = lubridate::day(publishedAt)) %>% 
#   mutate(month = lubridate::month(publishedAt)) %>% 
#   mutate(cdate = lubridate::ydm(paste0(year, "-", day, "-", month))) %>% 
#   group_by(cdate) %>%
#   tally() %>% 
#   ggplot(aes(cdate, n)) +
#   geom_line() +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#   ggplot2::labs(
#     x = NULL, y = NULL,
#     title = "Frequency of #rstats Twitter statuses from past 9 days",
#     subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
#     caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#   ) 
# 
# 
# 
# tuber::get_playlist_items("PL771ITT1lG9RsfPHm688fwJVV_aBB8j_6")
# 
# tuber::get_playlist_items(filter = 
#                             c(playlist_id = "PLQkR-DR1A9x2KI15Q_m0lQeRteffUlrlF"), max_results = 1000)
# ```
# 
# ```{r}
# 
# holohoax_links <- read_html("data/holohoax - YouTube.html") %>% 
#   # html_attr("title")
#   html_nodes("#view-more .yt-formatted-string") %>% 
#   html_attr("href")
# 
# holohoax_title <- read_html("data/holohoax - YouTube.html") %>% 
#   # html_attr("title")
#   html_nodes("#title-wrapper #video-title , #video-title.ytd-playlist-renderer") %>% 
#   html_attr("title")
# 
# 
# 
# holohoax_playlist <- tibble(holohoax_links, holohoax_title) %>% 
#   mutate(
#     holohoax_filter = ifelse(
#       str_detect(stringr::str_to_lower(holohoax_title),
#                  "hoax|fake|debunk|deprogram|holo|lie|revision|denial|fraud"), 1, 0)) %>% 
#   filter(holohoax_filter == 1) %>% 
#   filter(!(str_detect(holohoax_title, "PROBABLE FALSE FLAGS, FRAUDS, PSYOPS|Pro holocaust"))) %>% 
#   mutate(holohoax_links = str_remove(holohoax_links, "https://www.youtube.com/playlist\\?list="))
# 
# 
# get_playlist_children <- function(links) {
#   tuber::get_playlist_items(filter = c(playlist_id = links), max_results = 100000)  
# }
# 
# get_playlist_children(holohoax_playlist$holohoax_links[1])
# 
# holohoax_children <- holohoax_playlist$holohoax_links %>% 
#   purrr::map_df(tidyMBO::progressively(get_playlist_children, 
#                                        .n = length(holohoax_playlist$holohoax_links)))
# 
# holohoax_children$contentDetails.videoPublishedAt
# 
# holohoax_plot <- holohoax_children %>% 
#   dplyr::mutate(video_id = dplyr::case_when(
#     is.na(contentDetails.videoId) ~ videoId,
#     TRUE ~ contentDetails.videoId
#   )) %>% 
#   dplyr::distinct(video_id, .keep_all = T) %>% 
#   dplyr::mutate(year = lubridate::year(contentDetails.videoPublishedAt)) %>% 
#   dplyr::mutate(day = lubridate::day(contentDetails.videoPublishedAt)) %>% 
#   dplyr::mutate(month = lubridate::month(contentDetails.videoPublishedAt, label = T)) 
# 
# # holohoax_plot %>% drop_na(year)
# ```