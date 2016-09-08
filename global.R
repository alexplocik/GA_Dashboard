#### Deploy to Google Drive and run from zip file ####
# system("cd '/Users/alexplocik/Google Drive/R scripts/Shiny Apps'; zip -r9X 'GA_Dashboard.zip' 'GA_Dashboard/'")
# library(shiny)
# runUrl("https://drive.google.com/uc?export=download&id=0B6rxEqShLDxASVFScHRNZHJVbk0", filetype = ".zip")

required_package <- function(pkg){
  if(!require(package = as.character(pkg), character.only = T, quietly = T)) {install.packages(pkgs = pkg); require(package = pkg, character.only = T)}
}
required_package("googleAnalyticsR")
required_package("googleAuthR")
required_package("shiny")
required_package("dplyr")
required_package("DT")
required_package("lubridate")
required_package("ggplot2")
required_package("plotly")
# 
# require("googleAnalyticsR")
# require("googleAuthR")
# require("shiny")
# require("dplyr")
# require("DT")
# require("lubridate")
# require("ggplot2")
# require("plotly")

#### Twitter
# install.packages("twitteR")
# install.packages("ROAuth")
# library("twitteR")
# library("ROAuth")

#### Facebook 
# library(devtools)
# install_github("pablobarbera/Rfacebook/Rfacebook")
# library(Rfacebook)

#     #### testing ####
#     input <- NULL
#     input$dateRange1 <- c(start = "2016-06-01", end = "2016-06-30")
#     input$dateRange2 <- c(start = "2016-07-01", end = "2016-07-31")
#     input$dateRange3 <- c(start = "2016-08-01", end = "2016-08-14")
# 
# google_analytics_4(102857018,
#                    c("2016-07-01","2016-07-31"),
#                    dimensions=c('userType'),
#                    metrics = c('sessions')
# )

# segment
# se1 <- segment_element(name = "city", operator = "EXACT", type = "DIMENSION", not = T, expressions = "Danvers", scope = "USER")
# se2 <- segment_element(name = "city", operator = "EXACT", type = "DIMENSION", not = T, expressions = "Tuzla", scope = "USER")
# se3 <- segment_element(name = "city", operator = "EXACT", type = "DIMENSION", not = T, expressions = "Danvers", scope = "SESSION")
# se4 <- segment_element(name = "city", operator = "EXACT", type = "DIMENSION", not = T, expressions = "Tuzla", scope = "SESSION")
# se5 <- segment_element(name = "country", operator = "EXACT", type = "DIMENSION", not = F, expressions = "United States", scope = "SESSION")
# sv_simple1 <- segment_vector_simple(list(list(se1), list(se2)))
# sv_simple2 <- segment_vector_simple(list(list(se3), list(se4)))
# sv_simple3 <- segment_vector_simple(list(list(se5)))
# sv_simple4 <- segment_vector_simple(list(list(se3), list(se4), list(se5)))
# 
# # # sv_simple1 <- segment_vector_simple(list(list(se1), list(se2), list(se3), list(se4), list(se5)))
# # # seg_defined <- segment_define(list(sv_simple1, sv_simple2, sv_simple3))
# seg_defined1 <- segment_define(list(sv_simple1))
# seg_defined2 <- segment_define(list(sv_simple2))
# seg_defined3 <- segment_define(list(sv_simple3))
# seg_defined4 <- segment_define(list(sv_simple4))
# # # segment_location <- segment_ga4("location", user_segment = list(seg_defined1), session_segment = list(seg_defined2, seg_defined3))
# segment_location1 <- segment_ga4("user_loc", user_segment = seg_defined1, session_segment = seg_defined4)
# segment_location1 <- segment_ga4("user_loc", user_segment = seg_defined1)
# segment_location1 <- segment_ga4("user_loc", session_segment = seg_defined4)
# # 
# # segment test
# google_analytics_4(ga_id,
#                    c("2016-07-01","2016-07-31"),
#                    dimensions=c('userType', 'segment'),
#                    segments = list(segment_location1),
#                    metrics = c('sessions')
# )

#### veritas genetics google analytics id ####
ga_id <- 102857018

#### location filters for requests ####
df1 <- list(dim_filter("city", "EXACT", "Danvers", not = T))
df2 <- list(dim_filter("city", "EXACT", "Tuzla", not = T))
df3 <- list(dim_filter("country", "EXACT", "United States", not = F))
df_global <- c(df1, df2, df3)

#### request convienence function ####
request <- function(dateRange, metrics, dimensions, df_global = NULL, df_add = NULL){
  output <- google_analytics_4(viewId = ga_id, date_range = c(dateRange[1], dateRange[2]),
                               metrics = metrics, dimensions = dimensions,
                               if(!is.null(df_global) | !is.null(df_add)){
                                 dim_filters = filter_clause_ga4(c(df_global, df_add), operator = "AND")
                               }
  )
  if(is.null(output)) {output <- 0 %>% as.data.frame; output} else {output}
}

request_sum <- function(dateRange, metrics, dimensions, df_global = NULL, df_add = NULL){
  output <- google_analytics_4(viewId = ga_id, date_range = c(dateRange[1], dateRange[2]),
                               metrics = metrics, dimensions = dimensions, max = 5000,
                               if(!is.null(df_global) | !is.null(df_add)){
                                 dim_filters = filter_clause_ga4(c(df_global, df_add), operator = "AND")
                               }
  )
  if(is.null(output)) {output <- 0 %>% as.vector; output} else {output %>% select(get(metrics)) %>% sum}
}

#### get all google analytics stats for statistics ####
getGAstats <- function(dateRange, range_name){
  
  user_types <- request(dateRange, 'sessions', 'userType', df_global)
  new_site_visitors <- user_types[1, 2]
  returning_site_visitors <- user_types[2, 2]
  
  start_date <- dateRange[1]
  end_date <- dateRange[2]
  days <- as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1])) + 1
  
  user_loc <- request_sum(dateRange, 'sessions', c('userType', 'city'), df_global)# %>% group_by(userType)  %>% arrange(desc(sessions))
  
  myG_all_sessions <- request_sum(dateRange, 'sessions', 'landingScreenName', df_global,
                                  df_add = list(dim_filter("landingScreenName", "PARTIAL", "myG")
                                  ))# %>% select(sessions) %>% sum
  
  myB_all_sessions <- request_sum(dateRange, 'sessions', 'landingScreenName', df_global,
                                  df_add = list(dim_filter("landingScreenName", "PARTIAL", "myB")
                                  ))# %>% select(sessions) %>% sum
  
  myG_orders_started <- request_sum(dateRange, 'uniqueEvents', 'eventLabel', df_global,
                                    df_add = list(dim_filter("eventLabel", "PARTIAL", "order"),
                                                  dim_filter("eventLabel", "PARTIAL", "myG")
                                    )) #%>% select(uniqueEvents) %>% sum
  
  myB_orders_started <- request_sum(dateRange, 'uniqueEvents', 'eventLabel', df_global,
                                    df_add = list(dim_filter("eventLabel", "PARTIAL", "order"),
                                                  dim_filter("eventLabel", "PARTIAL", "myB")
                                    )) #%>% select(uniqueEvents) %>% sum  
  
  myG_orders_completed <- request_sum(dateRange, 'uniqueEvents', 'eventLabel', df_global,
                                      df_add = list(dim_filter("eventLabel", "PARTIAL", "submit"),
                                                    dim_filter("eventLabel", "PARTIAL", "myG")
                                      )) #%>% select(uniqueEvents) %>% sum
  
  myB_orders_completed <- request_sum(dateRange, 'uniqueEvents', 'eventLabel', df_global,
                                      df_add = list(dim_filter("eventLabel", "PARTIAL", "submit"),
                                                    dim_filter("eventLabel", "PARTIAL", "myB")
                                      )) #%>% select(uniqueEvents) %>% sum
  
  myG_pct_order_start_per_session <- (myG_orders_started / myG_all_sessions * 100) %>% round(1)
  myB_pct_order_start_per_session <- (myB_orders_started / myB_all_sessions * 100) %>% round(1)
  myG_pct_order_submit_per_session <- (myG_orders_completed / myG_all_sessions * 100) %>% round(1)
  myB_pct_order_submit_per_session <- (myB_orders_completed / myB_all_sessions * 100) %>% round(1)
  
  adwords_clicks <- request_sum(dateRange, 'adClicks', 'adGroup')
  adwords_cost <- request_sum(dateRange, 'adCost', 'adGroup')
  adwords_CPC <- request_sum(dateRange, 'CPC', 'adGroup')
  
  # avg_session_duration <- request_sum(dateRange, 'avgSessionDuration', NULL, df_global)
  
  df <- data.frame(
    days,
    new_site_visitors, 
    returning_site_visitors,
    
    adwords_clicks,
    
    myG_all_sessions,
    myG_orders_started,
    myG_orders_completed,
    myG_pct_order_start_per_session,
    myG_pct_order_submit_per_session,
    
    myB_all_sessions,
    myB_orders_started,
    myB_orders_completed,
    myB_pct_order_start_per_session,
    myB_pct_order_submit_per_session #,
    
    # avg_session_duration
  )
  df <- df %>% t %>% as.data.frame() %>% round(1)
  colnames(df) <- range_name
  df
}

GAtable.rownames = c(
  "Days",
  "New Site Visitors", 
  "Returning Site Visitors",
  
  "Google Adwords Clicks",
  
  "myG: All Sessions",
  "myG: Orders Started",
  "myG: Orders Completed",
  "myG: Order Started / myG All Sessions (%)",
  "myG: Order Completed / myG All Sessions (%)",
  
  "myB: All Sessions",
  "myB: Orders Started",
  "myB: Orders Completed",
  "myB: Order Started / myB All Sessions (%)",
  "myB: Order Completed / myB All Sessions (%)" #,
  
  # "Avg Session Duration (seconds)"
)

getGAplot <- function(dateRange){
  myG_all_sessions <- request(c(min(dateRange), max(dateRange)), 'sessions', c('landingScreenName', 'date'), df_global,
                              df_add = list(dim_filter("landingScreenName", "PARTIAL", "myG")
                              ))
  myG_all_sessions <- cbind(myG_all_sessions, landingPage = "myG")
  
  myB_all_sessions <- request(c(min(dateRange), max(dateRange)), 'sessions', c('landingScreenName', 'date'), df_global,
                              df_add = list(dim_filter("landingScreenName", "PARTIAL", "myB")
                              ))
  myB_all_sessions <- cbind(myB_all_sessions, landingPage = "myB")
  all_sessions <- rbind(myG_all_sessions, myB_all_sessions) %>% tbl_df %>% group_by(date, landingPage) %>% summarize(sessions = sum(sessions))
  
  plot <- ggplot(all_sessions, aes(x = date, y = sessions, group = landingPage)) +
    geom_point(aes(color = landingPage)) +
    geom_line(size = 0.25) + theme_bw() + 
    geom_vline(xintercept = as.numeric(as.Date(dateRange[c(2,4)]))+1, linetype = 2, size = 0.25) + 
    geom_smooth(size = 0.25, aes(color = landingPage), se = F)
  ggplotly()
}