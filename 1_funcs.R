###############################################################################################
###############################################################################################
#####################   FUNCS for Running Feedback Reports for RTLS data
#####################
###############################################################################################
###############################################################################################
library(lubridate)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(officer)
library(DBI)
library(network)
library(here)
library(Microsoft365R)

####################################################################################################
############################## DB connection (pg) and management
####################################################################################################

get_connection <- function(db_name, db_u, db_pw){
    con <- DBI::dbConnect(RPostgres::Postgres(),
                           dbname   = db_name,
                           host     = 'localhost',
                           port     = 5433,
                           user     = db_u,
                           password = db_pw)
    return(con)
}

get_sqlite_con <- function(db_loc, db_name){
    prgdir <- getwd()
    setwd(db_loc)
    # connect to RTSL database
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_name)
    setwd(prgdir)
    return(con)
}

check_receiver_overlap <- function(){
    jhh <- get_receiver_loc_data(
        con = get_connection(
        db_name = paste0('rtls_','jhh'),
        db_u = config$db_u,
        db_pw = config$db_pw),
        t_name = 'rtls_receivers')
    bmc <- get_receiver_loc_data(
        con = get_connection(
        db_name = paste0('rtls_','bmc'),
        db_u = config$db_u,
        db_pw = config$db_pw),
        t_name = 'rtls_receivers')
    old_jhh <- get_receiver_loc_data(
        con = get_sqlite_con(
        db_loc = config$db_loc,
        db_name = config$db_name),
        t_name = 'rtls_receivers')
    pg_jhh_bmc <- intersect(
        unique(jhh$receiver),
        unique(bmc$receiver)
    )
    old_new_jhh <- setdiff(
        unique(old_jhh$Receiver),
        unique(jhh$receiver)
    )
    return(list(pg_jhh_bmc_overlap = pg_jhh_bmc, old_not_in_new_jhh = old_new_jhh))
}

migrate_location_codes <- function(pg_con, sqlite_con){
  old_loc_codes <- get_receiver_loc_data(
      con = sqlite_con,
      t_name = 'RTLS_Receivers'
  )
  manual_receiver_update(
      df = old_loc_codes,
      con = pg_con
  )
  #DBI::dbDisconnect(pg_con)
  #DBI::dbDisconnect(sqlite_con)
  NULL
}

updateRTLSReceivers2 <- function(con,df) {
  all_rcvrs <- con |>
    tbl('rtls_receivers') |>
    collect()
  print(nrow(all_rcvrs))
  df <- df |>
    dplyr::select(Receiver,ReceiverName) |>
    rename(receiver = Receiver, receiver_name = ReceiverName) |>
    filter(!(receiver %in% unique(all_rcvrs$receiver)))
  print(nrow(df))
  DBI::dbAppendTable(con,'rtls_receivers',value = df)
}

push_rtls_to_db <- function(tmp_csv_path, archive_csv_path, db_u, db_pw) {
  # this is a rewrite of the python csv_to_db_pg; as it stopped working with sa update
  sites <- c('jhh','bmc')
  for (site in sites) {
    print(paste0('rtls_',site))
    con <- get_connection(
      db_name = paste0('rtls_',site),
      db_u = config$db_u,
      db_pw = config$db_pw)
    if (!DBI::dbExistsTable(con,'rtls_receivers')) {
      DBI::dbWriteTable(con,'rtls_receivers',
                        data.frame(receiver = integer(),
                                   receiver_name = character(),
                                   location_code = character()))
    }
    # Reads in all the csvs for a given site
    fnames <- list.files(tmp_csv_path,pattern = paste0('rtls_',site), full.names = TRUE)
    site_dfs <- fnames %>%
      purrr::map_dfr(read.csv,skip = 2,fileEncoding = "UTF-16")
    names(site_dfs) <- c('Badge','Initials','FirstName','LastName','Receiver',
                         'ReceiverName','BadgeTimeIn','BadgeTimeOut')
    updateRTLSReceivers2(con,site_dfs)
    
    site_dfs <- site_dfs |>
      drop_na() |>
      mutate(
        across(contains('Time'), ~ lubridate::as_datetime(.x))) |>
      rename(RTLS_ID = Badge, receiver = Receiver, time_in = BadgeTimeIn, time_out = BadgeTimeOut) |>
      dplyr::select(RTLS_ID,receiver,time_in,time_out) |>
      distinct()
    
    for (badge in unique(site_dfs$RTLS_ID)) {
      t_name <- paste0('table_',badge)
      print(t_name)
      if (!DBI::dbExistsTable(con,t_name)) {
        DBI::dbWriteTable(con,t_name,
                          data.frame(receiver = integer(),
                                     time_in = lubridate::POSIXct(),
                                     time_out = lubridate::POSIXct()))
      }
      data <- site_dfs |>
        filter(RTLS_ID == badge) |>
        dplyr::select(-RTLS_ID)
      print(nrow(data))
      DBI::dbAppendTable(con,t_name,data)
    }
  } # end of 'sites' for loop
  for (f in list.files(tmp_csv_path)) {
    fs::file_move(fs::path(tmp_csv_path,f),fs::path(archive_csv_path,f))
  }
}


###############################################################################################
#####################   Pulls data for specific badges and time range
###############################################################################################

get_active_badges2 <- function(badge_file) {
  badges <- readxl::read_xlsx(badge_file) |>
    filter(Active == 'Yes') |>
    mutate(RTLS_ID = as.integer(RTLS_ID))
  return(unique(badges$RTLS_ID))
}

get_RTLS_data <- function(badges, strt, stp, con) {
  # set up empty dataframe to store
  print("Pulling RTLS data...")
  all_data <- data.frame(
    receiver = integer(),
    time_in = .POSIXct(character()),
    time_out = .POSIXct(character()),
    badge = character()
  )
  # makes sure badges is a list (and not just one badge var)
  if (length(badges) > 1){ badges <- paste0('table_',badges)}
  else if (length(badges) == 1) {
    if (badges == 'all') {
      tbls <- DBI::dbListTables(con)
      badges <- grep('table_',tbls, value = TRUE)
  } else {badges <- as.list(paste0('table_',badges))}
    }
  # loops through each badge and returns data in timerange
  for (badge in badges) {
    if (DBI::dbExistsTable(conn = con, name = badge)) {
      # Read in data and filter by time
      badge_data <- con %>%
      tbl(badge) %>%
      collect() %>%
      mutate(across(c('time_in','time_out'), lubridate::ymd_hms)) %>%
      filter(time_in > strt & time_in < stp) # start and stop times are non-inclusive

      badge_data$badge <- as.integer(stringr::str_split(badge, '_')[[1]][2])
      all_data <- rbind(all_data, badge_data)
    } else {print(paste('No Table for ',badge,' ...'))}
  }
  return(all_data)
}

split_days <- function(row) {
  # new_time_out <- lubridate::date(row$time_in[[1]])
  # print(row)
  row <- as.data.frame(row)
  day_one <- list(
    'receiver' = row$receiver,#[[1]],
    "time_in" = row$time_in,
    "time_out" = NA,#row$time_out,#new_time_out,
    "badge" = row$badge,
    "receiver_recode" = row$receiver_recode,
    "receiver_name" = row$receiver_name,
    "duration" = NA #row$duration
  )
  day_two <- list(
    'receiver' = row$receiver,#[[1]],
    "time_in" = NA, #row$time_in,
    "time_out" = row$time_out,#new_time_out,
    "badge" = row$badge,
    "receiver_recode" = row$receiver_recode,
    "receiver_name" = row$receiver_name,
    "duration" = NA #row$duration
  )
  return(dplyr::bind_rows(list(day_one,day_two)))
}

locCodeBadges <- function(badge_data,db_u,db_pw,site) {
  # get receiver data
  con <- get_connection(
    db_name = paste0('rtls_',site),
    db_u = config$db_u,
    db_pw = config$db_pw)
  receivers <- con %>%
    tbl('rtls_receivers') %>%
    collect() %>%
    rename(receiver_recode = location_code)
  badge_data <- badge_data %>%
    left_join(receivers, by = 'receiver')
  return(badge_data)
}

getActiveBadges <- function(badge_file){
  b <- readxl::read_excel(badge_file) %>%
    filter(Active == 'Yes') 
  return(unique(b$RTLS_ID))
}

get_and_locCode_RTLS_data_pg <- function(badges, strt, stp, sites, use_rules) {
  
  print("Pulling RTLS data...")
  all_data <- data.frame()
  for (site in sites){
    print(paste0("...for ",site,":"))
    # set up empty dataframe to store site results
    site_data <- data.frame(
      receiver = integer(),
      time_in = .POSIXct(character()),
      time_out = .POSIXct(character()),
      badge = character(),
      receiver_recode = character(),
      receiver_name = character(),
      duration = numeric()
    )

    con <- get_connection(
      db_name = paste0('rtls_',site),
      db_u = config$db_u,
      db_pw = config$db_pw)

    # makes sure badges is a list (and not just one badge var)
    if (length(badges) > 1){ site_badges <- paste0('table_',badges)}
    else if (length(badges) == 1) {
      if (badges == 'all') {
        tbls <- DBI::dbListTables(con)
        site_badges <- grep('table_',tbls, value = TRUE)
    } else {site_badges <- as.list(paste0('table_',badges))}
    }

    # loops through each badge and returns data in timerange
    for (badge in site_badges) {
      print(badge)
      if (DBI::dbExistsTable(conn = con, name = badge)) {
        # Read in data and filter by time
        badge_data <- con %>%
          tbl(badge) %>%
          collect() %>%
          mutate(across(c('time_in','time_out'), lubridate::ymd_hms)) %>%
          filter(time_in > strt & time_in < stp) # start and stop times are non-inclusive
        if (!plyr::empty(df)) {
          badge_data$badge <- as.integer(stringr::str_split(badge, '_')[[1]][2])
          site_data <- rbind(site_data, badge_data)
        } else {print(paste('No data in range for',badge,'...'))}
      } else {print(paste('No Table for ',badge,' ...'))}
    } # End iterating through badges
    # location code all data
    print('Here 1')
    site_data <- locCodeBadges(
      badge_data = site_data,
      db_u = config$db_u,
      db_pw = config$db_pw,
      site = site) %>%
      mutate(site = site)
    print('Here 2')
    if (dim(all_data)[1] == 0) 
      { all_data <- data.frame(site_data)} # creates copy
    else { all_data <- dplyr::bind_rows(all_data,site_data)} # appends site data together
    
  } # End site looping
  print('Here 3')
  all_data$duration <- as.numeric(difftime(all_data$time_out,all_data$time_in,units = 'mins'))
  # applies locatio screening rules.
  #
  return(all_data)
}

# pulls all reciever location data... used for manual review and update
get_receiver_loc_data <- function(con, t_name) {
  receiver_data <- con %>%
    tbl(t_name) %>%
    collect()
  return(receiver_data)
}

manual_receiver_update <- function(df, con) {
  for (i in rownames(df)) {
    update_stmt <- paste0("UPDATE rtls_receivers ",
                       "SET location_code = ",paste0('\'',df[i,'location_code'],'\''),
                       " WHERE receiver = ",df[i,"receiver"],";")
    print(update_stmt)
    res <- DBI::dbExecute(con, update_stmt)
    print(res)
    #DBI::dbClearResult(res)
    #DBI::dbSendQuery(con, update_stmt)
  }
  NULL
}

get_weekly_report2 <- function(
    anchor_date,look_back_days,db_u,db_pw,target_badges,weekly_report_dir) {
  # set date variables
  rght_win <- anchor_date
  lft_win <- rght_win - as.difftime(look_back_days, unit="days")
  
  # Get all data for active badges
  dfs <- list()
  for (site in c('jhh','bmc')) {
    df <- get_RTLS_data(
      badges = target_badges,
      strt = lft_win,
      stp = rght_win,
      con = get_connection(
        db_name = paste0('rtls_',site),
        db_u = config$db_u,
        db_pw = config$db_pw)
    )
    dfs[[site]] <- df
  }
  all_data <- purrr::reduce(dfs,rbind) |>
    mutate(duration = as.numeric(time_out - time_in)/60) |>
    group_by(badge) |>
    summarise(tot_duration_minutes = sum(duration)) |>
    ungroup() |>
    arrange(tot_duration_minutes)
  
  fname <- glue::glue('IM_Badge_data_from_{lft_win}_to_{rght_win}_runOn{Sys.Date()}.csv')
  print(fname)
  write.csv(all_data,here::here(weekly_report_dir,fname))
  print(glue::glue("Of the {length(target_badges)} active badges, {nrow(all_data)} had data between {lft_win} and {rght_win}"))
  print(glue::glue('These active badges did not have data: {setdiff(target_badges,unique(all_data$badge))}'))
  
  return(all_data)
}


###############################################################################################
#####################   Creates plots for reports
###############################################################################################

make_overall_bar <- function(df, badge_num){

  df$receiver_recode <- factor(df$receiver_recode,
                        levels = c('Patient room','MD Workroom','Ward Hall','Education','Supply and admin','Transit','OTHER/UNKNOWN','Family waiting space'))
  df <- df %>% transform(
    receiver_recode=plyr::revalue(receiver_recode,c("OTHER/UNKNOWN"="Other",'Family waiting space'='Family space')))
  ### structure data
  overall_summary <- df %>% group_by(receiver_recode) %>%
    summarise(total_time = sum(duration,na.rm = TRUE)) %>%
    mutate(proportion_time = (total_time / sum(total_time, na.rm = TRUE)) * 100 )
  overall_summary$Source <- 'All badges'
  badge_summary <- df %>% filter(badge == badge_num) %>%
    group_by(receiver_recode) %>%
    summarize(total_time = sum(duration, na.rm = TRUE)) %>%
    mutate(proportion_time = (total_time / sum(total_time, na.rm = TRUE)) * 100 )
  badge_summary$Source <- paste('Badge',badge_num)
  summary <- rbind(overall_summary,badge_summary) %>% drop_na(receiver_recode)

  ## overall figure
  summary_fig <- summary %>% ggplot() +
    aes(x = receiver_recode, fill = Source, weight = proportion_time) +
    geom_bar(position = "dodge") +#, fill = "#0c4c8a") +
    scale_fill_viridis(discrete = TRUE, option = 'C') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust=0.95), legend.position=c(.85,1), legend.title = element_blank()) +
    labs(
      title = 'Proportion of time spent in locations',
      subtitle = paste('From',lubridate::date(min(df$time_in)),'to',lubridate::date(max(df$time_out))),
      x = 'Locations',
      y = 'Proportion of time'
    ) + scale_y_continuous(labels = scales::percent_format(scale = 1))

  return(summary_fig)
}

make_area_plot <- function(df, perc, badge) {

  if (is.null(badge)) {
    source_title_str <- 'all badges'
  } else {
    source_title_str <- paste('badge',badge)
  }

  df$location <- factor(df$location,
                        levels = c('Patient room','MD Workroom','Ward Hall','Education','Supply and admin','Transit','OTHER/UNKNOWN','Family waiting space'))
  df <- df %>% transform(
            location=plyr::revalue(location,c("OTHER/UNKNOWN"="Other",'Family waiting space'='Family space')))
  #inserts 0 for all missing location by hour combintions to avoid gaps on area chart
  combinations <- expand.grid(hour = unique(df$hour), location = unique(df$location))
  df <- df %>%
    full_join(combinations, by = c('hour' = 'hour','location' = 'location')) %>%
    mutate(duration = ifelse(is.na(duration), 0, duration)) %>%
    arrange(hour, location)
  if (perc) {
    plt <- df %>%
      group_by(hour, location) %>%
      summarise(n = sum(duration)) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ggplot(
        aes(x=hour, y = percentage, fill = location)
      )
    plt <- plt + scale_y_continuous(labels = scales::percent_format(scale = 1))
    metric_title_string <- "Percent"
    y_lab_text <- "Percentage of time"
  } else {
  plt <- df %>% ggplot(
         aes(x=hour, y=duration,fill=location))
  metric_title_string <- "Total"
  y_lab_text <- "Minutes"
  }
  plt <- plt +
    geom_area(colour="white") + #alpha=0.6 , size=.5,
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    #theme_ipsum() +
    theme_light() +
    labs(
      title = paste(metric_title_string,"Time Spent in Locations"),
      # subtitle = paste('For',source_title_str,'from',lubridate::date(min(df$Time_In)),'to',lubridate::date(max(df$Time_Out))),
      y = y_lab_text,
      x = 'Hour of the day'
    ) +
    scale_x_continuous(breaks = seq(0, 23, by = 4))
  return(plt)
}

###############################################################################################
#####################   Functions for creating feedback reports
###############################################################################################

### Creates one feedback report

create_FB_reports <- function(df, FB_report_dir, save_badge_timeline, min_hours_for_fb) {
  # Notes: this currently does not account for small amounts of time a resident may spend at more than
  # one site per day
  print(nrow(df))
  sites <- unique(df$site)
  for (s in sites) {
    print(s)
    df_site <- df %>%
      dplyr::filter(site == s) %>%
      dplyr::select(c(-site))
    print(nrow(df_site))
    df_site <- df_site %>%
      group_by(badge) %>%
      dplyr::filter(sum(duration) > (min_hours_for_fb * 60)) %>%
      ungroup()
    print(nrow(df_site))
    # make reports
    for (badge in unique(df_site$badge)){
      overall_bar <- make_overall_bar(df = df_site, badge_num = badge)
      ind_area_norm <- make_area_plot(
        df = make_timeseries_df_for_dummies(df = df_site[which(df_site$badge == badge), ]),#,f = 'S'),
        perc = TRUE, #if true, will create porportional chart ,if false will do raw
        badge = badge # if NULL this assumes a summary plot; if an int it will use that in plot titles
      )
      ind_area_raw <- make_area_plot(
        df = make_timeseries_df_for_dummies(df = df_site[which(df_site$badge == badge), ]),#,f = 'S'),
        perc = FALSE, #if true, will create porportional chart ,if false will do raw
        badge = badge # if NULL this assumes a summary plot; if an int it will use that in plot titles
      )
      tot_hours <-sum(df_site[which(df_site$badge == badge),'duration']) / 60
      tot_hours <- round(tot_hours,digits = 1)
      area_plots <- ind_area_raw / ind_area_norm + plot_layout(guides = 'collect') & theme(legend.position='bottom')
      report <- officer::read_docx(path = config$FB_report_template) %>%
        body_add_par("Time in Location Data Report", style = "Title") %>%
        body_add_par(paste("Badge:",toString(badge)), style = 'Normal') %>%
        body_add_par(paste("From:",lubridate::date(min(df_site$time_in)),"to",lubridate::date(max(df_site$time_out))), style = 'Normal') %>%
        body_add_par(paste("Total hours in this report:",toString(tot_hours)), style = 'Normal') %>%
        body_add_par('') %>%
        body_add_par(config$FB_intro_para) %>%
        body_add_par('') %>%
        body_add_par("Where does the data come from?", style = 'Subtitle') %>%
        body_add_par(config$FB_where_para, style = 'Normal') %>%
        body_add_par('') %>%
        body_add_par('What am I supposed to do with this data?', style = 'Subtitle') %>%
        body_add_par(config$FB_what_para, style = 'Normal') %>%
        body_add_break(pos = "after") %>%
        body_add_par("Where you spend your time compared to your peers", style = 'Subtitle') %>%
        body_add_par('') %>%
        body_add_gg(overall_bar) %>%
        body_add_break(pos = "after") %>%
        body_add_par("Where you spend your time throughout the day", style = 'Subtitle') %>%
        body_add_par('') %>%
        body_add_gg(area_plots) %>%
        print(target = here(getwd(),FB_report_dir,s,paste0(toString(badge),'.docx')))
      if (save_badge_timeline == TRUE) {
        write.csv(df_site[which(df_site$badge == badge), ],here(getwd(),FB_report_dir,s,paste0(toString(badge),'_timeline.csv')))
      }
    }
  } # end looping through sites
}

###############################################################################################
#####################   Functions for network metrics and visualization
###############################################################################################

prep_net_data <- function(df) {

  # This funciton takes an RTLS df and creates files for:
  #   Edges
  #   Nodes
  nodes <- df %>%
    group_by(receiver) %>%
    summarize(duration = sum(duration)) %>%
    ungroup() %>%
    mutate(id = row_number() - 1) %>%
    left_join(
      distinct(df,receiver,receiver_recode, receiver_name),
      by = "receiver"
    ) %>%
    rename(rec_num = receiver,
           type = receiver_recode,
           description = receiver_name) %>%
    arrange(desc(duration))

  #adding back
  distinct(df,receiver,receiver_recode)

  df <- relabel_nodes(df,nodes) # this just recodes the reciever id to the 'id' var from above; why is that so hard in R?

  edges <- df %>%
    arrange(time_in) %>% # makes sure rows are ordered in ascending time order
    mutate(to = lead(receiver)) %>% # creates new colum for destination link based on shifted Receiver column
    na.omit() %>% # drops last row of NA created by shifting
    rename(from = receiver)  %>% # renames Receiver column to the 'from' end of edge
    group_by(from, to) %>%
    summarize(weight = n()) %>%
    ungroup()

  return(list('nodes' = nodes, 'edges' = edges))
}

###############################################################################################
#####################   Functions for automating data upload from email
###############################################################################################

get_files_from_outlk <- function(outlk_sess, n) {
  # Go to the folder for RTLS data
  folder <- outlk_sess$get_folder('RTLS_Data')
  tst_list <- folder$list_emails(n=n)

  for (em in tst_list) {
    # Check source of data
    if (grepl("BMC_RTLS-Reports",em$properties$sender$emailAddress$name, fixed = TRUE)) {
      print('Bayview')
      site <- "bmc"
    } else if (grepl("RTLSDB-Alerts", em$properties$sender$emailAddress$name, fixed = TRUE)) {
      print('jhh')
      site <- "jhh"
    } else if (grepl("Versus Reports", em$properties$sender$emailAddress$name, fixed = TRUE)) {
      site <- "battery"
      print(site)
    } else {
      site <- NA
      print('huh?')
    }

    #test file type
    if (grepl('.csv',em$list_attachments()[[1]]$properties$name,fixed = TRUE)){
      print('csv')
      kind <- '.csv'
      #em$list_attachments()[[1]]$download(dest = here('test.csv'),overwrite = TRUE)
    } else if (grepl('.pdf',em$list_attachments()[[1]]$properties$name,fixed = TRUE)){
      print('pdf')
      kind <- '.pdf'
    } else {
      kind <- NA
    }

    if (!is.na(site) & !is.na(kind)) {
      # Download attachemnt
      f <- paste0('rtls_',site,'_',lubridate::date(em$properties$sentDateTime),kind)
      em$list_attachments()[[1]]$download(dest = here('Data/RTLS_Data/tmp',f),overwrite = TRUE)
      # move email to archive folder
      em$move(dest = folder$get_folder('RTLS_archive'))
    }
  }
  NULL
}

###############################################################################################
#####################   Functions for replicating JAMA Open paper
###############################################################################################

clean_timelines <- function(df) {
  # Clean timelines
  df <- df %>%
    mutate(
      receiver_recode = as.character(receiver_recode)
    ) %>%
    filter(date(time_in) < date(time_out)) %>%
    apply(1,split_days) %>%
    dplyr::bind_rows() %>%
    mutate(
      time_in = as.POSIXct(time_in,origin = "1970-01-01", tz = "America/New_York"),
      time_out = as.POSIXct(time_out,origin = "1970-01-01", tz = "America/New_York")
    ) %>%
    mutate(
      time_in = if_else(is.na(time_in),
                        as.POSIXct(paste(as.character(lubridate::date(time_out)),"00:00:00"), format = "%Y-%m-%d %H:%M:%S"),
                        time_in),
      time_out = if_else(is.na(time_out),
                         as.POSIXct(paste(as.character(lubridate::date(time_in)),"23:59:59"), format = "%Y-%m-%d %H:%M:%S"),
                         time_out),
    ) %>%
    mutate(
      duration = as.numeric(difftime(time_out,time_in, units = "mins"))
    ) %>%
    full_join(df[which(date(df$time_in) == date(df$time_out)),]) %>%
    filter(if_any(everything(), ~ !is.na(.)))
  return(df)
}

get_loc_summaries <- function(df) {
  df <- df %>% mutate(date = as.Date(time_in)) %>%
    dplyr::select(badge,receiver_recode, date, duration) %>%
    group_by(badge, date, receiver_recode) %>%
    summarise(duration = sum(duration)) %>%
    ungroup() %>%
    filter(!is.na(receiver_recode)) %>% 
    pivot_wider(
      id_cols = c(badge,date),
      names_from = receiver_recode,
      values_from = duration
    ) %>%
    mutate(
      total = rowSums(.[-c(1:2)],na.rm=TRUE)
    ) %>%
    janitor::clean_names() %>%
    rowwise() %>%
    mutate(
      transit_perc = transit / total,
      ward_hall_perc = ward_hall / total,
      education_perc = education / total,
      family_waiting_space_perc = family_waiting_space / total,
      md_workroom_perc = md_workroom / total,
      patient_room_perc = patient_room / total,
      supply_and_admin_perc = supply_and_admin / total,
      other_unknown_perc = other_unknown / total
    ) %>% ungroup()
  return(df)
}

get_tot_sum <- function(df) {

  daily_sum <- get_loc_summaries(df) %>%
    mutate(interval = 'all_24') %>%
    filter(total >= 4*60)

  morning_df <- df %>%
    mutate(
      time_out = case_when(
        (hour(time_in) <= 12) & (hour(time_out) >= 12) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"12:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        (hour(time_in) <= 6) & (hour(time_out) >= 6) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"06:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      )
    ) %>%
    filter((hour(time_in) >= 6) & (hour(time_out) <= 12)) %>%
    get_loc_summaries(.) %>%
    mutate(interval = 'morning')  %>%
    filter(total >= 4*60)
  
  afternoon_df <- df %>%
    # filter((hour(time_in) <= 12) & (hour(time_out) >= 12))
    mutate(
      time_out = case_when(
        (hour(time_in) <= 18) & (hour(time_out) >= 18) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"18:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        (hour(time_in) <= 12) & (hour(time_out) >= 12) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"12:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      )
    ) %>%
    filter((hour(time_in) >= 12) & (hour(time_out) <= 18)) %>%
    get_loc_summaries(.) %>%
    mutate(interval = 'afternoon')  %>%
    filter(total >= 4*60)
  
  evening_df <- df %>%
    # filter((hour(time_in) <= 12) & (hour(time_out) >= 12))
    mutate(
      time_out = case_when(
        (hour(time_in) >= 18) & (hour(time_out) < hour(time_in)) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"23:59:59"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        (hour(time_in) <= 18) & (hour(time_out) >= 18) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"18:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      )
    ) %>%
    filter((hour(time_in) >= 18) & (hour(time_out) <= 23)) %>%
    get_loc_summaries(.) %>%
    mutate(interval = 'evening')  %>%
    filter(total >= 4*60)
  
  night_df <- df %>%
    # filter((hour(time_in) <= 12) & (hour(time_out) >= 12))
    mutate(
      time_out = case_when(
        (hour(time_in) <= 6) & (hour(time_out) >= 6) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"06:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      )
    ) %>%
    filter((hour(time_in) >= 0) & (hour(time_out) <= 6)) %>%
    get_loc_summaries(.) %>%
    mutate(interval = 'night')  %>%
    filter(total >= 4*60)
  
  rounds_df <- df %>%
    mutate(
      time_out = case_when(
        ((hour(time_in) <= 11) & (hour(time_out) >= 11)) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"11:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        ( ((hour(time_in)*60 + minute(time_in)) <= (8*60+30))  & ( ( hour(time_out)*60 + minute(time_out) ) <= (8*60+30) )) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"8:30:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      ),
      interval = 'rounds'
    ) %>%
    filter( (hour(time_in)*60 + minute(time_in)) <= (8*60+30) & (hour(time_out) <= 11)) %>%
    get_loc_summaries(.) %>%
    mutate(interval = 'rounds')  %>%
    filter(total >= 2.5*60)
  
  tot_sum <- bind_rows(
    daily_sum, morning_df, afternoon_df, evening_df, night_df, rounds_df)
  
  return(tot_sum)
}

###############################################################################################
#####################   Functions for workflow metrics
###############################################################################################


getEntropy <- function(g) {
  # creates a uniform time series by second
  # calculates shonnon entropy based on vector of receivers (not location categories)
  data <- g %>% 
    dplyr::select(-c(time_out,duration,receiver_name,receiver_recode,site)) %>%
    mutate(
      time_in = floor_date(time_in, unit = "second"))
  e <- data_frame(
    'time_in' = seq(min(g$time_in), by = 'sec', length.out = difftime(max(g$time_out), min(g$time_in), units = 'secs'))) %>%
    left_join(data, by = 'time_in', multiple = 'all') %>%
    fill(receiver, .direction = 'down') %>%
    dplyr::select(receiver) %>%
    drop_na() %>%
    table() %>%
    DescTools::Entropy()
  print(e)
  return(e)
}

getEntropy_sfly <- purrr::safely(getEntropy, otherwise = NA)

getBurstiness <- function(g) {
  # creates a uniform time series by second
  # identifies each second as being a 'transition' (change in location) or not, (1 or 0 respectively)
  # generates fano factor (sd of ts / mean of ts)
  data <- g %>% 
    dplyr::select(-c(time_out,duration,receiver_name,receiver_recode,site)) %>%
    mutate(
      time_in = floor_date(time_in, unit = "second"))
  b <- data_frame(
    'time_in' = seq(min(g$time_in), by = 'sec', length.out = difftime(max(g$time_out), min(g$time_in), units = 'secs'))) %>%
    left_join(data, by = 'time_in', multiple = 'all') %>%
    fill(receiver, .direction = 'down') %>%
    mutate(
      transition = if_else(receiver == lag(receiver), 0, 1)  # 1 if receiver is different than preceding receiver, 0 otherwise
    ) %>%
    dplyr::select(transition) %>%
    drop_na()
  ff <- var(b$transition) /  mean(b$transition) # fano factor
  print(ff)
  return(ff)
}

getBurstiness_sfly <- purrr::safely(.f = getBurstiness, otherwise = NA)

get_workflow_metrics_by_interval <- function(df){
  
  daily_sum <- df %>%
    mutate(d = date(time_in)) |>
    group_by(badge, d) |>
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    ) %>% ungroup() |>
    mutate(interval = 'all_24')
  
  morning_df <- df %>%
    mutate(
      time_out = case_when(
        (hour(time_in) <= 12) & (hour(time_out) >= 12) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"12:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        (hour(time_in) <= 6) & (hour(time_out) >= 6) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"06:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      )
    ) %>%
    filter((hour(time_in) >= 6) & (hour(time_out) <= 12)) %>%
    mutate(d = date(time_in)) |>
    group_by(badge, d) |>
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    ) %>% ungroup() |>
    mutate(interval = 'morning')
  
  afternoon_df <- df %>%
    # filter((hour(time_in) <= 12) & (hour(time_out) >= 12))
    mutate(
      time_out = case_when(
        (hour(time_in) <= 18) & (hour(time_out) >= 18) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"18:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        (hour(time_in) <= 12) & (hour(time_out) >= 12) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"12:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      )
    ) %>%
    filter((hour(time_in) >= 12) & (hour(time_out) <= 18)) %>%
    mutate(d = date(time_in)) |>
    group_by(badge, d) |>
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    ) %>% ungroup() |>
    mutate(interval = 'afternoon')
  
  evening_df <- df %>%
    mutate(
      time_out = case_when(
        (hour(time_in) >= 18) & (hour(time_out) < hour(time_in)) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"23:59:59"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        (hour(time_in) <= 18) & (hour(time_out) >= 18) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"18:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      )
    ) %>%
    filter((hour(time_in) >= 18) & (hour(time_out) <= 23)) %>%
    mutate(d = date(time_in)) |>
    group_by(badge, d) |>
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    ) %>% ungroup() |>
    mutate(interval = 'evening')
  
  night_df <- df %>%
    mutate(
      time_out = case_when(
        (hour(time_in) <= 6) & (hour(time_out) >= 6) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"06:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      )
    ) %>%
    filter((hour(time_in) >= 0) & (hour(time_out) <= 6)) %>%
    mutate(d = date(time_in)) |>
    group_by(badge, d) |>
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    ) %>% ungroup() |>
    mutate(interval = 'night')
  
  rounds_df <- df %>%
    mutate(
      time_out = case_when(
        ((hour(time_in) <= 11) & (hour(time_out) >= 11)) ~ as.POSIXct(paste(as.character(lubridate::date(time_out)),"11:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_out
      ),
      time_in = case_when(
        ( ((hour(time_in)*60 + minute(time_in)) <= (8*60+30))  & ( ( hour(time_out)*60 + minute(time_out) ) <= (8*60+30) )) ~ as.POSIXct(paste(as.character(lubridate::date(time_in)),"8:30:00"), format = "%Y-%m-%d %H:%M:%S"),
        .default = time_in
      ),
      interval = 'rounds'
    ) %>%
    filter( (hour(time_in)*60 + minute(time_in)) <= (8*60+30) & (hour(time_out) <= 11)) %>%
    mutate(d = date(time_in)) |>
    group_by(badge, d) |>
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    ) %>% ungroup() |>
    mutate(interval = 'rounds')
  
  tot_sum <- bind_rows(
    daily_sum, morning_df, afternoon_df, evening_df, night_df, rounds_df)
  
  return(tot_sum)
}

get_workflow_metrics <- function(df){
  
  wrkflw_df <- df %>%
    mutate(d = date(time_in)) %>%
    group_by(badge, d) %>%
    summarize(
      duration = difftime(max(time_out), min(time_in), units = 'secs'),
      min_in = min(time_in),
      max_out = max(time_out),
      entropy = getEntropy_sfly(pick(everything()))$result, 
      burstiness = getBurstiness_sfly(pick(everything()))$result,
      .groups = 'keep'
    )
  
  
  
  
  return(wrkflw_df)
}
###############################################################################################
#####################   Functions adapted from JAMA Open analysis
###############################################################################################

do_cleaning <- function(df) {
  df$month <- month(df$date)
  #formatting vars
  fac_vars <- c('rot_cat','interval','badge','month','site')
  for (var in fac_vars) {
    df[[var]] <- as.factor(df[[var]])
  }
  df$interval <- ordered(df$interval, levels = c('all_24', 'rounds', 'morning', 'afternoon', 'evening', 'night'))
  df$month <- ordered(df$month, levels = c(7,8,9,10,11,12,1,2,3,4,5,6))
  to_min_vars <- c('md_workroom', 'supply_and_admin','patient_room', 'education', 'other_unknown',
                   'family_waiting_space','transit', 'ward_hall', 'total')
  print(nrow(df))
  # df <- df %>% mutate(across(to_min_vars, ~.x/60))
  to_replace_na_vars <- append(
    to_min_vars,
    c('md_workroom_perc', 'supply_and_admin_perc','patient_room_perc', 'education_perc',
      'other_unknown_perc', 'family_waiting_space_perc','transit_perc', 'ward_hall_perc')
  )
  # exclusions
  big_intervals <- c('all_24', 'morning', 'afternoon', 'evening', 'night')
  print(nrow(df))
  df <- df %>%
    filter(
      (interval %in% big_intervals & total >= 240) | (interval == 'rounds' & total >= 60)
    ) %>%
    mutate(
      across(all_of(to_replace_na_vars), ~tidyr::replace_na(.x, 0))
    )

  df <- df %>%
    rowwise() %>%
    mutate(
      day_of_week = wday(date,week_start = 1) - 1, # Monday = 0
      week_day = if_else(day_of_week < 5, TRUE, FALSE),
      day_of_year = case_when(
        ((date >= lubridate::ymd('2020-07-01')) & (date <= lubridate::ymd('2021-06-30'))) ~ difftime(date,lubridate::ymd('2020-07-01'), units = c('days')),
        ((date >= lubridate::ymd('2021-07-01')) & (date <= lubridate::ymd('2022-06-30'))) ~ difftime(date,lubridate::ymd('2021-07-01'), units = c('days')),
        ((date >= lubridate::ymd('2022-07-01')) & (date <= lubridate::ymd('2023-06-30'))) ~ difftime(date,lubridate::ymd('2022-07-01'), units = c('days')),
        .default = NA
      )
    ) %>% ungroup()

  df <- df %>%
    drop_na(local_id,rotation) %>%
  filter(rot_cat != 'off' | rot_cat != 'other' | rot_cat != 'ambulatory') %>%
  mutate(rot_cat = case_match(rot_cat,
                              c('Subspecialty','subspecialty') ~ 'subspecialty',
                              .default = rot_cat))
  return(df)
}

# locations <- c('md_workroom', 'supply_and_admin','patient_room', 'education', 'other_unknown', 
#                'family_waiting_space','transit', 'ward_hall')
# locations_perc <- paste0(locations, '_perc')
# cmb_df4[,append(c('interval','total'),locations_perc)] %>% 
#   drop_na() %>%
#   gtsummary::tbl_summary(by = interval)
  
make_tables <- function(df) {
  # this isn't working; abandoned for gtsummary
  locations <- c('md_workroom', 'supply_and_admin','patient_room', 'education', 'other_unknown', 
                 'family_waiting_space','transit', 'ward_hall', 'total')
  df_sub <- df[,append(c('interval','day_of_week'),locations)]
  print(nrow(df_sub))
  # Weekdays & Weekends together
  stargazer::stargazer(df_sub[df_sub$interval == 'all_24',], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Overall_OverallTable.doc"))
  stargazer::stargazer(df_sub[df_sub$interval == 'morning',], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Overall_morning_Table.doc"))
  stargazer::stargazer(df_sub[df_sub$interval == 'afternoon',], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Overall_afternoon_Table.doc"))
  stargazer::stargazer(df_sub[df_sub$interval == 'evening',], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Overall_evening_Table.doc"))
  stargazer::stargazer(df_sub[df_sub$interval == 'night',], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Overall_night_Table.doc"))
  stargazer::stargazer(df_sub[df_sub$interval == 'rounds',], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Overall_rounds_Table.doc"))
  
  # Weekdays only
  stargazer::stargazer(df_sub[(df_sub$interval == 'all_24') & (df_sub$day_of_week < 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekdays_OverallTable.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'morning') & (df_sub$day_of_week < 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekdays_morning_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'afternoon') & (df_sub$day_of_week < 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekdays_afternoon_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'evening') & (df_sub$day_of_week < 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekdays_evening_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'night') & (df_sub$day_of_week < 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekdays_night_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'rounds') & (df_sub$day_of_week < 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekdays_rounds_Table.doc"))
  
  # Weekends only
  
  stargazer::stargazer(df_sub[(df_sub$interval == 'all_24') & (df_sub$day_of_week >= 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekend_OverallTable.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'morning') & (df_sub$day_of_week >= 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekend_morning_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'afternoon') & (df_sub$day_of_week >= 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekend_afternoon_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'evening') & (df_sub$day_of_week >= 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekend_evening_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'night') & (df_sub$day_of_week >= 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekend_night_Table.doc"))
  stargazer::stargazer(df_sub[(df_sub$interval == 'rounds') & (df_sub$day_of_week >= 5),], type = 'html',
            digits = 1,
            out = here::here('2023_output','tables',"Weekend_rounds_Table.doc"))
  NULL
}
