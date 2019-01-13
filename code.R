#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Your Name - Data Driven Securty                          #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping


### 1.1 Obtención de la página web

url <- "https://www.mediawiki.org/wiki/MediaWiki"

get_page <- function(url) {
  data <- httr::GET(url)
  content <- httr::content(data, as="text")
  xml <- xml2::read_html(content)

  return (xml)
}

### 1.2 Analisis de el contenido de la web

get_title <- function(xml) {
  title <- xml2::xml_text(xml2::xml_find_first(xml, "//head/title/text()"))

  return (title)
}

### 1.3.	Extracción de enlaces

get_links <- function(xml) {
  links <- xml2::xml_find_all(xml, "//a")

  return (links)
}

### 1.4 Exploración de enlaces

handle_links <- function(links) {
  mat <- matrix(ncol=6, nrow=length(links))

  i <- 1
  for (link in links) {
    mat[i,] <- handle_link(link)
    i <- i + 1
  }

  df <- data.frame(mat)
  names(df) <- c("href", "absolute_url", "title", "status_code", "is_absolute", "is_internal")

  return (df)
}

handle_link <- function(link) {
  href <- xml2::xml_attr(link, "href")
  text <- xml2::xml_text(link)
  absolute_url <- NA
  is_absolute <- NA
  is_internal <- NA

  status_code <- NA
  if (!is.na(href)) {
    absolute_url <- href
    is_absolute <- TRUE
    if (!startsWith(href, "https://") && !startsWith(href, "http://")) {
      absolute_url <- paste("https://www.mediawiki.org", href, collapse = "", sep = "")
      is_absolute <- FALSE
    }

    is_internal <- grepl("www.mediawiki.org", absolute_url)

    response <- httr::HEAD(absolute_url)
    status_code <- httr::status_code(response)
  }

  return (c(href, absolute_url, text, status_code, is_absolute, is_internal))
}

### Gráficos en R

### 2.1 Histograma

make_histogram <- function(df) {
  c <- ggplot2::ggplot(df, ggplot2::aes(href))
  c + ggplot2::geom_histogram(stat="count") + ggplot2::facet_grid(~is_absolute)
}

### 2.2 Un gráfico de barras

bar_plot <- function(df) {
  c <- ggplot2::ggplot(df, ggplot2::aes(is_internal))
  c + ggplot2::geom_bar()
}

### 2.3 Pie Chart

pie_chart <- function(df) {
  df <- df %>%
    group_by(status_code) %>%
    count() %>%
    ungroup() %>%
    mutate(per=`n` / sum(`n`)) %>%
    arrange(desc(status_code))

  df$label <- scales::percent(df$per)

  ggplot2::ggplot(data=df) +
    ggplot2::geom_bar(ggplot2::aes(x="", y=per, fill=status_code), stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::theme_void() +
    ggplot2::geom_text(ggplot2::aes(x=1, y=cumsum(per) - per / 2, label=label))
}
