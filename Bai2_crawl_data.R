library(rvest)
library(httr)

fetch_page_content <- function(url, timeout_seconds = 5) {
  tryCatch(
    {
      response <- GET(url, timeout(timeout_seconds))

      if (status_code(response) != 200) {
        stop("Failed to fetch the page. HTTP status: ", status_code(response))
      }
      return(content(response, as = "text", encoding = "UTF-8"))
    },
    error = function(e) {
      message("Error when fetching URL: ", url, "\n", e$message)
      return(NULL)
    }
  )
}

extract_article_urls <- function(page_content) {
  tryCatch(
    {
      page_html <- read_html(page_content)

      links <- page_html %>%
        html_elements("a") %>%
        html_attr("href")

      valid_links <- links[grepl("^https?://", links) & grepl("\\.html$", links)]
      valid_links <- unique(valid_links)
      return(valid_links)
    },
    error = function(e) {
      message("Error when extracting URLs: ", e$message)
      return(NULL)
    }
  )
}

get_all_urls <- function(base_url, max_urls) {
  collected_urls <- c()
  page_number <- 1

  while (is.null(max_urls) || length(collected_urls) < max_urls) {
    current_url <- ifelse(page_number == 1, base_url, paste0(base_url, "-p", page_number))
    message("Fetching page: ", current_url)

    page_content <- fetch_page_content(current_url)

    if (is.null(page_content)) {
      message("Failed to fetch page: ", current_url)
      break
    }

    article_urls <- extract_article_urls(page_content)

    if (is.null(article_urls) || length(article_urls) == 0) {
      message("No more URLs found on: ", current_url)
      break
    }

    new_urls <- setdiff(article_urls, collected_urls)
    collected_urls <- c(collected_urls, new_urls)

    if (!is.null(max_urls) && length(collected_urls) >= max_urls) {
      collected_urls <- collected_urls[1:max_urls]
      break
    }

    if (is.null(max_urls)) {
      break
    }

    page_number <- page_number + 1
  }
  return(collected_urls)
}

fetch_article_text <- function(url) {
  tryCatch(
    {
      page_content <- fetch_page_content(url)
      if (is.null(page_content)) {
        stop(paste("Failed to fetch content for URL:", url))
      }

      page_html <- read_html(page_content)

      article_paragraphs <- page_html %>%
        html_elements("p:not([style='text-align:right;'])") %>%
        html_text2()

      article_text <- paste(article_paragraphs, collapse = " ")
      article_text <- gsub('\\"', '', article_text)

      return(article_text)
    },
    error = function(e) {
      message("Error when fetching article content from URL: ", url, "\n", e$message)
      return(NULL)
    }
  )
}

combine_article_texts <- function(urls) {
  all_texts <- ""

  for (url in urls) {
    message("Fetching content from: ", url)
    article_text <- fetch_article_text(url)

    if (!is.null(article_text) && nchar(article_text) > 0) {
      all_texts <- paste(all_texts, article_text, sep = "\n")
    } else {
      message("Failed to fetch content or content is empty for URL: ", url)
    }
  }

  all_texts <- trimws(all_texts, which = "left")
  return(all_texts)
}

base_url_input <- readline(prompt = "Enter the base URL to fetch (leave blank for default): ")
if (base_url_input == "") {
  base_url_input <- "https://vnexpress.net/kinh-doanh/quoc-te"
}

max_urls_input <- readline(prompt = "Enter the number of URLs to fetch (leave blank for all): ")
if (max_urls_input == "") {
  max_urls <- NULL
} else {
  max_urls <- as.integer(max_urls_input)
}

article_urls <- get_all_urls(base_url_input, max_urls)
combined_article_text <- combine_article_texts(article_urls)

if (nchar(combined_article_text) > 0) {
  print(paste("Combined content from", length(article_urls), "articles"))
  writeLines(combined_article_text, "/home/c/Downloads/Truc quan hoa du lieu/combined_content.txt")
  print("Content saved to file.")
} else {
  print("No content could be fetched.")
}
