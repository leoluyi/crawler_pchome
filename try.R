library(httr)
library(dplyr)
source("utils/saveHTML.r") # saveHTML()


# try ---------------------------------------------------------------------

## GET
url_sitemap <- "http://ecapi.pchome.com.tw/ecshop/cateapi/v1.5/sitemap&fields=Id,Name,Sort,Nodes&_callback=sitemapCategory"
res_sitemap <- GET(url_sitemap,
                   user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36"),
                   add_headers(referer = "http://shopping.pchome.com.tw/sitemap/"))

## parse
res_json <- content(res_sitemap, as="text",encoding = "UTF-8")
res_json <- sub("^try[{]sitemapCategory[(]", "", res_json)
res_json <- sub("\\);\\}catch\\(e\\)\\{if\\(window\\.console\\)\\{console\\.log\\(e\\);\\}\\}$", "", res_json)
json_data <- jsonlite::fromJSON(res_json)
names(json_data$Nodes) <- json_data$Name
sitemap <- dplyr::bind_rows(json_data$Nodes, .id = "category")
sitemap <- sitemap %>%
  dplyr::select(-Sort) %>%
  mutate(shop = ifelse(stringr::str_extract(Id, "^.") == "D", "24h", "mall")) %>%
  mutate(link = sprintf("http://%s.pchome.com.tw/region/%s", shop, Id)) %>%
  mutate(Name = gsub("\\s+|　+", "", Name)) %>%
  rename(Id_1 = Id)
sitemap


# class_side --------------------------------------------------------------------

class_side_ls <- lapply(sitemap$Id, function(x) {
  # x <- "DHAA"
  url <- sprintf("http://ecapi.pchome.com.tw/cdn/ecshop/cateapi/v1.5/region/%s/menu&_callback=jsonp_nemu", x)
  res <- GET(url,
      user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36"),
      add_headers(referer = "http://shopping.pchome.com.tw/sitemap/"))
  res_json <- content(res, as="text",encoding = "UTF-8")
  res_json <- sub("^try\\{jsonp_nemu\\(", "", res_json)
  res_json <- sub("\\);\\}catch\\(e\\)\\{if\\(window\\.console\\)\\{console\\.log\\(e\\);\\}\\}$", "", res_json)
  json_data <- jsonlite::fromJSON(res_json)
  names(json_data$Nodes) <- json_data$Name
  result <- dplyr::bind_rows(json_data$Nodes, .id = "class_side") %>% dplyr::select(-Sort)
  result
})

names(class_side_ls) <- sitemap$Id
class_side_df <- class_side_ls %>%
  dplyr::bind_rows(., .id = "Id_1") %>%
  rename(Id_2 = Id)

class_side_df

# join --------------------------------------------------------------------

# save(sitemap, class_side_df, file="result/result.RData")

left_join(class_side_df, sitemap, by="Id_1") %>%
  readr::write_csv(., "result/pchome_result.csv")


# item ----------------------------------------------------------------------

item_ls <- lapply(class_side_df$Id_2, function(x) {
  # x <- "DGBS0V"
  url <- sprintf("http://ecapi.pchome.com.tw/ecshop/prodapi/v2/store/%s/prod&offset=0&limit=3000&_callback=top_prod", x)
  res <- GET(url,
             user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36"),
             add_headers(referer = "http://shopping.pchome.com.tw/sitemap/"))
  res_json <- content(res, as="text",encoding = "UTF-8")
  res_json <- sub("^try\\{top_prod\\(", "", res_json)
  res_json <- sub("\\);\\}catch\\(e\\)\\{if\\(window\\.console\\)\\{console\\.log\\(e\\);\\}\\}$", "", res_json)
  json_data <- jsonlite::fromJSON(res_json)

  json_data <- do.call(cbind, unclass(json_data))
  if (!is.list(json_data)) return(NULL)

  json_data <- json_data %>% as_data_frame() # eliminate data.frame
  json_data <- json_data %>% mutate(Nick = gsub("<.*?>", "", Nick)) # remove html tag
  json_data
})


names(item_ls) <- class_side_df$Id_2
class_side_df <- item_ls %>%
  dplyr::bind_rows(., .id = "Id_2") %>%
  rename(Id_2 = Id)




