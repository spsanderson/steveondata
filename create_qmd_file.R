create_qmd_file <- function(filename = "index.qmd", title = "", date = Sys.Date(), 
                            categories = c("code", "rtip"), 
                            keywords = c("Programming")) {
  # Define the base path
  base_path <- paste0(getwd(),"/posts")
  
  # Convert date to string and create the full directory path
  date_str <- as.character(date)
  full_path <- file.path(base_path, date_str)
  
  # Create the directory if it doesn't exist
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    message("Directory created: ", full_path)
  }
  
  # Define the full file path
  file_path <- file.path(full_path, filename)
  
  # Create the content to be written in the file
  content <- paste0(
    "---\n",
    'title: "', title, '"\n',
    'author: "Steven P. Sanderson II, MPH"\n',
    'date: "', date_str, '"\n',
    "categories: [", paste(categories, collapse = ", "), "]\n",
    "toc: TRUE\n",
    "description: ''\n",
    "keywords: [", paste(keywords, collapse = ", "), "]\n",
    "---\n\n",
    'You can connect with me at any one of the below:\n\n',
    'Telegram Channel here: [https://t.me/steveondata](https://t.me/steveondata)\n\n',
    'LinkedIn Network here: [https://www.linkedin.com/in/spsanderson/](https://www.linkedin.com/in/spsanderson/)\n\n',
    'Mastadon Social here: [https://mstdn.social/@stevensanderson](https://mstdn.social/@stevensanderson)\n\n',
    'RStats Network here: [https://rstats.me/@spsanderson](https://rstats.me/@spsanderson)\n\n',
    '<script src="https://giscus.app/client.js"\n',
    '        data-repo="spsanderson/steveondata"\n',
    '        data-repo-id="R_kgDOIIxnLw"\n',
    '        data-category="Comments"\n',
    '        data-category-id="DIC_kwDOIIxnL84ChTk8"\n',
    '        data-mapping="url"\n',
    '        data-strict="0"\n',
    '        data-reactions-enabled="1"\n',
    '        data-emit-metadata="0"\n',
    '        data-input-position="top"\n',
    '        data-theme="dark"\n',
    '        data-lang="en"\n',
    '        data-loading="lazy"\n',
    '        crossorigin="anonymous"\n',
    '        async>\n',
    '</script>\n'
  )
  
  # Write the content to the file
  writeLines(content, file_path)
  message("QMD file created: ", file_path, " - Opening File.")
  file.edit(file_path)
}