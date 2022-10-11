#' @export
downloadRunLog <- function(owner, repo, run_id, output_dir) {
  
  url <- sprintf("https://api.github.com/repos/%s/%s/actions/runs/%s/logs", owner, repo, run_id)
  
  req <- request(url) |>
    req_headers("Accept" = "application/vnd.github.v3+json") |>
    req_headers("Authorization" = paste("token", Sys.getenv("GITHUB_PAT")))
  
  resp <- req_perform(req) |>
    resp_body_raw()
  
  tf <- tempfile(fileext = ".zip")
  writeBin(resp, con = tf)
  unzip(zipfile = tf, exdir = output_dir)
  
}