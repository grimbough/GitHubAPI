#' @importFrom stringr str_match
npages <- function(response) {
  if(resp_header_exists(response, header = "link")) {
    link_header <- resp_header(response, header = "link")
    res <- stringr::str_match(string = link_header, pattern = "page=([0-9]+)>; rel=\"last\"")[1,2] |>
      as.integer()
  } else {
    res <- 1L
  }
  return(res)
}

#' @import httr2
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows as_tibble
#' @export
listArtifacts <- function(owner, repo, run_id) {
  if(missing(run_id)) {
    res <- listArtifactsForRepo(owner, repo)
  } else {
    res <- listArtifactsForWorkflow(owner, repo, run_id)
  }
  return(res)
}

listArtifactsForWorkflow <- function(owner, repo, run_id) {
  url <- paste("https://api.github.com/repos", owner, repo, "actions/runs", run_id, "artifacts", sep = "/")
  res <- .listArtifactsCore(url)
  return(res)
}

listArtifactsForRepo <- function(owner, repo) {
  url <- paste("https://api.github.com/repos", owner, repo, "actions/artifacts", sep = "/")
  res <- .listArtifactsCore(url)
  return(res)
}

.listArtifactsCore <- function(url) {
  req <- request(url) |>
    req_headers("Accept" = "application/vnd.github.v3+json") |>
    req_headers("Authorization" = paste("token", Sys.getenv("GITHUB_PAT"))) |>
    req_url_query("per_page" = "100")

  ## get the first page of results
  resp <- req_perform(req)
  ## identify if there are more pages
  npages <- npages(resp)

  res <- resp |>
    resp_body_string() |>
    jsonlite::fromJSON() |>
    getElement("artifacts")

  if(npages >= 2L) {

    res_list <- vector(length = npages, mode = "list")
    res_list[[1]] <- res

    for(i in seq(2, npages)) {
      req <- req |> req_url_query("page" = i)
      resp <- req_perform(req)
      res_list[[i]] <- resp |>
        resp_body_string() |>
        jsonlite::fromJSON() |>
        getElement("artifacts")
    }

    res <- dplyr::bind_rows(res_list)
  }

  return(as_tibble(res))
}

#' @export
deleteArtifact <- function(owner, repo, artifact_id) {
  url <- sprintf("https://api.github.com/repos/%s/%s/actions/artifacts/%s", owner, repo, artifact_id)
  req <- request(url) |>
    req_headers("Accept" = "application/vnd.github.v3+json") |>
    req_headers("Authorization" = paste("token", Sys.getenv("GITHUB_PAT"))) |>
    req_method("DELETE")

  resp <- req_perform(req)
}

#' @export
downloadArtifact <- function(owner, repo, artifact_id, output_dir) {
  
  url <- sprintf("https://api.github.com/repos/%s/%s/actions/artifacts/%s/zip", owner, repo, artifact_id)
  
  req <- request(url) |>
    req_headers("Accept" = "application/vnd.github.v3+json") |>
    req_headers("Authorization" = paste("token", Sys.getenv("GITHUB_PAT")))
    
  resp <- req_perform(req) |>
    resp_body_raw()
  
  tf <- tempfile(fileext = ".zip")
  writeBin(resp, con = tf)
  unzip(zipfile = tf, exdir = output_dir)
  
}

#' @export
downloadArtifact2 <- function(owner, repo, artifact_id, output_dir) {
  
  url <- sprintf("https://api.github.com/repos/%s/%s/actions/artifacts/%s/zip", owner, repo, artifact_id)
  tf <- tempfile(fileext = ".zip")
  
  system2("curl", 
          args = c("--output", tf, "-L",
                   "-H", '"Accept: application/vnd.github+json"',
                   "-H", sprintf('"Authorization: Bearer %s"', 
                                 Sys.getenv("GITHUB_PAT")),
                   "-H", '"X-GitHub-Api-Version: 2022-11-28"',
                   url)
  )
  
  unzip(zipfile = tf, exdir = output_dir)
}
