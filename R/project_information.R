project_information_strwrap <-
    function(x)
{
    paste(strwrap(x, indent = 2, exdent = 2), collapse = "\n")
}

project_information_title_preclean <-
    function(title)
{
    sub("\\.$", "", title)
}

project_information_title_clean <-
    function(title)
{
    title <- project_information_title_preclean(title)
    project_information_strwrap(title)
}

project_information_contributors_clean <-
    function(contributors)
{        
    clean0 <- sub(",,", " ", contributors[[1]])
    project_information_strwrap(paste(clean0, collapse = ", "))
}

project_information_description_clean <-
    function(description)
{
    project_information_strwrap(description)
}

project_information_url_clean <-
    function(url)
{
    url <- paste(unlist(url, use.names = FALSE), collapse = " ")
    project_information_strwrap(url)
}

#' @rdname project_information
#'
#' @title Project Summaries from Project IDs
#'
#' @description `project_information()` queries the HCA database for
#'     project title, description, contact, DOI, and publication URI.
#'
#' @param project_id `character(1)` project identifier, e.g.,
#'     `"3c9d586e-bd26-4b46-8690-3faaa18ccf38"`.
#'
#' @return `project_information()` returns a tibble with a single row,
#'     and columns containing information about the project. The
#'     tibble is of class `project_information` and is printed in an
#'     interactive session formatted so long columns, e.g.,
#'     `projectDescription`, are more easily read.
#'
#' @importFrom dplyr mutate bind_cols
#'
#' @examples
#' project_id <- "3c9d586e-bd26-4b46-8690-3faaa18ccf38"
#' project_information(project_id)
#'
#' @export
project_information <-
    function(project_id)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_project_id(project_id)
    )

    filter <- filters(projectId = list(is = project_id))
    columns <- c(
        projectId = "hits[*].projects[*].projectId",
        projectTitle = "hits[*].projects[*].projectTitle",
        projectDescription = "hits[*].projects[*].projectDescription",
        contributors = "hits[*].projects[*].contributors[*].contactName",
        doi = "hits[*].projects[*].publications[*].doi",
        url = "hits[*].projects[*].publications[*].publicationUrl"
    )
               
    project <- projects(filter, columns = columns)
    if (identical(NROW(project), 0L))
        stop("did not find project_id '", project_id, "'")
    add_columns <- setdiff(names(columns), colnames(project))
    if (length(add_columns))
        project[add_columns] = list(NA)
    project_base_url <- "https://data.humancellatlas.org/explore/projects/"
    project <- bind_cols(
        project,
        tibble(hca_project_url = paste0(project_base_url, project_id))
    )
    class(project) <- c("project_information", class(project))
    project
}

#' @rdname project_information
#'
#' @description `project_title()` returns the title of the project,
#'     cleaned to remove trailing trailing `.`.
#'
#' @return `project_title()` returns a character(1) vector containing
#'     the project title.
#'
#' @examples
#' project_title(project_id)
#'
#' @export
project_title <-
    function(project_id)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_project_id(project_id)
    )

    project_information(project_id) |>
        pull("projectTitle") |>
        project_information_title_preclean()
}

#' @rdname project_information
#'
#' @description `print.project_information()` formats the result of
#'     `project_information()` in a more legible manner.
#'
#' @param x an object of class `project_information`, the result of a
#'     call to `project_information()`.
#'
#' @param ... additional arguments, required to conform with the
#'     `print` generic but not used.
#'
#' @return `print.project_information()` is invoked automatically when
#'     the result of `project_information()` is displayed for it's
#'     side effect of displaying the object.
#'
#' @export
print.project_information <-
    function(x, ...)
{
    cat(
        "Title\n",
        project_information_title_clean(x$projectTitle), "\n",
        "Contributors (unknown order; any role)\n",
        project_information_contributors_clean(x$contributors), "\n",
        "Description\n",
        project_information_description_clean(x$projectDescription), "\n",
        "DOI\n", project_information_url_clean(x$doi), "\n",
        "URL\n", project_information_url_clean(x$url), "\n",
        "Project\n", project_information_url_clean(x$hca_project_url), "\n",
        sep = ""
    )
}
