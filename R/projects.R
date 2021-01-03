.PROJECTS_PATH <- "/index/projects"

#' @rdname projects
#'
#' @title FIXME
#'
#' @description FIXME
#'
#' @param filters FIXME
#'
#' @param n integer(1) maximum number of results to return; default:
#'     all projects mathcing `filter`.
#'
#' @return `projects()` returns a tibble with each row representing an
#'     HCA project, and columns summarizing the project.
#'
#' @examples
#' FIXME
#'
#' @export
projects <-
    function(
        filters = filters(),
        n = Inf,
        FIXME # sort, order, etc
    )
{
    ## FIXME: validate inputs

    ## FIXME: construct params <- .projects_params()

    ## FIXME: construct '/index/projects?<param>' using path <- .projects_path()

    ## FIXME: invoke content <- .hca_GET(path) from hca.R to do initial processing

    ## FIXME: invoke .projects_as_tibble(content)
}        

.projects_param <-
    function(FIXME)
{
    FIXME
}

.projects_path <-
    function(param)
{
    paste0(.PROJECTS_PATH, "?", param)
}

.projects_as_tibble <-
    function(content)
{
    FIXME
}
