#' All columns for the projects, files, samples, and bundles endpoints
#'
#' @rdname all_columns
#'
#' @name all_columns
#'
#' @param type character() type of all columns wanted; one of either "projects",
#' "files", "samples", or "bundles"
#'
#' @description `all_columns()` queries the API for all available columns for
#' either projects, files, samples, or bundles
#'
#' @importFrom dplyr %>% filter pull .data
#'
#' @return character() vector of all columns formatted like "hits[*]..."
#'
#' @examples all_columns("projects")
#'
#' @export
all_columns <- local({
    ## create a 'local' environment with variables
    ALL_COLUMNS_PROJECTS <- NULL
    ALL_COLUMNS_FILES <- NULL
    ALL_COLUMNS_SAMPLES <- NULL
    ALL_COLUMNS_BUNDLES <- NULL
    ## this function is the value assigned to `all_columns`
    function(type = NULL) {
        stopifnot(
            ## type must non-null character scalar
            `"type" must be a non-null character scalar` =
                .is_scalar_character(type),
            ## type must be one of permissible values
            `"type" must be one of the following: ["projects", "files", "samples", "bundles"]` =
                type %in% c("projects", "files", "samples", "bundles")
        )
        ## first time evaluations --  if NULL, assign it
        if (is.null(ALL_COLUMNS_PROJECTS)) {
            ## grab just first 10? 100? 1000?
            ## core elements may be represented even if they're not filled in
            ## hits[*].projects[*].matrices is project specific > filter out
            ## is the first one always going to have all columns?
            projects_json <- .hca_GET("/index/projects")
            ## `<<-` assigns _outside_ the function, to the variable
            ## `ALL_COLUMNS_PROJECTS` in the local environment; the assignment
            ## persists for the session, so all_columns() is only
            ## expensive once
            all_unnamed_project_columns <- projects_json$content %>%
                lol() %>%
                lol_path() %>%
                filter(.data$is_leaf == TRUE) %>%
                pull("path")
            # must give names to required/default columns
            new_project_column_names <- as.character(all_unnamed_project_columns)
            default_col_indices <- match(.PROJECTS_DEFAULT_COLUMNS, new_project_column_names)
            new_project_column_names[default_col_indices] <- names(.PROJECTS_DEFAULT_COLUMNS)
            names(all_unnamed_project_columns) <- new_project_column_names
            ALL_COLUMNS_PROJECTS <<- all_unnamed_project_columns
        }

        if (is.null(ALL_COLUMNS_FILES)) {
            files_json <- .hca_GET("/index/files")
            ## `<<-` assigns _outside_ the function, to the variable
            ## `ALL_COLUMNS_FILES` in the local environment; the assignment
            ## persists for the session, so all_columns() is only
            ## expensive once
            all_unnamed_file_columns <- files_json$content %>%
                lol() %>%
                lol_path() %>%
                filter(.data$is_leaf == TRUE) %>%
                pull("path")
            # must give names to required/default columns
            new_file_column_names <- as.character(all_unnamed_file_columns)
            default_col_indices <- match(.FILES_DEFAULT_COLUMNS, new_file_column_names)
            new_file_column_names[default_col_indices] <- names(.FILES_DEFAULT_COLUMNS)
            names(all_unnamed_file_columns) <- new_file_column_names
            ALL_COLUMNS_FILES <<- all_unnamed_file_columns
        }

        if (is.null(ALL_COLUMNS_SAMPLES)) {
            samples_json <- .hca_GET("/index/samples")
            ## `<<-` assigns _outside_ the function, to the variable
            ## `ALL_COLUMNS_SAMPLES` in the local environment; the assignment
            ## persists for the session, so all_columns() is only
            ## expensive once
            all_unnamed_sample_columns <- samples_json$content %>%
                lol() %>%
                lol_path() %>%
                filter(.data$is_leaf == TRUE) %>%
                pull("path")
            # must give names to required/default columns
            new_sample_column_names <- as.character(all_unnamed_sample_columns)
            default_col_indices <- match(.SAMPLES_DEFAULT_COLUMNS, new_sample_column_names)
            new_sample_column_names[default_col_indices] <- names(.SAMPLES_DEFAULT_COLUMNS)
            names(all_unnamed_sample_columns) <- new_sample_column_names
            ALL_COLUMNS_SAMPLES <<- all_unnamed_sample_columns
        }

        if (is.null(ALL_COLUMNS_BUNDLES)) {
            bundles_json <- .hca_GET("/index/bundles")
            ## `<<-` assigns _outside_ the function, to the variable
            ## `ALL_COLUMNS_BUNDLES` in the local environment; the assignment
            ## persists for the session, so all_columns() is only
            ## expensive once
            all_unnamed_bundle_columns <- bundles_json$content %>%
                lol() %>%
                lol_path() %>%
                filter(.data$is_leaf == TRUE) %>%
                pull("path")
            # must give names to required/default columns
            new_bundle_column_names <- as.character(all_unnamed_bundle_columns)
            default_col_indices <- match(.BUNDLES_DEFAULT_COLUMNS, new_bundle_column_names)
            new_bundle_column_names[default_col_indices] <- names(.BUNDLES_DEFAULT_COLUMNS)
            names(all_unnamed_bundle_columns) <- new_bundle_column_names
            ALL_COLUMNS_BUNDLES <<- all_unnamed_bundle_columns
        }

        all_columns <- switch(
            type,
            "projects" = ALL_COLUMNS_PROJECTS,
            "files" = ALL_COLUMNS_FILES,
            "samples" = ALL_COLUMNS_SAMPLES,
            "bundles" = ALL_COLUMNS_BUNDLES
        )

        all_columns
    }
})
