filters.R

- filters.R:19 I think it would be good to explore if the factets can
  be extacted programmatically from the API specification. Probabaly
  in a helper function, maybe exposed to the user `filters_facets()`
  (I'm not really sure whether the facets are specific to 'projects',
  or whether they apply to the other indexes as well... the names of
  the functions / location in files might need to be adjusted, e.g.,
  `project_facets()`, `project_filters()`, ...
  
  ```{r}
  api <- "https://service.azul.data.humancellatlas.org/openapi"
  json <- jsonlite::read_json(api)
  filter_parameter <- json$paths$`/index/projects`$get$parameters[[2]]
  facets <- names(filter_parameter$content$`application/json`$schema$properties)
  ```
  
- filters.R:49 the arguments to `stopifnot()` have to evaluate to a
  scalar (length 1) logical, but `names(x) %in% facets` will evaluate
  to a logical vector of `lenght(x)`; use `all(names(x) %in% facets)`.

hca.R

- I think the `.parameters*()` functions belong in projects.R, with
  hca.R limited to querying the service and initial processing of the
  response
  
- hca.R:41 in some ways sticking args into a list is less robust than
  just passing arguments explicitly. For instance

    ```
    fun <- function(x, y) { 
        stopifnot(x == 1, y == 2)
    }
    fun(x = 1, y = 2, z = 3)
    ```
    
  fails because `z` isn't an argument of `fun()`, but
  
    ```
    fun <- function(lst) {
        stopifnot(
            lst$x == 1,
            lst$y == 2
        )
    }
    fun(list(x = 1, y = 2, z = 3))
    ```

  succeeds even though one of the arguments is gibberish. Comment also
  applies to `url_path_list` a bit later in the file.
  


Other

- would be great to get through `projects()` with a tibble as outcome.
- update tests.
- long term goal -- I'm not exactly sure 'how to get there from here'
  -- is to retrieve specfic files for each project, like the 'loom'
  file on this page
  https://data.humancellatlas.org/explore/projects/8185730f-4113-40d3-9cc3-929271784c2b/m/expression-matrices?catalog=dcp1
  ; I'm not sure how one goes from the project to the file, maybe via
  the `projectId` returned from `/index/projects` as a filter in
  `/index/files`? This would mean implementing infrastructure like
  we're doing for `/index/projects`, but for `/index/files` --
  probably very similar, so re-using modular functions (rather than
  copy/paste nearly identical code.
