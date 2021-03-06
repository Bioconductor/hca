% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects.R
\name{projects}
\alias{projects}
\alias{projects_facets}
\alias{projects_default_columns}
\alias{projects_detail}
\title{HCA Project Querying}
\usage{
projects(
  filters = NULL,
  size = 1000L,
  sort = "projectTitle",
  order = c("asc", "desc"),
  catalog = c("dcp2", "it2", "dcp1", "it1"),
  as = c("tibble", "lol", "list"),
  columns = projects_default_columns("character")
)

projects_facets(facet = character(), catalog = c("dcp2", "it2", "dcp1", "it1"))

projects_default_columns(as = c("tibble", "character"))

projects_detail(uuid, catalog = c("dcp2", "it2", "dcp1", "it1"))
}
\arguments{
\item{filters}{filter object created by \code{filters()}, or \code{NULL}
(default; all projects).}

\item{size}{integer(1) maximum number of results to return;
default: all projects matching \code{filter}. The default (10000) is
meant to be large enough to return all results.}

\item{sort}{character(1) project facet (see \code{facet_options()}) to
sort result; default: \code{"projectTitle"}.}

\item{order}{character(1) sort order. One of \code{"asc"} (ascending) or
\code{"desc"} (descending).}

\item{catalog}{character() catalog to use for query.}

\item{as}{character(1) return format. Default: \code{"tibble"}, a tibble
summarizing essential elements of HCA projects. \code{"lol"}: a
representation of the JSON returned by the query as a
'list-of-lists' data structure, indexed and presented to enable
convenient filtering, selection, and extraction. \code{"list"} an R
list (typically, highly recursive) containing detailed project
information, constructed from the JSON response to the original
query.}

\item{columns}{named character() indicating the paths to be used
for parsing the 'lol' returned from the HCA to a tibble. The
names of \code{columns} are used as column names in the returned
tibble.}

\item{facet}{character() of valid facet names. Summary results (see
'Value', below) are returned when missing or length greater
than 1; details are returned when a single facet is specified.}

\item{uuid}{character() unique identifier (e.g., \code{projectId}) of
the object.}
}
\value{
When \code{as = "tibble"}, \code{projects()} returns a tibble with
each row representing an HCA project, and columns summarizing
the project. Each \code{.hit} is a single result; a result may
contain several projects, as indexed by \code{.project}.\preformatted{When `as = "lol"`, `projects()` returns a list-of-lists data
structure representing detailed information on each project
('hit').

When `as = "list"`, `projects()` returns an R list, typically
containing other lists or atomic vectors, representing detailed
information on each project.
}

\code{projects_facets()} invoked with no \verb{facet=} argument returns a
tibble summarizing terms available as \code{projects()} return
values, and for use in filters. The tibble contains columns\preformatted{- `facet`: the name of the facet.
- `n_terms`: the number of distinct values the facet can take.
- `n_values`: the number of occurrences of the facet term in the
   entire catalog.

`projects_facets()` invoked with a scalar value for `facet=`
returns a tibble summarizing terms used in the facet, and the
number of occurrences of the term in the entire catalog.
}

\verb{*_columns()} returns a tibble with column \code{name}
containing the column name used in the tibble returned by
\code{projects()}, \code{files()}, \code{samples()}, or \code{bundles()}, and
\code{path} the path (see \code{lol_hits()}) to the data in the
list-of-lists by the same functions when \code{as = "lol"}. When \code{as = "character"}, the return value is a named list with paths as
elements and abbreviations as names.

list-of-lists containing relevant details about the project.
}
\description{
\code{projects()} takes user input to be used to query the
HCA API for information about available projects.

\code{projects_facets()} summarizes facets and terms used by
all records in the projects index.

\verb{*_columns()} returns a tibble or named
character vector describing the content of the tibble returned
by \code{projects()}, \code{files()}, \code{samples()}, or \code{bundles()}.

\code{projects_detail()} takes a unique project_id and catalog for
the project, and returns details about the specified project as a
list-of-lists
}
\examples{
projects(filters())

projects_facets()
projects_facets("genusSpecies")

projects_default_columns()

projects_detail(
    uuid = "4a95101c-9ffc-4f30-a809-f04518a23803",
    catalog = "dcp1"
)

}
\seealso{
\code{lol()} and other \verb{lol_*()} functions for working with the
list-of-list data structure returned when \code{as = "lol"}.
}
