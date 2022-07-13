Changes in version 1.6

New features

+ (v. 1.5.1) Implement `hca_view()` for intereactive tibble view & filter

User visible changes

+ (v. 1.5.3) Provide more default `projects()` columns (similar to
  data portal).

Bug fixes

+ (v. 1.5.2) `projects()` constructs the complete tibble of projects
  by paging through result sets of size 100, to accommodate upstream
  changes. See https://github.com/Bioconductor/hca/issues/33

Changes in version 1.2

New features

+ (v. 1.1.5) Implement `manifest()` endpoint. Impement `"tibble_expand"`
  option for `projects()`, `bundles()`, `samples()`, and `files()`. Implement
  `optimus_loom_annotation()`.

Bug fixes

+ (v. 1.2.1) `manifest()` uses all rows to guess column format.

Changes in RELEASE version 1.0 (2021-08-19)

Bug fixes

+ (v. 1.0.3) Update unnamed query columns s.t. they are named more compactly
+ (v. 1.0.3) Update how `.as_tbl_hca()` supports queries where
  all values are `NULL`
+ (v. 1.0.3) Update how class definitions for tibbles returned by
  `projects()`, `files()`, `samples()`, and `bundles()` are applied;
  ensure that all tibbles have minimum necessary columns.
+ (v. 1.0.2) Update `files_download()` to clarify error message
+ (v. 1.0.1) Update `files_download()` to avoid unnecessary downloads
+ (v. 1.0.1) Update `filters()` to handle long character strings
  without newline breaks

Changes in version 0.99.0 (2021-02-26)

+ Prep for submission to Bioconductor
+ Added a `NEWS.md` file to track changes to the package.
