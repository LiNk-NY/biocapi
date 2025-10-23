.TEST_API_URL <- "http://127.0.0.1:8000/"

#' Bioconductor Package Build Report
#'
#' Queries the Bioconductor API to retrieve build reports for all packages
#'
#' @inheritParams maintainerPkgs
#'
#' @param api `character(1)` The base URL for the Bioconductor API (default is a
#'   local test server)
#'
#' @param version `package_version`  The Bioconductor version to use for
#'   `biocMaintained` function corresponding to the list of packages associated
#'   with the given maintainer.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom BiocPkgTools biocMaintained
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @return A `data.frame` with the build report information for all packages
#'   associated with the specified maintainer.
#'
#' @examplesIf interactive()
#' buildreport(main = "maintainer@bioconductor.org")
#' @export
buildreport <- function(
    api = .TEST_API_URL,
    main = "maintainer@bioconductor.org",
    version = BiocManager::version()
) {
    stopifnot(
        isScalarCharacter(api),
        isScalarCharacter(main)
    )
    pkgs <- maintainerPkgs(main = main, version = version)[["Package"]]
    pkgList <- lapply(
        pkgs,
        function(pkg) {
            paste0(.TEST_API_URL, "checkResults/package/", pkg) |>
                request() |>
                req_perform() |>
                resp_body_json(simplifyVector = TRUE)
        }
    )
    return(
        do.call(
            what = rbind.data.frame,
            args = pkgList
        )
    )
}
