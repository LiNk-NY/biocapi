#' Get Build Status for a Maintainer's Packages
#'
#' This function queries the Bioconductor API to retrieve build status
#' information for all packages maintained by a specified maintainer.
#'
#' @inheritParams maintainerPkgs
#'
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @returns A `data.frame` with the build status information for all packages
#'   associated with the specified maintainer. The data frame contains columns
#'   such as `pkg`, `node`, `stage`, and `result` corresponding to the
#'   Bioconductor Build System (BBS) builders, stages, and results.
#'
#' @examplesIf interactive()
#' buildstatus(main = "maintainer@bioconductor.org")
#' @export
buildstatus <- function(
    main = "maintainer@bioconductor.org"
) {
    paste0(.TEST_API_URL, "checkResults/maintainer/", main) |>
        request() |>
        req_perform() |>
        resp_body_json() |>
        do.call(what = rbind.data.frame, args = _)
}
