#'
#' Deploy new versions of a package.
#'
#' @param artifacts_dir Directory that contains the artifacts to be deployed
#' @param folder_pattern Pattern to find artifact folders
#' @param rversion_pattern Pattern to mask remove non-R-version on folder name
#' @param origin_dir Root directory that contains previous versions
#'
jenkins_deploy <- function(artifacts_dir,
                           folder_pattern = 'label=.*-RVERS-.*',
                           rversion_pattern = 'label=.*-RVERS-',
                           origin_dir) {
    folders <- list.dirs(artifacts_dir, pattern=folder_pattern)
    for (folder in folders) {
        artifacts <- list.files(paste(artifacts_dir, folder, sep = "/"))
        for (artifact in artifacts) {
            deploy_artifact(artifact,
                            paste(folder, artifact, sep = "/"),
                            origin_dir,
                            gsub(rversion_pattern, '', folder))
        }
    }
}

#'
#' Deploy a single artifact file
#'
#' @param artifact_file The artifact file to be deployed
#' @param artifact_file_path The path to the artifact file to be deployed
#' @param origin_dir Root directory that contains previous versions
#' @param rversion The R version that this artifact was built for
#' @param latestOnly Set to TRUE to skip deploying older versions
#'
deploy_artifact <- function(artifact_file,
                            artifact_file_path,
                            origin_dir,
                            rversion,
                            latestOnly=FALSE) {
    LINUX_SUFFIX <- '.tar.gz'
    MAC_SUFFIX <- '.tgz'
    WINDOWS_SUFFIX <- '.zip'
    if (endsWith(tolower(artifact_file), LINUX_SUFFIX)) {
        writePackagesType <- 'source'
        contribUrlType <- 'source'
    } else if (endsWith(tolower(artifact_file), MAC_SUFFIX)) {
        writePackagesType <- 'mac.binary'
        contribUrlType <- 'mac.binary'
    } else if (endsWith(tolower(artifact_file), WINDOWS_SUFFIX)) {
        writePackagesType <- 'win.binary'
        contribUrlType <- 'win.binary'
    } else {
        stop('Unknown package type', call. = FALSE)
    }
    dest <- contrib.url(origin_dir, type=contribUrlType)
    current_rversion <- substr(getRversion(), 1, 3)
    dest <- gsub(current_rversion, rversion, dest, fixed=TRUE)
    dir.create(dest, showWarnings=FALSE, recursive=TRUE)
    file.rename(artifact_file_path, file.path(dest, get_file_name(artifact_file)))
    tools:::write_PACKAGES(dest, type=writePackagesType, latestOnly=latestOnly)
}

