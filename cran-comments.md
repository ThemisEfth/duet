## Resubmission

This is a resubmission of the `duet` package, version 0.2.0.

### Changes

*   Resolved `WARNING` regarding the empty `demo/` subdirectory by adding it to `.Rbuildignore`.
*   Resolved `NOTE` for `no visible global function definition for ‘head’` by adding the appropriate `@importFrom utils head` tag to the function documentation and regenerating the `NAMESPACE` file.
*   Incremented the package version in the `DESCRIPTION` file.
*   Added several new functions:
    `op_compute_motionenergy` - compute motion energy (frame-differencing)
    `op_compute_coherence` - compute wavelet coherence
    `op_summarise` - compute descriptives for timeseries

## R CMD check results

There are 0 errors, 0 warnings, and 0 notes.

## Test Environments

*   local: macOS Sonoma 14.5, R 4.4.0
*   rhub: Windows Server 2022, R-devel, 64-bit
*   rhub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
*   win-builder: R-devel
