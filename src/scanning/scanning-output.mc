type FileToProcess = { path: String, outputFolder: String }


type ScanningOutput = {
    inputs: [FileToProcess],
    longestPrefix: String,
    onlyStdlib: Bool
}


let defaultScanningOutput : () -> ScanningOutput = lam. {
    inputs = [],
    longestPrefix = "",
    onlyStdlib = false
}
