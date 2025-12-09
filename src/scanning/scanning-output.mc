type FileToProcess = { path: String, outputFolder: String }


type ScanningOutput = {
    inputs: [FileToProcess],
    longestPrefix: String
}


let defaultScanningOutput : () -> ScanningOutput = lam. {
    inputs = []
}
