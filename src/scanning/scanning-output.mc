
type FileToProcess = { path: String, outputFolder: String }


type ScanningOutput = {
    inputs: [FileToProcess]
}


let defaultScanningOutput : () -> ScanningOutput = lam. {
    inputs = []
}
