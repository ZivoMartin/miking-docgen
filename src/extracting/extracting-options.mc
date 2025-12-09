include "../global/logger.mc"

type ExtractingOptions = {
     depth: Option Int,
     rootIsStdlib: Bool,
     longestPrefix: String,
     log: Logger
}
