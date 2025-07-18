# Miking Documentation Generator

This project is a **documentation generator** for the **Miking** language. It includes a custom parser designed to identify the different components of a Miking program. For each object (e.g., function, type, language...), the tool extracts all essential information needed for documentation, including comments, type annotations, and other metadata.

It then generates clean, readable documentation pages from this structured information.

For example, running the tool on the following code:

```
-- This function prints Hello World
let helloWorld = lam. print "Hello, World!\n"
```

Will produce a documentation page for the function helloWorld, including the extracted description and its inferred type.





---

## Usage

```bash
mi-doc-gen [options] <file>

Required:

    <file>: Path to the Miking source file to process.

Options:
Option	Description
--no-open	Do not open the result in a browser
--debug	Enable all debug modes
--[stage]-debug	Enable debug logs for a specific stage
--no-warn	Disable all warnings
--[stage]-warn	Disable warnings for a specific stage
--format <html	md>
--output-folder <name>	Set output folder. Default: doc-gen-output
--no-gen	Skip generation (reuse existing folder)
--skip-labeling	Skip type inference / annotation
```

Internals

The documentation is built through a multi-phase pipeline:

- **Parsing**: Generates a DocTree from source using context-sensitive breaker logic.
- **Extracting**: Converts it into an ObjectTree, filtering semantic elements.
- **Labeling** (optional): Infers types using Miking’s compiler infrastructure.
- **Rendering**: Generates output files

##  Project Structure

.
├── options.mc                 # CLI argument parsing
├── parsing/                  # Lexer + parser (breaker-based)
├── extracting/               # ObjectTree builder
├── labeling/                 # Optional AST-based type inference
├── rendering/
│   ├── md-renderer.mc        # Markdown backend
│   ├── html-rendering/       # HTML backend (with colorization and folding)
│   └── renderer.mc           # Rendering core logic
├── util.mc                   # Utilities (string, paths, hashmap, etc.)
├── logger.mc                 # Debug and warning logging
├── format.mc                 # Format types and helpers
└── server.mc                 # Local file server (optional)


Dependencies

**Miking**: The tool is written in Miking itself and must be compiled using the Miking toolchain.
