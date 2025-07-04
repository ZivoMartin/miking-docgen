# Miking Doc Generator

**Miking Doc Generator** is a documentation extractor and generator for Miking projects.  
It parses source files, extracts structure and comments, and produces clear and readable documentation in Markdown or HTML format.

---

## Purpose

Writing large Miking projects can quickly become hard to document and maintain.  
This tool solves that by generating automatic documentation — directly from the source — in a structured way.

The goal is to make it easy to:
- Understand a large Miking project
- Browse through types, functions, language definitions
- See comments and semantic variants
- Navigate across modules and includes

---

## Architecture

The tool is composed of several cleanly separated modules:

- **parsing**  
  Builds a `DocTree` representing the syntactic structure of the project

- **extracting**  
  Converts the `DocTree` into an `ObjectTree`, containing all the relevant documentation info

- **rendering**  
  Generates Markdown or HTML from the `ObjectTree`

---

## Current features

- Parse complex Miking source files  
- Extract comments and preserve them  
- Support project includes and standard library paths  
- Generate clean Markdown pages  
- CLI interface  
- HTML renderer  
- Local server to browse docs  
- Source code view from the documentation page

---

## 🚀 Usage

```bash
make
./mi-doc-gen src/main.mi
```


