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
```

## Dependencies

**Miking**: The tool is written in Miking itself and must be compiled using the Miking toolchain.
