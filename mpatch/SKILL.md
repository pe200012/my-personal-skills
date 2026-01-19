---
name: mpatch
version: 1.0.0
description: Use when applying unified diff patches via CLI with fuzzy matching for out-of-date patches
author: pe200012
license: BSD-3-Clause
tags: [git, patch, diff, cli, tool]
---

# mpatch Skill

Smart context-aware CLI tool for applying unified diffs with fuzzy matching.

## When to Use
- Applying unified diff patches to source files via command line
- Patches where line numbers may be outdated
- Fuzzy matching patches against modified content
- Processing Markdown diff blocks, unified diffs, or conflict markers

## Installation

```bash
cargo install mpatch
```

## CLI Usage

### Basic Syntax
```bash
mpatch <INPUT_FILE> <TARGET_DIR>
```

### Arguments
| Argument | Description |
|----------|-------------|
| `INPUT_FILE` | Path to patch file (Markdown, Unified Diff, or Conflict Markers) |
| `TARGET_DIR` | Directory containing files to patch |

### Options
| Option | Description |
|--------|-------------|
| `-n, --dry-run` | Show what would be done without modifying files |
| `-f, --fuzz-factor <0.0-1.0>` | Similarity threshold for fuzzy matching (default: 0.7). Higher = stricter. 0 = disable fuzzy |
| `-v, --verbose` | Increase verbosity: `-v` info, `-vv` debug, `-vvv` trace, `-vvvv` debug report |
| `-h, --help` | Print help |
| `-V, --version` | Print version |

## Examples

### Apply a patch
```bash
mpatch changes.patch ./src
```

### Dry run (preview changes)
```bash
mpatch -n changes.patch ./src
```

### Strict matching (no fuzzy)
```bash
mpatch -f 0 changes.patch ./src
```

### Lenient fuzzy matching
```bash
mpatch -f 0.5 changes.patch ./src
```

### Verbose output for debugging
```bash
mpatch -vv changes.patch ./src
```

## Supported Patch Formats

mpatch auto-detects the input format:

1. **Unified Diff** (standard `diff -u` output)
   ```diff
   --- a/file.txt
   +++ b/file.txt
   @@ -1,3 +1,4 @@
    line1
   +new line
    line2
   ```

2. **Markdown Code Blocks** (common in AI-generated patches)
   ````markdown
   ```diff
   --- a/file.txt
   +++ b/file.txt
   @@ -1,3 +1,4 @@
    context
   -old
   +new
   ```
   ````

3. **Conflict Markers** (git merge conflicts)
   ```
   <<<<<<< HEAD
   current content
   =======
   incoming content
   >>>>>>> branch
   ```

## Key Features

- **Ignores line numbers**: Uses context matching instead
- **Fuzzy matching**: Applies patches even when source has minor changes
- **Auto-detection**: No need to specify patch format
- **Smart indentation**: Handles indentation differences

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Forgetting target directory | Second argument is the directory, not the file |
| Expecting exact match | Use `-f 0` if you want strict matching |
| Not checking results first | Use `-n` (dry-run) to preview |
| Patch not applying | Try `-f 0.5` for more lenient matching |

## Workflow Example

```bash
# 1. Preview what will change
mpatch -n patch.diff ./myproject

# 2. Apply with verbose output
mpatch -v patch.diff ./myproject

# 3. If fuzzy fails, try more lenient
mpatch -f 0.5 patch.diff ./myproject
```
