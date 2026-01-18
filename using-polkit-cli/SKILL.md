---
name: using-polkit-cli
description: Use when needing to run privileged commands (root) and sudo is unavailable or failing, or when explicitly interacting with PolicyKit (pkexec).
---

# Using Polkit CLI (pkexec)

## Overview
`pkexec` is the PolicyKit equivalent of `sudo`. It allows you to execute a command as another user (typically root). In some environments where `sudo` is restricted or misconfigured for non-interactive shells, `pkexec` may work as a drop-in replacement.

## When to Use
- You need to run a command as root (or another user).
- `sudo` is unavailable, restricted, or failing.
- You are on a system using PolicyKit (standard on most modern Linux distros).

## The Core Pattern
Simply use `pkexec` followed by the command you want to run.

```bash
pkexec <command>
```

If the system is configured correctly, it will prompt for the necessary authentication or execute if passwordless access is permitted.

## Example Scenario

**User**: "Install nginx, but sudo is failing."

**Agent Response**:
```bash
# Use pkexec as a sudo alternative
pkexec apt-get install -y nginx
```

## Common Mistakes
- **Using `pkttyagent` unnecessary**: Do not use `pkttyagent` unless specifically debugged or requested. It can confuse agentic environments. `pkexec` often handles the TTY correctly on its own.
- **Thinking `sudo` is the only way**: `pkexec` is a valid alternative.

## Notes
- Unlike `sudo`, `pkexec` defaults to `root` but can run as other users with `--user <username>`.
- If a password is required, the system will prompt for it on the standard input.
