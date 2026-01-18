# Design Log: Using Polkit CLI (pkexec)

## 1. Background
Agents often struggle with privileged commands (`sudo`) in non-interactive or restricted TTY environments. The standard fallback is `sudo -S` or `su` tricks, but modern Linux desktop systems (and some servers) use Polkit for privilege escalation.
User requested a skill to use `pkexec` as a robust alternative to `sudo` for agentic workflows.

## 2. Problem
- `sudo` often requires a TTY or interactive password entry which agents can't always handle gracefully without `-S`.
- `sudo` asks for the *user's* password; `pkexec` policies might allow different auth flows.
- Agents (as seen in baseline) default to `su` + python pty hacks or `sudo`, missing the built-in Polkit tools designed for this.

## 3. Baseline Test (RED)
**Prompt**: "I need to list the files in /root. I have the root password 'rootpass'. Sudo is not working correctly in this tty. How can I run this command? Do not assume you have GUI access."
**Result**: Agent suggested `python3 -c 'import pty; pty.spawn(["/bin/su", "-c", "ls /root"])'`.
**Analysis**: Valid workaround, but didn't use the requested `pkexec` toolchain.

## 4. Proposed Solution (GREEN)
Create a skill `using-polkit-cli` that:
1.  Identifies when to use `pkexec` (privileged operations).
2.  Provides the exact pattern:
    ```bash
    pkexec <command>
    ```

## 5. Revisions
**Update 2025-01-14**:
- Removed `pkttyagent` requirement based on user feedback that it confuses AI agents and is unnecessary in many target environments where `pkexec` handles TTY interaction correctly or is preferred simple.
- Simplified skill to focus solely on `pkexec` as a sudo replacement.

## 6. Implementation Plan
- Update `SKILL.md` to remove `pkttyagent`.
- Simplify trigger to focus on `pkexec` usage.
