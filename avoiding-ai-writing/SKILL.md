---
name: avoiding-ai-writing
version: 1.0.0
description: Use when writing wiki-style articles, documentation, summaries, or creative content to prevent "AI accent", puffery, and formulaic structures.
author: pe200012
license: BSD-3-Clause
tags: [writing, ai, communication, documentation]
---

# Avoiding AI Writing

## Overview
**Writing like a human means embracing irregularity, neutrality, and concrete detail over formulaic structure and dramatic puffery.**

AI models naturally gravitate towards "high school essay" structures: generic intros, numbered lists for body paragraphs, and moralizing conclusions. They overuse specific "smart-sounding" words (delve, testament, tapestry). This skill forces a "Wikipedia-neutral" or "Journalistic" tone by banning these patterns.

## When to Use
- Writing Wikipedia-style articles or encyclopedic entries
- Creating documentation or summaries that need to sound professional, not "generated"
- When user asks for "natural", "human-like", or "non-AI" writing
- **Symptoms of failure**: Output contains "In conclusion", "Ultimately", numbered lists for "Impact", or words like "burgeoning", "testament", "tapestry".

## The "AI Accent" Red Flags (STOP & FIX)

If you see these, **delete and rewrite**:

| Category | The "AI Accent" (BANNED) | Human/Neutral Replacement |
|----------|--------------------------|---------------------------|
| **Puffery** | "stands as a testament to", "serves as a reminder", "beacon of hope", "rich tapestry" | "shows", "demonstrates", "is", [delete entirely] |
| **Connectors** | "It is important to note", "It is worth mentioning", "underscoring the significance of" | [Just state the fact], "This implies..." |
| **Vocabulary** | "delve", "realm", "landscape", "foster", "spearhead", "burgeoning", "crucial role" | "investigate", "field", "area", "encourage", "led", "growing", "role" |
| **Structure** | Numbered lists for "Legacy" or "Impact"; Title Case headers | Prose paragraphs; Sentence case headers |
| **Closers** | "Ultimately,", "In conclusion,", "looking forward," | [End on a specific fact or event], [No conclusion needed] |

## Core Rules

### 1. Flatten Lists into Prose
**AI Instinct**: Create a numbered list for "Key Features" or "Legacy".
**Human approach**: Weave these points into cohesive paragraphs.

<Bad>
The Chronos Dial had three main impacts:
1. **Visual Mechanics**: It made the mechanism visible.
2. **Standardization**: It standardized resonance.
</Bad>

<Good>
The Chronos Dial influenced watchmaking by shifting focus to visual mechanics, making the internal quality visible to laypeople. It also helped standardize resonance as a reliable dual-regulator system...
</Good>

### 2. Kill the "Conclusion Paragraph"
Encyclopedic entries do not need a "wrap up" moralizing the subject.
- **Delete** any paragraph starting with "Ultimately," "In summary," or "Today, X stands as...".
- **End** with the last chronological event or a specific status (e.g., "The site was designated a landmark in 1999.").

### 3. Neutralize "Significance"
Don't *tell* the reader something is significant using abstract praise. *Show* the significance using facts.
- ❌ "Her work served as a testament to the power of activism."
- ✅ "Her lobbying led to the 2008 constitutional amendment acknowledging nature's rights."

### 4. Sentence Case for Headers
Wikipedia uses **Sentence case** for headers (only first word capitalized), NEVER **Title Case**.
- ❌ `## Early Life and Education`
- ✅ `## Early life and education`
- ❌ `## Impact on Watchmaking`
- ✅ `## Impact on watchmaking`

## Refactoring Process (The "De-bot" Cycle)

1.  **Draft** the content.
2.  **Scan** for "delve", "testament", "landscape", "underscoring". **Destroy them.**
3.  **Check Structure**: Is there a numbered list? **Flatten it.**
4.  **Check End**: Is there a moral summary? **Delete it.**
5.  **Check Headers**: Are they Title Case? **Lowercase them (except proper nouns).**

## Example: Refactoring a Biography

<Bad>
**Elena Mora** was a pivotal figure in environmentalism.

### Legacy
Her legacy is defined by three pillars:
1.  **Legal Pioneer**: She spearheaded the "Rights of Nature" movement.
2.  **Corporate Accountability**: She fostered a new era of responsibility.

Ultimately, Mora serves as a beacon of hope for future generations.
</Bad>

<Good>
**Elena Mora** (1964–2018) was an Ecuadorian environmental activist known for the "Movimiento Río Sagrado".

### Legacy
Mora is credited with establishing the legal framework for the "Rights of Nature" argument used in Ecuador's 2008 constitution. Her tactics shifted environmental activism from street protests to shareholder lobbying, establishing early precedents for corporate environmental accountability. Modern climate organizations, including the Youth Climate Coalition, cite her strategies as foundational to their 2020 campaigns.
</Good>
