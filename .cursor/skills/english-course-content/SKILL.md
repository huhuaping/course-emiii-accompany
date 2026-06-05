---
name: english-course-content
description: >-
  Author and edit EM III Accompany course materials in English for international
  students. Use when creating or updating .qmd pages, R scripts, README, Quarto
  config, labels, or translating existing Chinese content. Applies while the user
  chats in Chinese — chat stays 中文, artifacts stay English.
---

# English Course Content Authoring

## Language Split

| Channel | Language |
|---------|----------|
| User chat | 中文 (questions and agent replies) |
| Project files | English only |

## Content Types

### Quarto pages (`*.qmd`)

- YAML: `title`, `subtitle`, `fig-cap`, `tbl-cap` in English
- Structure: Case Description → Learning Targets → Models → Exercise Materials
- Callouts: `::: {.callout-note title="..."}` — title and body in English
- Keep Wooldridge / Hansen / Hill citations in standard English form

### R scripts (`*.R`)

- Comments: full sentences, pedagogical tone
- Student-visible strings: captions, labels, `stargazer` / `DT` titles in English
- Function and variable names: English (existing `motheduc`, `fatheduc` data column names unchanged)

### Quarto config (`_quarto.yml`)

- `website.title`, navbar `text`, sidebar `title` / `section` in English

## Translation Workflow

When converting existing Chinese text to English:

1. Preserve econometric meaning and section order
2. Use standard textbook phrasing (e.g. "weak instrument", "overidentification", "reduced form")
3. Do not leave Chinese fragments in student-facing output
4. Summarize the translation in 中文 in chat only

## Style Conventions

- **Tone**: clear, instructional, suitable for graduate-level econometrics
- **Terms**: OLS, 2SLS/TSLS, IV, SEM, endogeneity, exogeneity, Hausman test, J-test, LIML
- **Data**: refer to `wooldridge::mroz`, Card (1995), Hansen Ch. 12 as in existing materials
- **Code chunks**: `#| eval: false` for install demos; English chunk labels where used

## Checklist Before Finishing

- [ ] No Chinese in edited project files (unless user explicitly requested bilingual artifacts)
- [ ] Chat response to user is in 中文
- [ ] Headings, captions, and comments are English
- [ ] Math notation unchanged; surrounding prose is English
