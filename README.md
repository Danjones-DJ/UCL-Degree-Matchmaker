# UCL Degree Explorer

A Spotify-styled Shiny app for browsing, matching, and shortlisting UCL undergraduate degrees — built around one idea from the economics of education: **students should not undermatch.**

Browse 426 UCL undergraduate courses as album-style tiles, enter your predicted A-level grades and subjects, and the app scores every degree against you — surfacing exact matches and ambitious stretches first, flagging undermatches, and letting you build and compare a UCAS-style shortlist of up to five choices.

------------------------------------------------------------------------

## Why "mismatch"?

The app's matching logic is motivated by **Blanden, Cassagneau-Francis, Macmillan & Wyness (2025), *Private highs: Investigating university overmatch among students from elite schools*** (CEPEO Working Paper No. 25-07, UCL Centre for Education Policy and Equalising Opportunities).

Using linked administrative data on \~184,000 English students, the paper measures *match* as the gap between a student's position in the A-level attainment distribution and the rank of the degree course they enrol on. Three findings shape this app:

1.  **Undermatch is common and unequal.** Students from non-selective state schools and FE colleges are far more likely to enrol on courses ranked *below* what their grades would predict. Private-school students rarely undermatch (≈6% vs ≈15% for state-school students), and lower-attaining private-school pupils enrol on courses around **15 percentiles higher ranked** than similarly qualified state-school peers.
2.  **Applications drive the gap.** Up to \~75% of the difference is explained by application behaviour: private-school students aim higher with *every* choice, including their "safety" options. The gap is about which institutions students apply to, not which subjects they pick.
3.  **Ambition is rational under the UCAS system.** Students apply blind (before results, capped at five choices), which rewards confident, well-informed application strategies — exactly the support unevenly distributed across school types.

The takeaway the app operationalises: **exact matches and overmatches are good; undermatches are the failure mode to design against.** The Explorer tries to give any student the application instincts the paper shows elite schools provide by default.

> Suggested citation for the paper: Blanden, J., Cassagneau-Francis, O., Macmillan, L. & Wyness, G. (2025). *Private highs: Investigating university overmatch among students from elite schools* (CEPEO Working Paper No. 25-07). UCL Centre for Education Policy and Equalising Opportunities.

------------------------------------------------------------------------

## How match is computed here

Each course in the data carries a `grade_points` score for its typical offer, on a simple additive scale (A\* = 6, A = 5, B = 4, C = 3, D = 2; e.g. AAA = 15, A\*AA = 16). When you set your predicted grades:

```         
match = course grade points − your grade points
```

| Match value | Label | Treatment |
|------------------------|------------------------|------------------------|
| 0 | **Exact match** | Solid green badge; top of "Best match" sort |
| +1 to +2 | **Stretch** | Green outline; ranked just below exact matches |
| +3 and up | **Reach** | Amber outline; ambitious but visible |
| \< 0 | **Undermatch** | Grey badge, card dimmed, sorted last; can be hidden entirely with *"Hide undermatches (aim high)"* |

This mirrors the paper's course-percentile-minus-student-percentile measure, with one honest caveat: the dataset is **UCL-only**, so "undermatch" here means *relative to UCL's internal range of offers*, not the national course ranking used in the paper. The logic generalises directly if courses from more universities are added.

## Features

-   **Spotify-style UI** — near-black canvas, `#1DB954` accent, Figtree type, tile grid with hover play buttons, pill buttons, and compulsory modules rendered as a numbered "tracklist" in each course's modal.
-   **Profile-aware browsing** — set predicted grades + up to 4 A-level subjects; every card gets a match badge and the grid re-sorts to put exact matches and sensible stretches first (undermatches last or hidden).
-   **Per-subject requirement highlighting** — requirement text is scanned for subject names (alias-aware: "Maths" ≡ "Mathematics"; longest-match-first so "Further Mathematics" is never half-claimed). Each mentioned subject becomes a chip: **green ✓** if you take it, **red ✗** if you don't — so taking Physics but not Maths shows exactly that. The modal shows the full requirement sentence with the same inline highlighting, preserving "X *or* Y" phrasing.
-   **Module filter** — *"Must include modules"*: pick any compulsory modules (server-side searchable across the full catalogue) and only courses containing **all** of them remain; matches glow green in the modal tracklist.
-   **Entry-standards chart** — a dark-themed distribution of offers across all UCL degrees with the open course highlighted in green and (if set) a dashed white line marking *your* grades, making over/undermatch visible at a glance.
-   **Your choices (×5)** — a UCAS-style shortlist capped at five. Side-by-side comparison of offer, match badge, subject chips, competitiveness (faculty + UCL), and module counts, topped by a **portfolio health** check that warns when any choice undermatches and confirms when your whole slate aims at or above your level.
-   **Competitiveness bars** — Spotify-popularity-style bars derived from `faculty_competitiveness_rank` and `ucl_competitiveness_rank`.

## Running the app

``` r
# from the repository root
shiny::runApp()
```

The app looks for a data frame named `UCL.v2` in the environment; if absent, it falls back to reading `datasets/UCL_v2.csv` (adjust the path at the top of `app.R` to wherever your data lives).

Dependencies are handled by `pacman` on first run:

``` r
pacman::p_load(tidyverse, shiny, bslib, scales, htmltools)
```

Fonts (Figtree) are pulled from Google Fonts and cached locally by `bslib`.

## Data

One row per degree course (426 courses, 13 columns):

| Column | Description |
|------------------------------------|------------------------------------|
| `title` | Degree title (e.g. "Ancient History BA") |
| `degree_type` | Qualification (BA, BSc, MEng, LLB, ...) |
| `faculty` | UCL faculty |
| `degree_url` | Link to the UCL course page |
| `A_Level_Grades` | Typical offer (e.g. "AAB") |
| `A_Level_Subjects` | Free-text subject requirements |
| `all_compulsory_modules` | Pipe-separated "Name (CODE)" list |
| `about_section` / `graduate_section` | Course description / graduate attributes |
| `university` | "University College London" |
| `grade_points` | Numeric offer score (A\* = 6, A = 5, B = 4, ...) |
| `faculty_competitiveness_rank` | Rank within faculty (1 = most competitive in terms of standard A Level entry grade requirements) |
| `ucl_competitiveness_rank` | Rank across UCL (1 = most competitive in terms of standard A Level entry grade requirements) |

## Known limitations

-   **Subject detection is keyword-based.** Requirements phrased without naming a subject ("an ancient or modern language at grade A") produce no chips; the inline-highlighted full text in the modal is the source of truth. A red chip in an "X or Y" requirement does not necessarily mean ineligible.
-   **Single-institution scope.** Match is computed against UCL's offer distribution, not a national course ranking (see above).
-   **Match ≠ admission odds.** The score compares grades to typical offers; it doesn't model contextual offers, interviews, admissions tests, or predicted-grade dynamics — all of which the paper notes also matter.

## Acknowledgements

Mismatch framework and motivation: Blanden, Cassagneau-Francis, Macmillan & Wyness (2025), building on Campbell, Macmillan, Murphy & Wyness (2022), *"Matching in the dark? Inequalities in student to degree match"*, Journal of Labor Economics. Course data scraped/derived from UCL's prospective-undergraduate pages. Visual language inspired by Spotify (no affiliation).
