# Multi-Format Output Plan for vignette.qmd

## Current State
The vignette currently renders only to HTML and uses many HTML-specific features:
- Custom CSS classes (`.signature-header`, `.cosine-value`, etc.)
- Inline HTML (`<div>`, `<span>`, `<img>` with style attributes)
- CSS gradients, shadows, border-radius
- Interactive features (collapsible code, left sidebar TOC)
- Percentage-based image widths

## What Would Be Involved

### 1. YAML Header Changes
Add multi-format configuration:
```yaml
format:
  html:
    # existing HTML config...
  pdf:
    toc: true
    toc-depth: 3
    documentclass: article
  docx:
    toc: true
    toc-depth: 3
```

### 2. Replace HTML with Quarto Native Features

**Current approach (HTML-specific):**
```r
cat('<div class="signature-header">Title</div>')
```

**Multi-format approach (Quarto divs):**
```markdown
::: {.signature-header}
Title
:::
```

Quarto's fenced divs work across formats (with caveats for styling).

### 3. Conditional Output in R Helpers

Modify `vignette_helpers.R` functions to detect output format:
```r
generate_section_header <- function(sig_data) {
  format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (format == "html") {
    # HTML with divs and classes
  } else {
    # Plain markdown with bold/headers
  }
}
```

The `use_html` parameter already exists but isn't being used dynamically.

### 4. Image Width Handling

**Current:** `style="width: 80%;"` (HTML only)

**Multi-format:** Use Quarto's native image syntax:
```markdown
![](path/to/image.png){width=80%}
```
Or specify absolute widths for PDF/Word:
```markdown
![](path/to/image.png){width=6in}
```

### 5. Table Styling

**Current:** kable with inline HTML styles

**Multi-format:** Let kable auto-detect format:
```r
knitr::kable(df)  # Auto-selects format based on output
```
For PDF, use `kableExtra` for styling. For Word, styling is limited.

### 6. CSS Fallbacks for PDF

PDF uses LaTeX, which ignores CSS. Options:
- Use Quarto's `include-in-header` to add LaTeX styling
- Accept simpler styling in PDF output
- Use a custom LaTeX template

### 7. Features That Won't Translate

| Feature | HTML | PDF | Word |
|---------|------|-----|------|
| Gradient backgrounds | Yes | No | No |
| Box shadows | Yes | No | No |
| Left sidebar TOC | Yes | No | No |
| Code folding | Yes | No | No |
| Rounded corners | Yes | No | Partial |

## Effort Estimate

| Task | Complexity |
|------|------------|
| Add format configs to YAML | Low |
| Convert HTML divs to Quarto syntax | Medium |
| Make R helpers format-aware | Medium |
| Fix image width handling | Low |
| Fix table generation | Low |
| Accept styling limitations | N/A |

## Recommended Approach

**Option A: Separate styling per format (more work, better results)**
- Full-featured HTML with current styling
- Simplified but clean PDF/Word with basic formatting

**Option B: Unified minimal styling (less work, consistent)**
- Remove fancy CSS features
- Use only features that work across all formats
- Lose visual polish in HTML

## Files to Modify

1. `vignette/vignette.qmd` - Add format configs
2. `vignette/_signature_section.qmd` - Replace inline HTML with Quarto syntax
3. `vignette/vignette_helpers.R` - Make output format-aware
4. `vignette/styles.css` - Keep for HTML, ignored by others

## Verification

After changes:
```bash
quarto render vignette.qmd --to html
quarto render vignette.qmd --to pdf
quarto render vignette.qmd --to docx
```
Check that all three outputs are readable and properly formatted.
