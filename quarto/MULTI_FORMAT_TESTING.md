# Multi-Format Report Testing Guide

This document explains how to test the multi-format capabilities of the Global Consumer Pulse report.

## Prerequisites

1. **Quarto CLI**: Install from https://quarto.org/docs/get-started/
2. **R packages**: Ensure all required packages are installed
3. **LaTeX**: For PDF generation (install TinyTeX via Quarto: `quarto install tinytex`)

## Testing Commands

### 1. HTML Format (Interactive)
```bash
cd GlobalConsumerPulse/quarto
quarto render report.qmd --to html
```

**Expected Output**: `report.html` with interactive plotly charts, styled cards, and full interactivity.

**Features to Verify**:
- Interactive world maps with hover tooltips
- Clickable time series charts
- Styled highlight cards with CSS animations
- Professional navigation and layout
- Embedded CSS styling

### 2. PDF Format (Static)
```bash
cd GlobalConsumerPulse/quarto
quarto render report.qmd --to pdf
```

**Expected Output**: `report.pdf` with static charts, professional formatting, and print-ready layout.

**Features to Verify**:
- Static ggplot charts instead of plotly
- Professional headers and footers
- Proper page breaks and margins
- Formatted tables and text
- Consistent branding throughout

### 3. DOCX Format (Word Document)
```bash
cd GlobalConsumerPulse/quarto
quarto render report.qmd --to docx
```

**Expected Output**: `report.docx` compatible with Microsoft Word.

**Features to Verify**:
- Word-compatible formatting
- Static charts embedded as images
- Professional document structure
- Table of contents
- Consistent styling

### 4. Parameterized Generation

#### Recent Years Focus
```bash
quarto render report.qmd --to html --execute-params '{"start_year": 2018, "end_year": 2023, "focus_countries": ["United States", "China", "India", "Brazil", "Russia"], "primary_indicator": "inflation", "report_type": "recent_trends"}' --output report_recent.html
```

#### European Focus
```bash
quarto render report.qmd --to pdf --execute-params '{"start_year": 2015, "end_year": 2023, "focus_countries": ["Germany", "France", "United Kingdom", "Italy", "Spain"], "primary_indicator": "cpi", "report_type": "regional"}' --output report_europe.pdf
```

#### Custom Time Period
```bash
quarto render report.qmd --to html --execute-params '{"start_year": 2010, "end_year": 2020, "focus_countries": ["Japan", "South Korea", "Singapore"], "primary_indicator": "gdp", "report_type": "decade_review"}' --output report_custom.html
```

## Validation Checklist

### HTML Format Validation
- [ ] Interactive plotly charts load correctly
- [ ] Hover tooltips display economic data
- [ ] CSS styling applied consistently
- [ ] Navigation works smoothly
- [ ] Highlight cards display with animations
- [ ] Charts are responsive to screen size
- [ ] All sections render without errors

### PDF Format Validation
- [ ] Static charts render clearly
- [ ] Professional headers/footers present
- [ ] Page breaks occur appropriately
- [ ] Tables format correctly
- [ ] Text formatting is consistent
- [ ] Charts fit within page margins
- [ ] No interactive elements cause errors

### DOCX Format Validation
- [ ] Document opens in Microsoft Word
- [ ] Charts embedded as images
- [ ] Table of contents generated
- [ ] Formatting preserved
- [ ] Professional appearance maintained

### Parameterized Report Validation
- [ ] Parameters correctly filter data
- [ ] Focus countries appear in analyses
- [ ] Time ranges respected throughout
- [ ] Primary indicator emphasized
- [ ] Report type affects content structure
- [ ] All parameters reflected in output

## Format-Specific Features

### HTML Features
- **Interactive Charts**: Plotly-based visualizations with zoom, pan, hover
- **Styled Cards**: CSS-animated highlight cards with professional styling
- **Responsive Design**: Adapts to different screen sizes
- **Smooth Navigation**: Table of contents with smooth scrolling
- **Professional Branding**: Economic-canvas inspired color scheme

### PDF Features
- **Static Charts**: High-quality ggplot2 visualizations
- **Print Layout**: Optimized for printing and professional presentation
- **Headers/Footers**: Branded headers with page numbers
- **Professional Typography**: LaTeX-quality text rendering
- **Consistent Margins**: Proper spacing throughout document

### DOCX Features
- **Word Compatibility**: Full compatibility with Microsoft Word
- **Embedded Charts**: Charts saved as high-resolution images
- **Template Styling**: Professional document template
- **Editable Format**: Can be further customized in Word

## Troubleshooting

### Common Issues

1. **LaTeX Not Found (PDF)**
   ```bash
   quarto install tinytex
   ```

2. **Missing R Packages**
   ```r
   install.packages(c("dplyr", "readr", "plotly", "knitr", "DT", "htmltools", "ggplot2"))
   ```

3. **Chart Rendering Issues**
   - Ensure visualization_functions.R is in correct location
   - Check data file paths in report.qmd
   - Verify all required data columns exist

4. **Parameter Parsing Errors**
   - Ensure JSON syntax is correct in parameter strings
   - Check that parameter values match expected data
   - Verify country names exist in dataset

### Performance Optimization

- **Large Datasets**: Consider filtering data for faster rendering
- **Chart Complexity**: Simplify charts for PDF/DOCX formats
- **Memory Usage**: Monitor R memory usage during rendering
- **File Sizes**: Optimize image resolution for different formats

## Expected File Sizes

- **HTML**: 2-5 MB (includes embedded resources)
- **PDF**: 1-3 MB (static images, optimized for print)
- **DOCX**: 1-2 MB (embedded images, Word format)

## Quality Assurance

### Visual Inspection
1. Check chart clarity and readability
2. Verify color consistency across formats
3. Ensure professional appearance
4. Test on different devices/viewers

### Content Validation
1. Verify data accuracy in all formats
2. Check that parameters affect content correctly
3. Ensure all sections render completely
4. Validate cross-references and links

### Performance Testing
1. Measure rendering times for each format
2. Test with different parameter combinations
3. Verify memory usage stays reasonable
4. Check file size optimization

## Success Criteria

The multi-format report system is considered successful when:

- ✅ All three formats (HTML, PDF, DOCX) render without errors
- ✅ Interactive elements work in HTML, static alternatives in PDF/DOCX
- ✅ Parameterized generation works with different parameter sets
- ✅ Professional styling and branding consistent across formats
- ✅ Charts and visualizations display correctly in all formats
- ✅ Performance is acceptable for typical use cases
- ✅ Generated files meet quality standards for professional use

## Automation

For automated testing, create a script that:
1. Renders all formats with default parameters
2. Tests multiple parameter combinations
3. Validates output file existence and sizes
4. Checks for common error patterns
5. Generates test report summary

This ensures the multi-format system remains reliable across updates and changes.