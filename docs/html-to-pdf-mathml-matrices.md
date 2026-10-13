# MathML matrix layout

`mtable`, `mtr` and `mtd` use actual CSS table column and row allocation, with
the complete table centered on the selected font's MATH axis. The rules follow
[MathML Core's tabular math](https://www.w3.org/TR/2025/CR-mathml-core-20250624/#tabular-math).
Cell default padding uses the final selected font's x-height and em size.
`columnspan` and `rowspan` use the shared HTML table attribute and placement
validator. Spanning cells paint and contribute logical text once, in source order.

Separate-border matrices accept continuous point lengths for `border-spacing`,
cell padding and borders, CSS width and height constraints, `min()` sizes,
`box-sizing`, horizontal alignment and top, middle, bottom or baseline cell
alignment. Column spans include the internal column spacing; row spans use the
shared baseline allocator and include internal row spacing. Extra width goes
to automatic columns when any remain. Decorations use the allocated cell box,
so constraints and padding do not get applied twice.

```html
<div style="width:240pt">
  <math display="block">
    <mtable style="width:100%;border-spacing:0.5pt 0.375pt">
      <mtr><mtd columnspan="2"><mn>1234</mn></mtd></mtr>
      <mtr><mtd rowspan="2"><mn>5</mn></mtd><mtd><mn>6</mn></mtd></mtr>
      <mtr><mtd><mn>7</mn></mtd></mtr>
    </mtable>
  </math>
</div>
```

Block MathML resolves percentage matrix widths against its actual allocated
container content width. Initial intrinsic measurement retains the actual
styled source and its existing resource context only when the matrix needs
that width. Placement remeasures that source. An inline equation with an
indefinite percentage basis keeps its natural table width. A percentage inside
a default nested Math row remains indefinite, so its percentage table keeps
its natural width. Ordinary HTML token flow instead uses the actual allocated
cell content width. Its row height and baseline are measured after column
allocation, preserving the selected fonts, CSS whitespace and original logical
text on the visible glyph groups.

Ordinary-flow `mtext` retains its real source box. Math layout blockifies its
outer inline display, and the shared HTML layout measures CSS width, height,
min/max sizes, simple percentages and box sizing against the allocated cell.
The resulting whole box supplies its first actual content baseline and paints
its padding, borders and background once. The anonymous cell row retains its
allocated used width when percentage content reflows to a narrower painted box.
Preferred widths remain independent of the allocated width. True Math token glyph measurement uses its existing
selected-font metrics.

Fractions, radicals, native HTML text and nested matrices can participate in
spaced cells. Continuous spacing uses its authored length without quantizing it
to Chromium's pixel grid. Original full spacing and fraction comparison failures
remain preserved pending the complete MathML integration. Row border and image
painting, positioned or clipped cells, collapsed and fixed-layout tables, and
anonymous CSS table repair remain implementation work. Full milestone browser,
quality, coverage and performance acceptance has not been completed.
