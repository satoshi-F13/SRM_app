#let report(
  title: none,
  author: none,
  date: none,
  company: none, // Add the company parameter here
  aspect: "portrait",
  flipped: false,
  lang: "en",
  region: "US",
  font: "Verdana",
  fontsize: 12pt,
  body,
) = {
// Text
  set text(
  lang: lang,
  region: region,
  font: font,
  size: fontsize
)
  set page(
    "a4",
    margin: (left: 1in, right: 1in, top: 0.7in, bottom: 1in),
    background: place(top, 
                      rect(fill: rgb("51B464"),
                      width: 100%,
                      height: 0.5in)),
    header: align(
      horizon,
      grid(
        columns: (80%, 20%),
        align(left,
              text(size: 20pt,
              fill: white,
              weight: "bold",
              title)),
        align(right,
              text(size: 12pt,
              fill: white,
              weight: "bold",
              company)),
      ),
    ),
  )
// Configure headings.
  show heading.where(level: 1): set block(below: 0.8em)
  show heading.where(level: 1): underline
  show heading.where(level: 2): set block(above: 0.5cm, below: 0.5cm)
  
// Paragraphs
  set par(
  justify: false,
  linebreaks: "optimized"
  )  
  
// Define a style for links
  show link: set text(fill: rgb("#800080"))

  

// Placeholders for Quarto charts
body
  grid(
    columns: 2,          // Define two columns
    rows: auto,          // Automatically determine row count
    gutter: 10pt,        // Space between grid cells
    grid.cell(
     colspan: 1,        // Spanning one column
     include( "paragraph.txt")   // Include text from file
      
    ),
    grid.cell(
      colspan: 1,        // Spanning one column
      image("asset/radar_test.png", width: 100%) // Add image
    )
  )


  grid(
  columns: (130pt,1fr,1fr, 1fr),
  rows: (100pt,100pt, 100pt,auto),
  gutter: 15pt,
  grid.cell(
    colspan: 1,
    image("asset/chart1-1.png", width: 100pt, height: 100%),
  ),
    grid.cell(
    colspan: 1,
    image("asset/chart2-1.png", width: 100pt, height: 100%),
  ),
    grid.cell(
    colspan: 1,
    image("asset/chart3-1.png", width: 100pt, height: 100%),
  ),
    grid.cell(
    colspan: 1,
    image("asset/chart4-1.png", width: 100pt, height: 100%),
  ),
  grid.cell(
    colspan: 1,
    align: center + horizon, 
    text(size: 20pt, fill:green ,"hello world")),
    grid.cell(
    colspan: 1,
    image("asset/chart5-1.png", width: 100pt, height: 100%),
  ),
  )
  box(width: 1fr, line(length: 100%, stroke: luma(180)))

}
