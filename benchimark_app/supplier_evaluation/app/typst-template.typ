#let srm-report(
  title: "title",
  company: none, // Add the company parameter here. "none" is important.
  body,
) = {
 set text(
    font: "Verdana",
    size: 12pt,
  )
  set page(
    "a3",
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
    footer: align(
      grid(
        columns: (40%, 60%),
        align(horizon,
              text(fill: rgb("51B464"),
              size: 12pt,
              counter(page).display("1"))),
        align(right,
              image("asset/sample_logo.png",
              height: 50%)),
      )
    )
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
  
// Placeholders (for Quarto charts
    body
    
// Key Findings 1
    grid(
    columns:(1fr, 1fr),
    rows: 180pt,
    gutter:10pt,
    grid.cell(
      colspan: 1,        // Spanning one column
      align: center,
      image("asset/location-map.png") // Add image    
    ),
    grid.cell(
      colspan: 1,        // Spanning one column
      align: center,
      image("asset/radar_plot.png") // Add image
    )
    )
    // Place a single line
     box(width: 1fr, line(length: 100%, stroke: luma(180)))  
     
 //  Key Findings 2   
     grid(
    columns:(1fr,1fr,1fr, 1fr),
    rows: auto,
    gutter:5pt,
    grid.cell(
      colspan: 1,        // Spanning one column
      align: right + top,
      image("asset/score_plot.png") // Add image    
    ),
    grid.cell(
      colspan: 3,        // Spanning one column
      align: left + horizon,
      image("asset/horizontal_plot.png") // Add image
    )
    )
    // Place a single line
     box(width: 1fr, line(length: 100%, stroke: luma(180)))  
// Key Findings 3    
    grid(
      columns: (130pt,1fr,1fr, 1fr),
      rows: (100pt,100pt),
      gutter: 15pt,
      grid.cell(colspan: 1,align: center + horizon, text(size: 20pt, fill:green ,"Overview")),
      grid.cell(
        colspan: 1,
        align: horizon,
        image("asset/total_supplier_number.png"),
               ),
      grid.cell(
      colspan: 1,
      align: horizon,
      image("asset/rank_plot.png"),
               ),
      grid.cell(
      colspan: 1,
      align: horizon,
      image("asset/distance_plot.png"),
              ),
      grid.cell(colspan: 1,align: center + horizon, text(size: 20pt, fill:green ,"Category")),
      grid.cell(
      colspan: 2,
      align: horizon,
      image("asset/sup_category.png"),
               ),
      grid.cell(
      colspan: 1,
      align: horizon,
      image("asset/tec_level_plot.png"),
                ),

    )
      box(width: 1fr, line(length: 100%, stroke: luma(180)))
      
// Comparison amog group
         grid(
    columns:2,
    rows: auto,
    gutter:10pt,
    grid.cell(
      colspan: 1,        // Spanning one column
      image("asset/parallel_plot.png", width: 100%) // Add image    
    ),
    grid.cell(
      colspan: 1,        // Spanning one column
      image("asset/plot_cat.png", width: 100%) // Add image
    )
    )

}