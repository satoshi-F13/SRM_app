#let srm-report(
  title: "title",
  company: none, // Add the company parameter here. "none" is important.
  body,
) = {
set text(
      font: "Verdana",
      size: 12pt,
    )

// Set up font information for the caption.
 show figure.where(  
                   kind: image
      ): set figure.caption(position: bottom) // postion of caption
      // A function to change caption title as "Image".
        show figure.where(kind: image): set figure(supplement: none)
        show figure.caption: set align(left)  // Left-aligns the caption
        show figure.caption: set text(
      size: 10pt,     // Change size (e.g., 9pt, 10pt, 11pt)
      fill: blue     // Change color (e.g., blue, red, rgb("33B1FF"))
      )


set page(
      "us-letter",
      margin: (left: 0.5in, right: 0.5in, top: 1.5in, bottom: 1in),
      header:       
  // Define the header for the first page
             context { 
    if (counter(page).get().at(0) == 1) {
      // First page header with grid layout
      grid(
        columns: (70%, 30%),
        rows: 30pt,
        gutter: 10pt,
        // Title as text
        align(left, text(size: 25pt, fill: rgb("#0D1C12"), weight: "bold", title)),
        // Insert box
        box(width: 50pt,height: 50pt), // Empty box-space
        // Insert Parameter 2
        grid.cell(colspan: 1,align: top, text(size: 20pt, fill:rgb("#7C563A") ,company)),

      )
        //Insert horizontal line
        place(
            dx: -1in,  // Offset by left margin
            dy: -0.3em, // Adjust vertical position as needed
            rect(
              width: 140%, // Add margins to both sides
              height: 0.1in,
              fill: rgb("#61A60E")
            )
          )
    } else if (counter(page).get().at(0) == counter(page).final().at(0)) {
      // Last page header
      grid(
        columns: (30%, 40%,30%),
        align: auto,
        // company
        place(dy: -0.5in, text(size: 20pt, fill: rgb("#7C563A"), weight: "bold", company)),
        // Title
        place(dy: -0.5in,center, text(size: 15pt, fill: rgb("#0D1C12"), weight: "light", title)),

      )
      place(
            dx: -1in,  // Offset by left margin
            dy: -0.15in, // Adjust vertical position as needed
            rect(
              width: 140%, // Add margins to both sides
              height: 0.1in,
              fill: rgb("#002900")
            )
          )
    } else {
      
      // Regular header for other pages
     grid(
        columns: (30%, 40%,30%),
        rows:auto,
        // company
        place(dy: -0.5in, text(size: 20pt, fill: rgb("#2E2B70"), weight: "bold", company)),
        // Title
        place(dy: -0.5in,center, text(size: 15pt, fill: rgb("#0D1C12"), weight: "light", title)),

      )
      place(
            dx: -1in,  // Offset by left margin
            dy: -0.15in, // Adjust vertical position as needed
            rect(
              width: 140%, // Add margins to both sides
              height: 0.1in,
              fill: rgb("#B2B2B2")
            )
          )
    }
  },
      footer: {
            align(top, line(length: 100%, stroke: (thickness: 1pt, paint: rgb("#A6AEBF"))))
          context { if(counter(page).get().at(0)== 1) {
          grid(
            columns: (30%, 40%, 30%),
            gutter: 10pt,
            align(left, ""),
            align(center , text(fill: rgb("#aaaaaa"))[INTERNAL USE ONLY])
          )
  } else {
    // Define the header for all other pages
              grid(
            columns: (30%, 40%, 30%),
            gutter: 10pt,
              align(left , 
                  context [
                            #set align(left)
                            #set text(10pt)
                            #counter(page).display(
                                                  "1 / 1",
                                                  both: true,
                                                   )
                          ]),
            place(dy: 0em,center , text( fill: rgb("#aaaaaa"))[INTERNAL USE ONLY]),
          )
          }
        }
      }
    )
    
 text(size: 16pt, weight: "black")[Key Findings]

 grid(
    columns:(1.2fr,1fr, 1fr,1fr),
    rows: (auto),
    gutter:20pt,
    align: horizon,
    grid.cell(
      colspan: 1,        // Spanning one column
      align: horizon,
      [  // Insert the function by block 
  #grid.cell(text(size: 25pt, weight: "black", fill: rgb("#61A60E") )[GENERAL])
  ] 
    ),
  grid.cell(
    colspan: 1,
    align: left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("61A60E"))[100],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[points for evaluation scores. ]
      )
    ]
  ),
    grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("61A60E"))[ROOT],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[for the industry relevance for the expo.]
      )
    ]
  ),
    grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("61A60E"))[30],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[of competitors' presences.]
      )
    ]
  ),
   grid.cell(
      colspan: 1,        // Spanning one column
      align: horizon,
       [  // Insert the function by block 
  #grid.cell(text(size: 25pt, weight: "black", fill: rgb("#7C563A") )[LEAD])
  ] 
    ),
  grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("#7C563A") )[30%],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[of decision maker contacts from leads.]
      )
    ]
  ), 
  grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("#7C563A"))[30%],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[of new connections from leads.]
      )
    ]
  ), 
  grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("#7C563A"))[30%],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[of meeting conversion rate from leads.]
      )
    ]
  ), 
  grid.cell(
      colspan: 1,        // Spanning one column
      align: horizon,
       [  // Insert the function by block 
  #grid.cell(text(size: 25pt, weight: "black", fill: rgb("#7AB8D7") )[COST])
  ] 
    ),
      grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("#7AB8D7"))[30%],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[for ROI percentage. ]
      )
    ]
  ), 
    grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("#7AB8D7"))[30€],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[for cost per meeting.]
      )
    ]
  ), 
    grid.cell(
    colspan: 1,
    align:left + top,
    [
      #stack(
        spacing: 10pt,
        text(size: 30pt, weight: "bold", fill: rgb("#7AB8D7"))[3000€],
        text(size: 11pt, weight: "black", fill: rgb("#aaaaaa"))[for total participating cost.]
      )
    ]
  ), 
    )
  
  #align(top, line(length: 100%,stroke: (thickness: 0.3pt, paint: rgb("#A6AEBF"))))


  #grid(
  columns: (30%, 40%,30%),
  rows: (auto,auto, auto),
  gutter: 10pt,
  grid.cell(text(size: 15pt, weight: "black" )[About Event] ),
  grid.cell(text(size: 11pt)[#lorem(40)],  colspan: 2),
  grid.cell(colspan: 3,align(line(length: 100%,stroke: (thickness: 0.3pt, paint: rgb("#A6AEBF"))))),
  grid.cell(text(size: 15pt, weight: "black" )[Purpose of participant] ),
  grid.cell(text(size: 11pt)[#lorem(50)],  colspan: 2),
  grid.cell(colspan: 3,align(line(length: 100%,stroke: (thickness: 0.3pt, paint: rgb("#A6AEBF"))))),
  grid.cell(text(size: 15pt, weight: "black" )[Conclusion] ),
  grid.cell(text(size: 11pt)[#lorem(60)],  colspan: 2),

)

#pagebreak()

// Regular Page
// the 1st
 #place(dy: -0.5em)[
  #grid(
  columns: (35%, 35%,30%),
  rows: (auto,auto,auto),
  gutter: 10pt,
// Analysis 1
  grid.cell(align: left, text(size: 15pt, weight: "medium", fill: rgb("#002900") )[Performance Overview] ),
  grid.cell(colspan:2, rowspan: 3, align: center, image("bar_leads.png", width: 100%, height: 25%)),
  grid.cell(colspan:1, align: horizon, text(size: 20pt, fill: rgb("#61A60E"))[params_3]  ),
  grid.cell(colspan: 1, align: left + horizon, text(size: 12pt)[Average normalized performance score.]),
  grid.cell(colspan: 3,align(line(length: 100%,stroke: (thickness: 0.3pt, paint: rgb("#A6AEBF"))))),
// Analysis 2
  grid.cell(align: left, text(size: 15pt, weight: "medium", fill: rgb("#002900") )[Engagement Funnel] ),
  grid.cell(colspan:2, rowspan: 3, align: center, image("quadrat_plot.png", width: 100%, height: 20%)),
  grid.cell(colspan:1, align: horizon, text(size: 20pt, fill: rgb("#61A60E"))[params_4],  ),
  grid.cell(colspan: 1, align: left + horizon, text(size: 12pt)[Input the explanation of insights up to 20 words.]),
  grid.cell(colspan: 3,align(line(length: 100%,stroke: (thickness: 0.3pt, paint: rgb("#A6AEBF"))))),
// Analysis 3
  grid.cell(align: left, text(size: 15pt, weight: "medium", fill: rgb("#002900") )[ Cost Efficiency] ),
  grid.cell(colspan:2, rowspan: 3, align: center, image("radar_plot.png", width: 100%, height: 20%)),
  grid.cell(colspan:1, align: horizon, text(size: 20pt, fill: rgb("#61A60E"))[params_5],  ),
  grid.cell(colspan: 1, align: left + horizon, text(size: 12pt)[Total cost for this event.]),
  grid.cell(colspan: 3,align(line(length: 100%,stroke: (thickness: 0.3pt, paint: rgb("#A6AEBF"))))),
// Analysis 4
  grid.cell(align: left, text(size: 15pt, weight: "medium", fill: rgb("#002900") )[Network & Media Value] ),
  grid.cell(colspan:2, rowspan: 3, align: center, image("relevance_score_plot.png", width: 100%, height: auto)),
  grid.cell(colspan:1, align: horizon, text(size: 20pt, fill: rgb("#61A60E"))[params_6],  ),
  grid.cell(colspan: 1, align: left + horizon, text(size: 12pt)[Overviews about networking and media values. ])

)
]

#pagebreak()



#text(size: 16pt, weight: "black")[Additional Note]
 
#text([#lorem(50)])



