#show: srm-report.with(
  $if(title)$
    title: "$title$",
  $endif$
  $if(params.company)$
    company: "$params.company$",
  $endif$
)