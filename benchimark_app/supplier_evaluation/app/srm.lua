function Div(el)
  if el.classes:includes('discussion') then
    local blocks = pandoc.List({
      pandoc.RawBlock('typst', '#columns(2)[')
    })
    blocks:extend(el.content)
    blocks:insert(pandoc.RawBlock('typst', ']\n'))
    return blocks
  end
end