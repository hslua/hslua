function Pandoc (doc)
  local blocks = pandoc.List()
  for k, blk in ipairs(doc.blocks) do
    if blk.t == 'Header' and blk.level > 1 then
      break
    end
    blocks:insert(blk)
  end
  local fh = io.open(tostring(doc.meta.bodyfile))
  local new_body = pandoc.read(fh:read('a')).blocks
  -- remove hackage badge
  new_body:remove(1)
  blocks:extend(new_body)
  fh:close()
  return pandoc.Pandoc(blocks, doc.meta)
end
