local function is_badges_para (blk)
  return blk and blk.t == 'Para' and blk.content[1]
    and blk.content[1].t == 'Link'
    and blk.content[1].content[1] and blk.content[1].content[1].t == 'Image'
end

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
  if is_badges_para(new_body[1]) then
    new_body:remove(1)
  end
  blocks:extend(new_body)
  fh:close()
  return pandoc.Pandoc(blocks, doc.meta)
end
