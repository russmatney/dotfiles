-- from https://ch1p.io/awesome-reload-and-preserve-layout/
function table.indexof(t, needle)
  for k, v in pairs(t) do
    if v == needle then
      return k
    end
  end
  return -1
end
