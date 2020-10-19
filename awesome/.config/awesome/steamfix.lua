
client.connect_signal("property::position", function(c)
     if c.class == 'Steam' then
         local g = c.screen.geometry
         if c.y + c.height > g.height then
             c.y = g.height - c.height
             -- naughty.notify{
             --     text = "restricted window: " .. c.name,
             -- }
         end
         if c.x + c.width > g.width then
             c.x = g.width - c.width
         end
     end
 end)
