
send_string_to_client = function (s, c)
    local old_c = client.focus
    client.focus = c
    for i=1, #s do
        local char = s:sub(i,i)
        root.fake_input('key_press'  , char)
        root.fake_input('key_release', char)
    end
    client.focus = old_c
end
