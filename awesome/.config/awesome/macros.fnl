{:with-selected-tag (fn [name body1 ...]
                      `(let [,name (. mouse.screen :selected_tag)]
                         (when ,name
                           ,body1
                           ,...)))}
