renderSearchGeneric :: Site -> Html -> Params -> Pagination -> Maybe Text  -- comment
                    -> (a -> AttributeValue) -- linkItem function
                    -> [a] 
                    -> ItemRendering a ViewMode
                    -> (  Html   -- ^ page title
                       -> Html   -- ^ content
                       -> Html   -- ^ sidebar 
                       -> Page a
                       )
                    -> Html  -- ^ sidebar
                    -> Html  -- ^ page title
                    -> ActionM ()
